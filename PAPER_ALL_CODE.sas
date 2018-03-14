/***************************************************************
* OMSI_ALL_CODE.sas - Produce examples from BB-22 paper in SAS
***************************************************************
* Author: Korchak, Andrii
***************************************************************
* Input: "your_folder_name/mental-heath-in-tech-2016_20161114.csv"
* 
* Output: WORK.raw_ds  - file with demographic information from OMSI survey
*         html tables from PROC Print
***************************************************************/

*** reference to file with data ***;
filename csv_srs "/folders/myfolders/conf/Library/mental-heath-in-tech-2016_20161114.csv";

*** We cannot read columns names - we are reading only data.( datarow = 2, getnames = no ) ***; 
proc import datafile = csv_srs
	             out = raw_ds
	            dbms = csv replace ;
	guessingrows = MAX ;
	getnames = no  ;
	datarow=2;	
run;

*** reading first row with column names    ***;
*** writing attributes into temporary file ***;
filename code temp;
data  _NULL_ ;
  file code;
  infile csv_srs DELIMITER=',' DSD  obs=1 ;
  input label :$200. @@ ;
  put 'var' _N_ " label='" label  "'";
run;

*** apply labels ***;
options source2;
proc datasets lib=work noprint NODETAILS NOLIST;
  modify raw_ds;
    attrib %include code;;
  run;
quit;

*** list results ***;
title 'List Variable Attributes';
proc contents data=raw_ds;
quit;

*** Selecting some Demographic data;
data demog2;
   set raw_ds(
   rename = (
   VAR56 = AGE       
   VAR57 = GENDER    
   VAR63 = REMOTELY
   VAR4  = IT_ROLE)) ;
   
   keep  AGE GENDER REMOTELY IT_ROLE ;
run;

***Starting transforming our data ***;
data transform1;
	
	set demog;
	
	*** Converting Character Age into numeric values ***;
	AGEN = input(AGE, best.);
    if AGEN > 90 or AGEN < 10 then AGEN = . ;
    
	*** Converting Free-Text Gender into 3 groups ***;
	* all data into lower case to reduce diversity *;
	gender_l = lowcase(GENDER);

	length GENDER_O GENDER_M GENDER_F GENDER_C $10. ;

	if gender_l in ( "male (cis)","cis man","cis male","cis male","sex is male","male.","cis male","man","male","guy","cisdude", "dude", "dude", "m", "ml", "m|", "mail", "malr") 
	or gender_l = "i'm a man why didn't you make this a drop down question. you should of asked sex? and i would of answered yes please. seriously how much text can this take?" 
	   then GENDER_M = "Male";

    if gender_l in ("cis female","female assigned at birth","woman","female","female","i identify as female.","female or multi-gender femme","female/woman","cisgender female"," female","female (props for making this a freeform field, though)","f", "fem","fm","female-bodied; no feelings about gender","cis-woman")
    	then GENDER_F = "Female";

	if gender_l in ("genderflux demi-girl","bigender","non-binary","transitioned, m2f","genderfluid (born female)","other/transfeminine","androgynous","male 9:1 female, roughly","n/a","other","nb masculine","none of your business","genderqueer","human","genderfluid","enby","genderqueer woman","mtf","queer","agender","fluid","male/genderqueer","nonbinary","unicorn","male (trans, ftm)","afab","transgender woman" ) 
		then GENDER_O = "Other";
	
	* Checking if data mapped correctly *;
	if cmiss(GENDER_O, GENDER_M, GENDER_F) = 1 then put "WARN" "NING: Wrong mapping" GENDER_O= GENDER_M= GENDER_F= gender_l= ;
	if cmiss(GENDER_O, GENDER_M, GENDER_F) = 3 and not missing( gender_l ) 
	   then put "WARN" "NING: Wrong mapping" GENDER_O= GENDER_M= GENDER_F= gender_l= ;
	
	* Creating new grouping variable for GENDER *;
	GENDER_C = catx('',GENDER_O, GENDER_M, GENDER_F);
run;

*** Sorting the data before FREQ & MEANS Procedures ***;
proc sort data = transform1 out = transform1_sort  ;
	by GENDER_C ;
run;

*** Macro to check if parameter is missing ***;
*** Additional information can be found at http://changchung.com/download/022-2009.pdf ***;
%macro isBlank(param);
 %sysevalf(%superq(param)=,boolean)
%mend isBlank; 

*** Proc FREQ to calculate count and percent ***;
%macro loop_freq(var = , by_var = , where = ,ds = transform1_sort);
	proc freq data = &ds. noprint;
	   tables &var. / out= &var._output missing;
	   %if &by_var. ne %then by &by_var. ;;
	   %if not %isBlank(&where.) %then where &where. ;;
	run;	
%mend loop_freq;

options mprint;
%loop_freq(var = GENDER_C, by_var = )*, where = %str( not missing(GENDER_C) ));
%loop_freq(var = IT_ROLE, by_var = )*,  where = %str( IT_ROLE in (1,0) )    ) ;
%loop_freq(var = REMOTELY, by_var =)* , where = %str( REMOTELY in ("Always", "Never", "Sometimes" )));

*** PROC SUMMARY to analyse numeric variables ***;
proc summary data = transform1;
	var AGEN  ;
	output out = age_out;
run;

*** This step is required to calculate BigN ***;
proc sql noprint;
	select count(1)  into: bign
	from demog;
quit;

*** Transforming data from wide to long fomat ***;
proc transpose data = AGE_OUT out = AGE_OUT_T;
	by _type_ _freq_;
	id _stat_; 
run;

*** Combining all rows together ***;
data outcome;
	length PARAM $20. CAT $50. VALUE $20. ;
    
	set GENDER_C_output ( in = in_g   )
	    AGE_OUT_T       ( in = in_age )                        
	    IT_ROLE_output  ( in = in_it  )
	    REMOTELY_output ( in = in_rem ) ;
	                          
	   if in_g        then do; 
	        CAT = "Gender";
	        PARAM = GENDER_C;
	   end;
	   else if in_it  then do;
	   	    CAT = "Tech\IT Role?";
	   	    PARAM = IT_ROLE ;
	   	    if missing(IT_ROLE) then PARAM = "NA";
	   end;
	   else if in_rem then do; 
	       CAT = "Working Remotely?";
	       PARAM = REMOTELY;
	   end;
	   else if in_age then do;
	       CAT = "Age";
	   end;
	   
	   *** Changing appearance of missing values **;
	   if missing(PARAM) then PARAM = "NA";
	   
	   if in_age then do;
	   		PARAM = "N";
	   		VALUE = put(N,4.);
			output;
	   		PARAM = "Mean(SD)";
	   		VALUE = put(MEAN,5.1)||"("||put(STD,4.1)||")";
			output;
	   		PARAM = "Min-Max";
	   		VALUE = put(MIN,4.)||" - "||put(MAX,4.);
			output;
	   end;
	   else do;
	       VALUE = put(COUNT,4.)||"("||put(COUNT/&BIGN.*100,4.1)||"%)";
	       output;
	   end;
run;

*** Printing the results ***;
title "Demographic Information";
proc report data = outcome split='@';
	columns CAT PARAM VALUE  ;
	define CAT   /"Question"  order;
	define PARAM /"Parameter" display;
	define VALUE / "All responses@(N = &bign.)" display;

	compute after CAT;	line ' '; endcomp;
run;
