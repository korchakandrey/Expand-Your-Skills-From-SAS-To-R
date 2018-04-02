/***************************************************************
* BB_22_all_codes.sas - Produce examples from BB-22 paper in SAS
***************************************************************
* Author: Korchak, Andrii
***************************************************************
* Input: "your_folder_name/mental-heath-in-tech-2016_20161114.csv"
* 
* Output: WORK.raw_ds  - file with demographic information from OMSI survey
*         html tables from PROC Print
***************************************************************/
*** ## Table 1 - reading text data in. ;
**** 1.	Getting data **** ;
data dm;
	input SUBJID AGE POFL $;
	datalines;
1 23 Y
2 45 Y
3 61 N
;
run;
title "Data from &syslast.";
proc print data=dm;
run;
proc contents data=dm;
run;

*** ## Table 2 - reading CSV data in. ;
libname mylib "/folders/myfolders/conf/Library";
filename csv_srs "/folders/myfolders/conf/Library/mental-heath-in-tech-2016_20161114.csv";

*** We cannot read columns names - we are reading only data. ***; 
proc import datafile = csv_srs
	             out = raw_ds
	            dbms = csv replace ;
	guessingrows = MAX ;
	getnames = no  ;
	datarow = 2;	
run;

*** To apply meaningful labels we are reading first row, and saving it into the temporary text file ***;
filename code temp;
data  _NULL_ ;
  file code;
  infile csv_srs DELIMITER=',' DSD  obs=1 ;
  input label :$200. @@ ;
  put 'var' _N_ " label='" label  "'";
run;

* apply labels *;
options source2;
proc datasets lib=work ;
  modify raw_ds;
    attrib %include code;;
  run;
quit;

* list results *;
title 'List Variable Attributes';
proc contents data=raw_ds;
quit;

* Selecting Demographic data;
data mylib.demog2;
   set raw_ds(
   rename = (
   VAR56 = AGE       
   VAR57 = GENDER    
   VAR58 = COUNTRY   
   VAR59 = US_STATE  
   VAR60 = W_COUNTRY 
   VAR61 = W_US_STATE
   VAR62 = POSITION   
   VAR63 = REMOTELY
   VAR4  = IT_ROLE)) ;
   
   keep  AGE GENDER COUNTRY US_STATE W_COUNTRY W_US_STATE POSITION REMOTELY IT_ROLE ;
run;

%macro dummy;
*** Typical Data Flow Example **;
proc sort data = row_data;
	by BY_VAR1 descending BY_VAR2;
	where STAT ~="NOT DONE";
run;

data output_ds ( drop = ( VAR1 ) );
	set row_data (  keep = ( BY_VAR1  BY_VAR2 VAR1 )
	              rename = ( LOG_VAR1 = AVAL )
                 );
	by BY_VAR1 descending BY_VAR2;
	
	if first.BY_VAR1 then BFL = "Y" ;
	
	LOG_VAR1 = log( VAR1 );
run;
%mend dummy;

*** Example 2.2.1 ***;
*** keep option ***;
data dm_k;
   set dm( keep = SUBJID AGE);
run;
title "Data from &syslast.";
proc print;
run;
***    or by drop option ***;
data dm_d; 
  set dm ( drop = POFL);
run;
title "Data from &syslast.";
proc print; 
run;
*** keeping all columns between SUBJID and POFL ***;
data dm_k_betw;
  set dm ;
  keep SUBJID:POFL;
run;
title "Data from &syslast.";
proc print;
run;
*** Selecting all variables that starts with 'AGE' ***;
data dm_k_st;
  set dm ;
  keep AGE: ;
run;
title "Data from &syslast.";
proc print data = dm_k_st;
run;

*** Renaming variables ***;
data dm_rename ;
  set dm ;
  rename SUBJID = PATNUM;
run;
title "Data from &syslast.";
proc print data = dm_rename;
run;

*** Filtering data ***;
data dm_i;
  set dm;
  if POFL = "Y" and AGE > 21;
run;
title "Data from &syslast.";
proc print ;
run;

*** or use WHERE statement ***;
data dm_w;
  set dm;
  where POFL = "Y" and AGE > 21;
run;
***Both variants produce the same result but WHERE may be faster.***;
title "Data from &syslast.";
proc print;
run;

*** 2.4 Crreating new variables ***;
*** Creting new Varibles ***;
data df_new_var;
  set dm nobs=n_obs;
  N_ROWS = n_obs;
  col4 = ifc(_N_ >= n_obs/2,">=n/2","<n/2","missing");
  USUBJID = "A00-"||put(SUBJID,3.-l);
  drop SUBJID;
run; 
title "Data from &syslast.";
proc print;
run;

*** 2.5 Sorting Data ***;
*** 2.5.1 Sorting data ;
*** and saving result into new dataset***;
proc sort data=dm out= dm_sort;
	by 	POFL;
run;
title "Data from &syslast.";
proc print;
run;

*** 2.5.1 Sorting data ;
*** descending sort ;
proc sort data=dm out= dm_sort_desc;
	by POFL descending SUBJID;
run;
title "Data from &syslast.";
proc print;
run;

*** 2.5 By Groupping ***;
********* Grouping Variables ***;
data vs;
	input SUBJID HT WT PERIOD ;
	datalines;
001 180 . 1	
001 181 77 2	
002 173 85 1	
002 173 83 2	
;
run;
title "Data from &syslast.";
proc print;
run;

proc means data = vs;
	var HT WT;
	output out = vs_s;
	by SUBJID;
run;

**** LAG function *******************;
data vs_lag;
	set vs ( keep = SUBJID HT);
	by SUBJID;
	LAG_HT = lag( HT );
	if first.SUBJID then LAG_HT = .;
run;
title "Lag in SAS, Data from &syslast.";
proc print;

data advs ;
	merge vs( in = in_x )
	      dm( in = in_y );
	by SUBJID ;
	* inner join;
	if in_x and in_y ;
	keep HT WT SUBJID POFL;
run;
title  "Combined Data Set,";
title1 "Data from &syslast.";
proc print;
run;
