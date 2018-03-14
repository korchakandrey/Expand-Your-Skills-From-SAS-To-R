/***************************************************************
* OMSI_all_code.sas - Demog from OMSI survey, BB-22 paper in SAS
***************************************************************
* Author: Korchak, Andrii
***************************************************************
* Input: "your_folder_name/mental-heath-in-tech-2016_20161114.csv"
* 
* Output: WORK.raw_ds  - file with demographic information from OMSI survey
*         html tables from PROC Print
***************************************************************/
filename csv_srs "/your_folder_with_data/mental-heath-in-tech-2016_20161114.csv";

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
proc datasets lib=work ;
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
   VAR58 = COUNTRY   
   VAR59 = US_STATE  
   VAR60 = W_COUNTRY 
   VAR61 = W_US_STATE
   VAR62 = POSITION   
   VAR63 = REMOTELY
   VAR4  = IT_ROLE)) ;
   
   keep  AGE GENDER COUNTRY US_STATE W_COUNTRY W_US_STATE POSITION REMOTELY IT_ROLE ;
run;
