##################################################################
# OMSI_all_codes.R - Demog from OMSI survey, BB-22 paper in SAS
##################################################################
# Author: Korchak, Andrii
##################################################################
# Input: "your_folder_name/mental-heath-in-tech-2016_20161114.csv"
# 
# Output: csv_file  - data frame with demographic information from OMSI survey
#         results from print() function
##################################################################

require(dplyr)

### Setting Working Directory
setwd("your_folder_name/");

### Reading data into the system
csv_file<-read.csv("mental-heath-in-tech-2016_20161114.csv", stringsAsFactors = FALSE)

### Checks for Data Frame dimentions
dim(csv_file)

### Getting Vector of Column Names
names(csv_file)
### pick only necessary Column Names
names(csv_file[,c(4,56:63)])


### Selecting only necessary vars for a Demographic Table
### contains() & ends_with() functions allow to select columns without typing long names
demog <- csv_file %>% select(contains("remotely",ignore.case = TRUE),
                             contains(".age.",ignore.case = TRUE),
                             contains(".gender.",ignore.case = TRUE),
                             ends_with(".IT.")
                             )
### Also, this task can be done by rename function
names(demog) <- c("REMOTE","AGE","GENDER","IT_ROLE")
str(demog)

### Preparing variants of gender Groups;
### we will devide gender by 3 main groups: Female, Male and All The Other responders

### Creating Vectors with reference values
OTHER <- c("genderflux demi-girl","human","male 9:1 female, roughly"
           ,"male (trans, ftm)","genderqueer woman","other","afab","agender","androgynous"
           ,"bigender","enby","genderqueer","mtf","n/a","nb masculine","non-binary","nonbinary"
           ,"none of your business","unicorn","queer","transitioned, m2f","genderfluid (born female)"
           ,"other/transfeminine","genderfluid","fluid","male/genderqueer","transgender woman" )

FEMALE <- c("cis female","female assigned at birth","woman","female","female"
  ,"i identify as female.","female or multi-gender femme","female/woman"
  ,"cisgender female"," female","female (props for making this a freeform field, though)"
  ,"f", "fem","fm","female-bodied; no feelings about gender","cis-woman")

MALE <- c("male (cis)","cis man","cis male"
  ,"i'm a man why didn't you make this a drop down question. you should of asked sex? and i would of answered yes please. seriously how much text can this take?"
  ,"cis male","sex is male","male (cis)","male.","cis male","man","male","guy","cisdude", "dude", "dude",
  "m", "ml", "m|", "mail", "malr"  )

### Creating condition to separate GENDER values by groups.  
### Later case_when will be used as argument in mutate_() function.

case_when <- list(GENDER_C = ~  case_when( GENDER %in% FEMALE ~ "Female",
                                           GENDER %in% MALE   ~ "Male",
                                           GENDER %in% OTHER  ~ "Other"  ))

#### Differense between 'mutate()' and 'mutate_()' in Evaluation time. 
####   The second function uses standard evaluation (SE) - you can use other R objects inside it.
#### On the other hand in 'mutate()' function  uses Non Standard Evaluation(NSE).
####   It allows to access columns by both Names and Character Strings.
#### The same Pattern is used in filter_(), rename_() and other functions.
#### More about NSE can be fount here http://adv-r.had.co.nz/Computing-on-the-language.html


### Cleaning the data
demog <- demog %>% mutate(  AGEN = if_else( AGE > 90 | AGE < 10 , true = NA_integer_, false =AGE )) %>% 
                   mutate( GENDER = trimws(tolower(GENDER)),
                           BIG_N = n()     ) %>% 
                   mutate_( .dots =  case_when ) %>% select( BIG_N, current_vars(), - GENDER, -AGE)
head(demog)

### Apply table() function to each selected cloumn from demog data frame - we are getting counts
stable <- sapply(demog[,c("REMOTE", "IT_ROLE", "GENDER_C")],table,useNA = "ifany")

### Converting results of table() function into data frames
df_table  <- data.frame (Var = as.character(names(stable$REMOTE)),   Freq = as.vector(stable$REMOTE)   , param = "Working Remotely?" , stringsAsFactors = FALSE)
df_table1 <- data.frame (Var = as.character(names(stable$GENDER_C)), Freq = as.vector(stable$GENDER_C) , param = "Gender"            , stringsAsFactors = FALSE)
df_table2 <- data.frame (Var = as.character(names(stable$IT_ROLE)),  Freq = as.vector(stable$IT_ROLE)  , param = "It/Tech Role?"     , stringsAsFactors = FALSE)

### calculating statistics for AGE - getting n, mean, sdt, min, max
s_mean <- demog %>% select( AGEN ) %>% filter( ( !is.na(AGEN)) ) %>% 
  summarise(  "N"        = n(),
              "Mean(SE)" = paste0(round(mean(AGEN,na.rm = TRUE),1),"(",round(sd(AGEN,na.rm = TRUE),1),")"),
              "Min-Max"  = paste0(min(AGEN,na.rm = TRUE)," - ",max(AGEN,na.rm = TRUE))
           )
### Transposing the data by t() function, and converting results into data frame
df_table3 <- as.data.frame(t(s_mean), stringsAsFactors = FALSE) %>% tibble::rownames_to_column() %>%
              mutate( param = "Age") %>%  rename( Var = rowname , Value = V1) %>% select( param , Var, Value )


### To make an output readable we are creating a blank row. Later it will be added between blocks in the output
df_table_blank <- data.frame (Var = "",  Freq =NA_integer_, param = "", stringsAsFactors = FALSE)

### Copy Big_N into variable
big_n <- demog$BIG_N[1]
### Prepearing title
header <- paste0("All responses (N = ",big_n,")")

### Final step: combining together data from table() and summary() functions. Adding blank rows beetwen blocks
d_all_tabs <- bind_rows(df_table_blank, df_table1, df_table_blank, df_table2, df_table_blank, df_table )


### To compute propper table you need to use specialized packeges such as knit or stargazer

### Calcuulating 'Value' for output and printing the result table
d_all_tabs %>% select(param, Var, Freq) %>%
  mutate(Value = if_else(!is.na(Freq),paste0( round(Freq,digits = 1)," (",formatC(round(Freq/(!!big_n )*100,digits = 1), digits = 1 ,format = "f"),"%)"),"") ) %>%
  select( - Freq ) %>%
  bind_rows(df_table3, .) %>%
  rename( "Question" = param, "Parameter" = Var) %>% 
  rename_( .dots = setNames( "Value", header)  )%>%
   print.data.frame( row.names = FALSE) 


