##################################################################
# OMSI_all_codes.R - Demog from OMSI survey, BB-22 paper in R
##################################################################
# Author: Korchak, Andrii
##################################################################
# Input: "your_folder_name/mental-heath-in-tech-2016_20161114.csv"
# 
# Output: csv_file  - data frame with demographic information from OMSI survey
#         results from print() function
#         AGE density graph
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

df_table  <- data_frame (Var = names(stable$REMOTE),   Freq = as.vector(stable$REMOTE)   , param = "Working Remotely?" )
df_table1 <- data_frame (Var = names(stable$GENDER_C), Freq = as.vector(stable$GENDER_C) , param = "Gender"            )
df_table2 <- data_frame (Var = names(stable$IT_ROLE),  Freq = as.vector(stable$IT_ROLE)  , param = "It/Tech Role?"     )

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
df_table_blank <- data_frame (Var = "",  Freq =NA_integer_, param = "")

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


###############################################
#  Plotting AGE distribution
# We will try to recreate an grapgh from SAS SGPLOT 
###############################################
require(ggplot2)

### Creating an Object with ggplot() function;
### select() is used to pick columns
gobj <- demog %>% select(BIG_N, AGEN, GENDER_C ) %>%
  ### na.omit() excludes missing values
  na.omit  %>%
  ### AGEN is used on X axis, colors are different for each GENDER value
  ggplot( aes( x = AGEN, color = factor(GENDER_C, levels=c("Other","Female","Male") ) ))
### ploting results. 
### We can do it after each change to see the difference
gobj

### We can add elements to GGPLOT object.
### geom_density() draws densiti curves
gobj <- gobj +  geom_density()  +
  ### adding horizontal reference lines and customizing colors
  geom_hline(yintercept=0, colour="grey", size=1) +
  theme_light() + theme(panel.grid.minor = element_blank())
gobj

### Changing color of groups
gobj <- gobj + scale_color_manual(values=c("red","green","blue")) 
gobj

### Changing Legend
gobj <- gobj + guides(color= guide_legend("Gender\nGroup")) 
gobj

### We can continue and add vertical line for mean values
gobj <- gobj+ geom_vline(aes( xintercept = mean(AGEN, na.rm = TRUE) ), color = "grey40")  +
  geom_text(aes(x=mean(AGEN, na.rm = TRUE)*1.1, y = 0.07 , label=paste0("Mean Age=",round(mean(AGEN, na.rm = TRUE),1) ) ), colour="grey40", size=3.5 )
gobj

### Adding title
gobj <- gobj  +xlab("Age") + ylab("Number of Subjects") +
  ggtitle(label =  "Age Distribution by Gender Groups")
gobj

### Adding Footnotes
gobj <- gobj + labs( captions = paste("Data From: OSMI Mental Health in Tech Survey\nViersion 2016"))
gobj
