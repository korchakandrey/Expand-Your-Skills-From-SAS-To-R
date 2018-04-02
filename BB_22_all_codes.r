##################################################################
# BB_22_all_codes.R - Produce examples from BB-22 paper in SAS
##################################################################
# Author: Korchak, Andrii
##################################################################
# Input: "your_folder_name/mental-heath-in-tech-2016_20161114.csv"
# 
# Output: csv_file  - data frame with demographic information from OMSI survey
#       outputs from print() function with examples
##################################################################

### Example 1 - reading text data in.
dm<-read.table(header=TRUE,stringsAsFactors = FALSE,
                text ="
                SUBJID AGE POFL 
                001 23 Y
                002 45 Y
                003 61 N"
)
### Structure of dm
str(dm)
### Pronting the values
dm

### Example 2 reading data from CSV file

### Path for file with data
setwd("your_folder_name/")
PATH <- "mental-heath-in-tech-2016_20161114.csv"

### Reading data into the system
csv_file<-read.csv(PATH, stringsAsFactors = FALSE)
### Checking number of Rows and Coulumns
dim(csv_file)
### Reading names of First 3 columns
names(csv_file)[1:3]


### DPLYR package
### installing package
install.packages("dplyr") 
### Loading package
library("dplyr")
help("dplyr")

### Pipe operator
### variable # 56 is Age
names(csv_file)[56]
sort(unique(csv_file[,56]))

### The same task can be accomplished by Pipe Operator:
csv_file[,56] %>% unique %>% sort

### Using Dot with Pipe Operator
1 %>% rep(2)
1 %>% rep(2,.)


###  Example 2.2.1 ***;
### keep variables 
df_k <- dm %>% select( c("SUBJID","AGE"))
df_k
### drop variables 
df_d <- dm %>% select( -POFL )
df_d


### keeping all columns between SUBJID and POFL ***;
### keep SUBJID-POFL;
dm_k_betw <- dm %>% select(SUBJID:POFL)
dm_k_betw

# variables starts with AGE ;
dm_k_st <- dm %>% select(ends_with("AGE"))
dm_k_st

# variables rename ;
dm_rename <- dm %>%
  rename("PATNUM" = SUBJID )
dm_rename

### 2.3 ###
dm_i <- dm %>% filter(POFL=="Y", AGE > 21)
dm_i

### R base syntax – subset function
df_i <- subset( dm, POFL == "Y" & AGE > 21 )
df_i


### Creating new Variables by mutate()
### mutate() function and creating new variables ;
dm
df_new_var <- dm %>% mutate(  
  N_ROWS = n(),
  col4 = if_else(row_number() >= n()/2
                 ,">= n/2","<n/2","missing"),
  USUBJID = paste0("A00-",SUBJID)
) %>% select(-SUBJID)
df_new_var

### Sorting data with arrange
### Example 2.5.1 Sorting and 
### Saving into new data frame 
dm_sort <- dm %>% arrange( POFL )
dm_sort

### Example 2.5.2 Sorting and 
### Saving into new data frame 
dm_sort_desc <- dm %>% arrange( POFL, desc(SUBJID) )
dm_sort_desc


### Set & Merge & Join
### Grouping varibles and statistics in R;
vs <- read.table(header = TRUE,
                  stringsAsFactors = FALSE,
                  text = "
                  SUBJID HT WT PERIOD
                  001 180 NA 1	
                  001 181 77 2	
                  002 173 85 1
                  002 173 83 2	
                  ")
vs
### summarise() function produces 1 value for each by_group()
vs_s <- vs %>%
  group_by( SUBJID ) %>%
  summarise_at(
        vars(HT,WT),
        funs( sum(!is.na(.)),
             mean(., na.rm=TRUE),
               sd(., na.rm=TRUE),
              min(. ,na.rm = TRUE),
              max(. ,na.rm = TRUE))) %>%
  ungroup() 
vs_s

### lag and lead functions;
vs_lag_lead <- vs %>% 
  select(SUBJID, HT) %>% 
  group_by( SUBJID) %>%
  mutate( 
    LAG = lag( HT ),
    LEAD = lead( HT )
  ) %>% ungroup()
vs_lag_lead
### Note that lag() and lead() are working within groups;

### Joins;
###
advs <- vs %>%
  inner_join(y = dm,
             by = c("SUBJID" = "SUBJID")) %>%
  select(SUBJID,POFL, HT, WT  )
advs
