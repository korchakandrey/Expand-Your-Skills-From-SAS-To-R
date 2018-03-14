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
df1<-read.table(header=TRUE,stringsAsFactors = FALSE,
text ="
  col1 col2 col3 
                1 a T
                2 b T
                3 c F"
)
### Structure of df1
str(df1)
### Pronting the values
df1


### Example 2 reading data from CSV file

### Path for file with data
setwd("/folders/myfolders/conf/Library/");
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
df_k <- df1 %>% select( c("col1","col2"))
df_k
### drop variables 
df_d <- df1 %>% select( -col3 )
df_d


### keeping all columns between col1 and col3 ***;
### keep col1-col3;
df1_k_betw <- df1 %>% select(num_range("col", 1:3))
df1_k_betw

# variables starts with ;
df1_k_st <- df1 %>% select(starts_with("col"))
df1_k_st

# variables rename ;
df1_rename <- df1 %>%
   rename("Column1" = col1 )
df1_rename


### 2.3 ###
df1_i <- df1 %>% filter(col3==1, col2 == "a")
df1_i

### R base syntax – subset function
df_i <- subset( df1, col3 == 1)
df_i


### Creating new Variables by mutate()
### mutate() function and creating new variables ;
df1
df_new_var <- df1 %>% mutate(  
  n_2 = n()/2,
  col4 = if_else(row_number() >= n()/2
                 ,">= n/2","<n/2","missing"),
  col1 = col1 + 0.03
)
df_new_var

### Sorting data with arrange
### Example 2.5.1 Sorting and 
### Saving into new data frame 
df1_sort <- df1 %>% arrange( col3 )
df1_sort

### Example 2.5.2 Sorting and 
### Saving into new data frame 
df1_sort_desc <- df1 %>% arrange( col3, desc(col1) )
df1_sort_desc


### Set & Merge & Join
### Grouping varibles and statistics in R;
df2 <- read.table(header = TRUE,
                  stringsAsFactors = FALSE,
                  text = "
                  SUBJID TEST1 TEST2 PERIOD
                  S1 10 NA 1	
                  S1 8  7 2	
                  S2 7 4 1	
                  S2 5 3 2	
                  ") %>%
  mutate( TEST = rowMeans(.[,c("TEST1","TEST2")],na.rm=TRUE ) ) 
df2

### summarise() function produces 1 value for each by_group()
df2_s <- df2 %>%
  group_by( SUBJID ) %>%
  summarise(
    TEST_N    = n( ),
    TEST_MEAN = mean( TEST ,na.rm = TRUE),
    TEST_SDT  = sd ( TEST ,na.rm = TRUE),
    TEST_MIN  = min( TEST ,na.rm = TRUE),
    TEST_MAX  = max( TEST ,na.rm = TRUE)
  )     %>% ungroup() %>%  
  mutate( TOT_TEST_MEAN = mean( TEST_MEAN ) )
df2_s
df2_s %>% as.data.frame


### lag and lead functions;
df2_lag_lead <- df2 %>% 
  select(SUBJID, TEST1) %>% 
  group_by( SUBJID) %>%
  mutate( 
    LAG = lag( TEST1 ),
    LEAD = lead( TEST1 )
  )
df2_lag_lead
### Note that lag() and lead() are working within groups;


### Joins;
### Additional Information about subjects;
df_add_info <- read.table(header = TRUE,
                          stringsAsFactors = FALSE,
                          text = "
                          SUBJID  NAME 
                          S1 Nick
                          S2 Cate 
                          S3 Josh 
                          ");
df_add_info

###
df_test_name <- df2 %>%
  inner_join(y = df_add_info,
            by = c("SUBJID" = "SUBJID")) %>%
  select(NAME, TEST )
df_test_name
