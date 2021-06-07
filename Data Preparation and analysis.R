library(dplyr)
library(caret)
library(skimr)
#to deal with class imbalance
library(themis)
library(tidyr)
library(reshape2)
library(formattable)
library(stringi)
library(ggplot2)
library(DataExplorer)
detach(package:plyr) 
install.packages("benford.analysis")
library("benford.analysis")

#-------------------------------
# Preparing the data set
#-------------------------------

# load the original data set
data_frame <- read.csv("export_dataframe.csv")

# remove the variables which are not of interest to us
data_frame <- data_frame %>% select(-p_aaer,-new_p_aaer, -insbnk, -understatement, -option, - issue)

# rename the variables
data_frame<- data_frame %>% rename(
   crt_ast = act,
  acc_pyb = ap, 
  ast = at,
  cmn_equ = ceq,
  cash = che,
  crt_dbt = dlc,
  lt_db_is = dltis,
  lt_db = dltt,
  dpr_amrt = dp,
  ibei = ib,
  st_inv = ivst,
  crt_liab = lct,
  liab = lt,
  ppe = ppegt,
  receiv = rect)

data_frame$misstate <- as.factor(data_frame$misstate)
data_frame$fyear <- as.factor(data_frame$fyear)
data_frame$sich <- as.factor(data_frame$sich)
data_frame$gvkey <- as.character(data_frame$gvkey)
                   
# save the new data frame as csv file
write.csv(data_frame, "fraud_data.csv", row.names = FALSE)

#----------------------------------------
# Create the Benford indicators
#----------------------------------------


# Step 1: create a dataset per 5-year

fraud_data <- read.csv("fraud_data.csv")

fraud_data <- fraud_data %>% select(-misstate, -sich, -dch_wc, -dch_rec, -dch_inv, -ch_rsst, -ch_cs, -ch_cm, -ch_roa, -ch_fcf, -reoa, -dpi, -bm, -EBIT, -soft_assets)

str(fraud_data)

fraud_data1 <- fraud_data %>% subset( fyear == 1990 | fyear == 1991 | fyear == 1992 | fyear == 1993 | fyear == 1994)
fraud_data1$gvname <- paste("gv", fraud_data1$gvkey, sep = "")
fraud_data1$period <- 1

fraud_data2 <- fraud_data %>% subset( fyear == 1996 | fyear == 1997 | fyear == 1998 | fyear == 1999 | fyear == 2000)
fraud_data2$period <- 2
fraud_data2$gvname <- paste("gv", fraud_data2$gvkey, sep = "")

fraud_data3 <- fraud_data %>% subset( fyear == 2001 | fyear == 2002 | fyear == 2003 | fyear == 2004 | fyear == 2005)
fraud_data3$period <- 3
fraud_data3$gvname <- paste("gv", fraud_data3$gvkey, sep = "")


fraud_data4 <- fraud_data %>% subset( fyear == 2006 | fyear == 2007 | fyear == 2008 | fyear == 2009 | fyear == 2010)
fraud_data4$period <- 4
fraud_data4$gvname <- paste("gv", fraud_data4$gvkey, sep = "")


fraud_data5 <- fraud_data %>% subset( fyear == 2011 | fyear == 2012 | fyear == 2013 | fyear == 2014)
fraud_data5$period <- 5
fraud_data5$gvname <- paste("gv", fraud_data5$gvkey, sep = "")

# step 2: save each of the data sets as a CSV file

write.csv(fraud_data1, "sql_fr_1.csv")
write.csv(fraud_data2, "sql_fr_2.csv")
write.csv(fraud_data3, "sql_fr_3.csv")
write.csv(fraud_data4, "sql_fr_4.csv")
write.csv(fraud_data5, "sql_fr_5.csv")

# step 3: follow the steps below for each individual data set
 
### 3.1. create an ID variable based on the gvkey
fraud_data5$id <- ""
fraud_data5_trial <- fraud_data5 %>% group_by(gvkey) %>% mutate(id=as.numeric(cur_group_id()))
str(fraud_data4_trial)

### 3.2. split the individual data set into several subsets based on the ID variable 

frdt5_1 <- fraud_data5_trial[fraud_data5_trial$id < 1000,] %>% select(-fyear)

frdt5_2 <- fraud_data5_trial[fraud_data5_trial$id < 2500,]%>% select(-fyear)
frdt5_2 <- frdt5_2[frdt5_2$id > 999,]

frdt5_3 <- fraud_data5_trial[fraud_data5_trial$id < 3500,]%>% select(-fyear)
frdt5_3 <- frdt5_3[frdt5_3$id > 2499,]

frdt5_4 <- fraud_data5_trial[fraud_data5_trial$id < 10000,]%>% select(-fyear)
frdt5_4 <- frdt4_4[frdt4_4$id > 3499,]


### 3.3. For each sub individual data set, transpose the data set, rename the columns,
### save the gvkeys in a cv file, and make the variables in the transpose data set numeric
frdt5_4_T <-  as.data.frame(t(frdt5_4))

my.names <- frdt5_4$gvkey
colnames(frdt5_4_T) <- paste("gv", my.names, sep = "")

gvkey_dt <- as.character(frdt5_4$gvkey)
write.csv(gvkey_dt, "gvkey5_4.csv")


frdt5_4_T <- as.data.frame(lapply(frdt5_4_T, as.numeric))

write.csv(frdt5_4_T, "frddt5_3500-8000.csv")




# step 4: some visualization

#1. current assets - close conformity
result1 <- benford(fraud_data$crt_ast, number.of.digits = 2)
result1
plot(result1)
suspects1_crt_ast <- getSuspects(result1, fraud_data)
suspects1_crt_ast<-suspects1_crt_ast[,c(1,45)]
suspects1_crt_ast$crt_ast_bf <- 1

frd<- left_join(fraud_data, suspects1_crt_ast, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$crt_ast_bf <- as.factor(ifelse(is.na(frd$crt_ast_bf), 0, 1))


frd <- frd[,c(4,46)]


#20. PSTK  - /!\ non conformity
result20 <- benford(fraud_data$pstk, number.of.digits = 2)
result20
plot(result20)



# step 5: in Excel and WebAriadne

#  Each transposed data set (approximately 10 for each the five period-related datasets)
#  is opened in Excel. Using the transpose and hlookup function, the observations are grouped by gvkey.
#  Hence, each transposed data set contains 1000 unique gvkeys as columns, and between 30 and 150 rows of observations 
#  for each gvkey, depending on the number of years for which each gvkey is available.
#  Once the observations have been grouped, the new csv file is uploaded on WebAriadne and test for Newcomb-Benford law.
#  The output of WebAriadne is an excel file whc=ich states whether each of the 5-year - gvkey set of observations 
#  follows Benford law according to three statistical tests.The output of each of these three tests is a binary outcome
#  (i.e. if the observations follow benford's law according to test 1, the score is 1, and it is zero otherwise).



# Step 6: In SQL

# SQL is then used to join the WebAriadne output with the period-related fraud data subsets. 
# The SQL output is loaded below.

final_bf_1 <- read.csv("final_bf_1.csv") %>% select(-field1, -gvkey.1, -period.1, -gvname)
final_bf_2 <- read.csv("final_bf_2.csv") %>% select(-field1, -gvkey.1, -period.1, -gvname)
final_bf_3 <- read.csv("final_bf_3.csv") %>% select(-field1, -gvkey.1, -period.1, -gvname)
final_bf_4 <- read.csv("final_bf_4.csv") %>% select(-field1, -gvkey.1, -period.1, -gvname)
final_bf_5 <- read.csv("final_bf_5.csv") %>% select(-field1, -gvkey.1, -period.1, -gvname)

head(final_bf_1)

# Step 7: merge the data sets back together

final_data <- rbind(final_bf_1, final_bf_2, final_bf_3, final_bf_4, final_bf_5)

write.csv(final_data, "final_data.csv")


#----------------------------------
# Analyze the data 
#----------------------------------

# load the data set
final_data <- read.csv('final_data')

# dimensions (144083 obs,52 var)
glimpse(final_data)
dim(final_data)

final_data <- final_data %>% mutate(
  bf_1 = as.factor(as.numeric(bf_1)),
  bf_2 = as.factor(as.numeric(bf_1)),
  bf_3 = as.factor(as.numeric(bf_3)),
  bf_agr = as.factor(as.numeric(bf_agr)),
  gvkey = as.character(gvkey),
  sich = as.factor(as.character(sich)),
  misstate = as.factor(misstate),
  fyear = as.factor(as.character(fyear))
)

skim(final_data)

DataExplorer::plot_intro(final_data)

summary(final_data)


# misstate proportion
table(final_data$misstate)

percentage <- prop.table(table(final_data$misstate)) * 100

cbind(freq=table(final_data$misstate), percentage=percentage)


# again detect class imbalance
final_data %>% count(misstate) %>% 
  mutate(prop = n / sum(n))

ggplot(final_data, 
       aes(x = period, 
           fill = misstate)) + 
  geom_bar(position = "stack")


# visualisation with plots
ggplot(data_frame, aes(misstate)) +
  geom_bar(fill = "cornflowerblue") 


# A density plot showing the class separation for the numeric features are shown below:
  
  ggplot(Default, aes(x = income, fill = default)) + 
  geom_density(alpha = 0.5)



