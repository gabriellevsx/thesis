library(dplyr)

install.packages("caret")
library(caret)
install.packages("skimr")
library(skimr)
#to deal with class imbalance
install.packages("themis")
library(themis)
library(tidyr)
library(reshape2)

library(formattable)
library(stringi)


#load the data set

data_frame <- read.csv("export_dataframe.csv")

data_frame <- data_frame %>% select(-p_aaer)
data_frame <- data_frame %>% select(-new_p_aaer, -insbnk, -understatement, -option, - issue)


data_frame$key_id <- stri_rand_strings(146045, 3)


gvkey_id <- data_frame[,c(2,46)]

write.csv(gvkey_id, "gvkey_id_match.csv")

data_frame <- data_frame[, -2]

#rename the variable

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
data_frame$key_id <- as.factor(data_frame$key_id)

                   

str(data_frame)

write.csv(data_frame, "fraud_data.csv", row.names = FALSE)


fraud_data <- read.csv('fraud_data.csv')

#------------------ dataset analysis------------------# 

# dimensions (116836 obs,51 var)
glimpse(fraud_data)
dim(fraud_data)

# data info
skim(fraud_data)
summary(fraud_data)

# because we will start vy building a classification model, we have to convert "misstate" from a numeric to a factor variable

data_frame$misstate <- as.factor(fraud_data$misstate)

# misstate proportion
table(fraud_data$misstate)

percentage <- prop.table(table(fraud_data$misstate)) * 100

cbind(freq=table(fraud_data$misstate), percentage=percentage)


# again detect class imbalance
fraud_data %>% count(misstate) %>% 
  mutate(prop = n / sum(n))

# visualisation with plots




# create validation set which will be used later to compare the performance of all the models

validation_index <- createDataPartition(fraud_data$misstate, p=0.80, list=FALSE)

validation <-  fraud_data[-validation_index,]

dataset <- fraud_data[validation_index,]













#----------------------------------------
# Create the Benford indicators
#----------------------------------------


# Step 1: create a dataset per year

fraud_data <- read.csv("fraud_data.csv")

fraud_data <- fraud_data %>% select(-misstate, -sich)

fraud_data$ID <- seq.int(nrow(fraud_data))

fraud_split <- split(fraud_data, fraud_data$fyear)

new_names <- as.character(unique(fraud_data$fyear))

for (i in 1:length(fraud_split)) {
  assign(paste("year_",new_names[i], sep = ""), fraud_split[[i]])
}


# Step 2: transpose the dataset

year_1990 <- year_1990 %>% select(-fyear)

year_1990_T <- as.data.frame(t(year_1990))

my.names <- year_1990$key_id

colnames(year_1990_T) <- my.names

year_1990_T <- year_1990_T[-c(43,44),]

year_1990_T_numb <- formattable(as.data.frame(lapply(year_1990_T, as.numeric)), digits = 8, format = "f")

#year_1990_T_numb <- year_1990_T_numb[complete.cases(year_1990_T_numb),]


write.csv(year_1990_T_numb, "sub_1990.csv", row.names = FALSE)


year_1990_sub <- as.data.frame(year_1990_T_numb[,c(1:999)])

write.csv(year_1990_sub, "subsub9_1990.csv", row.names = FALSE)
