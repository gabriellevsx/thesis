library(dplyr)

install.packages("caret")
library(caret)
install.packages("skimr")
library(skimr)
#to deal with class imbalance
install.packages("themis")
library(themis)


#load the data set

data_frame <- read.csv("export_dataframe.csv")

data_frame2 <- data_frame %>% select(-p_aaer)
data_frame3 <- data_frame2 %>% select(-new_p_aaer, -insbnk, -understatement, -option)




#rename the variable

data_frame4<- data_frame3 %>% rename(
   crt_ast = act,
  acc_pyb = ap, 
  ast = at,
  cmn_equ = ceq,
  cash = che,
  crt_dbt = dlc,
  lg_term_dbt_iss = dltis,
  lg_term_dbt = dltt,
  depr_amort = dp,
  ibei = ib,
  sht_trm_inv = ivst,
  crt_liab = lct,
  liab = lt,
  ppe = ppegt,
  receiv = rect)

write.csv(data_frame4, "fraud_data.csv", row.names = FALSE)

#------------------ dataset analysis------------------# 

# dimensions (116836 obs,51 var)
dim(data_frame)

# data info
skim(data_frame)
summary(data_frame)

# because we will start vy building a classification model, we have to convert "misstate" from a numeric to a factor variable

data_frame$misstate <- as.factor(data_frame$misstate)

# misstate proportion
table(data_frame$misstate)

percentage <- prop.table(table(data_frame$misstate)) * 100

cbind(freq=table(data_frame$misstate), percentage=percentage)


# again detect class imbalance
data_frame %>% count(misstate) %>% 
  mutate(prop = n / sum(n))

# visualisation with plots




# create validation set which will be used later to compare the performance of all the models

validation_index <- createDataPartition(data_frame$misstate, p=0.80, list=FALSE)

validation <-  data_frame[-validation_index,]

dataset <- data_frame[validation_index,]




