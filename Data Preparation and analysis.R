library(dplyr)
library(caret)
library(skimr)
#to deal with class imbalance
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

data_frame <- data_frame[, -2]

# save the key - gvkey match
gvkey_id <- data_frame[,c(2,46)]
write.csv(gvkey_id, "gvkey_id_match.csv")



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

year_1990_T <- year_1990_T[-c(42,43),]

year_1990_T_numb <- as.data.frame(lapply(year_1990_T, as.numeric))


is.num <- sapply(year_1990_T_numb, is.numeric)
year_1990_T_numb[is.num] <- lapply(year_1990_T_numb[is.num], round, 5)


rm(year_1990_T_numb_compl)

year_1990_T_numb_compl <- year_1990_T_numb %>%
  select_if(~ !any(is.na(.)))


write.csv(year_1990_T_numb_compl, "df_1990.csv", row.names = FALSE)


year_1990_sub <- as.data.frame(year_1990_T_numb_compl[,c(1:10)])

write.csv(year_1990_sub, "df1_1990.csv", row.names = FALSE)










install.packages("benford.analysis")
library("benford.analysis")

#1. current assets - close conformity
result1 <- benford(fraud_data$crt_ast, number.of.digits = 2)
result1
plot(result1)
suspects1_crt_ast <- getSuspects(result1, fraud_data)
suspects1_crt_ast<-suspects1_crt_ast[,c(1,45)]
suspects1_crt_ast$crt_ast_bf <- 1

frd<- left_join(fraud_data, suspects1_crt_ast, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$crt_ast_bf <- as.factor(ifelse(is.na(frd$crt_ast_bf), 0, 1))


#2. acc_pyb - close conformity
result2 <- benford(fraud_data$acc_pyb, number.of.digits = 2)
result2
plot(result2)
suspects2_acc_pyb<- getSuspects(result2, fraud_data)
suspects2_acc_pyb<-suspects2_acc_pyb[,c(1,45)]
suspects2_acc_pyb$acc_pyb_bf <- 1

frd<- left_join(frd, suspects2_acc_pyb, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$acc_pyb_bf <- as.factor(ifelse(is.na(frd$acc_pyb_bf), 0, 1))


#3. assets - close conformity
result3 <- benford(fraud_data$ast, number.of.digits = 2)
result3
plot(result3)
suspects3_ast <- getSuspects(result3, fraud_data)
suspects3_ast<-suspects3_ast[,c(1,45)]
suspects3_ast$ast_bf <- 1

frd<- left_join(frd, suspects3_ast, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$ast_bf <- as.factor(ifelse(is.na(frd$ast_bf), 0, 1))


#4. common equity - close conformity
result4 <- benford(fraud_data$cmn_equ, number.of.digits = 2)
result4
plot(result4)
suspects4_cmn_equ <- getSuspects(result4, fraud_data)[,c(1,45)]
suspects4_cmn_equ$cmm_equ_bf <- 1

frd<- left_join(frd, suspects4_cmn_equ, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$cmm_equ_bf <- as.factor(ifelse(is.na(frd$cmm_equ_bf), 0, 1))


#5. cash - close conformity
result5 <- benford(fraud_data$cash, number.of.digits = 2)
result5
plot(result5)
suspects5_cash <- getSuspects(result5, fraud_data)[,c(1,45)]
suspects5_cash$cash_bf <- 1

frd<- left_join(frd, suspects5_cash, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$cash_bf <- as.factor(ifelse(is.na(frd$cash_bf), 0, 1))

#6. COGS - close conformity
result6 <- benford(fraud_data$cogs, number.of.digits = 2)
result6
plot(result6)
suspects6_cogs <- getSuspects(result6, fraud_data)[,c(1,45)]
suspects6_cogs$cogs_bf <- 1

frd<- left_join(frd, suspects6_cogs, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$cogs_bf <- as.factor(ifelse(is.na(frd$cogs_bf), 0, 1))


#7. csho - close conformity
result7 <- benford(fraud_data$csho, number.of.digits = 2)
result7
plot(result7)
suspects7_csho <- getSuspects(result7, fraud_data)[,c(1,45)]
suspects7_csho$csho_bf <- 1

frd<- left_join(frd, suspects7_csho, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$csho_bf <- as.factor(ifelse(is.na(frd$csho_bf), 0, 1))



#8. current debt - close conformity
result8 <- benford(fraud_data$crt_dbt, number.of.digits = 2)
result8
plot(result8)
suspects8_crt_dbt <- getSuspects(result8, fraud_data)[,c(1,45)]
suspects8_crt_dbt$crt_dbt_bf <- 1

frd<- left_join(frd, suspects8_crt_dbt, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$crt_dbt_bf <- as.factor(ifelse(is.na(frd$crt_dbt_bf), 0, 1))

#9. lt debt issues - no conformity /!\
result9 <- benford(fraud_data$lt_db_is, number.of.digits = 2)
result9
plot(result9)

#10. lt debt  - close conformity
result10 <- benford(fraud_data$lt_db, number.of.digits = 2)
result10
plot(result10)
suspects10_lt_db <- getSuspects(result10, fraud_data)[,c(1,45)]
suspects10_lt_db$lt_db_bf <- 1

frd<- left_join(frd, suspects10_lt_db, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$lt_db_bf <- as.factor(ifelse(is.na(frd$lt_db_bf), 0, 1))


#11. depr amort  - close conformity
result11 <- benford(fraud_data$dpr_amrt, number.of.digits = 2)
result11
plot(result11)
suspects11_depr <- getSuspects(result11, fraud_data)[,c(1,45)]
suspects11_depr$dpr_bf <- 1

frd<- left_join(frd, suspects11_depr, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$dpr_bf <- as.factor(ifelse(is.na(frd$dpr_bf), 0,1))

#12. ibei  - close conformity
result12 <- benford(fraud_data$ibei, number.of.digits = 2)
result12
plot(result12)
suspects12_ibei <- getSuspects(result12, fraud_data)[,c(1,45)]
suspects12_ibei$ibei_bf <- 1

frd<- left_join(frd, suspects12_ibei, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$ibei_bf <- as.factor(ifelse(is.na(frd$ibei_bf), 0,1))

#13. invt  - close conformity
result13 <- benford(fraud_data$invt, number.of.digits = 2)
result13
plot(result13)
suspects13_invt <- getSuspects(result13, fraud_data)[,c(1,45)]
suspects13_invt$invt_bf <- 1

frd<- left_join(frd, suspects13_invt, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$invt_bf <- as.factor(ifelse(is.na(frd$invt_bf), 0,1))



#14. ivao  - close conformity
result14 <- benford(fraud_data$ivao, number.of.digits = 2)
result14
plot(result14)
suspects14_ivao <- getSuspects(result14, fraud_data)[,c(1,45)]
suspects14_ivao$ivao_bf <- 1

frd<- left_join(frd, suspects14_ivao, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$ivao_bf <- as.factor(ifelse(is.na(frd$ivao_bf), 0,1))



#15. st_inv  - /!\ acceptable conformity
result15 <- benford(fraud_data$st_inv, number.of.digits = 2)
result15
plot(result15)


#16. current liab  - close conformity
result16 <- benford(fraud_data$crt_liab, number.of.digits = 2)
result16
plot(result16)
suspects16_crt_lb <- getSuspects(result16, fraud_data)[,c(1,45)]
suspects16_crt_lb$crt_lb_bf <- 1

frd<- left_join(frd, suspects16_crt_lb, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$crt_lb_bf <- as.factor(ifelse(is.na(frd$crt_lb_bf), 0,1))

#17. liab  - close conformity
result17 <- benford(fraud_data$liab, number.of.digits = 2)
result17
plot(result17)
suspects17_liab <- getSuspects(result17, fraud_data)[,c(1,45)]
suspects17_liab$liab_bf <- 1

frd<- left_join(frd, suspects17_liab, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$liab_bf <- as.factor(ifelse(is.na(frd$liab_bf), 0,1))

#18. net income  - close conformity
result18 <- benford(fraud_data$ni, number.of.digits = 2)
result18
plot(result18)
suspects18_ni <- getSuspects(result18, fraud_data)[,c(1,45)]
suspects18_ni$ni_bf <- 1

frd<- left_join(frd, suspects18_ni, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$ni_bf <- as.factor(ifelse(is.na(frd$ni_bf), 0,1))



#19. PPE  - close conformity
result19 <- benford(fraud_data$ppe, number.of.digits = 2)
result19
plot(result19)
suspects19_PPE <- getSuspects(result19, fraud_data)[,c(1,45)]
suspects19_PPE$ppe_bf <- 1

frd<- left_join(frd, suspects19_PPE, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$ppe_bf <- as.factor(ifelse(is.na(frd$ppe_bf), 0,1))



#20. PSTK  - /!\ non conformity
result20 <- benford(fraud_data$pstk, number.of.digits = 2)
result20
plot(result20)



#21. retained earning  - close conformity
result21 <- benford(fraud_data$re, number.of.digits = 2)
result21
plot(result21)
suspects21_RE <- getSuspects(result21, fraud_data)[,c(1,45)]
suspects21_RE$RE_bf <- 1

frd<- left_join(frd, suspects21_RE, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$RE_bf <- as.factor(ifelse(is.na(frd$RE_bf), 0,1))

#22. receivables  - close conformity
result22 <- benford(fraud_data$receiv, number.of.digits = 2)
result22
plot(result22)
suspects22_receiv <- getSuspects(result22, fraud_data)[,c(1,45)]
suspects22_receiv$receiv_bf <- 1

frd<- left_join(frd, suspects22_receiv, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$receiv_bf <- as.factor(ifelse(is.na(frd$receiv_bf), 0,1))

#23. sale  - close conformity
result23 <- benford(fraud_data$sale, number.of.digits = 2)
result23
plot(result23)
suspects23_sales <- getSuspects(result23, fraud_data)[,c(1,45)]
suspects23_sales$sale_bf <- 1

frd<- left_join(frd, suspects23_sales, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$sale_bf <- as.factor(ifelse(is.na(frd$sale_bf), 0,1))

#24. sstk  - close conformity
result24 <- benford(fraud_data$sstk, number.of.digits = 2)
result24
plot(result24)
suspects24_sstk <- getSuspects(result24, fraud_data)[,c(1,45)]
suspects24_sstk$sstk_bf <- 1

frd<- left_join(frd, suspects24_sstk, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$sstk_bf <- as.factor(ifelse(is.na(frd$sstk_bf), 0,1))


#25. txp  - close conformity
result25 <- benford(fraud_data$txp, number.of.digits = 2)
result25
plot(result25)
suspects25_txp <- getSuspects(result25, fraud_data)[,c(1,45)]
suspects25_txp$txp_bf <- 1

frd<- left_join(frd, suspects25_txp, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$txp_bf <- as.factor(ifelse(is.na(frd$txp_bf), 0,1))

#26. txt  - close conformity
result26 <- benford(fraud_data$txt, number.of.digits = 2)
result26
plot(result26)
suspects26_txt <- getSuspects(result26, fraud_data)[,c(1,45)]
suspects26_txt$txt_bf <- 1

frd<- left_join(frd, suspects26_txt, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$txt_bf <- as.factor(ifelse(is.na(frd$txt_bf), 0,1))

#27. xint  - close conformity
result27 <- benford(fraud_data$xint, number.of.digits = 2)
result27
plot(result27)
suspects27_xint <- getSuspects(result27, fraud_data)[,c(1,45)]
suspects27_xint$xint_bf <- 1

frd<- left_join(frd, suspects27_xint, by = c("key_id" = "key_id", "fyear"="fyear"))
frd$xint_bf <- as.factor(ifelse(is.na(frd$xint_bf), 0,1))

#28. prcc_f  - /!\ marginally acceptable conformity
result28 <- benford(fraud_data$prcc_f, number.of.digits = 2)
result28
plot(result28)

str(frd)

write.csv(frd, "frd_bf.csv")
