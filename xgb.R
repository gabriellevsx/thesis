library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(themis)
library(kernlab)
library("doParallel")
install.packages("themis", dependencies = TRUE)
library("themis")
library("xgboost")
library(vip)


#########################
# Preparing the dataset #
#########################

fraud <- read.csv("fraud_data.csv")
benford <- read.csv("frd_bf.csv")


fraud$misstate <- as.factor(fraud$misstate)
fraud$fyear <- as.factor(fraud$fyear)
fraud$sich <- as.factor(fraud$sich)
fraud$gvkey <- as.character(fraud$gvkey)

benford$misstate <- as.factor(benford$misstate)
benford$fyear <- as.factor(benford$fyear)
benford$sich <- as.factor(benford$sich)
benford$gvkey <- as.character(benford$key_id)
benford[45:68] <- lapply(benford[45:68], as.numeric)


library(dplyr)
fraud <- fraud %>% select(-gvkey, -sich)
benford <- benford[complete.cases(benford),] %>% select(-X, -key_id, -gvkey, -sich)


# split the data into trainng (75%) and testing (25%)
set.seed(466581)
fraud_split <- initial_split(fraud, prop = 7/10, strata = misstate)
fraud_split

set.seed(466581)
benford_split <- initial_split(benford,prop = 3/4, strata = misstate)
benford_split

# extract training and testing sets
fraud_train <- training(fraud_split)
fraud_test <- testing(fraud_split)

benford_train <- training(benford_split)
benford_test <- testing(benford_split)

# create validation set
fraud_cv <- vfold_cv(fraud_train, strata = misstate)

benford_cv<- vfold_cv(benford_train)

##################
# pre-processing #
##################

#note: could not use step_rose because too few oberve
#note: xgb requires dummy, imput, but not normalize




fraud_recipe <-  recipe(misstate ~., data = fraud_train) %>%
  step_normalize(crt_ast, acc_pyb, ast,cmn_equ, cash, cogs, csho, crt_dbt, lt_db_is, lt_db, dpr_amrt,ibei, invt, ivao, st_inv, crt_liab, liab,
                 ni, ppe, pstk, re, receiv, sale) %>% step_dummy(fyear)%>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.01) %>%
  step_downsample(misstate) %>%
  step_impute_knn(all_predictors())

########################
# Specifying the model #
########################

# fraud data


xgb_fr_model <-
  boost_tree(trees = tune(), tree_depth = tune(),
             learn_rate = tune(), stop_iter = 500) %>%
  set_mode("classification") %>%
  set_engine("xgboost")


######################
# Gather in workflow #
######################

# fraud data

xgb_fr_wf <- workflow() %>%
  add_recipe(fraud_recipe) %>%
  add_model(xgb_fr_model)


#######################
# Tune the parameters #
#######################

class_metrics <- metric_set(accuracy, kap, sensitivity,
                            specificity, roc_auc)

#speed up computation
registerDoParallel()

set.seed(8504)
grid_max_entropy(trees(range = c(0, 200)),
                 learn_rate(range = c(-2, -1)),
                 tree_depth(), size = 15)

xgb_grid <- expand.grid(trees = 500 * 1:20,
                        learn_rate = c(0.1, 0.01),
                        tree_depth = 1:5)

xgb_fr_tune_results <- tune_grid(
  xgb_fr_wf,
  resamples = fraud_cv,
  grid = xgb_grid,
  metrics = class_metrics
)

xgb_fr_tune_results$.notes

str(fraud_train)

xgb_fr_tune_metrics <- xgb_fr_tune_results %>%
  collect_metrics()

write.csv(xgb_fr_tune_metrics, " xgb_fr_tune.csv")


xgb_fr_tune_metrics %>%
  filter(.metric %in% c("accuracy", "sens", "spec")) %>%
  ggplot(aes(x = trees, y = mean, colour = .metric)) +
  geom_path() +
  facet_wrap(learn_rate ~ tree_depth)

xgb_fr_tune_metrics %>%
  filter(tree_depth >= 4, learn_rate == 0.01, trees >= 2000 & trees <= 5000) %>%
  select(trees:learn_rate, .metric, mean) %>%
  pivot_wider(trees:learn_rate,
              names_from = .metric,
              values_from = mean)

#########################
# Finalize the workflow #
#########################

#-------------
# fraud data
#-------------

#xgb

save(xgb_fr_tune_results)

xgb_fr_best <- xgb_fr_tune_metrics %>%
  filter(.metric == "sens", tree_depth == 4, learn_rate == 0.01, trees == 4500)
xgb_fr_final_wf <- finalize_workflow(xgb_fr_wf, xgb_fr_best)
xgb_fr_final_wf



#------------------
# Test on test set
#------------------

xgb_fr_final_fit <- xgb_fr_final_wf %>%
  last_fit(fraud_split, metrics = class_metrics)

xgb_fr_final_fit %>%
  collect_metrics()

xgb_fr_final_df <- as.data.frame(xgb_fr_final_fit %>% collect_metrics())
                                 
write.csv(xgb_fr_final_df, "gxb_fr_test_results.csv")
                                 
                                      
xgb_fr_final_fit %>% collect_predictions() %>%
                                   conf_mat(truth = misstate, estimate = .pred_class)
                                 
                                 xgb_fr_final_fit %>% collect_predictions() %>%
                                   roc_curve(misstate, .pred_1) %>%
                                   autoplot()
                                 
                                 xgb_fr_final_fit %>% collect_predictions() %>%
                                   lift_curve(misstate, .pred_1)%>%
                                   autoplot()
                                 
                                 xgb_fr_final_fit %>% collect_predictions() %>%
                                   gain_curve(misstate, .pred_1) %>%
                                   autoplot()
                                 
                                 
                                 xgb_fr_final_wf %>%
                                   fit(data = fraud_train) %>%
                                   pull_workflow_fit() %>%
                                   vip(geom = "point")
                                 
                                 
                                 
                                 
                                 
                                 