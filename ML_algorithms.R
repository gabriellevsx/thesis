library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(themis)
library(kernlab)
library("doParallel")
library("themis")
library("xgboost")



#########################
# Preparing the dataset #
#########################

fraud <- read.csv("fraud_data.csv")
benford <- read.csv("frd_bf.csv")

fraud$misstate <- as.factor(fraud$misstate)
fraud$fyear <- as.factor(fraud$fyear)
fraud$sich <- as.factor(fraud$sich)

benford$misstate <- as.factor(benford$misstate)
benford$fyear <- as.factor(benford$fyear)
benford$sich <- as.factor(benford$sich)

fraud <- fraud[complete.cases(fraud),] %>% select(-key_id, -sich)
benford <- benford[complete.cases(benford),]%>% select(-key_id, -sich)


# split the data into trainng (75%) and testing (25%)
set.seed(466581)
fraud_split <- initial_split(fraud, prop = 3/4)
fraud_split

set.seed(466581)
benford_split <- initial_split(benford,prop = 3/4)
benford_split

# extract training and testing sets
fraud_train <- training(fraud_split)
fraud_test <- testing(fraud_split)

benford_train <- training(benford_split)
benford_test <- testing(benford_split)

# create validation set
fraud_cv <- vfold_cv(fraud_train)

benford_cv<- vfold_cv(benford_train)

sum(is.na(fraud_train))

##################
# pre-processing #
##################

#note: could not use step_rose because too few oberve

fraud_recipe <-  recipe(misstate ~., data = fraud_train) %>%
  step_dummy(fyear) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  step_smote(misstate) %>% prep()
fraud_recipe

benford_recipe <-  recipe(misstate ~., data = benford_train) %>%
  step_dummy(fyear) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  step_smote(misstate) %>% prep()
benford_recipe

fraud_train_preprocessed <- fraud_recipe %>%
  # apply the recipe to the training data
  prep(fraud_train) %>%
  # extract the pre-processed training dataset
  juice()
fraud_train_preprocessed

benford_train_preprocessed <- benford_recipe %>%
  # apply the recipe to the training data
  prep(benford_train) %>%
  # extract the pre-processed training dataset
  juice()
benford_train_preprocessed

########################
# Specifying the model #
########################

# fraud data

svm_fr_model <- 
  svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("classification")


xgb_fr_model <- 
  boost_tree(trees = tune(), tree_depth = tune(), 
             learn_rate = tune(), stop_iter = 500) %>%
  set_mode("classification") %>%
  set_engine("xgboost")



mlp


# benford data

svm_bf_model <- 
  svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("classification")


xgb_bf_model <- 
  boost_tree(trees = tune(), tree_depth = tune(), 
             learn_rate = tune(), stop_iter = 500) %>%
  set_mode("classification") %>%
  set_engine("xgboost")


######################
# Gather in workflow #
######################

# fraud data

svm_fr_wf <- workflow() %>%
  add_recipe(fraud_recipe) %>%
  add_model(svm_fr_model)

xgb_fr_wf <- workflow() %>%
  add_recipe(fraud_recipe) %>%
  add_model(xgb_fr_model)

mlp


# benford data


svm_bf_wf <- workflow() %>%
  add_recipe(benford_recipe) %>%
  add_model(svm_bf_model)

xgb_bf_wf <- workflow() %>%
  add_recipe(benford) %>%
  add_model(xgb_bf_model)



#######################
# Tune the parameters #
#######################

class_metrics <- metric_set(accuracy, kap, sensitivity, 
                            specificity, roc_auc)

#speed up computation 
registerDoParallel()

#create tuning grid if needed

# svm


# xgb
set.seed(8504)
grid_max_entropy(trees(range = c(0, 200)), 
                 learn_rate(range = c(-2, -1)), 
                 tree_depth(), size = 20)

xgb_grid <- expand.grid(trees = 500 * 1:5, 
                        learn_rate = c(0.1, 0.01), 
                        tree_depth = 1:3)

xgb_fr_tune_results <- tune_grid(
  xgb_fr_wf,
  resamples = fraud_cv,
  grid = xgb_grid,
  metrics = class_metrics
)

xgb_bf_tune_results <- tune_grid(
  xgb_bf_wf,
  resamples = fraud_cv,
  grid = xgb_grid,
  metrics = class_metrics
)


#########################
# Finalize the workflow #
#########################


# fraud data

svm_fr_fit <- svm_fr_wf %>%
  last_fit(fraud_split)
