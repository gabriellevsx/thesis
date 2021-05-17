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

write.csv(data_frame3, "fraud_data.csv", row.names = FALSE)


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


#-------------------decision trees-------------------#

#Let's start by loading the packages we will need. 
install.packages("tidymodels")
library(tidymodels)

#We will also introduces functionality from the following packages in this notebook:
install.packages("rpart")
library("rpart")
install.packages("partykit")
library("partykit")
install.packages("rpart.plot")
library(rpart.plot)

## Model assessment setup - Before we analyse the data, we must plan ahead. We will need to tune our tree, which we will do using K-fold cross-validation. But let's also keep some data for testing our tuned tree. Therefore, we start by making a stratified train-test split. We keep 70% of the data for training and 30% for testing:


set.seed(466581)
fr_split <- initial_split(data = data_frame, prop = 0.7, 
                          strata = "misstate")
fr_train <- training(fr_split)
fr_test <- testing(fr_split)

# The proportion of positives (lmisstatement) and negatives (no misstatement) in these partitions should be similar to that of the original data since we used stratified splitting (i.e. 0.993 - 0.006):

fr_train %>% count(misstate) %>% 
  mutate(prop = n / sum(n))
fr_test %>% count(misstate) %>% 
  mutate(prop = n / sum(n))

# We will tune our model on the training data using 10-fold stratified CV. Let's create folds for this:

set.seed(67283)
cv_folds <- fr_train %>% vfold_cv(v = 10, strata = "misstate")

# Classification trees using **tidymodels** - Let's consider how build a classification tree using **tidymodels**. 

## Setting up a model - Here is what the model looks like, where we are going to tune the cost-complexity parameter:


tree_model_tune <- decision_tree(cost_complexity = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("rpart")


#Note that we will tune the cost-complexity parameter ($\alpha$ on the slides and `cp` in the **rpart** package). Also, we indicate that this is a classification problem.

# Now we need to construct a pre-processing recipe. 
# We do not have to create dummy variables or do normalization, but we will adjust for the class imbalance here. We use `step_downsample()` to select always 
# We also have to take care to use only the variables we intend to use in the model

tree_recipe <- recipe(misstate ~ ., data = fr_train) %>% 
  step_downsample(misstate)
tree_recipe

# We can combine the model and recipe into a workflow: 
  
tree_wf <- workflow() %>% 
  add_recipe(tree_recipe) %>% 
  add_model(tree_model_tune)



## Performing cost-complexity pruning


# First, we will need tuning grid. Let's use the following values in our grid search (can be changed later on)
tree_grid <- tibble(cost_complexity = 10^seq(from = -4, to = 0, length.out = 20))
tree_grid

# Now we can perform our 10-fold CV, calculating several metrics:
class_metrics <- metric_set(accuracy, kap, sensitivity, specificity)
tree_tune_res <- tree_wf %>% 
  tune_grid(resamples = cv_folds,
            grid = tree_grid,
            metrics = class_metrics)


#Here are the results:
  

tree_metrics <- tree_tune_res %>% collect_metrics()
tree_metrics


#Here is a plot showing both the sensitivity and specificity:
  

tree_metrics %>% filter(.metric %in% c("sens", "spec")) %>% 
  ggplot(aes(x = cost_complexity, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err, 
             colour = .metric)) +
  geom_errorbar(width = 0.1) + 
  geom_point() + scale_x_log10()

# accuracy 
tree_metrics %>% filter(.metric == "accuracy") %>% 
  ggplot(aes(x = cost_complexity, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err)) +
  geom_errorbar(width = 0.1) + 
  geom_point() + scale_x_log10()


#The values of $\alpha$ which give a mean estimated sensitivity between 0.65 and 0.75 are:

tree_metrics_sub <- tree_metrics %>% 
  pivot_wider(names_from = ".metric", 
              values_from = c("mean", "std_err")) %>% 
  filter(mean_sens > 0.65, mean_sens < 0.75) %>% 
  arrange(desc(mean_sens))
tree_metrics_sub

#Let's select `Model10', which achieves about 71% sensitivity and 67% specificity:

tree_selected <- tree_metrics_sub %>% filter(.config == "Preprocessor1_Model10")
tree_selected


# Now we can finalize our workflow using our selected value of the tuning parameter as:

tree_wf_finalized <- tree_wf %>% finalize_workflow(tree_selected)

#The tuned workflow can be trained on all the training data with -Note that only 1358 observations were used to train the tree, since downsampling was used. 
  

tree_wf_fit <- tree_wf_finalized %>% fit(fr_train)
tree_wf_fit

pull_workflow_fit(tree_wf_fit)$fit %>% rpart.plot(roundint = FALSE)

#To be able to compare our results here to other methods in subsequent notebooks, we predict the test set and calculate some metrics for those predictions. We will use the following metrics:
 
# Here are the predicted classess:
  
tree_test_pred <- tree_wf_fit %>% predict(fr_test)

tree_test_metrics <- fr_test %>% bind_cols(tree_test_pred) %>% 
  class_metrics(truth = misstate, estimate = .pred_class)
tree_test_metrics

## Cross-validation for pruning - We will specify the model using a complete formula. Here is a formula which uses all variables 
  
tree_formula <- misstate ~ . 

set.seed(24356)
rpart_fit <- rpart(tree_formula, data = fr_train, method = "class", 
                   control = rpart.control(cp = 0.003), parms = list(prior = c(0.5, 0.5)))


#The results for the CV are as follows: 

plotcp(rpart_fit)

printcp(rpart_fit)


#We can now prune the tree and plot it. Let's use the tree with 15 splits:

rpart_pruned <- rpart::prune(rpart_fit, cp = 0.0064822)
rpart.plot(rpart_pruned)
plot(as.party(rpart_pruned))


# Here are the predicted classes for the test data:
rpart_test_pred <- predict(rpart_pruned, newdata = fr_test, type = "class")

rpart_test_metrics <- 
  fr_test %>% mutate(rpart_pred = rpart_test_pred) %>% 
  class_metrics(truth = misstate, estimate = rpart_pred)
rpart_test_metrics

