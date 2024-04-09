library(tidymodels)
library(glmnet)
library(rsample)

# Load the data
set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Split the data
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Creating a new recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,lstat,dis,nox, degree=6) 

# Run the recipte
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# Create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

#----------------------------------------------------
# a-1 LASSO model
#----------------------------------------------------
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse") # rmse: root of mse
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
collect_metrics() %>% print

top_rmse %>% print(n = 1)

#----------------------------------------------------
# a-2 Penalty = 0.00139
#----------------------------------------------------
lasso_spec <- linear_reg(penalty=0.00139,mixture=1) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

lasso_fit <- lasso_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# predict RMSE in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict RMSE out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict R2 in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# predict R2 out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# in-sample RMSE: 0.137
# out-of-sample RMSE: 0.188

#----------------------------------------------------
# b Ridge model
#----------------------------------------------------
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 10-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%
#add_recipe(housing_recipe)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse") # rmse: root of mse
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
df <- last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print


top_rmse %>% print(n = 1)

#----------------------------------------------------
# b-2 Penalty = 0.0373
#----------------------------------------------------
ridge_spec <- linear_reg(penalty=0.0373,mixture=0) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

ridge_fit <- ridge_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# predict RMSE in sample
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict RMSE out of sample
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict R2 in sample
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# predict R2 out of sample
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print
# in-sample RMSE: 0.140
# out-of-sample RMSE: 0.180

# ----------------------------------------------------
# Simple regression
# ----------------------------------------------------
lm_model <- lm(log(medv) ~ ., data = housing_train_prepped)
summary(lm_model)

# RMSE
lm_pred <- predict(lm_model, newdata = housing_test_prepped)
lm_rmse <- sqrt(mean((exp(lm_pred) - exp(housing_test_y$medv))^2))
lm_rmse


