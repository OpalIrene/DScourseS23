# should be on and included in tidymodels package 
library(parsnip)
library(recipes)
library(rsample)
library(tune) 
library(workflows)
library(yardstick)
# uses these below
library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)
# install.packages("glmnet") for LASSO, ridge regression, and elastic net
library(glmnet)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# lm( ~ )


# this one worked! MAY BE WRONG! source: https://archive.ics.uci.edu/ml/machine-learning-databases/housing/
# housing <- read_table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/", col_names = FALSE)
# did not work for me
# housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
# did not work and already labeled like this!
# names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
# housing

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's


housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)



housing_recipe <- recipe(medv ~ ., data = housing) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(terms = ~ crim:nox) %>%
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

# run recipe
housing_prep <- prep(housing_recipe, training = housing_train)
housing_train_prepped <- juice(housing_recipe_prep)
housing_test_prepped <- bake(housing_prep, new_data = housing_test)


# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y  <- housing_test_prepped  %>% select( medv)

dim(housing)
dim(housing_train)

# Fit the regression model
est.ols <- lm(housing_train_y$medv ~ ., data = housing_train_x)
# Predict outcome for the test data
ols_predicted <- predict(est.ols, newdata = housing_test_x)
# Root mean-squared error
sqrt(mean((housing_test_y$medv - ols_predicted)^2))
# [1] 0.1908261

# easy way
est.ols.easy <- lm(log(medv) ~ crim + zn + indus + as.factor(chas) + 
                     rm + age + rad + tax + ptratio + b + 
                     lstat + crim:nox + poly(dis,2) + poly(nox,2), 
                   data = housing_train)
# Predict outcome for the test data
ols_easy_predicted <- predict(est.ols.easy, newdata = housing_test)
# Root mean-squared error
sqrt(mean((housing_test_y$medv - ols_easy_predicted)^2))
# [1] 0.1628675 

ols_spec <- linear_reg() %>%       # Specify a model
  set_engine("lm") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification


ols_fit <- ols_spec %>%
  fit(medv ~ ., data=juice(housing_recipe))

# inspect coefficients
tidy(ols_fit$fit$coefficients) %>% print
tidy(est.ols) %>% print
A tibble: 75 × 5
term        estimate std.error statistic   p.value
# <chr>          <dbl>     <dbl>     <dbl>     <dbl>
#  1 (Intercept)   3.31      0.106     31.1   3.83e-100
# 2 chasno       -0.0236    0.0355    -0.664 5.07e-  1
# 3 crim_x_nox   -0.103     0.0408    -2.53  1.17e-  2
# 4 crim_poly_1  10.4       4.98       2.09  3.75e-  2
# 5 crim_poly_2   0.639     0.579      1.10  2.70e-  1
# 6 crim_poly_3   0.365     0.385      0.947 3.44e-  1
# 7 crim_poly_4  -0.590     0.309     -1.91  5.69e-  2
# 8 crim_poly_5   0.331     0.261      1.27  2.05e-  1
# 9 crim_poly_6  -0.481     0.204     -2.36  1.90e-  2
# 10 zn_poly_1     0.313     0.391      0.800 4.24e-  1

# predict RMSE in sample
ols_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict RMSE out of sample
ols_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict R2 in sample
ols_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print
# in-sample RMSE was 0.181
# out-of-sample RMSE is 0.173

# predict R2 out of sample
ols_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print
# in-sample R^2 was 0.814
# out-of-sample R^2 is 0.764


# 8. LASSO 

lasso_spec <- linear_reg(penalty=0.5,mixture=1) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

lasso_fit <- lasso_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

lasso_fit
# predict RMSE in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print
# in sample: rmse    standard       0.413

# predict RMSE out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print
# out of sample: rmse    standard       0.390

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
# in-sample RMSE was 0.413
# out-of-sample RMSE is 0.390
# in-sample R^2 was 0
# out-of-sample R^2 is 0


#::::::::::::::::::::::::::::::::
# cross-validate the lambda
#::::::::::::::::::::::::::::::::
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 10-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 10)

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

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print


top_rmse %>% print(n = 1)

