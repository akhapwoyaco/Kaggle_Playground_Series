
# import Data -------------------------------------------------------------

# train data
#
library(readr)
library(glmnet)
#
train <- read_csv("playground-series-s5e5/train.csv")
dim(train)
View(train)
#
test <- read_csv("playground-series-s5e5/test.csv")
dim(test)
View(test)
#
submission <- read_csv("playground-series-s5e5/sample_submission.csv")
dim(submission)
View(submission)
#

# check NAs
#
sapply(X = train, FUN = function(x) sum(is.na(x)))
sapply(X = test, FUN = function(x) sum(is.na(x)))
#
sapply(X = train, FUN = function(x) summary(x))
sapply(X = test, FUN = function(x) summary(x))

# model multiple lm -------------------------------------------------------


#
model_1_lm <- lm(log(Calories) ~ .-id, data = train)
summary(model_1_lm)$r.squared
#
summary(model_1_lm)
#
model_1_lm_preds <- predict(model_1_lm, test)
dim(test)
length(model_1_lm_preds)
#
summary(model_1_lm_preds)
#
#
predictions_data_frame = data.frame(
    id = test$id,
    Calories = exp(model_1_lm_preds)
)
#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_multiple_lm', 
    strftime(lubridate::now(),"%Y%m%d%H%M"), '.csv', sep = ''), 
  row.names = F)
#
#
#

# leaps subsets model lm -----------------------------------------------------------
#
library(leaps)
#
library(leaps)
reg_fit_full <- regsubsets(
  log(Calories) ~ .-id, 
  data = train, nvmax = 6, method="exhaustive")
reg_summary <- summary(reg_fit_full)
summary(reg_fit_full)
which.max(reg_summary$adjr2) #Select model with 4 predictors
#
as.data.frame(reg_summary$outmat)[4,]
#
model_2_lm <- lm(log(Calories) ~ Age+Duration+Heart_Rate+Body_Temp, data = train)
summary(model_2_lm)$r.squared
#
summary(model_2_lm)
#
model_2_lm_preds <- predict(model_2_lm, test)
dim(test)
length(model_2_lm_preds)
#
summary(model_2_lm_preds)
#
predictions_data_frame = data.frame(
  id = test$id,
  Calories = exp(model_2_lm_preds)
)

#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_multiple_lm_full_leaps_', 
    strftime(lubridate::now(),"%Y%m%d%H%M"), '.csv', sep = ''), 
  row.names = F)
#
#
#
#


# model 3 -----------------------------------------------------------------
#
library(leaps)
reg_fit_full <- regsubsets(
  log(Calories) ~ .-id, 
  data = train, nvmax = 6, method="exhaustive")
reg_summary <- summary(reg_fit_full)
summary(reg_fit_full)
which.max(reg_summary$adjr2) #Select model with 4 predictors
#
as.data.frame(reg_summary$outmat)[4,]
#

#
m_formula = formula(
  Calories~Age+Duration+Heart_Rate+Body_Temp)

x_var = model.matrix(
  m_formula,
  data = train
)[,-1]
#
y_var = log(train$Calories)
#
lambdas = 10^seq(3,-3, by = -.1)
ridge_cv = cv.glmnet(
  x_var, y_var, alpha = 1, lambda = lambdas, parallel = !T, nfolds = 11
)
#
best_lambda = ridge_cv$lambda.min
# best_lambda
#
best_ridge = glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)
#
n_formula = update(
  m_formula, NULL ~Age+Duration+Heart_Rate+Body_Temp)
#
test_1 = model.matrix(
  n_formula,
  test
)[,-1]
#
prediction_c = predict(best_ridge, s = best_lambda, newx = test_1) |>
  c() |> exp()
# #
actual = y_var
pred = predict(best_ridge, s = best_lambda, newx = x_var)
rss = sum((pred - actual)^2)
tss = sum((actual - mean(actual))^2)
rsq = 1- rss/tss
print(paste("R Squared:",rsq, sep = " "))
#
model_3_lm_preds <- prediction_c
dim(test)
length(model_2_lm_preds)
#
summary(model_2_lm_preds)
#
predictions_data_frame = data.frame(
  id = test$id,
  Calories = prediction_c
)
#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_multiple_ridge_full_leaps_', 
    strftime(lubridate::now(),"%Y%m%d%H%M"), '.csv', sep = ''), 
  row.names = F)
#


# ridge with all ----------------------------------------------------------
#
# library(leaps)
# reg_fit_full <- regsubsets(
#   log(Calories) ~ .-id, 
#   data = train, nvmax = 6, method="exhaustive")
# reg_summary <- summary(reg_fit_full)
# summary(reg_fit_full)
# which.max(reg_summary$adjr2) #Select model with 4 predictors
# #
# as.data.frame(reg_summary$outmat)[4,]
# #

#
m_formula = formula(
  Calories~Sex+Age+Height+Weight+Duration+Heart_Rate+Body_Temp)

x_var = model.matrix(
  m_formula,
  data = train
)[,-1]
#
y_var = log(train$Calories)
#
lambdas = 10^seq(3,-3, by = -.1)
ridge_cv = cv.glmnet(
  x_var, y_var, alpha = 1, lambda = lambdas, parallel = !T, nfolds = 11
)
#
best_lambda = ridge_cv$lambda.min
# best_lambda
#
best_ridge = glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)
#
n_formula = update(
  m_formula, NULL ~Sex+Age+Height+Weight+Duration+Heart_Rate+Body_Temp)
#
test_1 = model.matrix(
  n_formula,
  test
)[,-1]
#
prediction_c = predict(best_ridge, s = best_lambda, newx = test_1) |>
  c() |> exp()
# #
actual = y_var
pred = predict(best_ridge, s = best_lambda, newx = x_var)
rss = sum((pred - actual)^2)
tss = sum((actual - mean(actual))^2)
rsq = 1- rss/tss
print(paste("R Squared:",rsq, sep = " "))
#
model_3_lm_preds <- prediction_c
dim(test)
length(model_2_lm_preds)
#
summary(model_2_lm_preds)
#
predictions_data_frame = data.frame(
  id = test$id,
  Calories = prediction_c
)
#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_multiple_ridge_full_leaps_', 
    strftime(lubridate::now(),"%Y%m%d%H%M"), '.csv', sep = ''), 
  row.names = F)
#

#



