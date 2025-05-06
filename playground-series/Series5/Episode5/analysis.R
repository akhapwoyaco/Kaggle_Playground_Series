
# import Data -------------------------------------------------------------

# train data
#
library(readr)
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

# leaps subsets -----------------------------------------------------------
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

