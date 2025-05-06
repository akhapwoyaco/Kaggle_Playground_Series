
# import Data -------------------------------------------------------------

# train data
#
library(readr)
#
train <- read_csv("playground-series-s5e4/train.csv")
dim(train)
View(train)
#
test <- read_csv("playground-series-s5e4/test.csv")
dim(test)
View(test)
#
# check NAs
#
sapply(X = train, FUN = function(x) sum(is.na(x)))
sapply(X = test, FUN = function(x) sum(is.na(x)))
#

#
model_1_lm <- lm(Listening_Time_minutes ~ .-id, data = train)
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
#
#
#
names(train)
#
1
