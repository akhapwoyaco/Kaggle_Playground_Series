#
series_data = list.files(
  path = "playground-series-s5e2/", recursive = T, pattern = '.csv$', 
  full.names = T)
series_data
#
library(tidyverse)
library(readr)
library(janitor)
#
# data import -------------------------------------------------------------
# 
ls()
for (csv_path in 1:length(series_data)){
  print(csv_path) 
  file_path = series_data[csv_path]
  print(file_path)
  file_name = basename(file_path) |> 
    # substitute the pattern, only 1 instance
    sub(x = _, pattern = ".csv", "")
  print(file_name)
  assign(
    file_name,
    read.csv(file_path) |> clean_names()
  )
}
ls()
#
#######
train |> head()
test |> head()
#
unique(train$brand)
unique(test$brand)
#
unique(train$material)
unique(test$material)
#
unique(train$size)
unique(test$size)
#
unique(train$compartments)
unique(test$compartments)
#
unique(train$laptop_compartment)
unique(test$laptop_compartment)
#
unique(train$waterproof)
unique(test$waterproof)
#
unique(train$style)
unique(test$style)
#
unique(train$color)
unique(test$color)
#
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
#
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
#
simple_lm_model <- lm(
  price ~ brand*material*size*compartments*waterproof*style+weight_capacity_kg, 
  data = train)
#
prediction_simple_lm_model <- predict(simple_lm_model, newdata = test)
#
sample_submission$price = prediction_simple_lm_model
sample_submission
#
write.csv(
  sample_submission, 
  paste0(
    'submissions/sample_submission_multiple_lm_model1_', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#
