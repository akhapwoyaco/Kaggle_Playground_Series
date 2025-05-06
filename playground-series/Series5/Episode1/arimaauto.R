#
series_data = list.files(recursive = T, pattern = '.csv$')

library(tidyverse)
library(readr)
library(fastDummies)
#
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
    read.csv(file_path)
  )
}
ls()
#

# data summary ------------------------------------------------------------
#
# train |> str()
# test |> str()
# sample_submission |> head()
# #
# head(train)
# head(test)
# #
# dim(train); dim(test); dim(sample_submission)
# #
# sapply(train, FUN = function(x) sum(is.na(x)))
# sapply(test, FUN = function(x) sum(is.na(x)))
#
train = train |> 
  mutate(
    date = lubridate::ymd(date)
  ) |>
  mutate(
    weekday = lubridate::wday(date),#, label = T, abbr = T), 
    month = lubridate::month(date),#, label = T, abbr = T), 
    year = lubridate::year(date)
  )
head(train)
#
train = fastDummies::dummy_columns(remove_first_dummy = T,
  train, remove_selected_columns = T,
  select_columns = c("country","store","product")) |> 
  select(-id, -date)
#
test = test |> 
  mutate(
    date = lubridate::ymd(date)
  ) |>
  mutate(
    weekday = lubridate::wday(date),#, label = T, abbr = T), 
    month = lubridate::month(date),#, label = T, abbr = T), 
    year = lubridate::year(date)
  )
test_id = test$id
test = fastDummies::dummy_columns(
  test, remove_first_dummy = T, remove_selected_columns = T,
  select_columns = c("country","store","product")) |>
  select(-id, -date)
head(test)
#
#
#
#
library(forecast)
#
model1 <- auto.arima( 
  log(train$num_sold),
  xreg = as.matrix(train[,-1]),
  nmodels = 200, stationary = T, seasonal = T,
  max.p = 15, max.d = 5,
  max.q = 15, max.D = 5,
  max.P = 12, allowmean = T,
  max.Q = 12, allowdrift = T,
  max.order = 35,
  lambda = "auto", 
  biasadj = T)
#
pred_s = forecast(
  model1, 
  h = h, 
  xreg = as.matrix(test)
)
#
pred_s$mean |> exp() 
#
preditions_arima = data.frame(
  id = test_id,
  num_sold = pred_s$mean |> exp()
  )
#
write.csv(
  preditions_arima, 
  paste0(
    'submissions/sample_submission_arima', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#