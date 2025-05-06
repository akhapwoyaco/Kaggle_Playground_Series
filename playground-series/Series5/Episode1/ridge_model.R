#

#
series_data = list.files(
  path = "playground-series-s5e1/", recursive = T, pattern = '.csv$', 
  full.names = T)
series_data
#
library(tidyverse)
library(readr)
library(glmnet)
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
source('ratios.R')
#
base_date_train = min(train$date)
base_date_test = min(test$date)
#
test = test |> 
  mutate(
    date = lubridate::ymd(date)
  ) |>
  mutate(
    weekday = lubridate::wday(date, label = T, abbr = T), 
    month = lubridate::month(date, label = T, abbr = T), 
    year = lubridate::year(date),
    day_year = difftime(time1 = date, time2 = base_date_train,
                        units = 'days') |> as.numeric()
  ) 
#
dim(test)
test = create_fourier_columns(test, "day_year")
dim(test)
#
#
# head(test)
#
imputation_function <- function(data, country_name, store_name){
  # print(head(data))
  data_1 = data |>
    filter(country %in% country_name, store %in% store_name) |>
    select(-id, -country, - store)
  # print(head(data_1))
  data_1 = data_1 |> 
    pivot_wider(
      id_cols = date, #{{"date"}},
      names_from = product, #{{"product"}},
      values_from = num_sold)#{{"num_sold"}})
  # print(head(data_1))
  #
  nrow_data_1 = nrow(data_1)
  for (i in 2:ncol(data_1)){
    if ( sum(is.na(data_1[,i])) == nrow_data_1  ){
      rates_data = ifelse(
        country_name == "Canada", canada_ratio$average_rates, 
        kenya_ratio$average_rates)
      data_1[,i] = rowSums(data_1[, c(-1,-i)])*rates_data
      # View(data_1)
    } 
    
    data_1[,i] = zoo::na.approx(data_1[,i]) 
    
  }
  
  data_1 = data_1 |> 
    pivot_longer(
      cols = !date,#`Holographic Goose`:`Kerneler Dark Mode`, 
      names_to = 'product', values_to = "num_sold"
    )
  # print(head(data_1))
  data_1$country = country_name
  data_1$store = store_name
  data_2 = data_1 |> 
    mutate(
      date = lubridate::ymd(date)
    ) |>
    mutate(
      weekday = lubridate::wday(date, label = T, abbr = T), 
      month = lubridate::month(date, label = T, abbr = T), 
      year = lubridate::year(date),
      day_year = difftime(time1 = date, time2 = base_date_train,
                          units = 'days') |> as.numeric()
    ) 
  
  data_2 = create_fourier_columns(data_2, "day_year")
  
  return(data_2)
}
#
train = train |> 
  mutate_if(is.character, as.factor)
test = test |> 
  mutate_if(is.character, as.factor)
#
# train |>
#   head()
# test |>
#   head()
#
predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
  #
  test_country = test |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
  #
  train_country_imputed = data.frame()
  for (unique_store in unique(train_country$store)){
    print(unique_store)
    train_country_imputation = imputation_function(
      data = train_country, country_name = unique_country, 
      store_name = unique_store)
    train_country_imputed = bind_rows(
      train_country_imputed,
      train_country_imputation
    )
    # print(sum(is.na(train_country_imputation)))
    
  }
  #
  print(dim(train_country_imputed))
  print(dim(train_country_imputed))
  # print(head(train_country_imputed))
  train_country_imputed = train_country_imputed |> drop_na()
  # #
  print(000)
  # print(sum(is.na(train_country_imputed)))
  #
  # print(str(train_country_imputed))
  m_formula = formula(num_sold ~ store*product*weekday*month*year+sin_2_7+cos_2_7+sin_2_30_4+cos_2_30_4+sin_2_91_3125+cos_2_91_3125+sin_2_182_625+cos_2_182_625+sin_2_365_25+cos_2_365_25+sin_2_730_5+cos_2_730_5+sin_2_1461+cos_2_1461+sin_4_7+cos_4_7+sin_4_30_4+cos_4_30_4+sin_4_91_3125+cos_4_91_3125+sin_4_182_625+cos_4_182_625+sin_4_365_25+cos_4_365_25+sin_4_730_5+cos_4_730_5+sin_4_1461+cos_4_1461+sin_6_7+cos_6_7+sin_6_30_4+cos_6_30_4+sin_6_91_3125+cos_6_91_3125+sin_6_182_625+cos_6_182_625+sin_6_365_25+cos_6_365_25+sin_6_730_5+cos_6_730_5+sin_6_1461+cos_6_1461+sin_8_7+cos_8_7+sin_8_30_4+cos_8_30_4+sin_8_91_3125+cos_8_91_3125+sin_8_182_625+cos_8_182_625+sin_8_365_25+cos_8_365_25+sin_8_730_5+cos_8_730_5+sin_8_1461+cos_8_1461+sin_10_7+cos_10_7+sin_10_30_4+cos_10_30_4+sin_10_91_3125+cos_10_91_3125+sin_10_182_625+cos_10_182_625+sin_10_365_25+cos_10_365_25+sin_10_730_5+cos_10_730_5+sin_10_1461+cos_10_1461+sin_12_7+cos_12_7+sin_12_30_4+cos_12_30_4+sin_12_91_3125+cos_12_91_3125+sin_12_182_625+cos_12_182_625+sin_12_365_25+cos_12_365_25+sin_12_730_5+cos_12_730_5+sin_12_1461+cos_12_1461+sin_14_7+cos_14_7+sin_14_30_4+cos_14_30_4+sin_14_91_3125+cos_14_91_3125+sin_14_182_625+cos_14_182_625+sin_14_365_25+cos_14_365_25+sin_14_730_5+cos_14_730_5+sin_14_1461+cos_14_1461)
  x_var = model.matrix(
    m_formula,
    data = train_country_imputed[,c(-1,-4,-9)]
  )[,-1]
  #
  y_var = log(train_country_imputed$num_sold)
  #
  lambdas = 10^seq(3,-3, by = -.1)
  ridge_cv = cv.glmnet(
    x_var, y_var, alpha = 1, lambda = lambdas, parallel = !T, nfolds = 11
  )
  #
  best_lambda = ridge_cv$lambda.min
  # best_lambda
  best_ridge = glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)
  #
  print(001)
  # print(sum(is.na(test_country1)))
  n_formula = update(m_formula, NULL ~ store*product*weekday*month*year+sin_2_7+cos_2_7+sin_2_30_4+cos_2_30_4+sin_2_91_3125+cos_2_91_3125+sin_2_182_625+cos_2_182_625+sin_2_365_25+cos_2_365_25+sin_2_730_5+cos_2_730_5+sin_2_1461+cos_2_1461+sin_4_7+cos_4_7+sin_4_30_4+cos_4_30_4+sin_4_91_3125+cos_4_91_3125+sin_4_182_625+cos_4_182_625+sin_4_365_25+cos_4_365_25+sin_4_730_5+cos_4_730_5+sin_4_1461+cos_4_1461+sin_6_7+cos_6_7+sin_6_30_4+cos_6_30_4+sin_6_91_3125+cos_6_91_3125+sin_6_182_625+cos_6_182_625+sin_6_365_25+cos_6_365_25+sin_6_730_5+cos_6_730_5+sin_6_1461+cos_6_1461+sin_8_7+cos_8_7+sin_8_30_4+cos_8_30_4+sin_8_91_3125+cos_8_91_3125+sin_8_182_625+cos_8_182_625+sin_8_365_25+cos_8_365_25+sin_8_730_5+cos_8_730_5+sin_8_1461+cos_8_1461+sin_10_7+cos_10_7+sin_10_30_4+cos_10_30_4+sin_10_91_3125+cos_10_91_3125+sin_10_182_625+cos_10_182_625+sin_10_365_25+cos_10_365_25+sin_10_730_5+cos_10_730_5+sin_10_1461+cos_10_1461+sin_12_7+cos_12_7+sin_12_30_4+cos_12_30_4+sin_12_91_3125+cos_12_91_3125+sin_12_182_625+cos_12_182_625+sin_12_365_25+cos_12_365_25+sin_12_730_5+cos_12_730_5+sin_12_1461+cos_12_1461+sin_14_7+cos_14_7+sin_14_30_4+cos_14_30_4+sin_14_91_3125+cos_14_91_3125+sin_14_182_625+cos_14_182_625+sin_14_365_25+cos_14_365_25+sin_14_730_5+cos_14_730_5+sin_14_1461+cos_14_1461)
  test_country1 = model.matrix(
    n_formula,
    test_country[,c(-1, -2,-3)]
  )[,-1]
  #
  #
  print(002)
  prediction_c = predict(best_ridge, s = best_lambda, newx = test_country1) |>
    c() |> exp()
  #
  actual = y_var
  pred = predict(best_ridge, s = best_lambda, newx = x_var)
  rss = sum((pred - actual)^2)
  tss = sum((actual - mean(actual))^2)
  rsq = 1- rss/tss
  print(paste("R Squared:",rsq, sep = " "))
  #
  rm(x_var)
  rm(test_country1)
  #
  predictions_data_frame = bind_rows(
    predictions_data_frame,
    data.frame(
      id = test_country$id,
      num_sold = prediction_c)
  )
  # print(head(predictions_data_frame))
}
#
View(predictions_data_frame)
#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_multiple_lm_hierachy_country', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#
#