#
#
series_data = list.files(
  path = "playground-series-s5e1/", recursive = T, pattern = '.csv$', 
  full.names = T)
series_data
#
library(tidyverse)
library(readr)
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
train |> str()
test |> str()
sample_submission |> head()
#
head(train)
head(test)
#
dim(train); dim(test); dim(sample_submission)
#
sapply(train, FUN = function(x) sum(is.na(x)))
sapply(test, FUN = function(x) sum(is.na(x)))
#
base_date = lubridate::ymd(train$date)[1]
base_date
#
train = train |> 
  mutate(
    date = lubridate::ymd(date)
  ) |>
  mutate(
    weekday = lubridate::wday(date, label = T, abbr = T), 
    month = lubridate::month(date, label = T, abbr = T), 
    year = lubridate::year(date),
    # date_seq = difftime(
    #   date, base_date, units = "days") |> as.numeric()
    is_weekend = chron::is.weekend(date)+0
  )
head(train)
#
test = test |> 
  mutate(
    date = lubridate::ymd(date)
  ) |>
  mutate(
    weekday = lubridate::wday(date, label = T, abbr = T), 
    month = lubridate::month(date, label = T, abbr = T), 
    year = lubridate::year(date)#,
    # date_seq = difftime(
    #   date, base_date, units = "days") |> as.numeric()
    # is_weekend = chron::is.weekend(date)+0
  )
head(test)
#
#
multiple_lm_model1 = lm(formula = log(num_sold) ~ country+store+product+weekday+month+year, data = train)
summary(multiple_lm_model1)
#
prediction_multiple_lm_model1 <- predict(multiple_lm_model1, newdata = test)
#
head(prediction_multiple_lm_model1)
#
sample_submission$num_sold = prediction_multiple_lm_model1
sample_submission = sample_submission |>
  mutate(
    num_sold = ifelse(num_sold < 0, 0, num_sold)
  )
#
write.csv(
  sample_submission, 
  paste0(
    'submissions/sample_submission_multiple_lm_model1_', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#
#
# multiple_lm_model2 = lm(formula = log(num_sold) ~ country*store*product+weekday*month+year, data = train)
# summary(multiple_lm_model2)
# #
# multiple_lm_model3 = lm(formula = log(num_sold) ~ country*store*product+is_weekend*weekday*month+year, data = train)
# summary(multiple_lm_model3)
# #
# prediction_multiple_lm_model2 <- predict(multiple_lm_model2, newdata = test) |> exp()
# #
# head(prediction_multiple_lm_model2)
# #
# summary(prediction_multiple_lm_model2)
# #
# sample_submission$num_sold = prediction_multiple_lm_model2
# # sample_submission = sample_submission |>
# #   mutate(
# #     num_sold = ifelse(num_sold < 0, 0, num_sold)
# #   )
# #
# write.csv(
#   sample_submission, 
#   paste0(
#     'submissions/sample_submission_multiple_lm_model2_', 
#     strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
# #
# #
# 
# 
# #
# nrow_train = nrow(train)
# nrow_train
# ## 75% of the sample size
# smp_size <- floor(0.95 * nrow(train))
# #
# set.seed(123)
# train_ind <- sample(seq_len(nrow(train)), size = smp_size)
# train_train = train[train_ind, ]
# train_test = train[-train_ind, ]
# #
# multiple_lm_model3 = lm(formula = log(num_sold) ~ country*store*product+weekday*month+year, data = train_train)
# summary(multiple_lm_model3)
# #
# prediction_multiple_lm_model3 <- predict(
#   multiple_lm_model3, newdata = train_test
# ) |> exp()
# #
# 
# #
# # ridge model
# #
# train = train[complete.cases(train$num_sold),]
# #
# train_x <- model.matrix(num_sold ~ country*store*product+weekday*month+year, data = train)[, -1]
# train_y <- log(train$num_sold)
# test_x <- model.matrix(1~country*store*product+weekday*month+year, data = test)
# #
# 
# library(glmnet)
# grid <- 10^seq(10, -2, length = 100)
# ridge.mod <- glmnet(train, train_y, alpha = 0, lambda = grid)
# #
# summary(ridge.mod)
# #
# ridge.pred <- predict(ridge.mod , s = 4, newx = test)
# #
# #
# #

#
# predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  test_country = test |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  #
  model_country = lm(
    formula = log(num_sold) ~ store*product*weekday*month+year,
    data = train_country)
  print(summary(model_country)$r.squared)
}
#
#
predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  test_country = test |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  #
  model_country = lm(
    formula = log(num_sold) ~ store*product*weekday*month*year,
    data = train_country)
  print(summary(model_country)$r.squared)
  #
  prediction_c = predict(
    model_country, newdata = test_country) |> 
    unname() |> exp()
  #
  predictions_data_frame = bind_rows(
    predictions_data_frame,
    data.frame(
      id = test_country$id,
      num_sold = prediction_c)
  )
  # for (unique_product in unique(train_country$store)){
  #   print("By Product")
  #   print(unique_product)
  # #   #
  #   train_country_store = train_country |>
  #     dplyr::filter(product %in% unique_product) |>
  #     select(-product)
  #   test_country_store = test_country |>
  #     dplyr::filter(product %in% unique_product) |>
  #     select(-product)
  #   #
  #   # print(unique(train_country_store$product))
  #   # print(unique(test_country_store$product))
  #   #
  #   model_country_store = lm(
  #     formula = log(num_sold) ~ store+weekday*month+year,
  #     data = train_country_store)
  #   print(summary(model_country_store)$r.squared)
  # 
  # prediction_s_store = predict(
  #   model_country_store, newdata = test_country_store) |> exp()
  # #
  # predictions_data_frame = bind_rows(
  #   predictions_data_frame,
  #   data.frame(
  #     id = test_country_store$id,
  #     num_sold = prediction_s_store)
  # )
  # }
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

########
#
global_holidays <- read_csv(
  "D:\\DATAPROJECTS\\Tidytuesday\\2024\\2024-12-24\\global_holidays.csv"
) |> 
  mutate(
    date = lubridate::ymd(Date),
    country = ADM_name
  ) |>
  select(-ADM_name, -Name, -ISO3, -Date)
#
unique_countries = unique(train$country)
global_holidays = global_holidays |>
  filter(country %in% unique_countries)
#
train_hol = merge(
  train, 
  global_holidays |> filter(date <= max(train$date)),
  by = c("date","country"), 
  all.x = T, all.y = T
)
train_hol = train_hol[!duplicated(train_hol$id),]
#
dim(train)
dim(train_hol)
# testdata
test_hol = merge(
  test, 
  global_holidays |> filter(date >= min(test$date)),
  by = c("date","country"), 
  all.x = T, all.y = T
)
test_hol = test_hol[!duplicated(test_hol$id),]
#
dim(test)
dim(test_hol)
#
train_hol$Type = ifelse(is.na(train_hol$Type), "No Holiday", train_hol$Type)
test_hol$Type = ifelse(is.na(test_hol$Type), "No Holiday", test_hol$Type)
#
# added holiday aspect
predictions_data_frame = data.frame()
for (unique_country in unique(train_hol$country)){
  print(unique_country)
  # by country train and test
  train_country = train_hol |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  test_country = test_hol |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  #
  model_country = lm(
    formula = log(num_sold) ~ store*product*weekday*month*year+Type,
    data = train_country)
  print(summary(model_country)$r.squared)
  #
  prediction_c = predict(
    model_country, newdata = test_country) |> 
    unname() |> exp()
  #
  predictions_data_frame = bind_rows(
    predictions_data_frame,
    data.frame(
      id = test_country$id,
      num_sold = prediction_c)
  )
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
# combination of store product + dates
# [1] "Canada"
# [1] 0.955172
# [1] "Finland"
# [1] 0.9738138
# [1] "Italy"
# [1] 0.9757831
# [1] "Kenya"
# [1] 0.9604168
# [1] "Norway"
# [1] 0.9619317
# [1] "Singapore"
# [1] 0.9760184
# #
# # all interactions
# #
# [1] "Canada"
# [1] 0.9782012
# [1] "Finland"
# [1] 0.9872581
# [1] "Italy"
# [1] 0.9893738
# [1] "Kenya"
# [1] 0.9846226
# [1] "Norway"
# [1] 0.9752516
# [1] "Singapore"
# [1] 0.9894303
#
1] "Canada"
[1] 0.97788
[1] "Finland"
[1] 0.9871063
[1] "Italy"
[1] 0.9892218
[1] "Kenya"
[1] 0.9839892
[1] "Norway"
[1] 0.975083
[1] "Singapore"
[1] 0.9892618
#
############################################################################
#
#
#
library(randomForest)
# predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) |>
    select(-country) |> drop_na()
  test_country = test |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  #
  set.seed(1)
  model_country = randomForest(
    formula = log(num_sold) ~ store*product*weekday*month*year,
    data = train_country, mtry = 12, importance = TRUE)
  model_country
  # print(summary(model_country)$r.squared)
}
#
library(tidyverse)
library(randomForest)
#
predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) |>
    select(-country) |> drop_na()
  test_country = test |>
    dplyr::filter(country %in% unique_country) |>
    select(-country)
  #
  set.seed(1)
  model_country = randomForest(
    formula = log(num_sold) ~ store*product*weekday*month*year,
    data = train_country, mtry = 5, importance = TRUE)
  #
  prediction_c = predict(
    model_country, newdata = test_country) |> 
    unname() |> exp()
  #
  predictions_data_frame = bind_rows(
    predictions_data_frame,
    data.frame(
      id = test_country$id,
      num_sold = prediction_c)
  )
}
#
View(predictions_data_frame)
#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_randomforest_hierachy_country', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#


########## IMPUTED DATA ######################################################
#
#
series_data = list.files(
  path = "playground-series-s5e1/", recursive = T, pattern = '.csv$', 
  full.names = T)
series_data
#
library(tidyverse)
library(readr)
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
test = test |> 
  mutate(
    date = lubridate::ymd(date)
  ) |>
  mutate(
    weekday = lubridate::wday(date, label = T, abbr = T), 
    month = lubridate::month(date, label = T, abbr = T), 
    year = lubridate::year(date)
  )
head(test)
#
imputation_function <- function(data, country_name, store_name){
  print(head(data))
  data_1 = data |>
    filter(country %in% country_name, store %in% store_name) |>
    select(-id, -country, - store)
  # print(head(data_1))
  data_1 = data_1 |> 
    pivot_wider(
      id_cols = date, #{{"date"}},
      names_from = product, #{{"product"}},
      values_from = num_sold)#{{"num_sold"}})
  print(head(data_1))
  #
  nrow_data_1 = nrow(data_1)
  for (i in 2:ncol(data_1)){
    if ( sum(is.na(data_1[,i])) == nrow_data_1  ){
      rates_data = ifelse(
        country_name == "Canada", canada_ratio$average_rates, 
        kenya_ratio$average_rates)
      data_1[,i] = rowSums(data_1[, c(-1,-i)])*rates_data
    } else {
      data_1[,i] = zoo::na.approx(data_1[,i]) 
    }
  }
  #
  data_1 = data_1 |> 
    pivot_longer(
      cols = !date,#`Holographic Goose`:`Kerneler Dark Mode`, 
      names_to = 'product', values_to = "num_sold"
    )
  data_1$country = country_name
  data_1$store = store_name
  data_1 |> 
    mutate(
      date = lubridate::ymd(date)
    ) |>
    mutate(
      weekday = lubridate::wday(date, label = T, abbr = T), 
      month = lubridate::month(date, label = T, abbr = T), 
      year = lubridate::year(date)
    )
}
#
# mm = imputation_function(train, country_name = "Canada", store_name = "Premium Sticker Mart")
#
predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
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
  }
  #
  model_country = lm(
    formula = log(num_sold) ~ store*product*weekday*month*year,
    data = train_country_imputed)
  print("R Squared: ")
  print(summary(model_country)$r.squared)
  #
  prediction_c = predict(
    model_country, newdata = test_country) |> 
    unname() |> exp()
  #
  predictions_data_frame = bind_rows(
    predictions_data_frame,
    data.frame(
      id = test_country$id,
      num_sold = prediction_c)
  )
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

##############################################################################################


#                                                                                       | usd       |     alpa 0       |                usd |           holiday | gbm                | alpha 0 fourier   | alpha with 80 fourier | fourier 14     | fourier 18x     | fourier 20 x           | fourier 25x       | fourier 30x    |  fourier 14

1] "Canada"
[1] 0.97788 | [1] 0.9868899 | 0.9874114 | 0.9879915 | 0.9884002 | 0.9885278 | 0.9889877 | 0.9929751 | 0.988687049772162 | 0.992511578999465 x| 0.988978955772425x| 0.992370188963454 x| 0.989027864898006 | 0.987982248550466 x| 0.989177869499172 | 0.989396009351356 | 0.994089490579641 | 0.994179127513711 | 0.994234359936191 | 0.993934786116181
[1] "Finland"
[1] 0.9871063 | 0.9872581 | 0.9879817 | 0.9887305 | 0.9882351 | 0.988635 | 0.9889766 | 0.9918637 | 0.988811321597638 | 0.991522538504228     | 0.989047738605308 | 0.992396406075167 | 0.989214025925875 | 0.989089222988345 | 0.989389982902007 | 0.989632520623183 | 0.993797141420986 | 0.993890557042008 | 0.993979109165165 | 0.993455664750426
[1] "Italy"
[1] 0.9892218 | 0.9893738 | 0.9899141 | 0.9905177 | 0.9900283 | 0.990143 | 0.9905351 | 0.9927071 |  0.990369857367207 | 0.992401473781122    | 0.990552897632068 | 0.992427956926557 | 0.990704974823084 | 0.990821002301315 | 0.990836746305168 | 0.991078333187913 | 0.994460524604009 | 0.994627911466513 | 0.994706645706474 | 0.994217339075757
[1] "Kenya"
[1] 0.9839892 | 0.9911747 | 0.991808 | 0.9926214 | 0.9915662 | 0.9917456 | 0.9920964 | 0.9922561 | 0.99003875042251 | 0.990181093006817      | 0.990301431553122 | 0.991717085745656 | 0.990471294998781 | 0.990893647256722 | 0.99059450155445 | 0.9908812415089 | 0.991633349127414 | 0.991824139274278 | 0.991890160275812 | 0.991396256735319
[1] "Norway"
[1] 0.975083 | 0.9752516 | 0.9756266 | 0.9763942 | 0.978721 | 0.9788632 | 0.9795607 | 0.991625 | 0.979359957546862 | 0.990779115655475       | 0.979862846825049 | 0.992638243562639 | 0.979870331208419 | 0.9765764668857   | 0.980151127995551 | 0.980461580550766 | 0.993546798110327 | 0.993742992477139 | 0.993845510871886 | 0.993178584571887
[1] "Singapore"
[1] 0.9892618 | 0.9894303 | 0.9898053 | 0.990377 | 0.9908017 | 0.9909214 | 0.9911693 | 0.99273 | 0.990982367198937 | 0.992376534959975       | 0.991122660164887 | 0.992475377064318 | 0.991356984164928 | 0.990513241723884 | 0.991475275944436 | 0.991684444885202 | 0.993834356131589 | 0.993966782205827   | 0.994023217280083  | 0.993662833228992             

#




#
0.9617992 | 0.9620517 | 0.9621641 | 0.9622963 | 0.9624735 | 0.9625704 | 0.9634297
0.9633155 | 0.9637352 | 0.9639744 | 0.9642038 | 0.9644018 | 0.9645619 | 0.9656219
0.9615938 | 0.9618182 | 0.9621296 | 0.9621876 | 0.9623743 | 0.9625094 | 0.9633892
0.9362303 | 0.9364141 | 0.9366014 | 0.93683 | 0.9370339 | 0.9371217 | 0.9383323
0.9423028 | 0.9425034 | 0.9427936 | 0.9428631 | 0.9428977 | 0.9429868 | 0.9440228
0.9614601 | 0.9615311 | 0.9616491 | 0.9616667 | 0.9617729 | 0.9618277 | 0.962641
#
#
################THIS IS IIITT ################################################
#
# 0.07
series_data = list.files(
  path = "playground-series-s5e1/", recursive = T, pattern = '.csv$', 
  full.names = T)
series_data
#
library(tidyverse)
library(readr)
library(glmnet)
#
# load("D:/KAGGLE/Kaggle Playground Series/playground-series/Series5/Episode1/usd_rates_data.RData")
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
source('ratios.R')
#
# global_holidays <- read_csv(
#   "D:\\DATAPROJECTS\\Tidytuesday\\2024\\2024-12-24\\global_holidays.csv"
# ) |> 
#   mutate(
#     date = lubridate::ymd(Date),
#     country = ADM_name
#   ) |>
#   select(-ADM_name, -Name, -ISO3, -Date) |>
#   mutate(
#     Type = "Holiday"
#   ) |> distinct()
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
  ) |>
  mutate(
    si_n = sin(2*pi*day_year/7),
    co_s = cos(2*pi*day_year/7),
    si_n4 = sin(2*pi*day_year/30.4),
    co_s4 = cos(2*pi*day_year/30.4),
    si_n6 = sin(2*pi*day_year/365.25/4),
    co_s6 = cos(2*pi*day_year/365.25/4),
    si_n8 = sin(2*pi*day_year/365.25/2),
    co_s8 = cos(2*pi*day_year/365.25/2),
    si_n12 = sin(2*pi*day_year/365.25),
    co_s12 = cos(2*pi*day_year/365.25),
    si_n24 = sin(2*pi*day_year/365.25*2),
    co_s24 = cos(2*pi*day_year/365.25*2),
    si_n1 = sin(4*pi*day_year/7),
    co_s1 = cos(4*pi*day_year/7),
    si_n41 = sin(4*pi*day_year/30.4),
    co_s41 = cos(4*pi*day_year/30.4),
    si_n61 = sin(4*pi*day_year/365.25/4),
    co_s61 = cos(4*pi*day_year/365.25/4),
    si_n81 = sin(4*pi*day_year/365.25/2),
    co_s81 = cos(4*pi*day_year/365.25/2),
    si_n121 = sin(4*pi*day_year/365.25),
    co_s121 = cos(4*pi*day_year/365.25),
    si_n241 = sin(4*pi*day_year/365.25*2),
    co_s241 = cos(4*pi*day_year/365.25*2),
    si_n11 = sin(6*pi*day_year/7),
    co_s11 = cos(6*pi*day_year/7),
    si_n411 = sin(6*pi*day_year/30.4),
    co_s411 = cos(6*pi*day_year/30.4),
    si_n611 = sin(6*pi*day_year/365.25/4),
    co_s611 = cos(6*pi*day_year/365.25/4),
    si_n811 = sin(6*pi*day_year/365.25/2),
    co_s811 = cos(6*pi*day_year/365.25/2),
    si_n1211 = sin(6*pi*day_year/365.25),
    co_s1211 = cos(6*pi*day_year/365.25),
    si_n2411 = sin(6*pi*day_year/365.25*2),
    co_s2411 = cos(6*pi*day_year/365.25*2),
    si_n111 = sin(8*pi*day_year/7),
    co_s111 = cos(8*pi*day_year/7),
    si_n4111 = sin(8*pi*day_year/30.4),
    co_s4111 = cos(8*pi*day_year/30.4),
    si_n6111 = sin(8*pi*day_year/365.25/4),
    co_s6111 = cos(8*pi*day_year/365.25/4),
    si_n8111 = sin(8*pi*day_year/365.25/2),
    co_s8111 = cos(8*pi*day_year/365.25/2),
    si_n12111 = sin(8*pi*day_year/365.25),
    co_s12111 = cos(8*pi*day_year/365.25),
    si_n24111 = sin(8*pi*day_year/365.25*2),
    co_s24111 = cos(8*pi*day_year/365.25*2),
    si_n1111 = sin(10*pi*day_year/7),
    co_s1111 = cos(10*pi*day_year/7),
    si_n41111 = sin(10*pi*day_year/30.4),
    co_s41111 = cos(10*pi*day_year/30.4),
    si_n61111 = sin(10*pi*day_year/365.25/4),
    co_s61111 = cos(10*pi*day_year/365.25/4),
    si_n81111 = sin(10*pi*day_year/365.25/2),
    co_s81111 = cos(10*pi*day_year/365.25/2),
    si_n121111 = sin(10*pi*day_year/365.25),
    co_s121111 = cos(10*pi*day_year/365.25),
    si_n241111 = sin(10*pi*day_year/365.25*2),
    co_s241111 = cos(10*pi*day_year/365.25*2),
    si_n11111 = sin(12*pi*day_year/7),
    co_s11111 = cos(12*pi*day_year/7),
    si_n411111 = sin(12*pi*day_year/30.4),
    co_s411111 = cos(12*pi*day_year/30.4),
    si_n611111 = sin(12*pi*day_year/365.25/4),
    co_s611111 = cos(12*pi*day_year/365.25/4),
    si_n811111 = sin(12*pi*day_year/365.25/2),
    co_s811111 = cos(12*pi*day_year/365.25/2),
    si_n1211111 = sin(12*pi*day_year/365.25),
    co_s1211111 = cos(12*pi*day_year/365.25),
    si_n2411111 = sin(12*pi*day_year/365.25*2),
    co_s2411111 = cos(12*pi*day_year/365.25*2),
    si_n111111 = sin(14*pi*day_year/7),
    co_s111111 = cos(14*pi*day_year/7),
    si_n4111111 = sin(14*pi*day_year/30.4),
    co_s4111111 = cos(14*pi*day_year/30.4),
    si_n6111111 = sin(14*pi*day_year/365.25/4),
    co_s6111111 = cos(14*pi*day_year/365.25/4),
    si_n8111111 = sin(14*pi*day_year/365.25/2),
    co_s8111111 = cos(14*pi*day_year/365.25/2),
    si_n12111111 = sin(14*pi*day_year/365.25),
    co_s12111111 = cos(14*pi*day_year/365.25),
    si_n24111111 = sin(14*pi*day_year/365.25*2),
    co_s24111111 = cos(14*pi*day_year/365.25*2),
    si_n1111111 = sin(16*pi*day_year/7),
    co_s1111111 = cos(16*pi*day_year/7),
    si_n41111111 = sin(16*pi*day_year/30.4),
    co_s41111111 = cos(16*pi*day_year/30.4),
    si_n61111111 = sin(16*pi*day_year/365.25/4),
    co_s61111111 = cos(16*pi*day_year/365.25/4),
    si_n81111111 = sin(16*pi*day_year/365.25/2),
    co_s81111111 = cos(16*pi*day_year/365.25/2),
    si_n121111111 = sin(16*pi*day_year/365.25),
    co_s121111111 = cos(16*pi*day_year/365.25),
    si_n241111111 = sin(16*pi*day_year/365.25*2),
    co_s241111111 = cos(16*pi*day_year/365.25*2),
    si_n11111111 = sin(18*pi*day_year/7),
    co_s11111111 = cos(18*pi*day_year/7),
    si_n411111111 = sin(18*pi*day_year/30.4),
    co_s411111111 = cos(18*pi*day_year/30.4),
    si_n611111111 = sin(18*pi*day_year/365.25/4),
    co_s611111111 = cos(18*pi*day_year/365.25/4),
    si_n811111111 = sin(18*pi*day_year/365.25/2),
    co_s811111111 = cos(18*pi*day_year/365.25/2),
    si_n1211111111 = sin(18*pi*day_year/365.25),
    co_s1211111111 = cos(18*pi*day_year/365.25),
    si_n2411111111 = sin(18*pi*day_year/365.25*2),
    co_s2411111111 = cos(18*pi*day_year/365.25*2),
    si_n111111111 = sin(20*pi*day_year/7),
    co_s111111111 = cos(20*pi*day_year/7),
    si_n4111111111 = sin(20*pi*day_year/30.4),
    co_s4111111111 = cos(20*pi*day_year/30.4),
    si_n6111111111 = sin(20*pi*day_year/365.25/4),
    co_s6111111111 = cos(20*pi*day_year/365.25/4),
    si_n8111111111 = sin(20*pi*day_year/365.25/2),
    co_s8111111111 = cos(20*pi*day_year/365.25/2),
    si_n12111111111 = sin(20*pi*day_year/365.25),
    co_s12111111111 = cos(20*pi*day_year/365.25),
    si_n24111111111 = sin(20*pi*day_year/365.25*2),
    co_s24111111111 = cos(20*pi*day_year/365.25*2)
  )
# test$day_year2 = difftime(time1 = test$date, time2 = base_date_test,
#                          units = 'days') |> as.numeric()
head(test)
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
    ) |>
    mutate(
      si_n = sin(2*pi*day_year/7),
      co_s = cos(2*pi*day_year/7),
      si_n4 = sin(2*pi*day_year/30.4),
      co_s4 = cos(2*pi*day_year/30.4),
      si_n6 = sin(2*pi*day_year/365.25/4),
      co_s6 = cos(2*pi*day_year/365.25/4),
      si_n8 = sin(2*pi*day_year/365.25/2),
      co_s8 = cos(2*pi*day_year/365.25/2),
      si_n12 = sin(2*pi*day_year/365.25),
      co_s12 = cos(2*pi*day_year/365.25),
      si_n24 = sin(2*pi*day_year/365.25*2),
      co_s24 = cos(2*pi*day_year/365.25*2),
      si_n1 = sin(4*pi*day_year/7),
      co_s1 = cos(4*pi*day_year/7),
      si_n41 = sin(4*pi*day_year/30.4),
      co_s41 = cos(4*pi*day_year/30.4),
      si_n61 = sin(4*pi*day_year/365.25/4),
      co_s61 = cos(4*pi*day_year/365.25/4),
      si_n81 = sin(4*pi*day_year/365.25/2),
      co_s81 = cos(4*pi*day_year/365.25/2),
      si_n121 = sin(4*pi*day_year/365.25),
      co_s121 = cos(4*pi*day_year/365.25),
      si_n241 = sin(4*pi*day_year/365.25*2),
      co_s241 = cos(4*pi*day_year/365.25*2),
      si_n11 = sin(6*pi*day_year/7),
      co_s11 = cos(6*pi*day_year/7),
      si_n411 = sin(6*pi*day_year/30.4),
      co_s411 = cos(6*pi*day_year/30.4),
      si_n611 = sin(6*pi*day_year/365.25/4),
      co_s611 = cos(6*pi*day_year/365.25/4),
      si_n811 = sin(6*pi*day_year/365.25/2),
      co_s811 = cos(6*pi*day_year/365.25/2),
      si_n1211 = sin(6*pi*day_year/365.25),
      co_s1211 = cos(6*pi*day_year/365.25),
      si_n2411 = sin(6*pi*day_year/365.25*2),
      co_s2411 = cos(6*pi*day_year/365.25*2),
      si_n111 = sin(8*pi*day_year/7),
      co_s111 = cos(8*pi*day_year/7),
      si_n4111 = sin(8*pi*day_year/30.4),
      co_s4111 = cos(8*pi*day_year/30.4),
      si_n6111 = sin(8*pi*day_year/365.25/4),
      co_s6111 = cos(8*pi*day_year/365.25/4),
      si_n8111 = sin(8*pi*day_year/365.25/2),
      co_s8111 = cos(8*pi*day_year/365.25/2),
      si_n12111 = sin(8*pi*day_year/365.25),
      co_s12111 = cos(8*pi*day_year/365.25),
      si_n24111 = sin(8*pi*day_year/365.25*2),
      co_s24111 = cos(8*pi*day_year/365.25*2),
      si_n1111 = sin(10*pi*day_year/7),
      co_s1111 = cos(10*pi*day_year/7),
      si_n41111 = sin(10*pi*day_year/30.4),
      co_s41111 = cos(10*pi*day_year/30.4),
      si_n61111 = sin(10*pi*day_year/365.25/4),
      co_s61111 = cos(10*pi*day_year/365.25/4),
      si_n81111 = sin(10*pi*day_year/365.25/2),
      co_s81111 = cos(10*pi*day_year/365.25/2),
      si_n121111 = sin(10*pi*day_year/365.25),
      co_s121111 = cos(10*pi*day_year/365.25),
      si_n241111 = sin(10*pi*day_year/365.25*2),
      co_s241111 = cos(10*pi*day_year/365.25*2),
      si_n11111 = sin(12*pi*day_year/7),
      co_s11111 = cos(12*pi*day_year/7),
      si_n411111 = sin(12*pi*day_year/30.4),
      co_s411111 = cos(12*pi*day_year/30.4),
      si_n611111 = sin(12*pi*day_year/365.25/4),
      co_s611111 = cos(12*pi*day_year/365.25/4),
      si_n811111 = sin(12*pi*day_year/365.25/2),
      co_s811111 = cos(12*pi*day_year/365.25/2),
      si_n1211111 = sin(12*pi*day_year/365.25),
      co_s1211111 = cos(12*pi*day_year/365.25),
      si_n2411111 = sin(12*pi*day_year/365.25*2),
      co_s2411111 = cos(12*pi*day_year/365.25*2),
      si_n111111 = sin(14*pi*day_year/7),
      co_s111111 = cos(14*pi*day_year/7),
      si_n4111111 = sin(14*pi*day_year/30.4),
      co_s4111111 = cos(14*pi*day_year/30.4),
      si_n6111111 = sin(14*pi*day_year/365.25/4),
      co_s6111111 = cos(14*pi*day_year/365.25/4),
      si_n8111111 = sin(14*pi*day_year/365.25/2),
      co_s8111111 = cos(14*pi*day_year/365.25/2),
      si_n12111111 = sin(14*pi*day_year/365.25),
      co_s12111111 = cos(14*pi*day_year/365.25),
      si_n24111111 = sin(14*pi*day_year/365.25*2),
      co_s24111111 = cos(14*pi*day_year/365.25*2),
      si_n1111111 = sin(16*pi*day_year/7),
      co_s1111111 = cos(16*pi*day_year/7),
      si_n41111111 = sin(16*pi*day_year/30.4),
      co_s41111111 = cos(16*pi*day_year/30.4),
      si_n61111111 = sin(16*pi*day_year/365.25/4),
      co_s61111111 = cos(16*pi*day_year/365.25/4),
      si_n81111111 = sin(16*pi*day_year/365.25/2),
      co_s81111111 = cos(16*pi*day_year/365.25/2),
      si_n121111111 = sin(16*pi*day_year/365.25),
      co_s121111111 = cos(16*pi*day_year/365.25),
      si_n241111111 = sin(16*pi*day_year/365.25*2),
      co_s241111111 = cos(16*pi*day_year/365.25*2),
      si_n11111111 = sin(18*pi*day_year/7),
      co_s11111111 = cos(18*pi*day_year/7),
      si_n411111111 = sin(18*pi*day_year/30.4),
      co_s411111111 = cos(18*pi*day_year/30.4),
      si_n611111111 = sin(18*pi*day_year/365.25/4),
      co_s611111111 = cos(18*pi*day_year/365.25/4),
      si_n811111111 = sin(18*pi*day_year/365.25/2),
      co_s811111111 = cos(18*pi*day_year/365.25/2),
      si_n1211111111 = sin(18*pi*day_year/365.25),
      co_s1211111111 = cos(18*pi*day_year/365.25),
      si_n2411111111 = sin(18*pi*day_year/365.25*2),
      co_s2411111111 = cos(18*pi*day_year/365.25*2),
      si_n111111111 = sin(20*pi*day_year/7),
      co_s111111111 = cos(20*pi*day_year/7),
      si_n4111111111 = sin(20*pi*day_year/30.4),
      co_s4111111111 = cos(20*pi*day_year/30.4),
      si_n6111111111 = sin(20*pi*day_year/365.25/4),
      co_s6111111111 = cos(20*pi*day_year/365.25/4),
      si_n8111111111 = sin(20*pi*day_year/365.25/2),
      co_s8111111111 = cos(20*pi*day_year/365.25/2),
      si_n12111111111 = sin(20*pi*day_year/365.25),
      co_s12111111111 = cos(20*pi*day_year/365.25),
      si_n24111111111 = sin(20*pi*day_year/365.25*2),
      co_s24111111111 = cos(20*pi*day_year/365.25*2)
      
      
      
      
      
      
    )
  return(data_2)
}
#
# test2 = left_join(
#   test, 
#   global_holidays |> filter(date >= min(test$date)),
#   by = c("date","country")
# )
# test = merge(
#   test, 
#   global_holidays |> filter(date >= min(test$date)),
#   by = c("date","country"), 
#   all.x = T, all.y = F) |> distinct()
# test$Type = ifelse(
#   is.na(test$Type), "No Holiday", 
#   test$Type)
# # View(test2)
dim(test)
#
train = train |> 
  mutate_if(is.character, as.factor)
test = test |> 
  mutate_if(is.character, as.factor)
#
train |>
  head()
test |>
  head()
#
# str(train)
# str(test)
##train

# mm = imputation_function(train, country_name = "Canada", store_name = "Premium Sticker Mart")
#
predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # usd
  # country_usd = usd_rates_approx_long |>
  #   filter(country %in% unique_country) |> select(-country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
  # country_dates = global_holidays |> 
  #   dplyr::filter(country %in% unique_country) |>
  #   filter(date <= max(train_country_imputed$date)) |>
  #   select(-country)
  #
  test_country = test |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
  # test_country = merge(
  #   test_country, country_usd, by = 'date', all.x = T) |>
  #   distinct() |> fill(usd, .direction = "downup")
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
  # print(sapply(train_country_imputed, FUN = function(x) (sum(is.na(x)))))
  # train_country_imputed = merge(
  #   train_country_imputed, country_usd, by = 'date', all.x = T
  # ) |> distinct() |> fill(usd, .direction = "downup")
  print(dim(train_country_imputed))
  #print(head(train_country_imputed))
  # train_country_imputed = merge(
  #   train_country_imputed,
  #   country_dates,
  #   by = c("date"), 
  #   all.x = T, all.y = F) |> distinct()
  # train_country_imputed = train_country_imputed |> 
  #   mutate(
  #     Type = ifelse(
  #       is.na(Type), "No Holiday", Type)) |>
  #   mutate_if(is.character, as.factor)
  print(dim(train_country_imputed))
  # print(head(train_country_imputed))
  #
  
  
  # print(head(train_country_imputed))
  train_country_imputed = train_country_imputed |> drop_na()
  # #
  print(000)
  # print(sum(is.na(train_country_imputed)))
  #
  # print(str(train_country_imputed))
  m_formula = formula(num_sold ~ store*product*weekday*month*year+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411+si_n111+co_s111+si_n4111+co_s4111+si_n6111+co_s6111+si_n8111+co_s8111+si_n12111+co_s12111+si_n24111+co_s24111+si_n1111+co_s1111+si_n41111+co_s41111+si_n61111+co_s61111+si_n81111+co_s81111+si_n121111+co_s121111+si_n241111+co_s241111+si_n11111+co_s11111+si_n411111+co_s411111+si_n611111+co_s611111+si_n811111+co_s811111+si_n1211111+co_s1211111+si_n2411111+co_s2411111+si_n111111+co_s111111+si_n4111111+co_s4111111+si_n6111111+co_s6111111+si_n8111111+co_s8111111+si_n12111111+co_s12111111+si_n24111111+co_s24111111+si_n1111111+co_s1111111+si_n41111111+co_s41111111+si_n61111111+co_s61111111+si_n81111111+co_s81111111+si_n121111111+co_s121111111+si_n241111111+co_s241111111+si_n11111111+co_s11111111+si_n411111111+co_s411111111+si_n611111111+co_s611111111+si_n811111111+co_s811111111+si_n1211111111+co_s1211111111+si_n2411111111+co_s2411111111+si_n111111111+co_s111111111+si_n4111111111+co_s4111111111+si_n6111111111+co_s6111111111+si_n8111111111+co_s8111111111+si_n12111111111+co_s12111111111+si_n24111111111+co_s24111111111)
                        #si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411 + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411  +si_n111+co_s111+si_n4111+co_s4111+si_n6111+co_s6111+si_n8111+co_s8111+si_n12111+co_s12111+si_n24111+co_s24111 + si_n1111+co_s1111+si_n41111+co_s41111+si_n61111+co_s61111+si_n81111+co_s81111+si_n121111+co_s121111+si_n241111+co_s241111 + si_n11111+co_s11111+si_n411111+co_s411111+si_n611111+co_s611111+si_n811111+co_s811111+si_n1211111+co_s1211111+si_n2411111+co_s2411111 + si_n111111+co_s111111+si_n4111111+co_s4111111+si_n6111111+co_s6111111+si_n8111111+co_s8111111+si_n12111111+co_s12111111+si_n24111111+co_s24111111  )
  x_var = model.matrix(
    m_formula,# log(num_sold) ~ ., #store*product*weekday*month*year+.,
    data = train_country_imputed[,c(-1,-4,-9)]
  )[,-1]
  #
  y_var = log(train_country_imputed$num_sold)
  #
  # print(sum(is.na(x_var)))
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
  print(001)
  # print(sum(is.na(test_country1)))
  # test_country1$num_sold = 100
  n_formula = update(m_formula, NULL ~ store*product*weekday*month*year+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411+si_n111+co_s111+si_n4111+co_s4111+si_n6111+co_s6111+si_n8111+co_s8111+si_n12111+co_s12111+si_n24111+co_s24111+si_n1111+co_s1111+si_n41111+co_s41111+si_n61111+co_s61111+si_n81111+co_s81111+si_n121111+co_s121111+si_n241111+co_s241111+si_n11111+co_s11111+si_n411111+co_s411111+si_n611111+co_s611111+si_n811111+co_s811111+si_n1211111+co_s1211111+si_n2411111+co_s2411111+si_n111111+co_s111111+si_n4111111+co_s4111111+si_n6111111+co_s6111111+si_n8111111+co_s8111111+si_n12111111+co_s12111111+si_n24111111+co_s24111111+si_n1111111+co_s1111111+si_n41111111+co_s41111111+si_n61111111+co_s61111111+si_n81111111+co_s81111111+si_n121111111+co_s121111111+si_n241111111+co_s241111111+si_n11111111+co_s11111111+si_n411111111+co_s411111111+si_n611111111+co_s611111111+si_n811111111+co_s811111111+si_n1211111111+co_s1211111111+si_n2411111111+co_s2411111111+si_n111111111+co_s111111111+si_n4111111111+co_s4111111111+si_n6111111111+co_s6111111111+si_n8111111111+co_s8111111111+si_n12111111111+co_s12111111111+si_n24111111111+co_s24111111111)
                       #si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411 + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411 +si_n111+co_s111+si_n4111+co_s4111+si_n6111+co_s6111+si_n8111+co_s8111+si_n12111+co_s12111+si_n24111+co_s24111 + si_n1111+co_s1111+si_n41111+co_s41111+si_n61111+co_s61111+si_n81111+co_s81111+si_n121111+co_s121111+si_n241111+co_s241111 + si_n11111+co_s11111+si_n411111+co_s411111+si_n611111+co_s611111+si_n811111+co_s811111+si_n1211111+co_s1211111+si_n2411111+co_s2411111 + si_n111111+co_s111111+si_n4111111+co_s4111111+si_n6111111+co_s6111111+si_n8111111+co_s8111111+si_n12111111+co_s12111111+si_n24111111+co_s24111111)
  test_country1 = model.matrix(
    n_formula,
    test_country[,c(-1, -2,-3)]
  )[,-1]
  #
  #
  print(002)
  prediction_c = predict(best_ridge, s = best_lambda, newx = test_country1) |>
    c() |> exp()
  # #
  actual = y_var
  pred = predict(best_ridge, s = best_lambda, newx = x_var)
  rss = sum((pred - actual)^2)
  tss = sum((actual - mean(actual))^2)
  rsq = 1- rss/tss
  print(paste("R Squared:",rsq, sep = " "))
  #
  rm(x_var)
  rm(test_country1)

  # LM
  # model_country = lm(# *weekday*month*year
  #   formula = log(num_sold) ~ store*product*weekday*month*year+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411 + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411, # *weekday*month*year+si_n+co_s,
  #   data = train_country_imputed)
  # print("R Squared: ")
  # print(summary(model_country)$r.squared)
  # #
  # prediction_c = predict(
  #   model_country, newdata = test_country) |>
  #   unname() |> exp()
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
####### gbm ##################################################


##
#
#
# 0.07
series_data = list.files(
  path = "playground-series-s5e1/", recursive = T, pattern = '.csv$', 
  full.names = T)
series_data
#
library(tidyverse)
library(readr)
library(glmnet)
library(gbm)
#
# load("D:/KAGGLE/Kaggle Playground Series/playground-series/Series5/Episode1/usd_rates_data.RData")
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
source('ratios.R')
#
global_holidays <- read_csv(
  "D:\\DATAPROJECTS\\Tidytuesday\\2024\\2024-12-24\\global_holidays.csv"
) |> 
  mutate(
    date = lubridate::ymd(Date),
    country = ADM_name
  ) |>
  select(-ADM_name, -Name, -ISO3, -Date) |>
  mutate(
    Type = "Holiday"
  ) |> distinct()
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
  ) |>
  mutate(
    si_n = sin(2*pi*day_year/7),
    co_s = cos(2*pi*day_year/7),
    si_n4 = sin(2*pi*day_year/30.4),
    co_s4 = cos(2*pi*day_year/30.4),
    si_n6 = sin(2*pi*day_year/365.25/4),
    co_s6 = cos(2*pi*day_year/365.25/4),
    si_n8 = sin(2*pi*day_year/365.25/2),
    co_s8 = cos(2*pi*day_year/365.25/2),
    si_n12 = sin(2*pi*day_year/365.25),
    co_s12 = cos(2*pi*day_year/365.25),
    si_n24 = sin(2*pi*day_year/365.25*2),
    co_s24 = cos(2*pi*day_year/365.25*2),
    si_n1 = sin(4*pi*day_year/7),
    co_s1 = cos(4*pi*day_year/7),
    si_n41 = sin(4*pi*day_year/30.4),
    co_s41 = cos(4*pi*day_year/30.4),
    si_n61 = sin(4*pi*day_year/365.25/4),
    co_s61 = cos(4*pi*day_year/365.25/4),
    si_n81 = sin(4*pi*day_year/365.25/2),
    co_s81 = cos(4*pi*day_year/365.25/2),
    si_n121 = sin(4*pi*day_year/365.25),
    co_s121 = cos(4*pi*day_year/365.25),
    si_n241 = sin(4*pi*day_year/365.25*2),
    co_s241 = cos(4*pi*day_year/365.25*2),
    si_n11 = sin(6*pi*day_year/7),
    co_s11 = cos(6*pi*day_year/7),
    si_n411 = sin(6*pi*day_year/30.4),
    co_s411 = cos(6*pi*day_year/30.4),
    si_n611 = sin(6*pi*day_year/365.25/4),
    co_s611 = cos(6*pi*day_year/365.25/4),
    si_n811 = sin(6*pi*day_year/365.25/2),
    co_s811 = cos(6*pi*day_year/365.25/2),
    si_n1211 = sin(6*pi*day_year/365.25),
    co_s1211 = cos(6*pi*day_year/365.25),
    si_n2411 = sin(6*pi*day_year/365.25*2),
    co_s2411 = cos(6*pi*day_year/365.25*2),
    si_n111 = sin(8*pi*day_year/7),
    co_s111 = cos(8*pi*day_year/7),
    si_n4111 = sin(8*pi*day_year/30.4),
    co_s4111 = cos(8*pi*day_year/30.4),
    si_n6111 = sin(8*pi*day_year/365.25/4),
    co_s6111 = cos(8*pi*day_year/365.25/4),
    si_n8111 = sin(8*pi*day_year/365.25/2),
    co_s8111 = cos(8*pi*day_year/365.25/2),
    si_n12111 = sin(8*pi*day_year/365.25),
    co_s12111 = cos(8*pi*day_year/365.25),
    si_n24111 = sin(8*pi*day_year/365.25*2),
    co_s24111 = cos(8*pi*day_year/365.25*2)
  )
# test$day_year2 = difftime(time1 = test$date, time2 = base_date_test,
#                          units = 'days') |> as.numeric()
head(test)
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
    ) |>
    mutate(
      si_n = sin(2*pi*day_year/7),
      co_s = cos(2*pi*day_year/7),
      si_n4 = sin(2*pi*day_year/30.4),
      co_s4 = cos(2*pi*day_year/30.4),
      si_n6 = sin(2*pi*day_year/365.25/4),
      co_s6 = cos(2*pi*day_year/365.25/4),
      si_n8 = sin(2*pi*day_year/365.25/2),
      co_s8 = cos(2*pi*day_year/365.25/2),
      si_n12 = sin(2*pi*day_year/365.25),
      co_s12 = cos(2*pi*day_year/365.25),
      si_n24 = sin(2*pi*day_year/365.25*2),
      co_s24 = cos(2*pi*day_year/365.25*2),
      si_n1 = sin(4*pi*day_year/7),
      co_s1 = cos(4*pi*day_year/7),
      si_n41 = sin(4*pi*day_year/30.4),
      co_s41 = cos(4*pi*day_year/30.4),
      si_n61 = sin(4*pi*day_year/365.25/4),
      co_s61 = cos(4*pi*day_year/365.25/4),
      si_n81 = sin(4*pi*day_year/365.25/2),
      co_s81 = cos(4*pi*day_year/365.25/2),
      si_n121 = sin(4*pi*day_year/365.25),
      co_s121 = cos(4*pi*day_year/365.25),
      si_n241 = sin(4*pi*day_year/365.25*2),
      co_s241 = cos(4*pi*day_year/365.25*2),
      si_n11 = sin(6*pi*day_year/7),
      co_s11 = cos(6*pi*day_year/7),
      si_n411 = sin(6*pi*day_year/30.4),
      co_s411 = cos(6*pi*day_year/30.4),
      si_n611 = sin(6*pi*day_year/365.25/4),
      co_s611 = cos(6*pi*day_year/365.25/4),
      si_n811 = sin(6*pi*day_year/365.25/2),
      co_s811 = cos(6*pi*day_year/365.25/2),
      si_n1211 = sin(6*pi*day_year/365.25),
      co_s1211 = cos(6*pi*day_year/365.25),
      si_n2411 = sin(6*pi*day_year/365.25*2),
      co_s2411 = cos(6*pi*day_year/365.25*2),
      si_n111 = sin(8*pi*day_year/7),
      co_s111 = cos(8*pi*day_year/7),
      si_n4111 = sin(8*pi*day_year/30.4),
      co_s4111 = cos(8*pi*day_year/30.4),
      si_n6111 = sin(8*pi*day_year/365.25/4),
      co_s6111 = cos(8*pi*day_year/365.25/4),
      si_n8111 = sin(8*pi*day_year/365.25/2),
      co_s8111 = cos(8*pi*day_year/365.25/2),
      si_n12111 = sin(8*pi*day_year/365.25),
      co_s12111 = cos(8*pi*day_year/365.25),
      si_n24111 = sin(8*pi*day_year/365.25*2),
      co_s24111 = cos(8*pi*day_year/365.25*2)
    )
  return(data_2)
}
#
# test2 = left_join(
#   test, 
#   global_holidays |> filter(date >= min(test$date)),
#   by = c("date","country")
# )
test = merge(
  test, 
  global_holidays |> filter(date >= min(test$date)),
  by = c("date","country"), 
  all.x = T, all.y = F) |> distinct()
test$Type = ifelse(
  is.na(test$Type), "No Holiday", 
  test$Type)
# View(test2)
dim(test)
#
train = train |> 
  mutate_if(is.character, as.factor)
test = test |> 
  mutate_if(is.character, as.factor)
#
train |>
  head()
test |>
  head()
#
# str(train)
# str(test)
##train

# mm = imputation_function(train, country_name = "Canada", store_name = "Premium Sticker Mart")
#
predictions_data_frame = data.frame()
for (unique_country in unique(train$country)){
  print(unique_country)
  # usd
  # country_usd = usd_rates_approx_long |> 
  #   mutate(usd = 1/usd) |>
  #   filter(country %in% unique_country) |> 
  #   select(-country)
  # by country train and test
  train_country = train |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
  country_dates = global_holidays |> 
    dplyr::filter(country %in% unique_country) |>
    filter(date <= max(train_country_imputed$date)) |>
    select(-country)
  #
  test_country = test |>
    dplyr::filter(country %in% unique_country) #|>
  # select(-country)
  print(dim(test))
  # test_country = merge(
  #   test_country, country_usd, by = 'date', all.x = T) |>
  #   distinct() |> fill(usd, .direction = "downup")
  # #
  print(dim(test))
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
  # # print(sapply(train_country_imputed, FUN = function(x) (sum(is.na(x)))))
  # train_country_imputed = merge(
  #   train_country_imputed, country_usd, by = 'date', all.x = T
  # ) |> distinct() |> fill(usd, .direction = "downup")
  print(dim(train_country_imputed))
  #print(head(train_country_imputed))
  train_country_imputed = merge(
    train_country_imputed,
    country_dates,
    by = c("date"), 
    all.x = T, all.y = F) |> distinct()
  train_country_imputed = train_country_imputed |> 
    mutate(
      Type = ifelse(
        is.na(Type), "No Holiday", Type)) |>
    mutate_if(is.character, as.factor)
  print(dim(train_country_imputed))
  # print(head(train_country_imputed))
  #
  
  
  # print(head(train_country_imputed))
  train_country_imputed = train_country_imputed |> drop_na()
  # #
  print(000)
  # print(sum(is.na(train_country_imputed)))
  #
  # # print(str(train_country_imputed))
  # m_formula = formula(num_sold ~ store*product*weekday*month*year*Type+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411) # + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411)
  # x_var = model.matrix(
  #   m_formula,# log(num_sold) ~ ., #store*product*weekday*month*year+.,
  #   data = train_country_imputed[,c(-1,-4,-9)]
  # )[,-1]
  # #
  # y_var = log(train_country_imputed$num_sold)
  # #
  # # print(sum(is.na(x_var)))
  # #
  # lambdas = 10^seq(3,-3, by = -.1)
  # ridge_cv = cv.glmnet(
  #   x_var, y_var, alpha = 1, lambda = lambdas, parallel = !T, nfolds = 7
  # )
  # #
  # best_lambda = ridge_cv$lambda.min
  # # best_lambda
  # #
  # best_ridge = glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)
  # #
  # print(001)
  # # print(sum(is.na(test_country1)))
  # # test_country1$num_sold = 100
  # n_formula = update(m_formula, NULL ~ store*product*weekday*month*year+usd+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411) # + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411)
  # test_country1 = model.matrix(
  #   n_formula,
  #   test_country[,c(-1, -2,-3)]
  # )[,-1]
  # print(002)
  # prediction_c = predict(best_ridge, s = best_lambda, newx = test_country1) |>
  #   c() |> exp()
  # # #
  # actual = y_var
  # pred = predict(best_ridge, s = best_lambda, newx = x_var)
  # rss = sum((pred - actual)^2)
  # tss = sum((actual - mean(actual))^2)
  # rsq = 1- rss/tss
  # print(paste("R Squared:",rsq, sep = " "))
  # #
  # rm(x_var)
  # rm(test_country1)
  #
  # LM
  model_country = gbm(# *weekday*month*year
    formula = log(num_sold) ~ store*product*weekday*month*year*Type+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411 + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411, # *weekday*month*year+si_n+co_s,
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4, shrinkage = 0.2, verbose = F, 
    data = train_country_imputed)
  # print("R Squared: ")
  # print(summary(model_country)$r.squared)
  #
  print(002)
  prediction_c = predict(
    model_country, newdata = test_country, n.trees = 5000) |>
    unname() |> exp()
  #
  actual = train_country_imputed$num_sold
  pred = predict(
    model_country, newdata = train_country_imputed, 
    n.trees = 5000) |>
    unname() |> exp()
  rss = sum((pred - actual)^2)
  tss = sum((actual - mean(actual))^2)
  rsq = 1- rss/tss
  print(paste("R Squared:",rsq, sep = " "))
  #
  rm(model_country)
  # LM
  # model_country = lm(# *weekday*month*year
  #   formula = log(num_sold) ~ store*product*weekday*month*year+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24+si_n1+co_s1+si_n41+co_s41+si_n61+co_s61+si_n81+co_s81+si_n121+co_s121+si_n241+co_s241+si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411 + si_n11+co_s11+si_n411+co_s411+si_n611+co_s611+si_n811+co_s811+si_n1211+co_s1211+si_n2411+co_s2411, # *weekday*month*year+si_n+co_s,
  #   data = train_country_imputed)
  # print("R Squared: ")
  # print(summary(model_country)$r.squared)
  # #
  # prediction_c = predict(
  #   model_country, newdata = test_country) |>
  #   unname() |> exp()
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
#
# # data import -------------------------------------------------------------
# # 
# ls()
# for (csv_path in 1:length(series_data)){
#   print(csv_path) 
#   file_path = series_data[csv_path]
#   print(file_path)
#   file_name = basename(file_path) |> 
#     # substitute the pattern, only 1 instance
#     sub(x = _, pattern = ".csv", "")
#   print(file_name)
#   assign(
#     file_name,
#     read.csv(file_path)
#   )
# }
# ls()
# #
# base_date_train = min(train$date)
# base_date_test = min(test$date)
# #
# test = test |> 
#   mutate(
#     date = lubridate::ymd(date)
#   ) |>
#   mutate(
#     weekday = lubridate::wday(date, label = T, abbr = T), 
#     month = lubridate::month(date, label = T, abbr = T), 
#     year = lubridate::year(date),
#     day_year = difftime(time1 = date, time2 = base_date_train,
#                         units = 'days') |> as.numeric()
#   ) |>
#   mutate(
#     si_n = sin(2*pi*day_year/7),
#     co_s = cos(2*pi*day_year/7),
#     si_n4 = sin(2*pi*day_year/30.4),
#     co_s4 = cos(2*pi*day_year/30.4),
#     si_n6 = sin(2*pi*day_year/365.25/4),
#     co_s6 = cos(2*pi*day_year/365.25/4),
#     si_n8 = sin(2*pi*day_year/365.25/2),
#     co_s8 = cos(2*pi*day_year/365.25/2),
#     si_n12 = sin(2*pi*day_year/365.25),
#     co_s12 = cos(2*pi*day_year/365.25),
#     si_n24 = sin(2*pi*day_year/365.25*2),
#     co_s24 = cos(2*pi*day_year/365.25*2)
#   )
# # test$day_year2 = difftime(time1 = test$date, time2 = base_date_test,
# #                          units = 'days') |> as.numeric()
# head(test)
# #
# imputation_function <- function(data, country_name, store_name){
#   print(head(data))
#   data_1 = data |>
#     filter(country %in% country_name, store %in% store_name) |>
#     select(-id, -country, - store)
#   # print(head(data_1))
#   data_1 = data_1 |> 
#     pivot_wider(
#       id_cols = date, #{{"date"}},
#       names_from = product, #{{"product"}},
#       values_from = num_sold)#{{"num_sold"}})
#   # print(head(data_1))
#   #
#   nrow_data_1 = nrow(data_1)
#   for (i in 2:ncol(data_1)){
#     if ( sum(is.na(data_1[,i])) == nrow_data_1  ){
#       rates_data = ifelse(
#         country_name == "Canada", canada_ratio$average_rates, 
#         kenya_ratio$average_rates)
#       data_1[,i] = rowSums(data_1[, c(-1,-i)])*rates_data
#       # View(data_1)
#     } else {
#       data_1[,i] = zoo::na.approx(data_1[,i]) 
#     }
#   }
#   
#   data_1 = data_1 |> 
#     pivot_longer(
#       cols = !date,#`Holographic Goose`:`Kerneler Dark Mode`, 
#       names_to = 'product', values_to = "num_sold"
#     )
#   print(head(data_1))
#   data_1$country = country_name
#   data_1$store = store_name
#   data_1 |> 
#     mutate(
#       date = lubridate::ymd(date)
#     ) |>
#     mutate(
#       weekday = lubridate::wday(date, label = T, abbr = T), 
#       month = lubridate::month(date, label = T, abbr = T), 
#       year = lubridate::year(date),
#       day_year = difftime(time1 = date, time2 = base_date_train,
#                           units = 'days') |> as.numeric()
#     ) |>
#     mutate(
#       si_n = sin(2*pi*day_year/7),
#       co_s = cos(2*pi*day_year/7),
#       si_n4 = sin(2*pi*day_year/30.4),
#       co_s4 = cos(2*pi*day_year/30.4),
#       si_n6 = sin(2*pi*day_year/365.25/4),
#       co_s6 = cos(2*pi*day_year/365.25/4),
#       si_n8 = sin(2*pi*day_year/365.25/2),
#       co_s8 = cos(2*pi*day_year/365.25/2),
#       si_n12 = sin(2*pi*day_year/365.25),
#       co_s12 = cos(2*pi*day_year/365.25),
#       si_n24 = sin(2*pi*day_year/365.25*2),
#       co_s24 = cos(2*pi*day_year/365.25*2)
#     )
# }
# #
# train |>
#   head()
# test |>
#   head()
# # mm = imputation_function(train, country_name = "Canada", store_name = "Premium Sticker Mart")
# #
# predictions_data_frame = data.frame()
# for (unique_country in unique(train$country)){
#   print(unique_country)
#   # by country train and test
#   train_country = train |>
#     dplyr::filter(country %in% unique_country) #|>
#   # select(-country)
#   test_country = test |>
#     dplyr::filter(country %in% unique_country) #|>
#   # select(-country)
#   #
#   train_country_imputed = data.frame()
#   for (unique_store in unique(train_country$store)){
#     print(unique_store)
#     train_country_imputation = imputation_function(
#       data = train_country, country_name = unique_country, 
#       store_name = unique_store)
#     train_country_imputed = bind_rows(
#       train_country_imputed,
#       train_country_imputation
#     )
#     
#   }
#   #
#   model_country = lm(# *weekday*month*year
#     formula = log(num_sold) ~ store*product*weekday*month*year+si_n+co_s+si_n4+co_s4+si_n6+co_s6+si_n8+co_s8+si_n12+co_s12+si_n24+co_s24, # *weekday*month*year+si_n+co_s,
#     data = train_country_imputed)
#   print("R Squared: ")
#   print(summary(model_country)$r.squared)
#   #
#   prediction_c = predict(
#     model_country, newdata = test_country) |>
#     unname() |> exp()
#   #
#   predictions_data_frame = bind_rows(
#     predictions_data_frame,
#     data.frame(
#       id = test_country$id,
#       num_sold = prediction_c)
#   )
#   print(head(predictions_data_frame))
# }
# #
# View(predictions_data_frame)
# #
# write.csv(
#   predictions_data_frame, 
#   paste0(
#     'submissions/sample_submission_multiple_lm_hierachy_country', 
#     strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)

#

#
# train_country_imputed





# 
# # Create the model matrix for training data
# X_train <- model.matrix(
#   log(num_sold) ~ store*product*weekday*month*year,
#   data = train_country_imputed)
# 
# # Fit a model (e.g., ridge regression)
# library(glmnet)
# y_train <- train_country_imputed$y
# ridge_model <- glmnet(X_train, y_train, alpha = 0)
# 
# # *CRITICAL STEP: Use the SAME formula and the ORIGINAL training data*
# X_new <- model.matrix(
#   log(num_sold) ~ store*product*weekday*month*year,
#   data = train_country_imputed, test_country)
# 
# # Make predictions
# X_new <- X_new[,-1] #remove intercept column
# predictions <- predict(ridge_model, s = 0.01, newx = X_new) #replace 0.01 with whatever lambda you want
# 
# #print predictions
# print(predictions)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #
# #
# 
# #
# # library(gbm)
# set.seed(1)
# boost_boston <- gbm(
#   formula = log(num_sold) ~ store*product*weekday*month*year+Type,
#   data = train_country, distribution = "gaussian", 
#   n.trees = 5000, interaction.depth = 4)
# #