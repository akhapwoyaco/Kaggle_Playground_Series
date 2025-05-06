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
r_squared_vals = read.csv('Rsq_h_2_2_202_202501220526.csv')
source('ratios.R')
#
# global_holidays <- read_csv(
#   "D:\\DATAPROJECTS\\Tidytuesday\\2024\\2024-12-24\\global_holidays.csv"
# ) |>
#   mutate(
#     date = lubridate::ymd(Date),
#     country = ADM_name
#   ) |> 
#   # mutate_if(is.character, as.factor) |>
#   select(-ADM_name, -Name, -ISO3, -Date) |>
#   # mutate(
#   #   Type = "Holiday"
#   distinct()
#
# global_holidays$Type <- factor(
#   global_holidays$Type, 
#   levels = c(
#     "Public holiday", "Observance", "Special holiday", "Local holiday", 
#     "Local observance", "Half-day holiday", "Working day (replacement)", NA
#   )
# )
#
base_date_train = min(train$date)
base_date_test = min(test$date)
#
test = test |> #head(20) |>
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
for (k in seq(2, 6, 2)) {
  for (period in c(7, 30.4, 52, 365.25/4, 365.25/2, 365.25, 365.25*2, 365.25*3, 365.25*4)) {
    sin_colname <- paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "_", period))
    cos_colname <- paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "_", period))
    
    # Mutate the columns directly in the data frame
    test = test |>
      mutate(
        !!sym(sin_colname) := sin(k * pi * day_year / period),
        !!sym(cos_colname) := cos(k * pi * day_year / period)
      )
  }
}
dim(test)
#
paste(colnames(test), collapse = "+")
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
  for (k in seq(2, 6, 2)) {
    for (period in c(7, 30.4, 52, 365.25/4, 365.25/2, 365.25, 365.25*2, 365.25*3, 365.25*4)) {
      sin_colname <- paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "_", period))
      cos_colname <- paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "_", period))
      
      # Mutate the columns directly in the data frame
      data_2 = data_2 |>
        mutate(
          !!sym(sin_colname) := sin(k * pi * day_year / period),
          !!sym(cos_colname) := cos(k * pi * day_year / period)
        )
    }
  }
  
  return(data_2)
}
#
# test = merge(
#   test,
#   global_holidays |> filter(date >= min(test$date)),
#   by = c("date","country"),
#   all.x = T, all.y = F) |> distinct()
# test$Type = ifelse(
#   is.na(test$Type), "No Holiday",
#   test$Type)
# # View(test2)
# dim(test)
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
# str(train)
# str(test)
##train

# mm = imputation_function(train, country_name = "Canada", store_name = "Premium Sticker Mart")
rsq_val = data.frame()
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
  m_formula = formula(
    num_sold ~ store*product*weekday*month*year+sin_2_7+cos_2_7+sin_2_30_4+cos_2_30_4+sin_2_52+cos_2_52+sin_2_91_3125+cos_2_91_3125+sin_2_182_625+cos_2_182_625+sin_2_365_25+cos_2_365_25+sin_2_730_5+cos_2_730_5+sin_2_1095_75+cos_2_1095_75+sin_2_1461+cos_2_1461+sin_4_7+cos_4_7+sin_4_30_4+
      cos_4_30_4 + sin_4_52+cos_4_52+sin_4_91_3125+cos_4_91_3125+sin_4_182_625+cos_4_182_625+sin_4_365_25+cos_4_365_25+sin_4_730_5+cos_4_730_5+sin_4_1095_75+cos_4_1095_75+sin_4_1461+cos_4_1461+sin_6_7+cos_6_7+sin_6_30_4+cos_6_30_4+sin_6_52+cos_6_52+sin_6_91_3125+cos_6_91_3125+sin_6_182_625+
      cos_6_182_625+sin_6_365_25+cos_6_365_25+sin_6_730_5+cos_6_730_5+sin_6_1095_75+cos_6_1095_75+sin_6_1461+cos_6_1461)
  #
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
  n_formula = update(
    m_formula, NULL ~ store*product*weekday*month*year+sin_2_7+cos_2_7+sin_2_30_4+cos_2_30_4+sin_2_52+cos_2_52+sin_2_91_3125+cos_2_91_3125+sin_2_182_625+cos_2_182_625+sin_2_365_25+cos_2_365_25+sin_2_730_5+cos_2_730_5+sin_2_1095_75+cos_2_1095_75+sin_2_1461+cos_2_1461+sin_4_7+cos_4_7+
      sin_4_30_4+cos_4_30_4+sin_4_52+cos_4_52+sin_4_91_3125+cos_4_91_3125+sin_4_182_625+cos_4_182_625+sin_4_365_25+cos_4_365_25+sin_4_730_5+cos_4_730_5+sin_4_1095_75+cos_4_1095_75+sin_4_1461+cos_4_1461+sin_6_7+cos_6_7+sin_6_30_4+cos_6_30_4+sin_6_52+cos_6_52+sin_6_91_3125+cos_6_91_3125+
      sin_6_182_625+cos_6_182_625+sin_6_365_25+cos_6_365_25+sin_6_730_5+cos_6_730_5+sin_6_1095_75+cos_6_1095_75+sin_6_1461+cos_6_1461)
  #
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
  rsq_val = bind_rows(
    rsq_val,
    data.frame(
      country_name = unique_country,
      rsq = rsq,
      fourier = "2,6_2_WEEK_WEEK"
    )
  )
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
# View(predictions_data_frame)
#
write.csv(
  predictions_data_frame, 
  paste0(
    'submissions/sample_submission_multiple_RIDGE_hierachy_country_62_WEEK', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#
# ##########################################################################
#
#
# r_squared_vals = data.frame()
r_squared_vals = bind_rows(
  r_squared_vals, rsq_val
)
r_squared_vals
#
write.csv(
  r_squared_vals, 
  paste0(
    'Rsq_h_2_2_202_', 
    strftime(now(),"%Y%m%d%H%M"), '.csv', sep = ''), row.names = F)
#
#
rsquared_plot = r_squared_vals |>
  ggplot(aes(x = country_name, y = rsq, colour = fourier, group = fourier)) +
  geom_point() +
  geom_line() +
  labs(title = "RSquares") +
  scale_color_brewer(palette = 'Dark2') +
  theme_bw() + 
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.2,0.4),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
#
rsquared_plot
#
ggsave(
  filename = "rsquared_plot.png", 
  plot = rsquared_plot,  dpi = 400 
)
#
