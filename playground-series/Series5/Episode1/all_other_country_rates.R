#
library(tidyverse)
library(rvest)
library(polite)
library(janitor)
#
# url_canada = "//*[@id="data-table-popout="]/table"
# CANADA
page <- "https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm" |> 
  bow() |> 
  scrape()
#
page_nodeset = page |> 
  html_elements(xpath = '//*[@id="data-table-popout="]/table') |> 
  html_table()
page_nodeset[[1]] |> 
  setNames(c("date", "Canada")) |> 
  mutate(date = lubridate::dmy(date), Canada = as.numeric(Canada)) -> canada_usd
# FINLAND/ITALY
# EURO
#
page = "https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm" |> 
  bow() |> 
  scrape()
#
page_nodeset = page |> 
  html_elements(xpath = '//*[@id="data-table-popout="]/table') |> 
  html_table()
page_nodeset[[1]] |> 
  setNames(c("date", "Finland")) |> 
  mutate(date = lubridate::dmy(date), Finland = as.numeric(Finland)) -> finland_usd
page_nodeset[[1]] |> 
  setNames(c("date", "Italy")) |> 
  mutate(date = lubridate::dmy(date), Italy = as.numeric(Italy)) -> italy_usd
#
# SINGAPORE
page <- "https://www.federalreserve.gov/releases/h10/hist/dat00_si.htm" |> 
  bow() |> 
  scrape()
#
page_nodeset = page |> 
  html_elements(xpath = '//*[@id="data-table-popout="]/table') |> 
  html_table()
page_nodeset[[1]] |> 
  setNames(c("date", "Singapore")) |> 
  mutate(date = lubridate::dmy(date), Singapore = as.numeric(Singapore)) -> singapore_usd
#
#
# NORWAY 
page <- "https://www.federalreserve.gov/releases/h10/hist/dat00_no.htm" |> 
  bow() |> 
  scrape()
#
page_nodeset = page |> 
  html_elements(xpath = '//*[@id="data-table-popout="]/table') |> 
  html_table()
page_nodeset[[1]] |> 
  setNames(c("date", "Norway")) |> 
  mutate(date = lubridate::dmy(date), Norway = as.numeric(Norway)) -> norway_usd
#
#
usd_rates = merge(
  merge(
    merge(
      merge(
        merge(canada_usd, finland_usd), italy_usd
      ), singapore_usd), norway_usd), kenyan_usd2)
#
head(usd_rates)
usd_rates_approx = usd_rates
#
for (i in 2:ncol(usd_rates)){
  usd_rates_approx[,i] = zoo::na.approx(usd_rates_approx[,i]) 
}
#
str(usd_rates_approx)
#
usd_rates_approx_long = usd_rates_approx |>
  pivot_longer(
    cols = !date,
    values_to = 'usd',
    names_to = 'country'
  )
#
save(usd_rates, usd_rates_approx, usd_rates_approx_long, 
     file = 'playground-series/Series5/Episode1/usd_rates_data.RData')
#
# library(quantmod)
# from <- c("CAD", "JPY", "USD")
# to <- c("USD", "USD", "EUR")
# getQuote(paste0(from, to, "=X"), )
# #
# library(priceR)
# library(tidyverse)
# options(scipen = 100); options(digits = 6)
#
#
# # Retrieve CANADIAN to USD exchange rates
# ken_usd <- historical_exchange_rates(
#   from = "KES", to = "USD",
#   start_date = "2010-01-01", end_date = "2019-12-31")