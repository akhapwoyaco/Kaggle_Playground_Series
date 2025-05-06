#

library(tidyverse)
library(lubridate)
# options(digits=3)
library(httr)
library(jsonlite)
library(janitor)
#
base_url = "https://www.centralbank.go.ke/wp-admin/admin-ajax.php?action=get_wdtable&table_id="
#
json_data = list(
  "draw" = "2",
  "columns[0][data]" = "0",
  "columns[0][name]" = "year",
  "columns[0][searchable]" = "true",
  "columns[0][orderable]" = "true",
  "columns[0][search][value]" = "01/01/2010~31/12/2019",
  "columns[0][search][regex]" = "false",
  "columns[1][data]" = "1",
  "columns[1][name]" = "currency",
  "columns[1][searchable]" = "true",
  "columns[1][orderable]" = "true",
  "columns[1][search][value]" = "US DOLLAR",
  "columns[1][search][regex]" = "false",
  "columns[2][data]" = "2",
  "columns[2][name]" = "deposit",
  "columns[2][searchable]" = "true",
  "columns[2][orderable]" = "true",
  "columns[2][search][value]" = "~",
  "columns[2][search][regex]" = "false",
  "columns[3][data]" = "3",
  "columns[3][name]" = "savings",
  "columns[3][searchable]" = "true",
  "columns[3][orderable]" = "true",
  "columns[3][search][value]" = "~",
  "columns[3][search][regex]" = "false",
  "columns[4][data]" = "4",
  "columns[4][name]" = "lending",
  "columns[4][searchable]" = "true",
  "columns[4][orderable]" = "true",
  "columns[4][search][value]" = "~",
  "columns[4][search][regex]" = "false",
  "columns[5][data]" = "5",
  "columns[5][name]" = "overdraft",
  "columns[5][searchable]" = "true",
  "columns[5][orderable]" = "true",
  "columns[5][search][value]" = "~",
  "columns[5][search][regex]" = "false",
  "order[0][column]" = "0", 
  "order[0][dir]" = "desc", 
  "start" = "0", 
  "length" = "-1", 
  "search[value]" = "", 
  "search[regex]" = "false",
  "sRangeSeparator" = "~"
)
#
get_data_post <- function(ID){
  resp = POST(
    paste(base_url,ID, sep=""),
    body = json_data, encode = "form"
  )
  data_final = jsonlite::fromJSON(
    content(resp, 'text', encoding = 'UTF-8'))
  data_final_data <- data.frame(data_final$data)
  data_final_data
}
#
kenyan_usd = get_data_post(ID = 32)
kenyan_usd2 = kenyan_usd |> select(X1, X4) |> 
  setNames(c("date", "Kenya")) |> 
  mutate(
    date = lubridate::dmy(date), 
    Kenya = as.numeric(Kenya))
#