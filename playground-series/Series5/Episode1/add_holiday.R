#
#
library(readr)
global_holidays <- read_csv(
  "D:\\DATAPROJECTS\\Tidytuesday\\2024\\2024-12-24\\global_holidays.csv"
) |> 
  mutate(
    date = lubridate::ymd(Date),
    country = ADM_name
  ) |>
  select(-ADM_name, -Name, -ISO3, -Date)
#
head(global_holidays)
#
summary(global_holidays)
#
unique_countries = unique(train$country)
global_holidays |>
  filter(country %in% unique_countries) -> mm
head(mm)
# View()
unique(mm$Type)
#
head(train)
#
mm = mm |>
  filter(date <= max(train$date))
#
merge(
  train, mm, by = c("date","country"), 
  all.x = T, all.y = T
) -> m
head(m)
#
dim(train)
dim(m)
#
m = m[!duplicated(m$id),]
#
#
mm2 = left_join(
  train, mm, by = c("date","country"), unmatched = 'drop') |> 
  distinct()
dim(mm2)
#