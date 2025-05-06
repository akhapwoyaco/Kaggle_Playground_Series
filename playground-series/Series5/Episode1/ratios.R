# # canada
# train |> 
#   group_by(country, store, product) |> 
#   summarise(n = n(), missing = sum(is.na(num_sold))) |> 
#   arrange(desc(missing)) |> View()
# #
# train |>
#   group_by(country, store, product) |>
#   summarise(
#     n = n(),
#     average_num = mean(num_sold, na.rm = T)
#   ) |> 
#   # filter(country == "Canada") |>
#   mutate(
#     ration = n/average_num) |> View()
#
train |> 
  group_by(country, store) |> 
  summarise(n = sum(num_sold, na.rm =T)) |> 
  pivot_wider(
    names_from = store, values_from = n) |> 
  mutate(
    r1 = `Discount Stickers` /(`Discount Stickers` + `Premium Sticker Mart` + `Stickers for Less`),
    r2 = `Premium Sticker Mart`/(`Discount Stickers` + `Premium Sticker Mart` + `Stickers for Less`),
    r3 = `Stickers for Less`/(`Discount Stickers` + `Premium Sticker Mart` + `Stickers for Less`))
#
##################################################################################
train |>
  select(-id) |> #, -weekday, -month, -year) |>
  filter(country == "Canada", store == "Stickers for Less" ) |>
  pivot_wider(
    id_cols = date,
    names_from = product,
    values_from = num_sold
  ) |>  
  rowwise() |>
  mutate(
    sum_sold = sum(c_across(`Holographic Goose`:`Kerneler Dark Mode`), 
                   na.rm = T)
  ) |> 
  mutate(
    r_1 = `Holographic Goose`/sum_sold
  ) -> sfl
sfl$`Holographic Goose` = zoo::na.approx(sfl$`Holographic Goose`)
#
stickers_for_less = sfl |> 
    mutate(r_2 = `Holographic Goose`/sum_sold) 
#
train |>
  select(-id) |> #, -weekday, -month, -year) |>
  filter(country == "Canada", store == "Premium Sticker Mart" ) |>
  pivot_wider(
    id_cols = date,
    names_from = product,
    values_from = num_sold
  ) |>  
  rowwise() |>
  mutate(
    sum_sold = sum(c_across(`Holographic Goose`:`Kerneler Dark Mode`), 
                   na.rm = T)
  ) |> 
  mutate(
    r_1 = `Holographic Goose`/sum_sold
  ) -> psm
psm$`Holographic Goose` = zoo::na.approx(psm$`Holographic Goose`)
#
premium_sticker_mart = psm |> 
    mutate(
      r_2 = `Holographic Goose`/sum_sold
    )
#
canada_ratio = data.frame(
  sfl = stickers_for_less$r_2, 
  psm = premium_sticker_mart$r_2
) |>
  mutate(
    average_rates = (sfl+psm)/2
  )
# View(canada_ratio)
#
# canada_ratio$average_rates
#
# kenyan
train |>
  select(-id) |> 
  filter(country == "Kenya", store == "Stickers for Less" ) |>
  pivot_wider(
    id_cols = date,
    names_from = product,
    values_from = num_sold
  ) |>
  rowwise() |>
  mutate(
    sum_sold = sum(c_across(`Holographic Goose`:`Kerneler Dark Mode`), 
                   na.rm = T)
  ) |> 
  mutate(
    r_1 = `Holographic Goose`/sum_sold
  ) -> sfl
sfl$`Holographic Goose` = zoo::na.approx(sfl$`Holographic Goose`)
#
stickers_for_less = sfl |> 
  mutate(r_2 = `Holographic Goose`/sum_sold) 
#
train |>
  select(-id) |> #, -weekday, -month, -year) |>
  filter(country == "Kenya", store == "Premium Sticker Mart" ) |>
  pivot_wider(
    id_cols = date,
    names_from = product,
    values_from = num_sold
  ) |>  
  rowwise() |>
  mutate(
    sum_sold = sum(c_across(`Holographic Goose`:`Kerneler Dark Mode`), 
                   na.rm = T)
  ) |> 
  mutate(
    r_1 = `Holographic Goose`/sum_sold
  ) -> psm
psm$`Holographic Goose` = zoo::na.approx(psm$`Holographic Goose`)
#
premium_sticker_mart = psm |> 
  mutate(
    r_2 = `Holographic Goose`/sum_sold
  )
#
kenya_ratio = data.frame(
  sfl = stickers_for_less$r_2, 
  psm = premium_sticker_mart$r_2
) |>
  mutate(
    average_rates = (sfl+psm)/2
  )
# View(kenya_ratio)
#
# kenya_ratio$average_rates
#