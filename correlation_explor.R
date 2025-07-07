library(corrplot)

# Correlation plot for data from May 26, 2021
may_26 <- covid_2021 |>
  filter(Date == '2021-05-26')

cor_may26 <- may_26 |>
  select(c(4:12)) |>
  cor(use="complete.obs")

corrplot(cor_may26)


# Correlation plot for data from December 29, 2021
dec_29 <- covid_2021 |>
  filter(Date == '2021-12-29')

cor_dec29 <- dec_29 |>
  select(c(4:12)) |>
  cor(use="complete.obs")

corrplot(cor_dec29)


#hhc and pop_vacs_pct have some negative correlation
# rems and new.cases &&& rems and pop_vacs have some positive correlationo