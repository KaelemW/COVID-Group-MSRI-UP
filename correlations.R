norm_2021 <- full_join(covid_cases_2021, counties_pop_2021, by = c("County"))
norm_2021 <- norm_2021 %>%
  mutate(rate_per_100k = (cumulative_cases / Cumm_pop_2021) * 100000)
 
covid_vacs_2021_yr <- covid_vacs_clean_2021 %>%
  filter(Date == as.Date("2021-12-31"))
sum_vacs_100k <- full_join(covid_vacs_2021_yr, counties_pop_2021, by = c("County"))
sum_vacs_100k <- sum_vacs_100k %>%
  mutate(vacs_rate_100k = (Pop_Vacs / Cumm_pop_2021) * 100000)
sum_vacs_100k <- full_join(sum_vacs_100k, ca_svi_better, by = c("County"))
sum_vacs_100k <- sum_vacs_100k %>%
  select(c(County, svi, ses, hhc, htt, rems, vacs_rate_100k)) %>%
  filter(vacs_rate_100k != 0) 
cor_vacs_100k <- sum_vacs_100k %>%
  select(-County)
cor_matrix <- cor(cor_vacs_100k, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

cor.test(sum_vacs_100k$vacs_rate_100k, sum_vacs_100k$rems, use = "complete.obs")
