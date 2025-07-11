#Cases per 100,000 people for May 26, 2021
covid_cases_2021_may26 <- covid_cases_2021 %>%
  filter(Date == as.Date("2021-05-26"))
covid_cases_2021_may26 <- full_join(covid_cases_2021_may26, counties_pop_2021, by = c("County"))
norm_2021_may26 <- covid_cases_2021_may26 %>%
  mutate(rate_per_100k = (cumulative_cases / Cumm_pop_2021) * 100000)
#from here make a percentile and keep the top 50% 
norm_percent <- norm_2021_may26 %>%
  mutate(rate_per_100k >= quantile(rate_per_100k, 0.5, na.rm = TRUE), 
         percent_population = (cumulative_cases / Cumm_pop_2021) * 100
  )
norm_percent <- norm_percent %>%
  arrange(desc(percent_population))
top_50_percent <- norm_percent %>% 
  slice(1:29)
#then do the same for rems
rems_specific <- ca_svi_map %>%
  select(c(NAMELSAD, rems)) %>%
  rename(County = NAMELSAD)
top_half_rems <- rems_specific %>% 
  arrange(desc(rems)) %>% 
  slice(1:29)
#combined norm and rems overall
comb_rems_case <- rems_specific %>%
  full_join(norm_percent, by = "County")
comb_rems_case_clean <- comb_rems_case %>%
  select(c(County, rems, Date, percent_population, geometry))
comb_rems_case_clean <- comb_rems_case_clean %>%
  mutate(
    top_case_and_rems = percent_population >= quantile(percent_population, 0.5, na.rm = TRUE) &
      rems >= quantile(rems, 0.5, na.rm = TRUE)
  )
#combine the ones that are both in the top 50 and plot it on a map
#This graph didn't work
combined_halfs_rems <- top_50_percent %>% 
  full_join(top_half_rems, by = "County")
comb_half_rems_filt <- combined_halfs_rems %>%
  slice(1, 3:16, 18:21, 24:25)
#Graph attempt 2
# Make sure the logical column is a factor for custom coloring
comb_rems_case_clean <- comb_rems_case_clean %>%
  mutate(top_case_and_rems = factor(top_case_and_rems, levels = c(TRUE, FALSE)))
# Plot the map
ggplot(comb_rems_case_clean) +
  geom_sf(aes(fill = top_case_and_rems), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "blue"),
    name = "Top 50% Cases & REMS"
  ) +
  theme_minimal()  +
  labs(
    title = "Combined Top Percent Tile of Cases and REMS on May 26, 2021",
    subtitle = "Based on CDC",
    caption = "Data source: CDC SVI 2021, CDC COVID Tracker"
  )
