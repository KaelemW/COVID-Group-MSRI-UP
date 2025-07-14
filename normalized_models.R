# Models for Normalized Amounts

cases_pc <- covid_race_hhc |>
  select(c(County,Date, new_cases_pc))

covid_svi <- left_join(covid_svi, cases_pc, by = c('County', 'Date'))

covid_svi |>
  filter(Date == '2021-05-26') |>
  select(c(new_cases_pc, Pop_Vacs_Pct, svi, rems, hhc, htt, ses)) |>
  rename(cases_per_100k = new_cases_pc, pct_vac = Pop_Vacs_Pct) |>
  cor(use = 'complete.obs') |>
  corrplot()




# sum of cases/100k for all of 2021 correlation plot
cases_p_100k_svi <- covid_svi |>
  group_by(County) |>
  summarize(sum_cases_p100k = sum(new_cases_pc, na.rm = TRUE)) |>
  left_join(svi_2020, by = 'County') |>
  arrange(desc(sum_cases_p100k))

# Nice Table
library(gt)
cases_p_100k_svi |>
  select(c(County, sum_cases_p100k,rems, htt)) |>
  rename(`Sum of Cases/100k` = sum_cases_p100k) |>
  head(n = 5) |>
  gt() |>
    tab_header(
      title = md('**Top 5 Counties in Cases/100k in 2021**')
    ) |>
    tab_source_note(
      source_note = "rems: Racial and Ethnic Minority Status percentile"
    ) |>
    tab_source_note(
      source_note = "htt: Housing Type and Transportation vulnerability percentile"
    )
  

# Correlation Plot
covid_svi |>
  group_by(County) |>
  summarize(sum_cases_p100k = sum(new_cases_pc, na.rm = TRUE)) |>
  left_join(svi_2020, by = 'County') |>
  select(-1) |>
  cor(use = 'complete.obs') |>
  corrplot()

# Model for Cases
require(leaps)
bestss <- regsubsets(sum_cases_p100k~svi+ses+hhc+rems+htt, data = cases_p_100k_svi)
summary(bestss)

mod1 <- lm(sum_cases_p100k~rems, data = cases_p_100k_svi)
summary(mod1)
BIC(mod1) #1170.069

#Best Model for Predicting Sum Covid Cases /100k in 2021
mod2 <- lm(sum_cases_p100k~rems+htt, data = cases_p_100k_svi)
summary(mod2) # Multiple R-squared: 0.4489
BIC(mod2) # 1169.058
vif(mod2) # 1.247826

mod3 <- lm(sum_cases_p100k~rems+htt+ses, data = cases_p_100k_svi)
summary(mod3)
BIC(mod3) # 1170.4

# Plotting this model

# cor(rems, cases_p100k) = -0.24069721
# cor(htt, cases_p100k) = -0.14725080

cases_p_100k_svi <- cases_p_100k_svi |>
  mutate(htt_bin = (htt >= 0.5), rems_bin = (rems >= 0.5))

ggplot(cases_p_100k_svi, aes(x = rems, y = sum_cases_p100k, color = htt_bin)) +
  geom_point() +
  theme_bw() +
  ggtitle('Sum of Cases/100k in 2021 by Racial Minority Proportion') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('Sum of Cases/100k')+
  labs(color = "Housing Type and Transportation Vulnerability") + 
  scale_color_manual(limits = c(TRUE, FALSE),
                     values = c("#FFCC00","#336600"), 
                     labels = c('Above 50th Percentile', "Below 50th Percentile")) +
  theme_gray()







# Vaccinations / 100k

covid_2021 |>
  select(c(County, Date, Pop_Vacs)) |>
  group_by(County) |>
  summarize(sum_vacs = sum(Pop_Vacs)) |>
  head()

# As of 12/30/2021
vax_2021 <- covid_2021 |>
  filter(Date == '2021-12-30') |>
  select(c(County, Pop_Vacs, svi, ses, hhc, htt, rems))

pop <- race |>
  select(c(County, pop))

vax_2021 <- left_join(vax_2021, pop, by = 'County')

vax_2021 <- vax_2021 |>
  mutate(vacs_100k = Pop_Vacs/pop *100000)

vax_2021 |>
  filter(vacs_100k > 0) |>
  select(c(vacs_100k, svi, ses, rems, hhc, htt)) |>
  cor(use="complete.obs") |>
  corrplot()

# Excluded Counties
vax_2021 |>
  filter(Pop_Vacs == 0)
# They are also the 8 least populated counties
vax_2021 |>
  arrange(pop)

vax_2021_clean <- vax_2021 |>
  filter(vacs_100k > 0)


# Model for cases

require(leaps)
bestss <- regsubsets(vacs_100k~ses+hhc+rems+htt, data = vax_2021_clean)
summary(bestss)

mod1 <- lm(vacs_100k~hhc, vax_2021_clean)
summary(mod1)
BIC(mod1) # 1109

# Best Vax Model 
mod2 <- lm(vacs_100k~hhc+rems, vax_2021_clean)
summary(mod2)
BIC(mod2) # 1106

mod3 <- lm(vacs_100k~hhc+rems+ses, vax_2021_clean)
summary(mod3)
BIC(mod3) # 1109

mod4 <- lm(vacs_100k~hhc+rems+ses+htt, vax_2021_clean)
summary(mod4)
BIC(mod4) # 1113

vax_2021_clean <- vax_2021_clean |>
  mutate(hhc_bin = (hhc >= 0.5))

# Plot of Sum of Vax/100k in 2021 by Racial Minority Proportion
ggplot(vax_2021_clean, aes(x = rems, y = vacs_100k, color = hhc_bin)) +
  geom_point() +
  theme_bw() +
  ggtitle('Sum of Vax/100k in 2021 by Racial Minority Percentile') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('Sum of Vax/100k')+
  labs(color = "Household Characteristics Vulnerability") + 
  scale_color_manual(limits = c(TRUE, FALSE),
                     values = c("#FFCC00","#336600"), 
                     labels = c('Above 50th Percentile', "Below 50th Percentile")) +
  theme_gray()






vax_cases_svi <- left_join(vax_2021_clean, cases_p_100k_svi, 
          by = c("County", 'svi', 'rems', 'htt', 'hhc', 'ses'))


# Plots from same df

# Cases Plot
ggplot(vax_cases_svi, aes(x = rems, y = sum_cases_p100k, color = htt_bin)) +
  geom_point() +
  theme_bw() +
  ggtitle('Sum of Cases/100k in 2021 by Racial Minority Proportion') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('Sum of Cases/100k')+
  labs(color = "Housing Type and Transportation Vulnerability") + 
  scale_color_manual(limits = c(TRUE, FALSE),
                     values = c("#FFCC00","#336600"), 
                     labels = c('Above 50th Percentile', "Below 50th Percentile")) +
  theme_gray()

# Vax Plot
ggplot(vax_cases_svi, aes(x = rems, y = vacs_100k, color = hhc_bin)) +
  geom_point() +
  theme_bw() +
  ggtitle('Sum of Vax/100k in 2021 by Racial Minority Percentile') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('Sum of Vax/100k')+
  labs(color = "Household Characteristics Vulnerability") + 
  scale_color_manual(limits = c(TRUE, FALSE),
                     values = c("#FFCC00","#336600"), 
                     labels = c('Above 50th Percentile', "Below 50th Percentile")) +
  theme_gray()
