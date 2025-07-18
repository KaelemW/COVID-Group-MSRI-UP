# Start of scatter/line graphs of covid cases
#
#
#
#
# Convert date column from character to date format
# We need this in order to filter the data by date 
Covid_CA_clean <- Covid_CA_clean %>%
  mutate(Date = as.Date(Date, format='%m/%d/%Y'))

# Now, we filter out data to year 2021
covid_cases_2021 <- filter(Covid_CA_clean, between(Date, as.Date('2021-01-01'), 
                                                   as.Date('2021-12-31')))
ggplot(covid_cases_2021, aes(x = Date, y = cumulative_cases, color = County)) +
  geom_line(alpha = 0.6, size = 1) +
  labs(
    titile = "Cumulative COVID Cases in 2021",
    x = "Date",
    y = "Cumulative Cases", 
    color = "County"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "turbo")

covid_cases_2020 <- filter(Covid_CA_clean, between(Date, as.Date('2020-01-01'), 
                                                   as.Date('2020-12-31')))
ggplot(covid_cases_2020, aes(x = Date, y = cumulative_cases, color = County)) +
  geom_line(alpha = 0.6, size = 1) +
  labs(
    titile = "Cumulative COVID Cases in 2020",
    x = "Date",
    y = "Cumulative Cases", 
    color = "County"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "turbo")

covid_cases_2022 <- filter(Covid_CA_clean, between(Date, as.Date('2022-01-01'), 
                                                   as.Date('2022-12-31')))
ggplot(covid_cases_2022, aes(x = Date, y = cumulative_cases, color = County)) +
  geom_line(alpha = 0.6, size = 1) +
  labs(
    titile = "Cumulative COVID Cases in 2022",
    x = "Date",
    y = "Cumulative Cases", 
    color = "County"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "turbo")
ggplot(Covid_CA_clean, aes(x = Date, y = cumulative_cases, color = County)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  labs(
    title = "Cumulative COVID Cases in 2020-2022",
    x = "Date",
    y = "Cumulative Cases", 
    color = "County"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")

#Start of scatter/line graphs for covid vacs
#
#
#
#
covid_vacs_clean <- covid_vacs_clean %>%
  mutate(Date = as.Date(Date, format='%m/%d/%Y'))
covid_vacs_clean_2022 <- filter(covid_vacs_clean, between(Date, as.Date('2022-01-01'), 
                                                          as.Date('2022-12-30')))
ggplot(covid_vacs_clean_2022, aes(x = Date, y = Pop_Vacs, color = County)) +
  geom_line() +
  labs(
    titile = "Cumulative COVID Vacinated Population in 2022",
    x = "Date",
    y = "Population Vacinated", 
    color = "County"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "turbo") +
  scale_y_continuous(limits = c(0,10000000))

covid_vacs_clean_2021 <- filter(covid_vacs_clean, between(Date, as.Date('2021-01-01'), 
                                                          as.Date('2021-12-31')))
ggplot(covid_vacs_clean_2021, aes(x = Date, y = Pop_Vacs, color = County)) +
  geom_line() +
  labs(
    titile = "Cumulative COVID Vacinated Population in 2022",
    x = "Date",
    y = "Population Vacinated", 
    color = "County"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d(option = "turbo")

