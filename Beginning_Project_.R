library(tidyverse)
library(readr)
library(dplyr)

#VACS data from CDC
covid_vacs <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/COVID-19_Vaccinations_in_the_United_States_County_20250630.csv")
covid_vacs_start <- covid_vacs %>%
  filter(Date == "12/25/2020")

#COVID cases CDC
covid_cases_CDC <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/Weekly_United_States_COVID-19_Cases_and_Deaths_by_County_-_ARCHIVED_20250701.csv")
covid_cases_CDC_CA <- covid_cases_CDC %>%
  filter(state == "CA")

#SVI Data
svi_data <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/California_county.csv")

# normalize data per 100,000 population 
norm_pop_2021 <- counties_pop_2021 %>%
  mutate(rate_per_100k = (cumulative_cases / Cumm_pop_2020) * 100000)
