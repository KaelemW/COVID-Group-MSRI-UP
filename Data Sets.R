library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(corrplot)
library(plotly)
library(mapview)
library(leaflet)
library(RColorBrewer)
library(tigris)
options(tigris_use_cache = TRUE)

#VACS data from CDC
covid_vacs <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/COVID-19_Vaccinations_in_the_United_States_County_20250630.csv")
covid_vacs_clean <- covid_vacs [, c("Date", "Recip_County", "Recip_State", "Administered_Dose1_Recip", "Administered_Dose1_Pop_Pct")] %>% 
  rename(County = Recip_County, State = Recip_State, Pop_Vacs = Administered_Dose1_Recip, Pop_Vacs_Pct = Administered_Dose1_Pop_Pct)

#COVID cases CDC
covid_cases_CDC <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/Weekly_United_States_COVID-19_Cases_and_Deaths_by_County_-_ARCHIVED_20250701.csv")
covid_CA <- filter(covid_cases_CDC, state == 'CA') |>
  rename(Date = date, State = state, County = county)
Covid_CA_clean = subset(covid_CA, select = -c(fips_code, state_fips, cumulative_deaths, New.deaths) )

#SVI Data
svi_2020_full <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/California_county.csv")
svi_2020 <- svi_2020_full |>
  select(c(COUNTY, RPL_THEMES, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4)) |>
  rename(County = COUNTY, svi = RPL_THEMES, ses = RPL_THEME1, hhc = RPL_THEME2, rems = RPL_THEME3, htt = RPL_THEME4)

#merged data set
covid_cases_vacs <- inner_join(Covid_CA_clean, covid_vacs_clean, by = c("Date", "County", "State"))
covid_cases_vacs_out <- full_join(Covid_CA_clean, covid_vacs_clean, by = c("Date", "County", "State"))
write_csv(covid_cases_vacs_out, "covid_cases_vacs.csv")

#Counties Population data per year
counties_pop <- read.csv("~/MSRI-UP-KBW-2/Covid Trial 2 please work/counties_pop.csv")
counties_pop_2021 <- counties_pop %>% 
  select(c(county, Cumm_pop_2021)) %>%
  rename(County = county)

#Rename column of ca_svi_map
ca_svi_better <- ca_svi_map %>%
  select(c(NAMELSAD, svi, ses, hhc, rems, htt)) %>%
  rename(County = NAMELSAD)
