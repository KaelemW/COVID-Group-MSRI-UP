library(tidyverse)
library(lubridate)

# Load in Full Data for 2020 SVI in California
svi_2020_full <- read_csv("socialvulnerability.csv")

# Clean Data
# Selected columns for overall svi percentile, as well as the percentile for each of 4 categories
svi_2020 <- svi_2020_full |>
  select(c(COUNTY, RPL_THEMES, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4)) |>
  rename(svi = RPL_THEMES, ses = RPL_THEME1, hhc = RPL_THEME2, rems = RPL_THEME3, htt = RPL_THEME4, County = COUNTY)

# Note: 
# svi = social vulnerability index
# ses = socioeconomic status
# hhc = household characteristics
# rems = racial and ethnic minority status
# htt = housing type / transportation

#Here is how I got the cleaned data frame into a csv file:
# write_csv(svi_2020, "svi_2020.csv")

covid <- read_csv("covid_cases_vacs.csv")

for(i in 1:58){
  svi_2020[i,1] <- str_c(svi_2020[i,1], " County")
}

covid_svi <- left_join(covid, svi_2020, by = "County")

covid_svi$Date <- mdy(covid_svi$Date)



# Adding in hhc information

svi_select <- svi_2020_full |>
  select(c(COUNTY, RPL_THEME2, RPL_THEME3, E_AGE65, E_DISABL, E_LIMENG, E_AGE17, E_SNGPNT)) |>
  rename(County = COUNTY, hhc = RPL_THEME2, rems = RPL_THEME3, age65 = E_AGE65,  
         disabl = E_DISABL, limengl = E_LIMENG, age17 = E_AGE17, singpar = E_SNGPNT)



for(i in 1:58){
  svi_select[i,1] <- str_c(svi_select[i,1], " County")
}
svi_select[58,1] <- 'Yuba County'



# Adding in Racial Demographics

race_full <- read_csv("race.csv")


race <- race_full |>
  rename(pop = `Total Population - Estimate`, 
         latino = `Hispanic or Latino - Estimate`,
         white = `White alone, Not Hispanic or Latino - 
Estimate`, 
         black = `Black alone, Not Hispanic or Latino - Estimate`,
         native = `American Indian and Alaska Native alone, Not Hispanic or Latino - Estimate`,
         asian = `Asian alone, Not Hispanic or Latino - Estimate`,
         pacisl = `Native Hawaiian and Other Pacific Islander alone, Not Hispanic or Latino - 
Estimate`,
         other = `Some other race alone, Not Hispanic or Latino - 
Estimate`,
         multi = `Two or more races alone, Not Hispanic or Latino - Estimate`) |>
  select(Geography, pop, white, black, latino, native, asian, pacisl, other, multi) |>
  rename(County = Geography)


race <- race |>
  mutate(white_pct = white/pop,
         black_pct = black/pop,
         latino_pct = latino/pop,
         native_pct = native/pop,
         asian_pct = asian/pop,
         pacisl_pct = pacisl/pop,
         other_pct = other/pop,
         multi_pct = multi/pop)



race_and_hhc <- left_join(svi_select, race)

covid_race_hhc <- left_join(covid, race_and_hhc, by = "County")
covid_race_hhc$Date <- mdy(covid_race_hhc$Date)

covid_race_hhc <- covid_race_hhc |>
  mutate(new_cases_pc = New.cases/pop * 100000)
