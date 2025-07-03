library(tidyverse)

# Load in Full Data for 2020 SVI in California
svi_2020_full <- read_csv("svi_2020.csv")

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

