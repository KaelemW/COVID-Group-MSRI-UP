# Heat map of vaccinations in 2021

#libraries and options
library(sf)
library(ggplot2)
library(tigris)
options(tigris_use_cache = TRUE)

covid_svi <- read_csv("covid_svi.csv")
covid_svi$Date <- mdy(covid_svi$Date)

#Filter full covid+svi dataset to 2021
covid_2021 <- covid_svi |>
  filter(Date > '2021-01-01' & Date < '2021-12-31')

#Load all CA counties data from tigris
ca_counties <- counties(state = "CA", cb = TRUE, class = "sf")

# Join counties sf data with covid+svi data
counties_map <- ca_counties |>
  left_join(covid_2021, by = c("NAMELSAD" = 'County'))

# Looking at vaccinations in May because that is when 
# "the F.D.A. extended its emergency use authorization for the Pfizer vaccine to children 12 and older."
may_counties <- counties_map |>
  filter(Date == '2021-05-26')
  

#Plot Percentage of Population Vaccinated Against COVID
ggplot(may_counties) +
  geom_sf(aes(fill = Pop_Vacs_Pct), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "Vaccination Rate (Overall)") +
  theme_minimal() +
  labs(
    title = "Percent of Population Vaccinated Against COVID by California County",
    subtitle = "May 26, 2021",
    caption = "Data source: CDC SVI 2020"
  )
4