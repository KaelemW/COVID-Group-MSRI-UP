library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(lubridate)
library(plotly)
library(maps)
library(tigris)
library(scales)
library(leaflet)
library(RColorBrewer)
library(sf)
library(rcartocolor)
library(wesanderson)
library(ggtext)


covid_county <- read.csv('~/Creative Cloud Files/MSRI-UP/Weekly_United_States_COVID-19_Cases_and_Deaths_by_County_-_ARCHIVED_20250702.csv')

covid_CA <- filter(covid_county, state == 'CA')

Covid_CA = subset(covid_CA, select = -c(fips_code, state_fips, cumulative_deaths, New.deaths) )

 #write_csv(Covid_CA, 'Covid_CA.csv')

# Convert date column from character to date format
# We need this in order to filter the data by date 
Covid_CA <- Covid_CA %>%
  mutate(date = as.Date(date, format='%m/%d/%Y'))

# Now, we filter out data to year 2021
 covid_cases_2021 <- filter(Covid_CA, between(date, as.Date('2021-01-01'), as.Date('2021-12-31')))



# lets create an interactive heatmap
# First we convert out date to spatial data if not in that form
library(sf)

norm_pop_2020_new<- sf::st_as_sf(norm_pop_2020_new)  # make sure your sf data has spatial data for what you are trying to plot

# now we create our color scheme function 
color_palette <- colorNumeric(palette = "YlOrRd",domain = norm_pop_2020_new$rate_per_100k, na.color = "transparent") 


# Prepare the text for the hover feature:
mytext <- paste(
  "County: ", norm_pop_2020_new$county, "<br/>",
  "Total Cases: ", comma(norm_pop_2020_new$cumulative_cases), "<br/>",  #the comma fcn adds comma's in your displayed value
  "Cases Per 100,000 : ",comma(norm_pop_2020$rate_per_100k), "<br/>" ,  # <br/> breaks the lines of text so theyre on top of eachother
  sep = ""
) %>%
  lapply(htmltools::HTML)

# Final Map
inter_map_2020 <- leaflet() %>%
  addTiles() %>%
  addControl("Reported Covid Cases in CA by County Per 100,000 People - 2020", position = "topleft") %>%  # this is the title of map
  addPolygons(
    data = norm_pop_2020_new, 
    fillColor = ~color_palette(rate_per_100k), #the variable you want to use goes into parenthesis 
    stroke = TRUE,  # this displays outline of geography 
    fillOpacity = 0.9,  # ranges from 0-1: 0 = sheer and 1= completely opaque for color fill of heat map
    color = "white",  # color out outline of geography features 
    weight = 0.3,  # thickness of outline ^ 
    label = mytext,  # this displays the text when you hover over
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = color_palette, values = norm_pop_2020_new$rate_per_100k, opacity = 0.9,
    title = "Cases Per 100,000 People", position = "bottomleft"
  )

inter_map_2020 

# interactive map for 2021
norm_pop_2021_new<- sf::st_as_sf(norm_pop_2021_new)

color_palette <- colorNumeric(palette = "YlOrRd",domain = norm_pop_2021_new$rate_per_100k, na.color = "transparent")

# Prepare the text for the hover feature:
mytext <- paste(
  "County: ", norm_pop_2021_new$county, "<br/>",
  "Total Cases: ", comma(norm_pop_2021_new$cumulative_cases), "<br/>",
  "Cases Per 100,000 : ",comma(norm_pop_2021_new$rate_per_100k) , "<br/>" ,
  sep = ""
) %>%
  lapply(htmltools::HTML)

# Final Map
inter_map_2021 <- leaflet() %>%
  addTiles() %>%
  addControl("Reported Covid Cases in CA by County Per 100,000 People - 2021", position = "topleft") %>%
  addPolygons(
    data = norm_pop_2021_new, 
    fillColor = ~color_palette(rate_per_100k),
    stroke = TRUE,
    fillOpacity = 0.9,
    color = "white",
    weight = 0.3,
    label = mytext,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto")
  ) %>%
  addLegend(
    pal = color_palette, values = norm_pop_2021_new$rate_per_100k, opacity = 0.9,
    title = "Cases Per 100,000 People", position = "bottomleft"
  )
                                                                                                                                                                                                                                 

inter_map_2021



# interactive map for vaccinations 

# covert date column to date format
covid_cases_vacs <- covid_cases_vacs %>%
  mutate(Date = as.Date(Date, format='%m/%d/%Y'))

# Filter out by date: 2021
vacs_2021 <- filter(covid_cases_vacs, between(Date, as.Date('2021-01-01'), 
                                             as.Date('2021-12-31')))


# Join spatial data to vacs_2021
# this was how I combined the data for each county so it displayed per row

vacs_summary <- cases_vacs_2021_spatial %>%   # I just renamed vacs_2021 to cases_vacs_2021_spatial
  group_by(county) %>%
  summarize(
    Dose1_Total = sum(Dose1_Total),  # Calculates the total cases for each county
    Dose1_Avg_Pct = mean(Dose1_Pop_Pct), # calculates average 
    Dose1_Avg_decimal = ((mean(Dose1_Pop_Pct))/100) , # I did this to convert percentages into decimal format for later 
    # You can add more summary statistics as needed
    Dose1_65Plus = sum(Dose1_65Plus),
    cumulative_cases = sum(cumulative_cases),
    new_cases = sum(New.cases),
    Dose1_65Plus_Pct = mean(Dose1_65Plus_Pct), 
    Dose1_65Plus_dec = ((mean(Dose1_65Plus_Pct))/100) 
  )

# lets create the heatmap 
color_palette <- colorNumeric(palette = "Spectral",domain = vacs_summary$Dose1_65Plus_Pct, na.color = "transparent")

# Prepare the text for the hover feature:
mytext2 <- paste(
  "County: ", vacs_summary$county, "<br/>",
  "Total Dose 1 Vaccinations: ", comma(vacs_summary$Dose1_Total), "<br/>",
  "Dose 1 Vaccination Percentage : ",percent(vacs_summary$Dose1_Avg_decimal, accuracy = 0.1), "<br/>" , # this convert the value into a percent and adds 
                                                                                                       # the percent symbol next to it 
  "Age 65+ : ", percent(vacs_summary$Dose1_65Plus_dec, accuracy = 0.1), "<br/>",
  sep = ""
) %>%
  lapply(htmltools::HTML) 

head(mytext2)

# Final Map
inter_Vacsmap_2021 <- leaflet() %>%
  addTiles() %>%
  addControl("Percentage of Vaccinations in CA Per County - 2021", position = "topleft") %>%
  addPolygons(
    data = vacs_summary, 
    fillColor = ~color_palette(Dose1_Avg_Pct),
    stroke = TRUE,
    fillOpacity = 0.9,
    color = "white",
    weight = 0.2,
    label = mytext2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = color_palette, values = vacs_summary$Dose1_Avg_Pct, labFormat = labelFormat(suffix = '%'), opacity = 0.9,
    title = "Percentage of Vaccinations", position = "bottomleft"
  )

inter_Vacsmap_2021
