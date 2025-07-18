covid_county <- read.csv('~/Creative Cloud Files/MSRI-UP/Weekly_United_States_COVID-19_Cases_and_Deaths_by_County_-_ARCHIVED_20250702.csv')

covid_CA <- filter(covid_county, state == 'CA')

Covid_CA = subset(covid_CA, select = -c(fips_code, state_fips, cumulative_deaths, New.deaths) )



county_csv <- read_csv("~/Downloads/county_CA_pop1.csv")

write_csv(counties_pop, 'counties_pop.csv')

#write_csv(Covid_CA, 'Covid_CA.csv')

#SVI data csv form Audrey
svi_2020 <- read.csv("~/Downloads/svi_2020.csv")

#Vacs data from Kaelem
covid_cases_vacs <- read.csv("~/Downloads/covid_cases_vacs.csv")

vacs_CDC <- read.csv("~/Downloads/COVID-19_Vaccinations_in_the_United_States_County_20250709.csv")

#write_csv(covid_cases_2021, "cases_2021.csv")

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(lubridate)
library(plotly)
library(tigris)
library(scales)
library(leaflet)
library(sf)


#install.packages("choroplethr, choroplethrMaps,RColorBrewer")

p_load(tidyverse, choroplethr, choroplethrMaps,RColorBrewer)

#install.packages('tigris')
#install.packages('sf')
#install.packages('rcartocolor')
#install.packages("wesanderson")
#install.packages('ggtext')
#install.packages('mapview')
#install.packages("viridis")
library(rcartocolor)
library(wesanderson)
library(ggtext) # text in plots
library(ggiraph)
library(viridis)


# Converts date column from character to date format
# We need this in order to filter the data by date 
Covid_CA <- Covid_CA %>%
  mutate(date = as.Date(date, format='%m/%d/%Y'))


# Filter Covid data to year 2020
Covid_CA_2020 <- filter(Covid_CA, 
                        between(date, as.Date('2020-01-01'), as.Date('2020-12-31')))


# Create the heat-map for Covid cases in 2020 by county

# First obtaining Spatial data for CA by county 
ca_counties <- tigris::counties(state = "CA", cb = TRUE) %>%
  sf::st_as_sf()

# Joining the spatial data with the Covid 2020 data

# First renaming a column name in ca_county to match the column name in Covid_CA_2020 
# so we can merge them 
ca_counties <- ca_counties %>% rename(county = NAMELSAD)

# Now we left join 
Covid_join <- ca_counties %>%
  left_join(Covid_CA_2020, ca_counties, by = 'county')

# lets get the summary of our data 
summary(Covid_CA_2020)

# Now let's create the heatmap 
ggplot(data = Covid_join) +
  geom_sf(aes(fill = cumulative_cases)) +
  #geom_sf_text(aes(label = county), size = 1) + # This adds the names of the counties to map, I didnt like how it looked so i commented it out 
  scale_fill_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous"), 
                       breaks = c(0,200000, 360000,550000,754601), 
                       labels = c("0","200000","360000","550000", "754601"), 
                      name = 'Cumulative Cases') + # Customize colors and legend title
  theme_void() + # Remove axis and grid lines
  ggtitle("Covid Cases in CA by County - 2020") # Add a title

# ----------------------------------------------------------------------------------------------
# THESE ARE JUST COLOR PALETTES I PLAYED WITH 

#colors = rcartocolor::carto_pal(name = "TealRose", n = 7)
#palette = 'YlOrRd' different color pallete <--- use distiller instead of gradientn
#colors = wes_palette("Zissou1", 10, type = "continuous")) different color pallete

# ----------------------------------------------------------------------------------------------
# Lets create another heatmap of covid cases for 2021

# First, we filter out data to 2021
 covid_cases_2021 <- filter(Covid_CA, between(date, as.Date('2021-01-01'), 
                                              as.Date('2021-12-31')))

cases_vacs_2021_new <- filter(cases_vacs_2021_new, between(Date, as.Date('2021-01-01'), 
                                             as.Date('2021-12-31')))

# Now we left join the Spatial data of CA to our filtered dataset 
cases_2021_join <- ca_counties %>%
  left_join(covid_cases_2021, ca_counties, by = 'county')

summary(covid_cases_2021)

# Now we create the heatmap  
#ggplot(data = cases_2021_join) +
  #geom_sf(aes(fill = cumulative_cases)) +
  #geom_sf_text(aes(label = county), size = 1) + # This adds the names of the counties to map, I didnt like how it looked so i commented it out 
  #scale_fill_gradientn(colors = wes_palette("Zissou1", 10, type = "continuous"), 
                       #name = 'Cumulative Cases') + # Customize colors and legend title
  #theme_void() + # Remove axis and grid lines
  #ggtitle("Covid Cases in CA by County - 2021") # Add a title 3 


# remove year 2024 since we dont need it 
counties_pop <- county_csv[-c(6)]


# Now we left join the  new Spatial data of CA to our filtered dataset 
counties_pop_new <- counties_pop %>%
  left_join(Covid_join, counties_pop_spatial, by = 'county')

counties_pop_new <- counties_pop_new %>% 
  rename(Cumm_pop_2020 = '2020', Cumm_pop_2021 = '2021', Cumm_pop_2022 = '2022', Cumm_pop_2023 = '2023')

# I am just renaming it 
counties_pop_2020 <- counties_pop_new

rm(counties_pop_new)


# normalize 2020 data per population 
norm_pop_2020 <- counties_pop_2020 %>%
  mutate(rate_per_100k = (cumulative_cases / Cumm_pop_2020) * 100000)

# I did not normaslize this correctly so the plot is not an accurate representation
# Now we create the heatmap for 2020
ggplot() +
  geom_sf(data = norm_pop_2020, aes(geometry = geometry, fill = rate_per_100k),lwd = 0.01, color = "white") +
  #geom_sf_text(aes(label = county), size = 1) + # This adds the names of the counties to map, I didnt like how it looked so i commented it out 
  scale_fill_distiller(palette = 'YlOrRd', n = 6, direction = 1, 
                       name = 'Cumulative Cases Per 100,000 People') + # Customize colors and legend title
  theme_void() + # Remove axis and grid lines
  ggtitle("Covid Cases in CA by County - 2020") # Add a title 3


# Lets create a heatmap for 2021 to compare 

# I renamed the column names so i didnt have to keep doing it per data set 
counties_pop <- counties_pop %>% 
  rename(Cumm_pop_2020 = '2020', Cumm_pop_2021 = '2021', Cumm_pop_2022 = '2022', Cumm_pop_2023 = '2023')


# Again, this was not done right
# create the dataset we are going to use 
vacs_CDC_2021_new <- counties %>%
  left_join(vacs_CDC_2021_new, counties, by = 'county')

# normalize 2021 data per population 
norm_pop_2020_new <- norm_pop_2020_new %>%
  mutate(rate_per_100k = (cumulative_cases / Cumm_pop_2021) * 100000)

# Create the heatmap for Vaccines 2021
ggplot() +
  geom_sf(data = vacs_CDC_2021_new, aes(geometry = geometry, fill = Dose1_Pop_Pct),lwd = 0.01, color = "white") +
  scale_fill_distiller(palette = 'YlOrRd', n = 6,  direction = 1, 
                       name = 'Vaccination Percentage',
                       labels = scales::label_percent(scale = 1)) + # makes the lengend scales in percent format
  theme_void() + # Remove axis and grid lines
  ggtitle("Vaccination Rates")   # title 

# This is the correct heatmaps for 2021

# Create the heatmap for Cases 2021
ggplot() +
  geom_sf(data = vacs_CDC_2021_new, aes(geometry = geometry, fill = cases_per_100k),lwd = 0.01, color = "white") +
  scale_fill_distiller(palette = 'YlOrRd', n = 6,  direction = 1, 
                       name = 'Covid Cases Per 100,000',
                       labels = scales::label_comma()) + # Adds comma to legend scales 
  theme_void() + # Remove axis and grid lines
  ggtitle("Covid Cases")  # title


# Create the heatmap for SVI 
ggplot() +
  geom_sf(data = vacs_CDC_2021_new, aes(geometry = geometry, fill = svi),lwd = 0.01, color = "white") +
  scale_fill_distiller(palette = 'YlOrRd', n = 6,  direction = 1, 
                       name = 'Social Vulnerability Index') +
  theme_void() + # Remove axis and grid lines
  ggtitle("Social Vulnerablity")  # title

# Create the heatmap for rems 
ggplot() +
  geom_sf(data = vacs_CDC_2021_new, aes(geometry = geometry, fill = rems),lwd = 0.01, color = "white") +
  scale_fill_distiller(palette = 'YlOrRd', n = 6,  direction = 1, 
                       name = 'Racial and Ethnic Minority Status Index') +
  theme_void() + # Remove axis and grid lines
  ggtitle("Racial and Ethnic Minority Status")  # title

# lets create an interactive heatmap for cases 2020
# First we convert out df to spatial data 
library(sf)

norm_pop_2020_new<- sf::st_as_sf(norm_pop_2020_new)


# now we create our color scheme function 
color_palette <- colorNumeric(palette = "YlOrRd",domain = norm_pop_2020_new$rate_per_100k_new, na.color = "transparent")


# Prepare the text for the hover feature:
mytext <- paste(
  "County: ", norm_pop_2020_new$county, "<br/>",
  "Total Cases: ", comma(norm_pop_2020_new$cumulative_cases), "<br/>",
  "Cases Per 100,000 : ",comma(norm_pop_2020_new$rate_per_100k_new, accuracy = 0.1), "<br/>" ,
  sep = ""
) %>%
  lapply(htmltools::HTML)

# Final Map
inter_map_2020 <- leaflet() %>%
  addTiles() %>%
  addControl("Reported Covid Cases in CA by County Per 100,000 People - 2020", position = "topleft") %>%
  addPolygons(
    data = norm_pop_2020_new, 
    fillColor = ~color_palette(rate_per_100k_new),
    stroke = TRUE,
    fillOpacity = 0.9,
    color = "white",
    weight = 0.3,
    label = mytext,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = color_palette, values = norm_pop_2020_new$rate_per_100k_new, opacity = 0.9,
    title = "Cases Per 100,000 People", position = "bottomleft"
  )

inter_map_2020

# interactive map for 2021
norm_pop_2021_new<- sf::st_as_sf(norm_pop_2021_new)

color_palette <- colorNumeric(palette = "YlOrRd",domain = norm_pop_2021_new$rate_per_100k_new, na.color = "transparent")


mytext <- paste(
  "County: ", norm_pop_2021_new$county, "<br/>",
  "Total Cases: ", comma(norm_pop_2021_new$cumulative_cases), "<br/>",
  "Cases Per 100,000 : ",comma(norm_pop_2021_new$rate_per_100k_new, accuracy = 0.1) , "<br/>" ,
  sep = ""
) %>%
  lapply(htmltools::HTML)

# Final Map
inter_map_2021 <- leaflet() %>%
  addTiles() %>%
  addControl("Reported Covid Cases in CA by County Per 100,000 People - 2021", position = "topleft") %>%
  addPolygons(
    data = norm_pop_2021_new, 
    fillColor = ~color_palette(rate_per_100k_new),
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
    pal = color_palette, values = norm_pop_2021_new$rate_per_100k_new, opacity = 0.9,
    title = "Cases Per 100,000 People", position = "bottomleft"
  )
                                                                                                                                                                                                                                 

inter_map_2021



# interactive map for vaccinations

# covert date column to date format
vacs_CDC_2021 <- vacs_CDC_2021 %>%
  mutate(Date = as.Date(Date, format='%m/%d/%Y'))



# Filter out by the last date recorded in 2021 to get cumulative total and ppercentage of population for the year
vacs_CDC_2021_new <- vacs_CDC_2021[vacs_CDC_2021$date == '2021-12-31', ]


# Join spatial data to vacs_2021
vacs_CDC_2021_new<- ca_counties %>%
  left_join(vacs_CDC_2021_new, ca_counties, by = 'county')

counties <- ca_counties[, c('county','geometry' )]

# Convert the data back to sf in order to plot using leaflet
vacs2_CDC_2021_new<- sf::st_as_sf(vacs2_CDC_2021_new)

# lets create the heatmap 
color_palette <- colorNumeric(palette = "YlOrRd",domain = vacs2_CDC_2021_new$Dose1_Pop_Pct, na.color = "transparent")

# Prepare the text for the hover feature:
mytext2 <- paste(
  "County: ", vacs2_CDC_2021_new$county, "<br/>",
  "Total Dose 1 Vaccinations: ", comma(vacs2_CDC_2021_new$Dose1_Total), "<br/>",
  "Dose 1 Vaccination Percentage : ",percent(vacs2_CDC_2021_new$Dose1_Pop_Pct/100, accuracy = 0.1), "<br/>",
  "65 + : ", percent(vacs2_CDC_2021_new$Dose1_65Plus_Pct/100, accuracy = 0.1), "<br/>", 
  sep = ""
) %>%
  lapply(htmltools::HTML) 


# Final Map
inter_Vacsmap_2021 <- leaflet() %>%
  addTiles() %>%
  addControl("Percentage of Vaccinations in CA Per County - 2021", position = "topleft") %>%
  addPolygons(
    data = vacs2_CDC_2021_new, 
    fillColor = ~color_palette(Dose1_Pop_Pct),
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
    pal = color_palette, values = vacs2_CDC_2021_new$Dose1_Pop_Pct, labFormat = labelFormat(suffix = '%'), opacity = 0.9,
    title = "Percentage of Vaccinations", position = "bottomleft"
  )

inter_Vacsmap_2021


# Average Daily cases for covid 

# Sum the cumulative cases per week
norm_pop_2020_new <- norm_pop_2020_new %>%
  group_by(county) %>%
  summarize(
    new_cases = sum(cases),  # Calculates the total cases for each county
    cumulative_cases = cumulative_cases,
    Cumm_pop_2020 = sum(Cumm_pop_2020), 
    Cumm_pop_2021 = sum(Cumm_pop_2021),
    Cumm_pop_2022 = sum(Cumm_pop_2022), 
    Cumm_pop_2023 = sum(Cumm_pop_2023))



# Case count for each day
covid_2021 = covid_2021 %>%
  mutate(cases2 = cumulative_cases - lag(cumulative_cases,default = 0))
covid_2021 %>%
  ggplot(aes(x = date, y = cases2)) + 
  geom_line()




# To create the 7-day average, we will sum the case counts from the current day 
# and the previous 6 days and divide by 7
cases_smooth2021 = covid_2021 %>%
  mutate(smooth = sum(cases2,
                      lag(cases2, 1), lag(cases2, 2),
                      lag(cases2, 3), lag(cases2, 4),
                      lag(cases2, 5), lag(cases2, 6))
         /7)

rolling_average <- function(x, period = 7){
  total = x
  for(i in 1:period-1){
    total = total + lag(x,i)
  }
  return(total/period)
}

cases_smooth2021 = covid_2021 %>%
  mutate(smooth = rolling_average(cases2))
cases_smooth2021

plot_2021_cases <- cases_smooth2021 %>%
  filter(!is.na(smooth)) %>%
  ggplot(aes(x = date, y = smooth)) +
  geom_line(color = "red") #+
  #geom_area(fill = "red", alpha = .25) 
plot_2021_cases

# Update the Theme
plot_2021_cases <- plot_2021_cases +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = -.5,
                               margin = margin(r = -30)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey',
                                      linetype = 'dashed',size = .35),
    axis.ticks.x = element_line(color = "grey"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "grey", linetype = "solid",
                               size = .5))

plot_2021_cases

# Adjust the scale, add a title and some labels
breaks <- scales::extended_breaks()(cases_smooth2021$smooth)
breaks <- breaks[2:length(breaks)]
plot_2021_cases <- plot_2021_cases +
  scale_x_date(expand = c(0,0),
               date_labels = "%b-%Y") +
  scale_y_continuous(expand = c(0,0),
                     breaks = breaks,
                     
                     limits = c(0,max(cases_smooth2021$smooth))) +
  
  labs(y = NULL,
       x = NULL,
       title = "Average Daily COVID Cases In California - 2021")
plot_2021_cases

# We simply take a slice from the center of the chart (120 day period) and
#find the maximum point on the smooth line. We will then add our 
#annotations off this reference point.
max_date = covid_2021$date %>% 
  median() + 60
min_date = covid_2021$date %>% 
  median() - 60
label_vals = cases_smooth2021 %>%
  filter(date > min_date, date < max_date) %>%
  arrange(desc(smooth)) %>%
  top_n(1)

label_vals


# Now we add annotations 
plot_2021_cases <- plot_2021_cases +
  annotate("text", x = label_vals$date,
           y = label_vals$smooth * 2,
           label = "7-day\naverage",
           size = 2.5,
           fontface = "plain") +
  annotate("segment", x = label_vals$date, xend = label_vals$date,
           y = label_vals$smooth,
           yend = label_vals$smooth * 1.75,
           size = .25)
plot_2021_cases

# Average Daily Cases for 2020-2021

# Filter out data 

covid_20_21 <- filter(Covid_CA, between(date, as.Date('2020-01-01'), as.Date('2021-12-31')))

# Sum the cumulative cases per week
covid20_21<- covid_20_21 %>%
  group_by(date) %>%
  summarize(
    cases = sum(New.cases),  # Calculates the total cases for each county
    cumulative_cases = sum(cumulative_cases)) # cumulative cases for all counties on a specific date


# Case count for each day
vacs_CDC_2021_new = vacs_CDC_2021 %>%
  mutate(new_dose1 = Dose1_Total - lag(Dose1_Total,default = 0))
covid20_21 %>%
  ggplot(aes(x = date, y = cases2)) +
  geom_line()

# To create the 7-day average, we will sum the case counts from the current day 
# and the previous 6 days and divide by 7
cases_smooth20_21 = covid20_21 %>%
  mutate(smooth = sum(cases2,
                      lag(cases2, 1), lag(cases2, 2),
                      lag(cases2, 3), lag(cases2, 4),
                      lag(cases2, 5), lag(cases2, 6))
         /7)

rolling_average <- function(x, period = 7){
  total = x
  for(i in 1:period-1){
    total = total + lag(x,i)
  }
  return(total/period)
}

cases_smooth20_21 = covid20_21 %>%
  mutate(smooth = rolling_average(cases2))
cases_smooth20_21

plot_20_21_cases <- cases_smooth20_21 %>%
  filter(!is.na(smooth)) %>%
  ggplot(aes(x = date, y = smooth)) +
  geom_line(color = "red") +
  geom_area(fill = "red", alpha = .25)
plot_20_21_cases

# Update the Theme
plot_20_21_cases <- plot_20_21_cases +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = -.5,
                               margin = margin(r = -30)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey',
                                      linetype = 'dashed',size = .35),
    axis.ticks.x = element_line(color = "grey"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "grey", linetype = "solid",
                               size = .5))

plot_20_21_cases

# Adjust the scale, add a title and some labels
breaks <- scales::extended_breaks()(cases_smooth20_21$smooth)
breaks <- breaks[2:length(breaks)]
plot_20_21_cases <- plot_20_21_cases +
  scale_x_date(expand = c(0,0),
               date_labels = "%b-%Y") +
  scale_y_continuous(expand = c(0,0),
                     breaks = breaks,
                     
                     limits = c(0,max(cases_smooth20_21$smooth))) +
  
  labs(y = NULL,
       x = NULL,
       title = "Average Daily COVID Cases In California 2020-2021")
plot_20_21_cases

# We simply take a slice from the center of the chart (120 day period) and
#find the maximum point on the smooth line. We will then add our 
#annotations off this reference point.
max_date = covid20_21$date %>% 
  median() + 60
min_date = covid20_21$date %>% 
  median() - 60
label_vals = cases_smooth20_21 %>%
  filter(date > min_date, date < max_date) %>%
  arrange(desc(smooth)) %>%
  top_n(1)

label_vals

# Now we add annotations 
plot_20_21_cases <- plot_20_21_cases +
  annotate("text", x = label_vals$date,
           y = label_vals$smooth * 1.2,
           label = "7-day\naverage",
           size = 2.5,
           fontface = "plain", vjust=2) +
  annotate("segment", x = label_vals$date, xend = label_vals$date,
           y = label_vals$smooth,
           yend = label_vals$smooth * 1.07,
           size = 0.25)

# Creating daily average for vaccinations 
vacs_2021<- cases_vacs_2021 %>%
  group_by(date) %>%
  summarize(
    Dose1_Total = sum(Dose1_Total),  # Calculates the total cases for each county
    Dose1_65Plus = sum(Dose1_65Plus)) 



# Case count for each day
vacs_2021_new = vacs_2021 %>%
  mutate(Dose1_new = Dose1_Total - lag(Dose1_Total,default = 0))
vacs_2021_new %>%
  ggplot(aes(x = date, y = Dose1_new)) +
  geom_line()

# To create the 7-day average, we will sum the case counts from the current day 
# and the previous 6 days and divide by 7
vacs_2021_smooth = vacs_2021_new %>%
  mutate(smooth = sum(Dose1_new,
                      lag(Dose1_new, 1), lag(Dose1_new, 2),
                      lag(Dose1_new, 3), lag(Dose1_new, 4),
                      lag(Dose1_new, 5), lag(Dose1_new, 6))
         /7)

rolling_average <- function(x, period = 7){
  total = x
  for(i in 1:period-1){
    total = total + lag(x,i)
  }
  return(total/period)
}

vacs_2021_smooth = vacs_2021_new %>%
  mutate(smooth = rolling_average(Dose1_new))
vacs_2021_smooth


plot_vacs_2021 <- vacs_2021_smooth %>%
  filter(!is.na(smooth)) %>%
  ggplot(aes(x = date, y = smooth)) +
  geom_line(color = "forestgreen") #+
  #geom_area(fill = "forestgreen", alpha = .25)
plot_vacs_2021

# Update the Theme
plot_vacs_2021 <- plot_vacs_2021 +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = -.5,
                               margin = margin(r = -30)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey',
                                      linetype = 'dashed',size = .35),
    axis.ticks.x = element_line(color = "grey"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "grey", linetype = "solid",
                               size = .5))

plot_vacs_2021

# Adjust the scale, add a title and some labels
breaks <- scales::extended_breaks()(covid_2021$cases)
breaks <- breaks[2:length(breaks)] 

plot_vacs_2021 <- plot_vacs_2021 +
  scale_x_date(expand = c(0,0),
               date_labels = "%b-%Y") +
  scale_y_continuous(expand = c(0,0),
                     breaks = breaks,
                     
                     limits = c(0,max(cases_smooth20_21$smooth))) +
  
  labs(y = NULL,
       x = NULL,
       title = "Average Daily Dose 1 Vaccinations In California 2021")
plot_vacs_2021

# We simply take a slice from the center of the chart (120 day period) and
#find the maximum point on the smooth line. We will then add our 
#annotations off this reference point.
max_date = vacs_2021_new$date %>% 
  median() + 60
min_date = vacs_2021_new$date %>% 
  median() - 60
label_vals = vacs_2021_smooth %>%
  filter(date > min_date, date < max_date) %>%
  arrange(desc(smooth)) %>%
  top_n(1)

label_vals

# Now we add annotations 
plot_vacs_2021 <- plot_vacs_2021 +
  annotate("text", x = label_vals$date,
           y = label_vals$smooth * 1.3,
           label = "7-day\naverage",
           size = 2.5,
           fontface = "plain", vjust=2) +
  annotate("segment", x = label_vals$date, xend = label_vals$date,
           y = label_vals$smooth,
           yend = label_vals$smooth * 1.13,
           size = 0.25) 
plot_vacs_2021

plotly_vacs_21 <- ggplotly(plot_vacs_2021)
plotly_cases_21 <- ggplotly(plot_2021_cases)

plotly_vacs_21
plotly_cases_21


# creating line graph for cases and vaccinations 
library(ggplot2)

combined <- ggplot() + 
  geom_line(data = covid_2021, aes(x=date, y=cases2, color = 'steelblue' )) + 
  
  geom_line(data = vacs_2021_new, aes(x=date, y=Dose1_new, color = 'coral2')) + 
  
  scale_color_manual(labels = c("COVID Cases", "Vaccinations"), values = c("steelblue", "coral2")) + 
  
  guides(color=guide_legend('Legend')) 

combined 

combined <- combined + 
  theme(
    panel.background = element_blank(),
    axis.text.y = element_text(angle = 0, vjust = -.5,
                               margin = margin(r = -30)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey',
                                      linetype = 'dashed',size = .35),
    axis.ticks.x = element_line(color = "grey"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "grey", linetype = "solid",
                               size = .5)) + 
  labs(x = 'Date', y = 'Average Weekly Total', title = 'COVID Cases and Vaccinations of CA - 2021') 


  
combined


# Dataset for creating interactive line graphs for each county 
# Case count for each day
cases_vacs_2021_new = cases_vacs_2021_new %>%
 
  mutate(Vaccinations = Pop_Vacs - lag(Pop_Vacs,default = 0))



# I am removing counties that had N/A for the data rows 
covid_filtered_gg <- cases_vacs_2021_new[-(3069:21587), ]

summary(vacs_CDC_2021)


# filtering out last date recorded for 2021 to get cumulative total 
vacs_CDC_2021_new <- vacs_CDC_2021[vacs_CDC_2021$date == '2021-12-31', ]

# Now we left join the  new Spatial data of CA to our filtered dataset 
vacs_CDC_2021_new<- counties_pop %>%
  left_join(vacs_CDC_2021_new, counties_pop, by = 'county')

# filtering out columns i want 
vacs_CDC_2021_new <- vacs_CDC_2021_new[, c('county','date', 'Cumm_pop_2021', 'Dose1_Total', 'Dose1_Pop_Pct')]

# joining svi data
vacs_CDC_2021_new<- vacs_CDC_2021_new %>%
  left_join(svi_2020, vacs_CDC_2021_new, by = 'county')

# redoing it 
vacs_CDC_2021_new<- cases_2021 %>%
  left_join(vacs_CDC_2021_new, cases_2021, by = 'county')

# removing unallocated county 
vacs_CDC_2021_new <- vacs_CDC_2021_new[-c(56), ]

# normalize cases by pop
vacs_CDC_2021_new <- vacs_CDC_2021_new %>%
  mutate(cases_per_100k = (cases / Cumm_pop_2021) * 100000)


svi_2020$county <- paste(svi_2020$county, "County") #adds 'County' next to each county name to join the data in earlier lines of code 


# lets create a bubble map 

# first, we categorize the data into intervals 
vacs_CDC_2021_new$SVI_Group <- cut(vacs_CDC_2021_new$svi,     # this cuts the svi data into intervals 
                      breaks = c(0, 0.25, 0.75, 1),
                      labels = c("Least Vulnerable", "50th Percentile", "Most Vulnerable"), 
                      include.lowest = TRUE) # include lowest value (if any)



ggplot(vacs_CDC_2021_new, aes(x = Dose1_Pop_Pct / 100, y = SVI_Group, size = Cumm_pop_2021)) + 
  geom_point(alpha = 0.6, color = 'steelblue') + # transparency of circles w/ alpha 
  # Bubble size range for better visualization
  scale_size_continuous(range = c(.1, 24), name="Total County Population", 
                        labels = label_comma()) + # adds comma to the legend label 
  # x-axis limits and breaks
  scale_x_continuous(breaks = seq(0.20, 0.80, by = 0.05),  # breaks up the x-axis into this interval 
                     labels = scales::percent_format(accuracy = 1)) + # Format x-axis labels as percentages
  scale_y_discrete(labels = c("Least Vulnerable", "", "Most Vulnerable")) +. 
  coord_cartesian(xlim = c(0.20, 0.80)) +. # This ensures values in data outside breaks range are included 
  # labels & title customization 
  labs(
    title = "Vaccination rates by County Social Vulnerability",
    x = "Percentage of Fully Vaccinated Residents Per County in CA",
    y = "Social Vulnerability" # Change the y-axis label
  ) +
  theme_minimal() +
  # this just improves the appearance of the y-axis labels 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))



# lets create a bubble graph adding a 4th dimension: covid cases 

# we need to categorize the covid cases into intervals for the color scheme 
vacs_CDC_2021_new$color_range <- cut(vacs_CDC_2021_new$cases_per_100k,     # this cuts the svi data into intervals 
                                   breaks = c(3000, 6000, 8000, 10000, 14000), 
                                   labels = c("3,000 - 6,000", "6,000 - 8,000", "8,000 - 10,000", " > 10,000"), # this gives the intervals names
                                   include.lowest = TRUE)


# lets calculate average of Pop Percentage w/ vaccine 1 to add the Avg label on the graph
vacs_2021_avg = vacs_CDC_2021_new %>%
  group_by(SVI_Group) %>%
  summarise(avg = mean(Dose1_Pop_Pct/100))



# Ensures SVI_Group is a factor to set it in the order I want on the graph
vacs_CDC_2021_new$SVI_Group <- factor(vacs_CDC_2021_new$SVI_Group,
                                      levels = c("Least Vulnerable", "50th Percentile", "Most Vulnerable"))


# GG Plot of bubble map
ggplot(vacs_CDC_2021_new, aes(x = Dose1_Pop_Pct / 100, y = SVI_Group, size = Cumm_pop_2021, fill = color_range)) +
  geom_point(alpha = 0.8, shape=21) + # Adjust transparency with alpha, shape = 21 creates the outline of the bubbles 
  scale_size_continuous(range = c(2, 40), name="Total County Population", 
                        labels = label_comma()) + 
  scale_x_continuous(breaks = seq(0.40, 0.95, by = 0.05), 
                     labels = scales::percent_format(accuracy = 1)) + 
  scale_y_discrete(labels = c("Least Vulnerable", "", "Most Vulnerable")) + 
  scale_fill_manual(values = c('3,000 - 6,000' = 'gold', '6,000 - 8,000' =  'orange', '8,000 - 10,000' = 'darkorange1', ' > 10,000' = 'orangered'),  name = 'COVID Cases Per 100,000') +
  coord_cartesian(xlim = c(0.40, 0.95)) + 
  labs( 
    x = "Percentage of Fully Vaccinated Residents Per County in CA",
    y = ''
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, size = 10, face="bold", color = 'black'), # edits the text of the y axis 
        axis.text.x = element_text(angle = 0, size = 11)) +   # edits text of x axis 
  # lets add the Avg Annotation we created 
  geom_text(data = vacs_2021_avg,   
            aes( x = avg, y = SVI_Group, 
                label = paste0("Avg: ",scales::percent(avg, accuracy = 1))), # the scales function controls the appearance of axis & legend 
            color = "black", size = 3.7,  # color and size of text
            hjust = 0.7, # horizontal adjustment of annotation 
            vjust = -3.5,  # vertical adjustment of annotation
            inherit.aes = FALSE)    # this makes sure none of the aesthetics from previous layers are inherited  

# data cleaning and filtering 
cases_2021_g <- filter(Covid_CA, between(date, as.Date('2021-01-01'), as.Date('2021-12-31')))


cases_2021 <- cases_2021_g %>%
  group_by(county) %>%
  summarise( cases = sum(New.cases, na.rm=TRUE)
    
  )


#This is just a bubble graph of covid cases that we didnt use 

# Cases per 100,000 by SV 
ggplot(vacs_CDC_2021_new, aes(x = cases_per_100k, y = SVI_Group, size = Cumm_pop_2021)) + 
  
  geom_point(alpha = 0.6, color = 'pink2') + # transparency of circles w/ alpha 
  
  scale_size_continuous(range = c(.1, 24), name="Total County Population", 
                        labels = label_comma()) +
  
  scale_x_continuous(breaks = seq(4000, 11000, by = 1000),
                     labels = scales::label_comma()) + # Format labels as percentages
  
  scale_y_discrete(labels = c("Least Vulnerable", "", "Most Vulnerable")) +
  coord_cartesian(xlim = c(4000, 11000)) +
  labs(
    title = "COVID Cases by County Social Vulnerability",
    x = "COVID Cases Per 100,000 Per County in CA",
    y = "Social Vulnerability" # Change the y-axis label
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))


vacs_CDC_2021_new<- vacs_CDC_2021_new[-c(2,14,22,25,26,32,46,53), ]

# Now we left join the  new Spatial data of CA to our filtered dataset, cant remember why i did this 
vacs2_CDC_2021_new<- ca_counties %>%
  left_join(vacs_CDC_2021_new, ca_counties, by = 'county')





