covid_county <- read.csv('~/Creative Cloud Files/MSRI-UP/Weekly_United_States_COVID-19_Cases_and_Deaths_by_County_-_ARCHIVED_20250702.csv')

covid_CA <- filter(covid_county, state == 'CA')

Covid_CA = subset(covid_CA, select = -c(fips_code, state_fips, cumulative_deaths, New.deaths) )

 #write_csv(Covid_CA, 'Covid_CA.csv')

# Convert date column from character to date format
# We need this in order to filter the data by date 
Covid_CA <- Covid_CA %>%
  mutate(date = as.Date(date, format='%m/%d/%Y'))

# Now, we filter out data to year 2021
 covid_cases_2021 <- filter(Covid_CA, between(date, as.Date('2021-01-01'), 
                                              as.Date('2021-12-31')))
