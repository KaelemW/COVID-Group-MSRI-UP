covid_county <- read.csv('~/Creative Cloud Files/MSRI-UP/Weekly_United_States_COVID-19_Cases_and_Deaths_by_County_-_ARCHIVED_20250702.csv')

covid_CA <- filter(covid_county, state == 'CA')

Covid_CA = subset(covid_CA, select = -c(fips_code, state_fips, cumulative_deaths, New.deaths) )

 #write_csv(Covid_CA, 'Covid_CA.csv')
