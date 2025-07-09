# We have the general model for vaccinations
vacs_model

# and the general model for cases
cases_model

# Plot for cases model
predicted <- data.frame(rems = seq(0, 1, length.out = 100))
predicted$New.cases <- predict(cases_model, predicted)

p <- ggplot(may_26, aes(x = rems, y = New.cases)) +
  geom_point() +
  theme_bw() +
  ggtitle('New Cases by Racial and \nEthnic Minority Status') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('New Cases in a Week')+
  labs(subtitle = 'May 26, 2021')
p + geom_line(data = predicted, color = 'blue')



# Plot of Vac by REMS, color is HHC
ggplot(may_26, aes(x = rems, y = Pop_Vacs_Pct, color = hhc)) +
  geom_point() +
  theme_bw() +
  ggtitle('Vaccinated Population Percentage by REMS and HHC') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('Percentage of Pop. with a COVID Vaccine')+
  labs(subtitle = 'May 26, 2021', color = "HHC") +
  stat_smooth(method="lm",se = FALSE, color = "red")

# Plot of Vac by HHC, color is REMS
ggplot(may_26, aes(x = hhc, y = Pop_Vacs_Pct, color = rems)) +
  geom_point() +
  theme_bw() +
  ggtitle('Vaccinated Population Percentage by REMS and HHC') +
  xlab('Household Characteristics Percentile') +
  ylab('Percentage of Pop. with a COVID Vaccine')+
  labs(subtitle = 'May 26, 2021', color = "REMS") +
  stat_smooth(method="lm",se = FALSE, color = "red") 

# a way to improve is to use binary instead of continuous to visualize the color
# so maybe like top 75th+ percentile is one color


# make dots multicolored, chnage color gradient 
