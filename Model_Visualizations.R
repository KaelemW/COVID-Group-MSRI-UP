# We have the general model for vaccinations
vacs_model

# and the general model for cases
cases_model

# Plot for cases model
predicted <- data.frame(rems = seq(0, 1, length.out = 100))
predicted$New.cases <- predict(cases_model, predicted)

p <- ggplot(may_26, aes(x = rems, y = New.cases)) +
  geom_point() +
  ggtitle('New Cases by Racial and \nEthnic Minority Status') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('New Cases in a Week')+
  labs(subtitle = 'May 26, 2021') +
  theme_grey()
p + geom_line(data = predicted, color = 'red')



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

# Plots where high risk is is highlighted

may_26 <- may_26 |>
  mutate(hbin = (hhc >= 0.75), rbin = (rems >= 0.5))

ggplot(may_26, aes(x = rems, y = Pop_Vacs_Pct, color = hbin)) +
  geom_point() +
  theme_bw() +
  ggtitle('Vaccinated Population Percentage by REMS and HHC') +
  xlab('Racial and Ethnic Minority Status Percentile') +
  ylab('Percentage of Pop. with a COVID Vaccine')+
  labs(subtitle = 'May 26, 2021', color = "HHC") +
  stat_smooth(method="lm",se = FALSE, color = "red")



# I like this plot the best

ggplot(may_26[-c(60,34),], aes(x = hhc, y = Pop_Vacs_Pct, color = rbin)) +
  geom_point() +
  ggtitle('Vaccinated Population Percentage by REMS and HHC') +
  xlab('Household Characteristics Percentile') +
  ylab('Percentage of Pop. with a COVID Vaccine')+
  labs(subtitle = 'May 26, 2021', color = "Percentile of REMS") +
  stat_smooth(method="lm",se = FALSE, color = "black", size = .7) +
  scale_color_manual(limits = c(TRUE, FALSE),
    values = c("#FFCC00","#336600"), 
                     labels = c('Above 50th Percentile', "Below 50th Percentile")) +
  theme_gray()


summary(vacs_model)
anova(vacs_model)

# Making nice table

library(gt)


vac_table <- matrix(c('Intercept', 39.671, 5.200, 7.629, 3.52e-10,
             'rems', 33.069, 8.056, 4.105, 0.000135,
             'hhc', -29.284, 8.058, -3.634, 0.000615),
           nrow = 3, byrow = TRUE)
colnames(vac_table) <- c('Independent Variable', "Estimate", 'Std. Error', "t value", "Pr(>|t|)")

vac_tib <- as_tibble(vac_table)

vac_tib |>
  gt(rowname_col = 'Independent Variable') |>
  tab_header(
    title = md('**Vaccination Model Summary**')
  ) |>
  tab_source_note(
    source_note = "Multiple R-squared: 0.2941"
  ) |> 
  tab_source_note(
    source_note = 'F-statistic: 11.46 on 2 and 55 DF'
  ) |> 
  tab_source_note(
    source_note = 'p-value: 6.926e-05'
  )


# nice table for cases model

cases_table <- matrix(c('Intercept', -23.80, 54.38, -0.438, 0.663314,
         'rems', 348.18, 94.10, 3.700, 0.000493),
         nrow = 2, byrow = TRUE)
colnames(cases_table) <- c('Independent Variable', "Estimate", 'Std. Error', "t value", "Pr(>|t|)")

cases_tib <- as_tibble(cases_table)

cases_tib |>
  gt(rowname_col = 'Independent Variable') |>
  tab_header(
    title = md('**COVID Cases Model Summary**')
  ) |>
  tab_source_note(
    source_note = "Multiple R-squared: 0.1965"
  ) |> 
  tab_source_note(
    source_note = 'F-statistic: 13.69 on 1 and 56 DF'
  ) |> 
  tab_source_note(
    source_note = 'p-value: 0.0004929'
  )
