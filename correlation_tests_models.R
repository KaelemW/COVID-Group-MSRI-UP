# Pearson correlation coefficient and p-value
# Pearson's product moment correlation coefficient
library(gtExtras)


# Tests for correlation for vaccinations

# Variables in our model of vaccinations:
cor.test(vax_2021_clean$rems, vax_2021_clean$vacs_100k) # high p-value 0.29
# cor: 0.150797

cor.test(vax_2021_clean$hhc, vax_2021_clean$vacs_100k) # low p-value: 0.002204
# cor: -0.4231003

#Variables not in our model of vaccinations:
cor.test(vax_2021_clean$ses, vax_2021_clean$vacs_100k) # low p-value: 0.009749
# cor: -0.3621765
cor.test(vax_2021_clean$htt, vax_2021_clean$vacs_100k) # high p-value: 0.6
# cor: 0.07628

cor.test(vax_2021_clean$svi, vax_2021_clean$vacs_100k) # low p-value: 0.03367
# cor: -0.3009942




# Tests for correlation for cases

#Variables in our model of cases:
cor.test(cases_p_100k_svi$rems, cases_p_100k_svi$sum_cases_p100k) # low p-value 1.077e-07
# cor: 0.6313289

cor.test(cases_p_100k_svi$htt, cases_p_100k_svi$sum_cases_p100k) # low p-value 0.0001265
# cor: 0.4822368

# Variables not in our model of cases:
cor.test(cases_p_100k_svi$ses, cases_p_100k_svi$sum_cases_p100k) # low p-value: 0.00007289
# cor: 0.4311109
cor.test(cases_p_100k_svi$hhc, cases_p_100k_svi$sum_cases_p100k) # low p-value: 0.01243
# cor: 0.3262856

cor.test(cases_p_100k_svi$svi, cases_p_100k_svi$sum_cases_p100k) # low p-value: 1.79e-05
# cor: 0.53109




# Table Summarizing Correlations
# for cases

correlations <- matrix(c('Social Vulnerability Index', 0.5311, 0.0000179,
                         'Racial & Ethnic Minority Status', 0.6313, 0.0000001077,
                         'Housing Type and Transportation', 0.4822, 0.0001265,
                         'Household Characteristics', 0.3263, 0.01243,
                         'Socioeconomic Status', 0.4311, 0.00007289),
                       nrow = 5, byrow = TRUE)
colnames(correlations) <- c('Variable', 'Correlation', 'p-value')

cor_tib <- as_tibble(correlations)


cor_tib$`p-value` <- as.numeric(cor_tib$`p-value`)
color_palette <- c("darkgreen", "white")

cor_tib |>
  gt(rowname_col = 'Variable') |>
  tab_header(
    title = md('**Correlations with Covid-19 Cases**')
  ) |>
  gt_theme_538() |>
  gt_color_rows(
    columns = 3, 
    domain = c(0, 0.05),
    palette = color_palette
  )


# for vax
correlations <- matrix(c('Social Vulnerability Index', -0.3010, 0.03367,
                         'Racial & Ethnic Minority Status', 0.1508, 0.29,
                         'Housing Type & Transportation', 0.0763, 0.6,
                         'Household Characteristics', -0.4231, 0.002204,
                         'Socioeconomic Status', -0.3622, 0.009749),
                       nrow = 5, byrow = TRUE)
colnames(correlations) <- c('Variable', 'Correlation', 'p-value')
# change column name for p-value


cor_tib <- as_tibble(correlations)


cor_tib$`p-value` <- as.numeric(cor_tib$`p-value`)
color_palette <- c("darkgreen", "white")

cor_tib |>
  gt(rowname_col = 'Variable') |>
  tab_header(
    title = md('**Correlations with Covid-19 Vaccinations**')
  ) |>
  gt_theme_538() |>
  gt_color_rows(
    columns = 3, 
    domain = c(0, 0.05),
    palette = color_palette
  )
