# Pearson correlation coefficient and p-value


# Tests for correlation for vaccinations

cor.test(vax_2021_clean$rems, vax_2021_clean$vacs_100k) # high p-value 0.29
# cor: 0.150797

cor.test(vax_2021_clean$hhc, vax_2021_clean$vacs_100k) # low p-value: 0.002204
# cor: -0.4231003



# Tests for correlation for cases

cor.test(cases_p_100k_svi$rems, cases_p_100k_svi$sum_cases_p100k) # low p-value 1.077e-07
# cor: 0.6313289

cor.test(cases_p_100k_svi$htt, cases_p_100k_svi$sum_cases_p100k) # low p-value 0.0001265
# cor: 0.4822368