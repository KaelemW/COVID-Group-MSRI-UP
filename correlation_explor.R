library(corrplot)

# Correlation plot for data from May 26, 2021
may_26 <- covid_2021 |>
  filter(Date == '2021-05-26')

# i selected only the numerical columns so i can find the correlation between them
cor_may26 <- may_26 |>
  select(c(4:12)) |>
  rename(new_cases = New.cases, num_vac = Pop_Vacs, pct_vac = Pop_Vacs_Pct,
         cumul_cases = cumulative_cases) |>
  cor(use="complete.obs")

corrplot(cor_may26)


#hhc and pop_vacs_pct have some negative correlation
# rems and new.cases &&& rems and pop_vacs have some positive correlation

pairs(may_26 |>
        select(c(4:12)))


#Model relating hhc and rems to Pop_Vacs_Pct
mod <- lm(Pop_Vacs_Pct~hhc+rems, data = may_26)
summary(mod)
plot(mod)
anova(mod)

# Model selection for Pop_Vacs_pct
require(leaps)
bestss <- regsubsets(Pop_Vacs_Pct~svi+ses+hhc+rems+htt, data = may_26)
summary(bestss)

mod1 <- lm(Pop_Vacs_Pct~rems, may_26)
BIC(mod1) # 515
AIC(mod1) # 509

#best model for predicting Pop_vacs_pct
vacs_model <- lm(Pop_Vacs_Pct~rems+hhc, may_26)
BIC(mod2) # 506
AIC(mod2) # 498

# measure variance inflation factor for vacs_model multilinear regression
require(car)
vif(vacs_model)

mod3 <- lm(Pop_Vacs_Pct~rems+hhc+ses, may_26)
BIC(mod3) # 510
AIC(mod3) # 500

mod4 <- lm(Pop_Vacs_Pct~rems+hhc+ses+svi, may_26)
BIC(mod4) # 512

mod5 <- lm(Pop_Vacs_Pct~rems+hhc+ses+svi+htt, may_26)
BIC(mod5) # 516



#Model relating race to new cases
mod <- lm(New.cases~rems, data = may_26)
summary(mod)
plot(mod)
anova(mod)

require(leaps)
bestss2 <- regsubsets(New.cases~svi+ses+hhc+rems+htt, data = may_26)
summary(bestss2)

# best model
cases_model <- lm(New.cases~rems, may_26)
BIC(mod1) # 795
AIC(mod1) # 788

mod2 <- lm(New.cases~rems+hhc, may_26)
BIC(mod2) # 797
AIC(mod2) # 789

mod3 <- lm(New.cases~rems+hhc+ses, may_26)
BIC(mod3) # 798
AIC(mod3) # 788



# More Advanced Models




# Now using svi data of race and hhc

covid_race_hhc |>
  filter(Date == '2021-05-26') |>
  select(c(4:29)) |>
  cor(use="complete.obs") |>
  corrplot()



# Predicting vaccination rates by race

bestss_r <- regsubsets(Pop_Vacs_Pct~white_pct + black_pct + latino_pct + 
                          native_pct + asian_pct + pacisl_pct + other_pct + multi_pct, 
                        data = covid_race_hhc)
summary(bestss_r)

mod1 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~native_pct))
summary(mod1)
BIC(mod1) #319575

mod2 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~native_pct+asian_pct))
summary(mod2)
BIC(mod2) #317376

mod3 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~native_pct+white_pct+multi_pct))
summary(mod3)
BIC(mod3) #315516

mod4 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~white_pct+multi_pct+latino_pct+asian_pct))
summary(mod4)
BIC(mod4) #315100

mod5 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~multi_pct+latino_pct+asian_pct+native_pct+other_pct))
summary(mod5) # R-squared of 0.2243
BIC(mod5) # 314785

# BEST MODEL FOR PREDICTING VACCINATION FROM JUST RACE
mod6 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~white_pct+black_pct+latino_pct+asian_pct+other_pct+multi_pct))
summary(mod6) #r-squared of 0.2283
BIC(mod6) # 314623


mod7 <- with(covid_race_hhc, lm(Pop_Vacs_Pct~white_pct+black_pct+latino_pct+
                                  asian_pct+other_pct+native_pct+pacisl_pct))
summary(mod7) 
BIC(mod7) # 314628

#maybe i should make simple linear models for each race



# ALSO MODEL PREDICTING BASED OFF OF HHC AND RACE

covid_race_hhc_may <- covid_race_hhc |>
  filter(Date == '2021-05-26')




# Predicting cases by race


bestss_rh <- regsubsets(new_cases_pc~white_pct + black_pct + latino_pct + 
                          native_pct + asian_pct + pacisl_pct + other_pct + multi_pct, 
                        data = covid_race_hhc_may)
summary(bestss_rh)

mod1 <- with(covid_race_hhc_may, lm(new_cases_pc~white_pct))
summary(mod1)
BIC(mod1) 

mod2 <- with(covid_race_hhc_may, lm(new_cases_pc~asian_pct+multi_pct))
summary(mod2)
BIC(mod2) 

mod3 <- with(covid_race_hhc_may, lm(new_cases_pc~asian_pct+multi_pct+other_pct))
summary(mod3) #
BIC(mod3) 

mod4 <- with(covid_race_hhc_may, lm(new_cases_pc~multi_pct+white_pct+other_pct+latino_pct))
summary(mod4)
BIC(mod4) 

mod5 <- with(covid_race_hhc_may, lm(new_cases_pc~white_pct+latino_pct+pacisl_pct+multi_pct+other_pct))
summary(mod5) # r-sq 0.2218
BIC(mod5) 

mod6 <- with(covid_race_hhc_may, lm(new_cases_pc~white_pct+latino_pct+pacisl_pct+multi_pct+other_pct+asian_pct))
summary(mod6) # r-sq 0.2223
BIC(mod6) 


mod7 <- with(covid_race_hhc_may, lm(new_cases_pc~black_pct+asian_pct+pacisl_pct+
                                  white_pct + latino_pct + other_pct + native_pct))
summary(mod7) # r-sq 0.2223
BIC(mod7)

mod8 <-with(covid_race_hhc_may, lm(new_cases_pc~black_pct+asian_pct+pacisl_pct+
                                              white_pct + latino_pct + other_pct + native_pct+multi_pct))
summary(mod8)
BIC(mod8)


