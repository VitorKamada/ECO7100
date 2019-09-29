


library(AER)
library(plm)
library(stargazer)

data(Fatalities)
attach(Fatalities)


# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000


Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")


fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")




# plot the observations and add the estimated regression line for 1982 data
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5)



plot(x = Fatalities1988$beertax, 
     y = Fatalities1988$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1988_mod, lwd = 1.5)



# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")




# plot the differenced data
plot(x = diff_beertax, 
     y = diff_fatal_rate, 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5)



# Manually estimate the fixed effects
fe_Dummy <- lm(fatal_rate ~ beertax + state -1 , data = Fatalities)

coeftest(fe_Dummy, vcov = vcovHC, type = "HC1")




# estimate the fixed effects regression with plm()
FE_TD <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")

# print summary using robust standard errors
coeftest(FE_TD, vcov. = vcovHC, type = "HC1")



# estimate the random effects regression with plm()
RE <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "random")

# print summary using robust standard errors
summary(RE, vcov. = vcovHC, type = "HC1")


# estimate a combined time and entity fixed effects regression model

# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_tefe_lm_mod


# via plm()
FETE <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

# obtain a summary based on clusterd standard errors 
# (adjustment for autocorrelation + heteroskedasticity)
coeftest(FETE, vcov = vcovHC, type = "HC1")


