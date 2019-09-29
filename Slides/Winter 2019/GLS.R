

rm(list = ls())


library(foreign); library(lmtest); library(car); library(stargazer)
d401k<-read.dta("https://github.com/VitorKamada/ECO7100/raw/master/Data/401ksubs.dta")

summary(d401k)

stargazer(d401k)



# OLS
OLS <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k, 
          data=d401k, subset=(fsize==1))

OLSRobRef <- coeftest(OLS,hccm)   # Refine White's Robust SE

# Stata use the classical version White's Robust SE
OLSRob <-  coeftest(OLS, vcov=hccm(OLS, type="hc0")) 


                
# WLS
wlsreg <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k, 
             weight=1/inc, data=d401k, subset=(fsize==1))

WLS <- coeftest(wlsreg)

# robust results (Refined White SE:)
WLSRob <- coeftest(wlsreTg,hccm)





library(stargazer)

stargazer(OLSRob,OLSRobRef,WLS,WLSRob,
                    title="Regression Results",
          dep.var.labels="Net Financial Wealth",
          column.labels=c("OLSRob","OLSRobRef","WLS","WLSRob"),
          no.space=TRUE,     
          column.sep.width = "1pt",
          omit.stat=c("ser","f"))



