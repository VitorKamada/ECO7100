rm(list = ls())

library(stargazer); library(foreign);library(car); library(lmtest)  # for robust SE

library(AER);
data(mroz, package='wooldridge')


stargazer(mroz[c(1,3:7,20:21 )])

# Estimate linear probability model
linprob <- lm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,data=mroz)
# Regression table with heteroscedasticity-robust SE and t tests:
coeftest(linprob,vcov=hccm)


# Estimate logit model
logitres<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,
              family=binomial(link=logit),data=mroz)
# Summary of results:
summary(logitres)
# Log likelihood value:
logLik(logitres) 
# McFadden's pseudo R2:
1 - logitres$deviance/logitres$null.deviance


# Estimate probit model
probitres<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,
               family=binomial(link=probit),data=mroz)
# Summary of results:
summary(probitres)
# Log likelihood value:
logLik(probitres) 
# McFadden's pseudo R2:
1 - probitres$deviance/probitres$null.deviance


stargazer(linprob,logitres,probitres, 
          omit.stat=c("ser","f"),
          type="text", out="Reg.txt")

################################################################
# Test of overall significance:
# Manual calculation of the LR test statistic:
probitres$null.deviance - probitres$deviance

# Automatic calculations including p-values,...:
library(lmtest)
lrtest(probitres)

################################################################
# Test of H0: experience and age are irrelevant
restr <- glm(inlf~nwifeinc+educ+ kidslt6+kidsge6, 
             family=binomial(link=logit),data=mroz)
lrtest(restr,probitres)
lrtest(probitres,restr)

# predictions for two "extreme" women (run Example-17-1-1.R first!):
xpred <- list(nwifeinc=c(100,0),educ=c(5,17),exper=c(0,30),
              age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))
predict(linprob,xpred)

# Predictions from linear probability, probit and logit model:
predict(linprob,  xpred,type = "response")
predict(logitres, xpred,type = "response")
predict(probitres,xpred,type = "response")



# Automatic APE calculations with package mfx
library(mfx)
logitmfx(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6, 
         data=mroz, atmean=FALSE)

logitmfx(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6, 
         data=mroz, atmean=TRUE)














# Simulated data 
set.seed(8237445)
y <- rbinom(100,1,0.5)
x <- rnorm(100) + 2*y

# Estimation
linpr.res <-  lm(y~x)
logit.res <- glm(y~x,family=binomial(link=logit))
probit.res<- glm(y~x,family=binomial(link=probit))

# Prediction for regular grid of x values
xp <- seq(from=min(x),to=max(x),length=50)
linpr.p <- predict( linpr.res, list(x=xp), type="response" )
logit.p <- predict( logit.res, list(x=xp), type="response" )
probit.p<- predict( probit.res,list(x=xp), type="response" )

# Graph
plot(x,y)
lines(xp,linpr.p, lwd=2,lty=1)
lines(xp,logit.p, lwd=2,lty=2)
lines(xp,probit.p,lwd=1,lty=1)
legend("topleft",c("linear prob.","logit","probit"),
       lwd=c(2,2,1),lty=c(1,2,1))












