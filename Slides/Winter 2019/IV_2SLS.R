

library(AER);library(stargazer)
data(mroz, package='wooldridge')

# restrict to non-missing wage observations
oursample <- subset(mroz, !is.na(wage))

myvars <- c("wage", "educ", "exper", "motheduc", "fatheduc")
stargazer(oursample[myvars], summary.stat = c("n", "mean", "sd", "min", "max"))

# OlS
ols <- lm(log(wage)~educ+exper+I(exper^2) 
         +exper+I(exper^2), data=oursample)

# 1st stage: reduced form
stage1 <- lm(educ~exper+I(exper^2)+motheduc+fatheduc, data=oursample)

# 2nd stage
man.2SLS<-lm(log(wage)~fitted(stage1)+exper+I(exper^2), data=oursample)

# Automatic 2SLS estimation
aut.2SLS<-ivreg(log(wage)~educ+exper+I(exper^2) 
                | motheduc+fatheduc+exper+I(exper^2) , data=oursample)

# Pretty regression table
stargazer(ols, stage1,man.2SLS,aut.2SLS,type="text",keep.stat=c("n","rsq"))

stargazer(ols, stage1,man.2SLS,aut.2SLS,type="latex",keep.stat=c("n","rsq"))




