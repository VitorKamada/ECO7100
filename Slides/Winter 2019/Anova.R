
# Clear All
rm(list = ls()) 

resin <- read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl3.2", header=TRUE)

# I recoded based on Textbook
resin$temp[resin$temp==1] <- 175
resin$temp[resin$temp==2] <- 194
resin$temp[resin$temp==3] <- 213
resin$temp[resin$temp==4] <- 231
resin$temp[resin$temp==5] <- 250


# No need to type "resin$" all the time
attach(resin)

summary(resin)

library(stargazer)
stargazer(resin)

boxplot(y~temp)

Dummy <- with(resin,as.factor(temp))
Result <- lm(y~Dummy)
anova(Result)
summary(Result)



# Predicted values are the treatment means
yhat <- predict(Result);  alpha <- yhat - 1.465
Residuals <- resid(Result);  boxplot(alpha, Residuals)



# Polynomial Model
p0 <- lm(y~1)
p1 <- lm(y~temp)
p2 <- lm(y~temp+I(temp^2))
p3 <- lm(y~temp+I(temp^2)+I(temp^3))
p4 <- lm(y~temp+I(temp^2)+I(temp^3)+I(temp^4))

anova(p1,p2,p3,p4)

summary(p2)
summary(p4)


stargazer(p1,p2,p3,p4, 
          omit.stat=c("ser","f"),
          type="text", out="Reg.txt")

stargazer(p1,p2,p3,p4, 
          title="Regression Results",
          dep.var.labels="Lifetime (in hours)",
          no.space=TRUE,      
          column.sep.width = "1pt",
          omit.stat=c("ser","f"),
          type="text", out="Reg.txt")

stargazer(p1,p2,p3,p4,
          title="Regression Results",
          dep.var.labels="Lifetime (in hours)",
          no.space=TRUE,     
          column.sep.width = "1pt",
          omit.stat=c("ser","f"))
