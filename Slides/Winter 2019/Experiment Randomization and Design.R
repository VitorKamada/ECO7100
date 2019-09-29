# Clear All
rm(list = ls()) 

Stitch <- read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl2.1", header=TRUE)

Stitch$d <- Stitch$std - Stitch$ergo
Stitch
summary(Stitch$d)

#Latex code
library(stargazer)
stargazer(Stitch)
#_________________

stem(Stitch$d)
hist(Stitch$d, freq=FALSE, col="red")

d <- Stitch$std - Stitch$ergo
G1 <- d[1:10]
G2 <- d[11:20]
G3 <- d[21:30]

boxplot(G1, G2, G3)


#Paired T-Test
t.test(d,alt="great")
t.test(G1,alt="great")
t.test(G2,alt="great")
t.test(G3,alt="great")

with(Stitch, t.test(std, ergo, paired=TRUE, alternative="greater"))

#########################################
# Two-Sample t-Test
# Clear All
rm(list = ls()) 

Data <- read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl2.2", header=TRUE)
Data

summary(Data)

library(psych)
describe.by(Data$y, Data$days)

t.test(Data$y~Data$days, var.equal=TRUE, alternative="less")
