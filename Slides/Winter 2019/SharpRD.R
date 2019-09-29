



rm(list=ls())

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)


data = read.dta("https://github.com/VitorKamada/ECO7100/raw/master/Data/CIT_2018_Cambridge_polecon.dta")
Y = data$Y
X = data$X
T = data$T
T_X = T*X

ols = lm(Y ~ X + T + T_X)
summary(ols)

out = lm(Y[X >= -20 & X <= 20] ~ X[X >= -20 & X
        <= 20] + T[X >= -20 & X <= 20] + T_X[X >= -20 &  X <= 20])
summary(out)


