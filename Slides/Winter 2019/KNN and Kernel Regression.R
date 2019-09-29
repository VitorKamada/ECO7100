rm(list = ls())


library("np")

data("cps71")
OLS <- lm(logwage ~ age + I(age^2), data = cps71)
summary(OLS)

# regtype: type of kernel regression estimator
# lc: local-constant estimator (Nadaraya-Watson)
# ll: local-linear estimator.

# bwmethod: method to use to select bandwidths. 
# cv.aic: expected Kullback-Leibler cross-validation (Hurvich, Simonoff, and Tsai (1998)). 
# cv.ls: least-squares cross-validation. Defaults to cv.ls.

# bwtype: type of bandwidth 
# fixed: compute fixed bandwidths
# generalized_nn: compute generalized nearest neighbors
# adaptive_nn: compute adaptive nearest neighbors

# Adaptive nearest-neighbor bandwidths change
# with each sample realization in the set, xi, when estimating the density at the point x. Generalized
# nearest-neighbor bandwidths change with the point at which the density is estimated, x.

# ckertype: gaussian, epanechnikov, or uniform


NP1 <- npreg(logwage ~ age,
                  regtype = "lc",
                  bwmethod = "cv.aic",
                  bwtype = "fixed",
                  gradients = TRUE,
                  ckertype = "gaussian",
                  data = cps71)

NP2 <- npreg(logwage ~ age,
                  regtype = "ll",
                  bwmethod = "cv.aic",
                  bwtype = "fixed",
                  gradients = TRUE,
                  ckertype = "epanechnikov",
                  data = cps71)

NP3 <- npreg(logwage ~ age,
             regtype = "ll",
             bwmethod = "cv.aic",
             bwtype = "generalized_nn",
             gradients = TRUE,
             ckertype = "epanechnikov",
             data = cps71)


summary(NP1)

npsigtest(NP1)

plot(cps71$age, cps71$logwage, xlab = "age", ylab = "log(wage)", cex=.1)
lines(cps71$age, fitted(NP1), lty = 1, col = "blue")
lines(cps71$age, fitted(OLS), lty = 2, col = " red")


plot(NP1, plot.errors.method = "asymptotic")

plot(NP1, gradients = TRUE)

lines(cps71$age, coef(OLS)[2]+2*cps71$age*coef(OLS)[3],
         lty = 2,
         col = "red")
plot(NP1, gradients = TRUE, plot.errors.method = "asymptotic")



summary(NP2)

npsigtest(NP2)

plot(cps71$age, cps71$logwage, xlab = "age", ylab = "log(wage)", cex=.1)
lines(cps71$age, fitted(NP2), lty = 1, col = "blue")
lines(cps71$age, fitted(OLS), lty = 2, col = " red")
plot(NP2, plot.errors.method = "asymptotic")
plot(NP2, gradients = TRUE)
lines(cps71$age, coef(OLS)[2]+2*cps71$age*coef(OLS)[3],
      lty = 2,
      col = "red")
plot(NP2, gradients = TRUE, plot.errors.method = "asymptotic")


summary(NP3)

npsigtest(NP3)

plot(cps71$age, cps71$logwage, xlab = "age", ylab = "log(wage)", cex=.1)
lines(cps71$age, fitted(NP3), lty = 1, col = "blue")
lines(cps71$age, fitted(OLS), lty = 2, col = " red")


plot(NP3, plot.errors.method = "asymptotic")
plot(NP3, gradients = TRUE)
lines(cps71$age, coef(OLS)[2]+2*cps71$age*coef(OLS)[3],
      lty = 2,
      col = "red")
plot(NP3, gradients = TRUE, plot.errors.method = "asymptotic")





rm(list = ls())

library(ISLR); library(class); library(MASS); library(FNN)


X_boston = Boston["lstat"]
y_boston = Boston$medv

lstat_grid = data.frame(lstat = seq(range(X_boston$lstat)[1],
                                    range(X_boston$lstat)[2], 
                                    by = 0.01))

pred_001 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 1)
pred_005 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 5)
pred_010 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 10)
pred_050 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 50)
pred_100 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 100)
pred_506 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 506)



plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 1")
lines(lstat_grid$lstat, pred_001$pred, col = "darkorange", lwd = 0.25)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 5")
lines(lstat_grid$lstat, pred_005$pred, col = "darkorange", lwd = 0.75)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 10")
lines(lstat_grid$lstat, pred_010$pred, col = "darkorange", lwd = 1)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 25")
lines(lstat_grid$lstat, pred_050$pred, col = "darkorange", lwd = 1.5)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 50")
lines(lstat_grid$lstat, pred_100$pred, col = "darkorange", lwd = 2)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 506")
lines(lstat_grid$lstat, pred_506$pred, col = "darkorange", lwd = 2)
