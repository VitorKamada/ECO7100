
# Clear All
rm(list = ls())

# Fair coin example
set.seed(7)
n <- 10000
x <- sample(0:1, n, repl=T)
s <- cumsum(x); p <- s/(1:n)
plot(p, ylim=c(.4, .6), type="l")
lines(c(0,n), c(.5,.5), col = "red")
round(cbind(x,s,p), 3)[1:10,]; p[n]



# Die example
rm(list = ls())
set.seed(7)

die <- 1:6
roll <- function(n) {
  mean(sample(die, size = n, replace = TRUE))
}

plot(sapply(1:1000, roll), type = "l",
     xlab = "# of dice", ylab = "average")
abline(h = 3.5, col = "red")
