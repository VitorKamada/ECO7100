

# CLT Example

rm(list = ls()) 
set.seed(7)

pHat <- rbinom(10^4, 62, 0.1)/62
hist(pHat, breaks=seq(-0.1,0.3,0.01))
sum(pHat<0.048)/10^4



# Delta
set.seed(7) 
x=rnorm(1000)*3+5
x2=x^2
var(x2)
sd(x2)


delta=2*5*9*2*5
delta
sqrt(delta)