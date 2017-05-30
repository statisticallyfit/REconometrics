library(tseries)
library(ggfortify)


# SImulate GARCH(1,1)
set.seed(2)
a0 <- 0.2
a1 <- 0.5
b1 <- 0.3
w <- rnorm(10000)
eps <- rep(0, 10000)
var <- rep(0, 10000)
for (i in 2:10000){
      var[i] <- a0 + a1 * (eps[i-1]^2) + b1 * var[i-1]
      eps[i] <- w[i] * sqrt(var[i])
}

# Decide if GARCH is indeed present (have we simulated well? )
autoplot(acf(eps, plot = FALSE)) 
autoplot(acf(eps^2, plot = FALSE)) # yes good since cond.heter in errors


# Now recover the parameters
eps.garch <- garch(eps, trace=FALSE)
eps.garch
confint(eps.garch)
