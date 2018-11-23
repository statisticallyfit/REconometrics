setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
#install.packages("moments")




## Part a)  estimate ARCH(1)
sp <- read.dta("sp.dta") # weekly returns to us s&p 500

# (1) check stationarity
mean(sp$r)
res <- dickeyFullerTest(sp$r, type="drift", diffed.lags = 10)
autoplot(acf(res, lag.max = 40, plot = FALSE))

# (2) estimate arch(1)
arch <- ARCH(sp, p=1)
autoplot(acf(arch@residuals, lag.max = 30, plot = FALSE))
# still some conditional heteroskedasticity in sp$r is left
autoplot(acf(arch@residuals^2, lag.max = 30, plot = FALSE))

describe(sp$r)
ggplot(data=sp, aes(x=r)) + geom_histogram(fill="coral")
autoplot(ts(sp$r), main="Time series")
autoplot(ts(arch@h.t), main="Conditional heteroskedasticity")




## Part b) estimate TARCH

# method 1 - but this looks like ARCH(1) not TARCH(1)
#tarch.spec <- ugarchspec(variance.model = list(model="sGARCH",
#                                               submodel="TGARCH", 
#                                               garchOrder=c(1, 0)), 
#                         mean.model = list(armaOrder=c(0, 0)))
#tarch.fit <- ugarchfit(spec=tarch.spec, data=sp)
#tarch.fit@fit$coef


# method 2 - not same as in book
tarchFit <- garchFit(~aparch(1,0), data=sp, 
                      delta=2, include.delta = F, trace=F)
tarchFit@fit$matcoef





## Part d) compare TARCH and ARCH
tarchFit@fit$matcoef
arch@fit$matcoef
# since the coefficient on the TARCH asymmetric term (gamma1) is 
# significant, then that means the TARCH is better than the ARCH