setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
#install.packages("moments")




## Part a) 
exrate <- read.dta("exrate.dta")
autoplot(ts(exrate$s), main="Change of US/AUS Exchange Rate", 
         ts.size=1, colour="blue")
ggplot(data=exrate, aes(x=s)) + 
      geom_histogram(fill="red") +
      ggtitle("Change of US/AUS Exchange Rate")

describe(exrate$s)




## Part b) estimate GARCH(1,1)

# (1) first test for stationarity => IS STATIONARY 
mean(exrate$s) #so it seems to use a drift (constant), just barely
res <- dickeyFullerTest(exrate$s, type="drift", diffed.lags = 0)
autoplot(acf(res, lag.max = 20, plot=FALSE))


# (2) second  estimate GARCH(1,1) 
# ****** TODO - why are these different than in question? 

# method 1 - garchFit package
# Very significant coefficient for lagged variance in variance function. 
garch <- GARCH(exrate, p=1, q=1)

# look at residuals of garch to see if good fit - pretty good
autoplot(acf(garch@residuals, lag.max = 20, plot = FALSE))
# any conditional heteroskedasticity left? - yes, not good, but 
# 5% of time it happens by chance so we might assume this one 
# happened by chance.
autoplot(acf(garch@residuals^2, lag.max = 20, plot = FALSE))


# method 2 - rugarch package
garch.spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
garch.fit <- ugarchfit(spec=garch.spec, data=exrate)
garch.fit@fit$coef
autoplot(acf(garch.fit@fit$residuals, lag.max = 20, plot=FALSE))
autoplot(acf(garch.fit@fit$residuals^2, lag.max = 20, plot=FALSE))





## Part c) 