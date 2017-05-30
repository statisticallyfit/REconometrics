setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)




## Part a)  estimate garch(1,1) and arch(5)
warner <- read.dta("warner.dta")
w <- warner$warner

# (1) is stationarity? 
mean(w)
# just one lag is significant when diffed.lags = 10 so use 1 lag
res <- dickeyFullerTest(w, type="none", diffed.lags = 1)
autoplot(acf(res, lag.max = 260, plot = FALSE))

# (2) GARCH(1,1)
warner.garch <- GARCH(warner, p=1, q=1)
autoplot(acf(warner.garch@residuals, lag.max = 30, plot = FALSE))
autoplot(acf(warner.garch@residuals^2, lag.max = 30, plot = FALSE))
# still cond.heter left - why? increasing lags doesn't help...

# (2) ARCH(5)
warner.arch5 <- ARCH(warner, p=5)
autoplot(acf(warner.arch5@residuals, lag.max = 30, plot = FALSE))
autoplot(acf(warner.arch5@residuals^2, lag.max = 30, plot = FALSE))
# still cond.heter left - why? increasing lags doesn't help...\




## Part c) forecast
df <- data.frame(ht=warner.garch@h.t, et=warner.garch@residuals)
tail(df)




## Part d) - estimate TARCH-in-mean