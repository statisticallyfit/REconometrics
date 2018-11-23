setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)



## Part a) compute time series and find ACF - autocorrelated? 
# IF so, then it indicates lagged mean effects
# TOOD: what does lagged mean effects mean? 
shares <- read.dta("share.dta")
returns <- data.frame(r = 100*log(shares$y[2:204] / shares$y[1:203]))
head(returns)

# none found
autoplot(acf(returns, lag.max = 20, plot=FALSE))





## Part b) square returns and find ACF. If autocorrelation, then
# it shows there are significant lagged variance effects
# TODO: what does this mean? 
autoplot(acf(returns^2, lag.max = 20, plot=FALSE))
# yes, there are lagged variance effects