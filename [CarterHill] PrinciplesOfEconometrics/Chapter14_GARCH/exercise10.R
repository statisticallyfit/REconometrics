setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)




## Part a) 
gold <- read.dta("gold.dta")
autoplot(ts(gold$gold), main="Returns to shares in company specializing in gold")
# data is not normal, lots of volatility



## Part b) hist
ggplot(data=gold, aes(x=gold)) + geom_histogram(fill="gold")
describe(gold$gold)




## Part c) test arch(1) effects
archTest(gold$gold, order.lags = 1)




## Part d) estimate GARCH(1,1)

# (1) stationary?
mean(gold$gold)
res <- dickeyFullerTest(gold$gold, type="none", diffed.lags = 0)
autoplot(acf(res, lag.max = 60, plot = FALSE)) # adding more lags doesn't help

# (2) estimate garch(1,1)
gold.garch <- GARCH(gold, p=1, q=1)





## Part e) how to improve forecasts of returns? With garch in mean
# but why would garch in mean improve them? 
gold.mgarch <- MGARCH11(gold$gold)
# not good model since  the volatility (gamma) term 
# (term on sqrt(ht)) is not significant.

