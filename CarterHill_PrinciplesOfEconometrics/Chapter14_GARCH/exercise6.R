setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)



## Part a) 
euro <- read.dta("euro.dta")
returns <- data.frame(r = 100*log(euro$r[2:204] / euro$r[1:203]))
sum(!is.na(returns$r))

autoplot(ts(euro$r))




## Part b) 
## TODO: HELP (??)
# no, it is not normal so that must mean it is the unconditional distribution
ggplot(data=euro, aes(x=r)) + geom_histogram(fill="dodgerblue")
jarque.bera.test(euro$r) # so definitely not normal




## Part c)
archTest(euro$r, order.lags=1)
# so yes there are arch effects



## Part d) estimate ARCH(1) model
arch <- ARCH(euro, p=1)
# the t-statistic on the arch effects is 2.23 which is significant



## Part e) 
autoplot(ts(arch@h.t), main="Conditional variance")
