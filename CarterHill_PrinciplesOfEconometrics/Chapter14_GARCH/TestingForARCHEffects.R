setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(FinTS)
#install.packages("FinTS")


byd <- read.dta("byd.dta")
head(byd)
# time series
autoplot(ts(byd))
# histogram
ggplot(data=byd, aes(x=r)) + geom_histogram(fill="purple")
ggplot(data=byd, aes(x=r)) + geom_density(lwd=1)
ggplot(data=byd, aes(x=r)) + 
      geom_histogram(aes(y=..density..), 
                     lwd=1, colour="grey48", fill="white") +
      geom_density(alpha=0.2, colour="grey48", fill="pink", lwd=1)



## Arch test
# This answers the question: Do we need ARCH model? 
archEffectsTest(byd$r, lags=1)

