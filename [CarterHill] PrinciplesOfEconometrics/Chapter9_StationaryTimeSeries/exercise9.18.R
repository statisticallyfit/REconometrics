setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
sales <- read.dta("ex9_13.dta")
s <- sales$sales
salesNA_2 <- data.frame(S=s, S_1=c(NA, s[1:156]), S_2=c(NA, NA, s[1:155]))
head(salesNA_2); tail(salesNA_2)

salesAR2.lm <- lm(data=salesNA_2, S ~ S_1 + S_2)
summary(salesAR2.lm)

# Correlogram shows now sign of autocorrelation of errors
acfs <- acf(salesAR2.lm$residuals, plot=FALSE); acfs
autoplot(acfs[2:21])



## Part b)  
salesNA_cutoff <- salesNA_2[1:153, ]
head(salesNA_cutoff); tail(salesNA_cutoff)
tail(salesNA_2)

salesAR2.cutoff.lm <- lm(data=salesNA_cutoff, S ~ S_1 + S_2)
summary(salesAR2.cutoff.lm)
autoplot(ts(sales$sales))
