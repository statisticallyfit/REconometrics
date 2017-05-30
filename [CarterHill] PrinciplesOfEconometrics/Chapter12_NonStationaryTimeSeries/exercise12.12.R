setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
library(foreign)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) 
i <- read.dta("inter2.dta"); i <- i$y
di <- c(NA, diff(i))
di_1 <- c(NA, di[1:299])
di_2 <- c(NA, NA, di[1:298])
ddi <- c(NA, diff(di))
dddi <- c(NA, diff(ddi))

t <- seq(1, 300)

autoplot(ts(i))
autoplot(ts(di))
autoplot(ts(ddi))


# (1) dickey fuller on level data
autoplot(ts(i)) # use trend, constant
level.lm <- lm(di ~ i + t + di_1)
summary(level.lm)
# Find out how many augmenting terms
autoplot(acf(level.lm$residuals, lag.max = 20, plot=F)) # so k = 1

# TAU -3.3708 > -3.41 so nonstationary
ur.df(i, type="trend", lags=1)


# (2) dickey fuller on first difference
autoplot(ts(di)) # constant, no trend
mean(di, na.rm=T)
firstdiff.lm <- lm(ddi ~ di)
autoplot(acf(firstdiff.lm$residuals, lag.max = 20, plot=F)) # so k = 0

# TAU -1.88 > -2.86 so nonstationary
ur.df(diff(i), type="drift", lags=0)




# (3) dickey fuller on second difference
autoplot(ts(ddi))
mean(ddi, na.rm=T) # no const, no trend
seconddiff.lm <- lm(dddi ~ ddi + 0)
autoplot(acf(seconddiff.lm$residuals, lag.max = 20, plot=F)) # so k = 0

# TAU -16.94 > -1.94 so stationary so original is I(2)
ur.df(diff(diff(i)), type="none", lags=0)
