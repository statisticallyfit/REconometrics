setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
library(foreign)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) Test using all 3 types of tests
csi <- read.dta("csi.dta")
ur.df(csi$csi, type="none", lags=0) # tau -0.2986 > -1.94 nonstationary
ur.df(csi$csi, type="drift", lags=0) # tau -3.001 < -2.86 stationary
ur.df(csi$csi, type="trend", lags=0) # tau -3.4839 < -3.41 stationary

# First test is not correct since it assumes mean = 0 but it is: 
mean(csi$csi)





## Part b) graph inspect
# Should have used test CONSTANT, NO TREND

autoplot(ts(csi$csi))





## Part c) does csi show consumers retain info for short time or long time?

# ANSWER: they retain for short time since stationarity means
# csi is temporary. 