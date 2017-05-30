setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
library(foreign)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) 
mexico <- read.dta("mexico.dta")
m <- mexico$mexico
u <- mexico$usa

# (1) cointegration test (no constant, no trend) -------------------------------
cointegration.lm <- lm(m ~ u + 0)
summary(cointegration.lm)

# Dickey fuller test of residuals
# TAU -1.9476 > -2.76 so nonstationary so spurious regression
ur.df(cointegration.lm$residuals, type="none", lags=0)


# (2) cointegration test (constant, no trend)----------------------------------
cointegration.lm <- lm(m ~ u)
summary(cointegration.lm)

# Dickey fuller test of residuals
# TAU -2.078 > -3.37 so nonstationary so spurious regression
ur.df(cointegration.lm$residuals, type="none", lags=0)



# (3) cointegration test (constant, trend) ------------------------------------
t <- seq(1, 107)
cointegration.lm <- lm(m ~ u + t)
summary(cointegration.lm)

# Dickey fuller test of residuals
# TAU -2.396 > -3.42 so nonstationary so spurious regression
ur.df(cointegration.lm$residuals, type="none", lags=0)


