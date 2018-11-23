setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(lmtest)
library(tseries)
library(foreign)
library(ggfortify)
library(reshape2)
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a)
oz <- read.dta("oz.dta")
autoplot(ts(oz$consumption)) # around trend
autoplot(ts(oz$income)) # around trend

# result = nonstationary consumption
dickeyFullerTest(oz$consumption, useTrend=TRUE, k=0)
# result = nonstationary income
dickeyFullerTest(oz$income, useTrend = TRUE, k = 0)

#c <- oz$consumption
#dc <- c(NA, diff(c))
#c_1 <- c(NA, c[1:81])
#df <- lm(dc ~ c_1)
## seems to include diffed lags until lag 6 (??? why didn't they?)
#autoplot(acf(df$residuals, lag.max=20, plot=F))





# Part b) what is their order of integration?
c <- oz$consumption
#c_1 <- c(NA, c[1:81])
#dc <- c(NA, diff(c))
#dc_1 <- c(NA, dc[1:81]); length(dc_1)
#ddc <- c(NA, diff(dc))
#c.df <- data.frame(c=c, c_1=c_1, dc=dc, ddc=ddc, dc_1=dc_1)
#head(c.df)

#lm <- lm(ddc ~ dc_1)
#summary(lm)

# CONSUMPTION - no augmented terms
# tau = -6.579 < -2.86 so first difference is stationary
# So consumption is I(1) integrated of order one
dickeyFullerTest(diff(c), useTrend = FALSE, k = 0)
#autoplot(acf(ts(diff(c)), lag.max=20, plot=F))

# CONSUMPTION - 2 augmented terms
# tau = -2.228 > -2.86 so  DC is not stationary
dickeyFullerTest(diff(c), useTrend = FALSE, k=2)

# CONSUMPTION - no augmented terms and DDC
autoplot(ts(diff(diff(c))))
# Below, use test of no constant, no trend --- TODO - how to get no constant??
# book says k=1.. ?why?
# why is my tau = -13.572 different than theirs? 
# their tau = -13.661 < -1.94 so DDC is stationary
dickeyFullerTest(diff(diff(c)), useTrend = FALSE, k=1)

# why conclude CCONSUMPTION is I(2) because in first equation without
# any augmentation the result was I(1) but when adding augmentation
# it becomes I(2) - why? Which to trust?)





# INCOME - no augmentation terms
i <- oz$income
autoplot(ts(i))

# INCOME - no augmentation, simple I
dickeyFullerTest(i, useTrend=TRUE, k=0)  # not stationary

# INCOME - no augmentation, diff(i)
autoplot(ts(diff(i))) # around a constant
# tau = -10.676 < -2.86 so DI is stationary so INCOME is I(1)
dickeyFullerTest(diff(i), useTrend = FALSE, k = 0)

#i_1 <- c(NA, i[1:81])
#di <- c(NA, diff(i))
#di_1 <- c(NA, di[1:81])
#ddi <- c(NA, diff(di))
#i.df <- data.frame(i=i, i_1=i_1, di=di, ddi=ddi, di_1=di_1)
#head(i.df)







## Part c) determine if cointegrated or spurious regression below

# (1) estimate the regression
cointegrating.lm <- lm(c ~ i)
summary(cointegrating.lm)

# (2) determine if residuals are stationary
autoplot(ts(cointegrating.lm$residuals))   # no trend, no constant
# HOw to make no constant for dickey fuller? 
dickeyFullerTest(cointegrating.lm$residuals, useTrend = FALSE, k = 0)

# their tau = -3.909 < -3.37 so residuals are stationary so CONSUMPTION
# and INCOME are cointegrated
