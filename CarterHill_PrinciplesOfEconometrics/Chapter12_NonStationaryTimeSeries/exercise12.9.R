setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
library(foreign)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a)
canada <- read.dta("canada.dta")
a <- canada$canada[1:204]
b <- canada$canada[205:433] # but 2006:12 seems to be until second last....


autoplot(ts(a))
t <- seq(1, 204)
a_1 <- c(NA, a[1:203])
da <- c(NA, diff(a)); length(da)
da_1 <- c(NA, da[1:203])
da.lm <- lm(da ~ a_1 + t + da_1)

# How many augmented terms needed in dickey fuller equation?
# BOOK says just 1 term but there are still crossings!!!
autoplot(acf(da.lm$residuals), lag.max=20, plot=FALSE)

summary(da.lm) # this is the dickey fuller equation

# Doing the test the easy way:
# TAU = -2.392 > -3.41 so nonstationary
dickeyFullerTest(a, useTrend = TRUE, k= 1)




autoplot(ts(b)) # seems to be around a constant
length(b)
b_1 <- c(NA, b[1:228])
db <- c(NA, diff(b))
db_1 <- c(NA, db[1:228])
db.lm <- lm(db ~ b_1 + db_1)

# How many augmented terms? 
autoplot(acf(db.lm$residuals, lag.max = 20, plot=FALSE)) # so k = 1

summary(db.lm)


# THEIR tau = -0.897 > -2.86 so nonstationary
dickeyFullerTest(b, useTrend = TRUE, k = 1)







## Part c) now test the whole sample to find the order of integration
autoplot(ts(canada$canada))
c <- canada$canada
c_1 <- c(NA, c[1:432])
dc <- c(NA, diff(c))
dc_1 <- c(NA, dc[1:432])
t <- seq(1, 433)
dc.lm <- lm(dc ~ c_1 + t + dc_1)
summary(dc.lm) # trend term is insignificant so don't use it
dc.lm <- lm(dc ~ c_1 + dc_1)
summary(dc.lm)

# seems to need more lags but that's not in the book...
autoplot(acf(dc.lm$residuals, plot=F))


# 1 - TESTING NONDIFFERENCED
# THEIR tau = -1.559 > -2.86 so  nonstationary
dickeyFullerTest(c, k = 1)


# 2 - TESTING FIRST DIFFERENCE
mean(dc, na.rm=T) # no constant, no trend test --- how to remove constant in eq?
# THEIR tau = -16.461 < -1.94 so is stationary I(1)
dickeyFullerTest(diff(c), k = 0)
