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
bond <- read.dta("bond.dta")
autoplot(ts(bond$bond))





## Part b) test if stationary
# Use dickey fuller test with constant + trend
b <- bond$bond
n <- length(b)
b_1 <- c(NA, b[1:(n-1)])
db <- c(NA, diff(b))
lm <- lm(db ~ b_1)

# Looking at below acfs, you need another diffed lag to 
# get rid of error autocorrelation
autoplot(acf(lm$residuals, lag.max=20, plot=F))
# that means k = 1
dickeyFullerTest(bond$bond, useTrend=TRUE, k=1) # this is tau





## Part c) test the first difference for stationarity
autoplot(ts(db)) ## seems fluctuating around constant, no trend
# tau is less than -2.86 so series is stationary
dickeyFullerTest(db, useTrend = F, k = 0)




#b <- bond$bond
#n <- length(b)
#b_1 <- c(NA, b[1:(n-1)])
#db <- c(NA, diff(b))
#ddb <- c(NA, diff(db))
#db_1 <- c(NA, db[1:101])
#dat <- data.frame(b=b, b_1=b_1, db=db, db_1=db_1, ddb=ddb)
#head(dat)
#summary(lm(ddb ~ db_1))




## Part d) BOND has to be differenced once to get stationary so it is I(1)