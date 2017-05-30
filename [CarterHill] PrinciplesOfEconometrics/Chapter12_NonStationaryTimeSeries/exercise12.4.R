setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(lmtest)
library(tseries)
library(foreign)
library(ggfortify)
library(reshape2)
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) plot the oil prices
oil <- read.dta("oil.dta")
head(oil)
oil.df <- data.frame(price=oil$oil, t=seq(1:88))
ggplot(data=oil.df, aes(x=t, y=price)) + geom_line()    # way 1
autoplot(ts(oil$oil))                                   # way 2




## Part b) do DIckey Fuller test to see if stationary

# manually
# 1 - estimate equation deltaOIL = oil_1 + vt
o <- oil$oil
oil.df <- data.frame(O = o, 
                     O_1 = c(NA, o[1:87]), 
                     dO = c(NA, diff(o)))
head(oil.df)

dickeyFuller.lm <- lm(data=oil.df, dO ~ O_1)
tau <- summary(dickeyFuller.lm)$coefficients[2, 3]; tau
# dickey fuller critical value = -1.94 so series is stationary


# automatically
# why not quite the same? 
adf.test(oil$oil, alt="stationary", k = 1)


# my function (automatically)
dickeyFullerTest(oil$oil, useTrend=FALSE, k = 0)




## Part c) 
# Since series above is stationary, it is I(0) (integrated of order one)