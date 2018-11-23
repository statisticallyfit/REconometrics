setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

#install.packages("tsDyn")
library(tsDyn)
library(ggfortify)
library(foreign)
library(reshape2)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



gdp <- read.dta("gdp.dta")
gdp$time <- seq(1, 124)
ggplot() + 
      geom_line(data=gdp, aes(x=time, y=usa), lwd=1, color="blue") + 
      geom_line(data=gdp, aes(x=time, y=aus), lwd=1, color="red")
# OR
gdp.melt <- melt(gdp, id="time")
ggplot(data=gdp.melt, aes(x=time, y=value, colour=variable)) + 
      geom_line(lwd=1)


# ESTIMATE VEC model

# Step 1 - check that series are nonstationary
ur.df(gdp$usa, type="trend", lags = 0) # nonstationary
ur.df(gdp$aus, type="trend", lags = 0) # nonstationary

# Step 2 - estimate cointegrating relationship to test for cointegration
# use no intercept since they say has no economic meaning here
cointegrating.lm <- lm(data=gdp, aus ~ usa + 0) 
summary(cointegrating.lm)

# Step 3 - are the residuals stationary? if so then usa and aus are
# cointegrated
autoplot(ts(cointegrating.lm$residuals))
mean(cointegrating.lm$residuals, na.rm = TRUE)
# dickey fuller with no constant (how we do it when testing cointegration)
# TAU -2.88 < -2.76 so resids are stationary so AUS and USA are cointeg.
ur <- ur.df(cointegrating.lm$residuals, type="none", lags=0); ur
autoplot(acf(ur@res)) # so one lag seems fine


# Step 4 - estimate vector error correction model (VEC)
e <- cointegrating.lm$residuals
e_1 <- c(NA, e[1:123])
a <- gdp$aus
da <- c(NA, diff(a))
u <- gdp$usa
du <- c(NA, diff(u))

aus.vec.lm <- lm(data=gdp, da ~ e_1); summary(aus.vec.lm)
usa.vec.lm <- lm(data=gdp, du ~ e_1); summary(usa.vec.lm)

# THe easy way.. (why not exactly the same?)
VEC(gdp, type="const", lag=0)
