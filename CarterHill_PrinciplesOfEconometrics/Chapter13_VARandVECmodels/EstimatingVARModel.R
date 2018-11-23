setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

#install.packages("tsDyn")
library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(reshape2)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



fred <- read.dta("fred.dta"); head(fred)
fred.diff <- data.frame(dc=diff(fred$c), dy=diff(fred$y))
fred$time <- seq(1, 200)
ggplot() + 
      geom_line(data=fred, aes(x=time, y=c), lwd=1, color="purple") + 
      geom_line(data=fred, aes(x=time, y=y), lwd=1, color="orange")


# ESTIMATE A VAR MODEL

# Step 1 - make sure they are I(1) variables (must also check for stationarity in differenced form)
# TAU -1.995 > -2.87 so nonstationary
c.df <- ur.df(fred$c, type="drift", lags = 3); c.df
autoplot(acf(c.df@res, lag.max = 20, plot=FALSE)) # shouldn't go up to 8...?

# TAU -2.741 > -2.87 so nonstationary
y.df <- ur.df(fred$y, type="drift", lags = 0); y.df
autoplot(acf(y.df@res, lag.max = 20, plot=FALSE))


# Step 2 - make sure they are NOT cointegrated
cointegrating.lm <- lm(data=fred, c ~ y)
summary(cointegrating.lm)
# are there enough augmentation terms? 
e.df <- ur.df(cointegrating.lm$residuals, type="none", lags = 0); e.df
autoplot(acf(e.df@res))
# add 1 lag - seems enough!
e.df <- ur.df(cointegrating.lm$residuals, type="none", lags = 1); e.df
autoplot(acf(e.df@res))
# TAU -2.873 > -3.37 so residuals are nonstationary so spurious so not cointegrated


# Step 3 - estimate a VAR model
# (in general must test for significance of lagged terms to know if to 
# include them)
c <- fred$c
dc <- c(NA, diff(c))
dc_1 <- c(NA, dc[1:199])
y <- fred$y
dy <- c(NA, diff(y))
dy_1 <- c(NA, dy[1:199])

c.var.lm <- lm(dc ~ dc_1 + dy_1 + 0); c.var.lm
summary(c.var.lm)
y.var.lm <- lm(dy ~ dy_1 + dc_1 + 0); y.var.lm
summary(y.var.lm)


var <- VAR(fred.diff, type="none", p=1)
var
summary(var)
