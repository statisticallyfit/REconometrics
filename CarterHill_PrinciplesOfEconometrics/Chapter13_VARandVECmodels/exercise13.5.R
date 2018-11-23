setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(ggplot2)
library(foreign)
library(ggfortify)
library(reshape2)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) - are the series stationary?
gdp <- read.dta("gdp.dta")
gdp$time <- seq(1,124)
ggplot() + 
      geom_line(data=gdp, aes(x=time, y=usa), lwd=1, colour="maroon1") + 
      geom_line(data=gdp, aes(x=time, y=aus), lwd=1, colour="dodgerblue") 

# lags is how many diffed lags to include (put as many until auto.corr is gone)
usa.df <- ur.df(gdp$usa, type="trend", lags=1); usa.df
usa.df@testreg
autoplot(acf(usa.df@res, lag.max = 20, plot=FALSE))  # one term needed

# lags = 0 so zero diffed lags. 
aus.df <- ur.df(gdp$aus, type="trend", lags=0); aus.df
aus.df@testreg
autoplot(acf(aus.df@res, lag.max = 20, plot=FALSE)) # ok so no augmentation terms are needed

# TAUs are too small so nonstationary





## Part b) test for cointegration
# with constant 
a <- gdp$aus; u <- gdp$usa
gdp <- data.frame(aus=a, usa=u)
cointegrationTest(gdp, type="constant")
# without constant (it was negative so not making sense)
cointegrationTest(gdp, type="none")





# Part c) estimate the VEC model
gdp$time <- NULL
VEC(gdp, type="const") #my custom function (using VECM underneath)
