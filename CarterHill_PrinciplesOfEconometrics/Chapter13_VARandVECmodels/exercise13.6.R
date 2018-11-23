setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) - are the series stationary?
fred <- read.dta("fred.dta")
fred$time <- seq(1, 200)
ggplot() + 
      geom_line(data=fred, aes(x=time, y=c), lwd=1, colour="purple") + 
      geom_line(data=fred, aes(x=time, y=y), lwd=1, colour="orange")

# Nonstationary
c.df <- ur.df(fred$c, type="trend", lags = 3); c.df
autoplot(acf(c.df@res, lag.max = 20, plot=FALSE)) #so lags = 3
c.df@testreg
# Nonstationary 
y.df <- ur.df(fred$y, type="trend", lags = 0); y.df
autoplot(acf(y.df@res, lag.max = 20, plot=FALSE)) #so lags = 0
y.df@testreg




## Part b) cointegrated? 
c <- cointegrationTest(fred, type="constant", lags = 1)
autoplot(acf(c)) # no more autocorrelations so use lags = 1
c <- cointegrationTest(fred, type="trend", lags = 2)
autoplot(acf(c)) # no more autocorrelations so use lags = 2




# Estimate VAR model
fred.diffed <- data.frame(dc=diff(fred$c), dy=diff(fred$y))
head(fred.diffed)
var <- VAR(fred.diffed, p=1, type="const")
summary(var)

# How many lags to include?

# autocorr is eliminated with p=3
var <- VAR(fred.diffed, p=3, type="const")
autoplot(acf(var$varresult$dc$residuals)) 
autoplot(acf(var$varresult$dy$residuals)) 

# lag coeffs for both C and Y are BOTH significant only for p = 1
var <- VAR(fred.diffed, p=3, type="const")
summary(var$varresult$dc)
summary(var$varresult$dy)
var <- VAR(fred.diffed, p=1, type="const")
summary(var$varresult$dc)
summary(var$varresult$dy)

# DC Joint test for both lag 1 coeffs = 0 (meaning dc.l1 and dy.l1 = 0)
r.lm <- lm(data=var$datamat, dc ~ dc.l2 + dy.l2 + dc.l3 + dy.l3)
u.lm <- lm(data=var$datamat, 
           dc ~ dc.l1 + dy.l1 + dc.l3 + dy.l3 + dc.l2 + dy.l2)
# CHI = F * J, where J = number of restrictions
anovaChiSquare(r.lm, u.lm, J=2)



# DC Joint test for both lag 2 coeffs = 0
r.lm <- lm(data=var$datamat, dc ~ dc.l1 + dy.l1 + dc.l3 + dy.l3)
anovaChiSquare(r.lm, u.lm, J=2)


# DC Joint test for both lag 3 coeffs = 0
r.lm <- lm(data=var$datamat, dc ~ dc.l1 + dy.l1 + dc.l2 + dy.l2)
anovaChiSquare(r.lm, u.lm, J=2)


# DY Joint test for both lag 1 coeffs = 0
r.lm <- lm(data=var$datamat, dy ~ dc.l2 + dy.l2 + dc.l3 + dy.l3)
u.lm <- lm(data=var$datamat, 
           dy ~ dc.l1 + dy.l1 + dc.l3 + dy.l3 + dc.l2 + dy.l2)
anovaChiSquare(r.lm, u.lm, J=2)

# DY Joint test for both lag 2 coeffs = 0
r.lm <- lm(data=var$datamat, dy ~ dc.l1 + dy.l1 + dc.l3 + dy.l3)
anovaChiSquare(r.lm, u.lm, J=2)

# DY Joint test for both lag 3 coeffs = 0
r.lm <- lm(data=var$datamat, dy ~ dc.l1 + dy.l1 + dc.l2 + dy.l2)
anovaChiSquare(r.lm, u.lm, J=2)



# Come back to test SUR (page 463 pdf in solutions manual, pg 511 in book)

