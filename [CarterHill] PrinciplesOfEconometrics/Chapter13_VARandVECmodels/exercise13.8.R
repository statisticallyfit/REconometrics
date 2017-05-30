setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) - estimate a VAR model
var.data <- read.dta("var.dta")
var.diffed <- data.frame(dw=diff(var.data$w), dz=diff(var.data$z))
var <- VAR(var.diffed, p=1, type="none"); var
summary(var)

# residuals from var should not be autocorrelated...
autoplot(acf(var$varresult$dw$residuals, lag.max = 20, plot=FALSE))
autoplot(acf(var$varresult$dz$residuals, lag.max = 20, plot=FALSE))
