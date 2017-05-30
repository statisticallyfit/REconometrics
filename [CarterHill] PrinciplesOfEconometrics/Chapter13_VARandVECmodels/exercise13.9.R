setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) determine the cointegrating relationship between P and M - is the
# quantity theory of money supported? 
## Part c) confirm the residuals are I(0) (so cointegration exists)

qtm <- read.dta("qtm.dta")
qtm <- data.frame(p=qtm$p, m=qtm$m)
qtm.diff <- data.frame(dp=diff(qtm$p), dm=diff(qtm$m))

# TODO: why isn't equation same as theirs? 
# TAU must be -3.663 so the series should be cointegrated. 
res <- cointegrationTest(qtm, type="constant", resid.lags = 1)
autoplot(acf(res, lag.max = 20, plot=FALSE))






## Part b) identify the error-correcting coefficients. Is system stable?
## Part d)

# TODO: not even the VEC ECT coeffs are not opposite signs... 
# OF course the VECM is not the same as in book if residuals aren't
# the same as in book. 
res <- VEC(qtm.diff, type="none", lag = 1)
autoplot(acf(res$y, lag.max = 20, plot=FALSE))
autoplot(acf(res$x, lag.max = 20, plot=FALSE))

