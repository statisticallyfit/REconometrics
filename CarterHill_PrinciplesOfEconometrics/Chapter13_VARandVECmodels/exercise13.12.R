setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## ESTIMATE the given VAR and ARDL model for the data
equity <- read.dta("equity.dta")
pn <- equity$pn
pn_1 <- c(NA, pn[1:90])
dn <- equity$dn
dn_1 <- c(NA, dn[1:90])
equity <- data.frame(sp=100*log(pn/pn_1), dv=100*log(dn/dn_1))
equity <- equity[2:90, ]
head(equity)

# VAR MODEL
# TODO: why is it different than in book? 
var <- VAR(equity, p=1, type="const")
var

# ARDL MODEL
attach(var$datamat); head(var$datamat)
sp.ardl.lm <- lm(sp ~ sp.l1 + dv.l1 + dv); sp.ardl.lm
dv.ardl.lm <- lm(dv ~ sp.l1 + dv.l1 + sp); dv.ardl.lm # why different than in book?


