setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
chard <- read.dta("chard.dta")
head(chard)
chard.lm <- lm(data=chard, q ~ xper + cap + lab)
summary(chard.lm)



## Part c) Hausman, instrument = AGE, endog = XPER
firststage.H.lm <- lm(data=chard, xper ~ age + cap + lab)
vhat <- firststage.H.lm$residuals
secondstage.H.lm <- lm(data=chard, q ~ xper + cap + lab + vhat)
summary(secondstage.H.lm)
# Significant vhat coefficient so there is evidence that XPER is endogeneous




## Part d) IV estimation
firststage.IV.lm <- lm(data=chard, xper ~ age + cap + lab)
xper.fit <- firststage.IV.lm$fitted.values
secondstage.IV.lm <- lm(data=chard, q ~ xper.fit + cap + lab)
summary(secondstage.IV.lm) # has incorrect standard errors

ivreg2(form=q~xper+cap+lab, endog="xper", iv=c("age"),
       digits=3, data=chard)
