setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
savings <- read.dta("savings.dta")
savings.lm <- lm(data=savings, savings ~ income)
summary(savings.lm)



## Part b) instrument estimates, z = avgincome
firststage.IV.lm <- lm(data=savings, income ~ avgincome)
summary(firststage.IV.lm)
income.fits <- firststage.IV.lm$fitted.values
secondstage.IV.lm <- lm(data=savings, savings ~ income.fits)
summary(secondstage.IV.lm)

# Instrumental estimates, stderrors, and significance:
iv <- ivreg2(form=savings~income, endog="income", digits = 10, 
       iv=c("avgincome"), data=na.omit(savings)); iv




## Part c) Hausman test
firststage.H.lm <- lm(data=savings, income ~ avgincome)
vhat <- firststage.H.lm$residuals
secondstage.H.lm <- lm(data=savings, savings ~ income + vhat)
# Significance of vhat coefficient means that x and e are correlated
summary(secondstage.H.lm)
