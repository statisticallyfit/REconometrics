setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter11_SimultaneousEquationsModels")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
#install.packages("systemfit")
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm





## Part b) 
truffles <- read.dta("truffles.dta")

# Now estimating with P as dependent variable
Q.reduced.lm <- lm(data=truffles, q ~ ps + di + pf)
P.reduced.lm <- lm(data=truffles, p ~ ps + di + pf)
summary(Q.reduced.lm)
summary(P.reduced.lm)

Q.fits <- Q.reduced.lm$fitted.values

# Now estimating 2sls simultaneous models
P.D.structural.lm <- lm(data=truffles, p ~ Q.fits + ps + di)
P.S.structural.lm <- lm(data=truffles, p ~ Q.fits + pf)
summary(P.D.structural.lm)
summary(P.S.structural.lm)



# The easy way, plus correct stderrors
structEq <- list(pdemand = p ~ q + ps + di, psupply = p ~ q + pf)
inst <- ~ ps + di + pf
two.sls <- systemfit(structEq, method="2SLS", 
                     inst=inst, data=truffles)
summary(two.sls)




