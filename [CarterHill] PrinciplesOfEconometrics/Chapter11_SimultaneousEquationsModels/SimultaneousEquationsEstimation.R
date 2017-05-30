setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter11_SimultaneousEquationsModels")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
#install.packages("systemfit")
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



truffles <- read.dta("truffles.dta")

# Structural (simultaneous) equations
# QD = P + PS + DI + ed
# QS = P + PF + es

# Doing 2sls for the reduced-form equations
# ... all the variables on right (ps, di, pf) are exogenous, while
# all variables on left (q, p) are endogenous
Q.reduced.lm <- lm(data=truffles, q ~ ps + di + pf)
P.reduced.lm <- lm(data=truffles, p ~ ps + di + pf)
summary(Q.reduced.lm)
summary(P.reduced.lm)

# Now on second part of 2sls, estimating the structural equations
# this is where the stderrors are incorrect (and thus t-stats and pvals)
P.fit <- P.reduced.lm$fitted.values
# quantity demanded model
QD.structural.lm <- lm(data=truffles, q ~ P.fit + ps + di)
summary(QD.structural.lm)
# quantity supplied model
QS.structural.lm <- lm(data=truffles, q ~ P.fit + pf)
summary(QS.structural.lm)


## 2SLS with correct stderrors
structEq <- list(qdemand = q ~ p + ps + di, sdemand = q ~ p + pf)
instrumentsOrExogenousVars <- ~ ps + di + pf
two.sls <- systemfit(structEq, method="2SLS", 
                     inst=instrumentsOrExogenousVars, data=truffles)

# see? in demand equation, there is inverse relationship between
# price and quantity demanded, but this relation is positive
# in quantity supplied. In QS as pf (factor price) goes up, QS falls. 
# Also in QD, as ps rises, QD rises, showing that ps are substitute
# goods. QD (quantity demanded of truffles) and income are positively 
# related so truffles are a normal good. 
summary(two.sls)


## NOTE: there is a Q.reduced.lm model and also QD.lm and QS.lm. This is
# not confusing since QD and QS are created just depending on what
# types of variables are on their right side. Quantity itself is
# just a variable in the data truffles list. 






## Part 11.7
fish <- read.dta("fultonfish.dta")

# 2sls with correct stderrors
simEq <- list(demand = lquan ~ lprice + mon + tue + wed + thu, 
              supply = lquan ~ lprice + stormy)
instOrExogVars <- ~ mon + tue + wed + thu + stormy
two.sls <- systemfit(simEq, method="2SLS", 
                     inst=instOrExogVars, data=fish)
summary(two.sls)



# 2sls the long way with incorrect std errors in second stage
lnQ.reduced.lm <- lm(data=fish, lquan ~ mon + tue + wed + thu + stormy)
lnP.reduced.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu + stormy)
summary(lnQ.reduced.lm)
summary(lnP.reduced.lm)

lnP.fit <- lnP.reduced.lm$fitted.values
# demand
lnQD.structural.lm <- lm(data=fish, lquan ~ lnP.fit + mon + tue + wed + thu)
summary(lnQD.structural.lm)
# supply
lnQS.structural.lm <- lm(data=fish, lquan ~ lnP.fit + stormy)
summary(QS.structural.lm)
