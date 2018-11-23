setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter11_SimultaneousEquationsModels")
rm(list=ls())

library(lmtest)
library(dplyr)
library(foreign)
library(ggfortify)
#install.packages("systemfit")
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm





## Part a) 
fish <- read.dta("fultonfish.dta")

# 2sls with correct stderrors
struct <- list(demand = lquan ~ lprice + mon + tue + wed + thu, 
              supply = lquan ~ lprice + stormy)
inst <- ~ mon + tue + wed + thu + stormy
two.sls <- systemfit(struct, method="2SLS", inst=inst, data=fish)
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
summary(lnQS.structural.lm)






## Part b)c) derive new demand eq using RAINY and COLD variables
# lquan ~ lnP.fit + mon + tue + wed + thu + RAINY + COLD
lnQ.reduced.lm <- lm(data=fish, lquan ~ mon + tue + wed + thu + stormy + 
                           rainy + cold)
lnP.reduced.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu + stormy + 
                           rainy + cold)
summary(lnQ.reduced.lm)
summary(lnP.reduced.lm)

lnP.fit <- lnP.reduced.lm$fitted.values
# demand
lnQD.structural.lm <- lm(data=fish, lquan ~ lnP.fit + mon + tue + wed + 
                               thu + rainy + cold)
summary(lnQD.structural.lm)
# supply
lnQS.structural.lm <- lm(data=fish, lquan ~ lnP.fit + stormy + rainy + cold)
summary(lnQS.structural.lm)


## Testing joint significance of all coefficients
## Cannot reject null that coefficients are zero. 
# Thus instruments (all vars on right side) are not adequate
# for estimating supply equation. 
u.lm <- lnP.reduced.lm
r.lm <- lm(data=fish, lprice ~ stormy)
anova(r.lm, u.lm)






## Part d) get OLS and 2SLS estimates of demand in part b)
# (so demand with rainy and cold) but supply without them
lnP.reduced.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu + 
                           stormy + rainy + cold)
summary(lnP.reduced.lm)

lnP.fit <- lnP.reduced.lm$fitted.values
# demand
lnQD.ols.lm <- lm(data=fish, lquan ~ lprice + mon + tue + wed + 
                        thu + rainy + cold)
summary(lnQD.ols.lm)
lnQD.structural.lm <- lm(data=fish, lquan ~ lnP.fit + mon + tue + wed + 
                               thu + rainy + cold)
summary(lnQD.structural.lm)         ## TODO: why aren't stderrrs the same?
# supply
lnQS.ols.lm <- lm(data=fish, lquan ~ lprice + stormy)
summary(lnQS.ols.lm)
lnQS.structural.lm <- lm(data=fish, lquan ~ lnP.fit + stormy)
summary(lnQS.structural.lm)





## Part e) Use MIXED for supply, and previous for demand
lnP.reduced.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu + 
                           stormy + rainy + cold + mixed)
summary(lnP.reduced.lm)

## Testing significance of all coefs but stormy and mixed in reduced lnP
u.lm <- lnP.reduced.lm
r.lm <- lm(data=fish, lprice ~ stormy + mixed)
anova(r.lm, u.lm)
# So cannot reject null that they aren't equal to zero. Therefore, 
# adding MIXED has not helped. 




## Part f) estimate the supply and demand by 2SLS from above and OLS
lnP.fit <- lnP.reduced.lm$fitted.values
# demand
lnQD.ols.lm <- lm(data=fish, lquan ~ lprice + mon + tue + wed + 
                        thu + rainy + cold)
summary(lnQD.ols.lm)
lnQD.structural.lm <- lm(data=fish, lquan ~ lnP.fit + mon + tue + wed + 
                               thu + rainy + cold)
summary(lnQD.structural.lm) 
# supply
lnQS.ols.lm <- lm(data=fish, lquan ~ lprice + stormy + mixed)
summary(lnQS.ols.lm)
lnQS.structural.lm <- lm(data=fish, lquan ~ lnP.fit + stormy + mixed)
summary(lnQS.structural.lm)    ## TODO: why aren't std.errors the same??
# BEfore, std.errors were not same for lnQD but now for lnQS. 

