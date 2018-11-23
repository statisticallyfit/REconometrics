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
fish <- tbl_df(fish) %>% filter(change == 1)
fish <- as.data.frame(fish)
head(fish)

lnP.reduced.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu + stormy)
summary(lnP.reduced.lm)

# Testing significance of STORMY
u.lm <- lnP.reduced.lm
r.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu)
anova(r.lm, u.lm)
# significant stormy since F = 28.995 > 10. 
# Good since it's the supply equation's shift variable which allows the
# demand equation to be identified, so STORMY needs to be significant
# otherwise the 2SLS will be unreliable. 





## Part b) Hausman test for endogeneity of lnPrice in the demand equation
vhat <- lnP.reduced.lm$residuals
hausman.lm <- lm(data=fish, lquan ~ lprice + mon + tue + wed + thu + vhat)
summary(hausman.lm)
# since vhat is significant, lnPrice is endogenous. 





## Part c) 2SLS and OLS of demand equation
lnP.fits <- lnP.reduced.lm$fitted.values
lnQD.lm <- lm(data=fish, lquan ~ lnP.fits + mon + tue + wed + thu)
summary(lnQD.lm)        ## TODO: why aren't std.errors same as in answers?
lnQD.ols.lm <- lm(data=fish, lquan ~ lprice + mon + tue + wed + thu)
summary(lnQD.ols.lm)





## Part d) find reduced form when CHANGE == 0
fish <- read.dta("fultonfish.dta")
fish <- tbl_df(fish) %>% filter(change == 0)
fish <- as.data.frame(fish)
head(fish)

lnP.reduced.lm <- lm(data=fish, lprice ~ mon + tue + wed + thu + stormy)
summary(lnP.reduced.lm)




## Part e) do Hausman test for endogeneity of lnPrice in part d)
vhat <- lnP.reduced.lm$residuals
hausman.lm <- lm(data=fish, lquan ~ lprice + mon + tue + wed + thu + vhat)
summary(hausman.lm)
# Now vhat is insignificant, showing that when CHANGE == 0
# (meaning changes in inventory are small) then price
# responsiveness to quantity is low so simultaneity
# (defined by endogeneity of lnPrice) does not exist 
# (shows no evidence of its presence)




## Part f) 
lnP.fits <- lnP.reduced.lm$fitted.values
lnQD.lm <- lm(data=fish, lquan ~ lnP.fits + mon + tue + wed + thu)
summary(lnQD.lm)        ## TODO: why aren't std.errors same as in answers?
lnQD.ols.lm <- lm(data=fish, lquan ~ lprice + mon + tue + wed + thu)
summary(lnQD.ols.lm)



## See? The OLS and 2SLS estimates are similar
# when CHANGE == 0 (small changes in inventory so 
# small price responsiveness) which means there is
# no simultaneity, but when CHANGE == 1, 
# there is simultaneity so the estimates are
# very different, comparing OLS and 2SLS. 