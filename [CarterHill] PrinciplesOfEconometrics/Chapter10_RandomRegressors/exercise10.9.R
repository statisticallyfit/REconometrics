setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
newbroiler <- read.dta("newbroiler.dta")
newbroiler$lnq <- log(newbroiler$qprod)
newbroiler$lnp <- log(newbroiler$p)
newbroiler$lnpf <- log(newbroiler$pf)
newbroiler$qprod_1 <- c(NA, newbroiler$qprod[1:51])
newbroiler$lnq_1 <- log(newbroiler$qprod_1)
head(newbroiler)
dat <- newbroiler[11:50,]
## TODO: the intercept is NOT the same!!!

broiler.lm <- lm(data=dat, lnq ~ lnp + lnpf + lnq_1 + year)
summary(broiler.lm)

# The coefficients:
summary(broiler.lm)$coeff[,1]





## Part b) IV estimates: iv = lny, lnpb, popgro, lnp_1, lnexpts
newbroiler$lny <- log(newbroiler$y)
newbroiler$lnpb <- log(newbroiler$pb)
newbroiler$popgro <- newbroiler$popgro
newbroiler$lnp_1 <- log(c(NA, newbroiler$p[1:51]))
names(newbroiler)
head(newbroiler); tail(newbroiler)
newbroiler <- newbroiler[11:50, ]

# TODO this is not correct ....
# TODO how to do 2sls estimation, since no endogeneous variable is given!!!
firststage.IV.lm <- lm(data=newbroiler, lnp ~ lny + lnpb + popgro + 
                             lnp_1 + lexpts + lnpf + year + lnq_1)
lnp.fit <- firststage.IV.lm$fitted.values; length(lnp.fit)
secondstage.IV.lm <- lm(data=newbroiler, lnq ~ lnp.fit + lnpf + year + lnq_1)
summary(secondstage.IV.lm)
# TODO: Why does this give an error???
iv <- ivreg2(form=lnq~lnp+lnpf+year+lnq_1, data=newbroiler, endog="lnp",
       iv=c("lny", "lnpb", "popgro", "lnp_1", "lexpts"), digits=7)

names(newbroiler)





## Part c)  Hausman
## TODO: why aren't the estimates the same???
summary(firststage.IV.lm)
vhat <- firststage.IV.lm$residuals; length(vhat)
secondstage.H.lm <- lm(data=newbroiler, lnq ~ lnp + lnpf + year + lnq_1 + vhat)
summary(secondstage.H.lm)
# vhat must have t-stat of -3.88 so lnp is endogeneous




## Part d) Weak instruments test

## TODO: why doesn't this work?
iv <- ivreg2(form=lnq~lnp+lnpf+year+lnq_1, data=newbroiler, digits=8, 
       endog="lnp", iv=c("lny", "lnpb", "popgro", "lnp_1", "lexpts"))

u.lm <- lm(data=newbroiler, lnp ~ lny + lnpb + popgro + lnp_1 + lexpts + 
                 lnpf + year + lnq_1)
r.lm <- lm(data=newbroiler, lnp ~ lnpf + year + lnq_1)
R.u <- summary(u.lm)$r.sq
R.r <- summary(r.lm)$r.sq
anova(r.lm, u.lm) # see? F-stat is 3.92
# Doing F-test manually
n <- nrow(newbroiler); k <- 9; q <- 5
# TODO: should be 3.92 -- why different? How to use ivreg, not working???
F.stat <- (R.u - R.r)/(1 - R.u) * (n-k-1)/q; F.stat

# Instruments are significantly correlated with endogeneous lnP but are not
# strong enough to make 2sls reliable





## Part e) 
