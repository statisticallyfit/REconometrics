setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
mroz <- read.dta("mroz.dta")
mroz <- mroz[which(mroz$lfp == 1), ]
head(mroz)

nwifeinc <- with(mroz, faminc - wage * hours)

# Hasuman test: endog = ln(wage), z = exper, exper2
lnwage <- with(mroz, log(wage))
exper <- mroz$exper; exper2 <- exper^2
firststage.H.lm <- lm(data=mroz, lnwage ~ educ + age + kidsl6 + 
                            kids618 + nwifeinc + exper + exper2)
summary(firststage.H.lm)
vhat <- firststage.H.lm$residuals
secondstage.H.lm <- lm(data=mroz, hours ~ lnwage + educ + age + kidsl6 + 
                             kids618 + nwifeinc + vhat)
# The vhat coefficient is significant so lnwage is endogeneous
summary(secondstage.H.lm) # has the wrong stderrors

# this does not have the right stderrors either
df <- with(mroz, data.frame(hours=hours, educ=educ, lnwage=lnwage, age=age,
                            kidsl6=kidsl6, kids618=kids618, nwifeinc=nwifeinc,
                            exper=exper, exper2=exper2))
head(df)
iv <- ivreg2(form=hours~lnwage+educ+age+kidsl6+kids618+nwifeinc, data=df, 
       endog="lnwage", iv=c("exper", "exper2"), digits=10); iv
# The Hausman F-stat is significant





## Part b) F-test exclusion for instrument validity
# H0: coefficients on exper and exper2 are zero (weak instruments)
# H1: strong instruments
u.lm <- lm(data=df, lnwage ~ educ + age + kidsl6 + kids618 + nwifeinc + 
                 exper + exper2)
r.lm <- lm(data=df, lnwage ~ educ + age + kidsl6 + kids618 + nwifeinc)
R.u <- summary(u.lm)$r.sq; R.u
R.r <- summary(r.lm)$r.sq; R.r
n <- nrow(df); n
k <- 8 # num parameters of unrestricted model (u.lm)
q <- 2 # difference in parameters between u.lm and r.lm
F.stat <- (R.u - R.r)/(1 - R.u) * (n-k-1)/q; F.stat # almost same ivreg weakidtest
# the slightly different (WHY?) F.statistic
F.fromIV <- iv$weakidtest[1]
# p-value
1 - pf(F.fromIV, df1=q, df2=(n-k-1))
# Reject null, we have strongish instruments (exper, exper2) but
# F is not greater than 10, so they are not strong, just in the middle





## Part c) Sargan test for surplus instruments (over-identifying restrictions)
iv # see? p-value is given 0.3542515
# Doing it manually
firststage.IV.lm <- lm(data=df, lnwage ~ educ + age + kidsl6 + kids618 + 
                            nwifeinc + exper + exper2)
summary(firststage.IV.lm)
lnwage.fit <- firststage.IV.lm$fitted.values
secondstage.IV.lm <- lm(data=df, hours ~ lnwage.fit + educ + age + kidsl6 + 
                             kids618 + nwifeinc)
ehat <- secondstage.IV.lm$residuals
secondstage.OVERID.lm <- lm(data=df, ehat ~ exper + exper2)
R2 <- summary(secondstage.OVERID.lm)$r.sq; R2   # TODO: should be 0.0020
N <- nrow(df); N
NR2 <- N * R2; NR2      # TOOD: should be 0.8581 -- why not same?
# The NR2 statistic is correct 0.8581 below, and p-value is 0.35425
iv






## Part d) testing for significant correlation between the 
# endogeneous (educ) and instrumental variables (mothereduc, fathereduc,
# siblings, heduc)
df$heduc <- mroz$heduc
df$siblings <-  mroz$siblings
df$mothereduc <- mroz$mothereduc
df$fathereduc <- mroz$fathereduc
u.lm <- lm(data=df, educ ~ age + kidsl6 + kids618 + nwifeinc + 
                 mothereduc + fathereduc + heduc + siblings)
r.lm <- lm(data=df, educ ~ age + kidsl6 + kids618 + nwifeinc)
R.u <- summary(u.lm)$r.sq; R.u
R.r <- summary(r.lm)$r.sq; R.r
q <- 4 # difference in parameters betwen r.lm and u.lm
k <- 9 # num parameters in u.lm
n <- nrow(df); n
# TODO: should be 60.67
F.statistic <- (R.u - R.r) / (1 - R.u) * (n-k-1)/q; F.statistic
iv <- ivreg2(form=hours~lnwage+educ+age+kidsl6+kids618+nwifeinc,
       data=df, digits=7, endog="educ", 
       iv=c("mothereduc", "fathereduc", "heduc", "siblings")); iv
# Here, the F-stat is 60.64 > 10 so we have really strong instruments


# Now doing individual t-tests (HELP)






## Part e) Significance Joint F-test for instrumentals for each endog variable (educ, lnwage)
n <- nrow(df); n
k <- 11
q <- 6

# Joint F-test for educ
educ.u.lm <- lm(data=df, educ ~ age + kidsl6 + kids618 + nwifeinc + 
                               exper + exper2 + mothereduc + fathereduc + 
                               heduc + siblings)
educ.r.lm <- lm(data=df, educ ~ age + kidsl6 + kids618 + nwifeinc)
Re.u <- summary(educ.u.lm)$r.sq; Re.u
Re.r <- summary(educ.r.lm)$r.sq; Re.r
F.stat <- (Re.u - Re.r)/(1 - Re.u) * (n-k-1)/q; F.stat
# Doing it the easy way
educ.ivreg <- ivreg2(form=hours~age + kidsl6 + kids618 + nwifeinc, data=df, digits=8, 
       endog="educ", iv=c("exper", "exper2", "mothereduc", "fathereduc", 
                          "heduc", "siblings"))
educ.ivreg        # see? F-stat is 41.02 (strong instruments since 41.02 > 10)


# Joint F-test for lnwage
lnwage.u.lm <- lm(data=df, lnwage ~ age + kidsl6 + kids618 + nwifeinc + 
                                 exper + exper2 + mothereduc + fathereduc + 
                                 heduc + siblings)
lnwage.r.lm <- lm(data=df, lnwage ~ age + kidsl6 + kids618 + nwifeinc)
Rw.u <- summary(lnwage.u.lm)$r.sq; Rw.u
Rw.r <- summary(lnwage.r.lm)$r.sq; Rw.r
F.stat <- (Rw.u - Rw.r)/(1 - Rw.u) * (n-k-1)/q; F.stat
# Doing it the easy way
lnwage.ivreg <- ivreg2(form=hours~age + kidsl6 + kids618 + nwifeinc, data=df, digits=8, 
                          endog="lnwage", iv=c("exper", "exper2", "mothereduc", "fathereduc", 
                                             "heduc", "siblings"))
lnwage.ivreg      # see? F-stat is 4.13 (weak instruments since 4.13 < 10)






## Part f) Hasuman test for combined: EDUC and lnwage
educ.vhat <- educ.u.lm$residuals
lnwage.vhat <- lnwage.u.lm$residuals
secondstage.H.lm <- lm(data=df, hours ~ lnwage + educ + age + kidsl6 + 
                             kids618 + nwifeinc + educ.vhat + lnwage.vhat)
summary(secondstage.H.lm)
# Both vhats are significant so endogeneity exists in both educ and lnwage

# Joint F-test
u.lm <- secondstage.H.lm
r.lm <- lm(data=df, hours ~ lnwage + educ + age + kidsl6 + 
                 kids618 + nwifeinc)
R.u <- summary(u.lm)$r.sq; R.u
R.r <- summary(r.lm)$r.sq; R.r
n <- nrow(df); n; q <- 2; k <- 9
## TODO: not quite the same as it should be: 18.25
F.stat <- (R.u - R.r)/(1 - R.u) * (n-k-1)/q; F.stat


# TODO: how to do this via ivreg?
#both.ivreg <- ivreg2(form=hours~age + kidsl6 + kids618 + nwifeinc, 
#                       data=df, digits=8, endog=c("lnwage", "educ"), 
#                       iv=c("exper", "exper2", "mothereduc", "fathereduc",
#                            "heduc", "siblings"))
#both.ivreg





## Part g) compute iv estimates of Hours with endog=educ, lnwage
educ.u.lm <- lm(data=df, educ ~ age + kidsl6 + kids618 + nwifeinc + 
                      exper + exper2 + mothereduc + fathereduc + 
                      heduc + siblings)
lnwage.u.lm <- lm(data=df, lnwage ~ age + kidsl6 + kids618 + nwifeinc + 
                        exper + exper2 + mothereduc + fathereduc + 
                        heduc + siblings)
educ.fit <- educ.u.lm$fitted.values
lnwage.fit <- lnwage.u.lm$fitted.values
secondstage.IV.lm <- lm(data=df, hours ~ lnwage.fit + educ.fit + 
                              age + kidsl6 + kids618 + nwifeinc)
summary(secondstage.IV.lm)

## TODO
# Need to use the ivreg2 function to get correct stderrors. How???





## Part h) test overidentifying restrictions (Sargan)
# H0: surplus instruments are valid
ehat <- secondstage.IV.lm$residuals
overid.lm <- lm(data=df, ehat ~ lnwage + educ + age + kidsl6 + kids618 +
                      nwifeinc + exper + exper2 + mothereduc + fathereduc + 
                      heduc + siblings)
R2 <- summary(overid.lm)$r.sq; R2   # R2 should be 0.006232
N <- nrow(df); N
NR <- N * R2; NR                    # NR2 should be 2.6673, p-value is 0.6149
L <- 6; B <- 2; DF <- L-B
X.crit <- qchisq(0.95, DF); X.crit
# So do not reject H0, so surplus instruments have no evidence of invalidity

## TODO: how to do the ivreg test to get Sargan F-test statistic???