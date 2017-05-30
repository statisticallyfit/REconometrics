setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Using just mother educ as instrumental variable
mroz <- read.dta("mroz.dta")
mroz <- mroz[1:428, ]

cor(mroz$mothereduc, mroz$educ) # see? positive correlation -> endogeneity

lnw <- log(mroz$wage)
exp <- mroz$exper; exp2 <- exp^2
original.lm <- lm(data=mroz, lnw ~ educ + exp + exp2)

firststage.m.lm <- lm(data=mroz, educ ~ exp + exp2 + mothereduc)

educFits <- firststage.m.lm$fitted.values; length(educFits)
secondstage.m.lm <- lm(lnw ~ educFits + exp + exp2)

# Since there was positive correlation, the previous original.lm overestimates
# the slope on EDUC, and this real one in secondstage.lm is lower. 
# Also the stderror of EDUC below is 2.5 times larger than in original.lm
# which shows that the IV/2SLS estimator is not efficient. 
summary(original.lm)
summary(firststage.m.lm)
# These are the IV estimates
summary(secondstage.m.lm)





## Adding father's educ as instrumental variable
# educ = endogeneous

firststage.fm.lm <- lm(data=mroz, educ ~ exp + exp2 + mothereduc + fathereduc)
summary(firststage.fm.lm)

educFits <- firststage.fm.lm$fitted.values
secondstage.fm.lm <- lm(lnw ~ educFits + exp + exp2 )
summary(secondstage.m.lm)
# These are the IV estimates
summary(secondstage.fm.lm)

# Doing F-test for instrument weakness
# Method 1
R2.u <- summary(lm(data=mroz, educ ~  exp + exp2 + mothereduc + fathereduc))$r.squared
R2.r <- summary(lm(data=mroz, educ ~ exp + exp2))$r.squared
R2.u; R2.r
F.stat <- (R2.u - R2.r)/(1 - R2.u) * (423/2); F.stat
F.crit <- qf(0.95, df1=423, df2=2); F.crit
# Method 2
u.lm <- lm(data=mroz, educ ~  exp + exp2 + mothereduc + fathereduc)
r.lm <- lm(data=mroz, educ ~  exp + exp2)
anova(r.lm, u.lm)




## Partialling out (partial correlation)
educ.lm <- lm(data=mroz, educ ~ exp + exp2) 
# the residuals from educ.lm are EDUC with EXPER and EXPER^2 effects removed
resE <- educ.lm$residuals
meduc.lm <- lm(data=mroz, mothereduc~ exp + exp2)
resM <- meduc.lm$residuals
resEM.lm <- lm(resE ~ resM)
resEM.lm$coefficients # coef on mothereduc is same as in firststage.m.lm
# Partial correlation coefficient is defined below: 
cor(resE, resM) # partial since effects of other variables are netted out. 




## Hausman Test for endogeneity (wage equation)
firststage.fm.lm <- lm(data=mroz, educ ~ exp + exp2 + mothereduc + fathereduc)
summary(firststage.fm.lm)
vhat <- firststage.fm.lm$residuals
# Since each instrument (mothereduc, fathereduc) is uncorrelated with the 
# original regression error EHAT, then the endogeneous variable (educ) is
# uncorrelated with EHAT only if VHAT is uncorrelated with EHAT.
# So we are testing the significance of coefficient on VHAT.
secondstage.hausman.lm <- lm(data=mroz, lnw ~ educ + exp + exp2 + vhat)
# NOTE: these estimates are identical to instrumental variable estimates from
# secondstage.fm.lm IV estimation
summary(secondstage.fm.lm)
summary(secondstage.hausman.lm)
# p-value from coefficient on vhat is not very significant but still concerning: 
# so EDUC is somewhat endogeneous (correlated with error)




## Sargan Test for instrumental validity
secondstage.fm.lm <- lm(lnw ~ educFits + exp + exp2 )
summary(secondstage.fm.lm)
ehat <- secondstage.fm.lm$residuals
secondstage.sargan.lm <- lm(data=mroz, ehat ~ mothereduc + fathereduc + exp + exp2)
s <- summary(secondstage.sargan.lm); s
R2 <- s$r.squared; R2
N <- nrow(mroz); N
NRsq <- N*R2; NRsq      # TODO: R2 should be 0.000883 - why not the same?
chi.crit <- qchisq(0.95, df=1); chi.crit

# H0: surplus instrument is valid, H1: it is not, so fail to reject it is valid. 
# so we are reassured that our instrumental variables estimator for wage equation
# is consistent. 


# Not quite the same... (source the Formulas.R file)
ivreg2(form=lnw~exp + exp2 + educ, endog="educ", 
      iv=c("mothereduc", "fathereduc"), data=na.omit(mroz))
