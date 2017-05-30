setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
homes <- read.dta("homes.dta")
h <- homes$homes
i <- homes$irate
dh <- diff(h); length(dh)
di <- diff(i); length(di)
homesNA_5_3 <- data.frame(DHOMES=dh, DIRATE=di, 
                          DHOMES_1=c(NA, dh[1:217]), 
                          DHOMES_5=c(NA, NA, NA, NA, NA, dh[1:213]),
                          DIRATE_1=c(NA, di[1:217]), 
                          DIRATE_2=c(NA, NA, di[1:216]), 
                          DIRATE_3=c(NA, NA, NA, di[1:215]))
head(homesNA_5_3); tail(homesNA_5_3)

autoplot(ts(h), main = "HOMES") # trending
autoplot(ts(i), main = "IRATE") # trending
autoplot(ts(dh), main="DHOMES") # more stationary
autoplot(ts(di), main="DIRATE") # more stationary

dhomes.lm <- lm(data=homesNA_5_3, DHOMES ~ DHOMES_1 + DIRATE_1 + DIRATE_2)
summary(dhomes.lm)



## Part d)  correlogram of residuals
acfs <- acf(dhomes.lm$residuals, plot=FALSE); acfs
autoplot(acfs[2:23])


## Part e) LM test for error autocorrelation
e <- dhomes.lm$residuals; length(e)
errorsNA_2 <- data.frame(E=c(NA, NA, e), 
                         E_1=c(NA, NA, NA, e[1:215]), 
                         E_2=c(NA, NA, NA, NA, e[1:214]), 
                         DHOMES_1=homesNA_5_3$DHOMES_1, 
                         DIRATE_1=homesNA_5_3$DIRATE_1, 
                         DIRATE_2=homesNA_5_3$DIRATE_2)
head(errorsNA_2); tail(errorsNA_2)

lagrange.lm <- lm(data=errorsNA_2, E ~ E_1 + E_2 + DHOMES_1 + DIRATE_1 + DIRATE_2)
s <- summary(lagrange.lm); s
R2 <- s$r.squared; R2
T <- nrow(na.omit(errorsNA_2)); T
LM <- T*R2; LM    # TODO: LM should be 4.8536
1 - pchisq(LM, 2) # TODO: p-value should be 0.0883



## Part f) estimate ARDL(5, 3)  model - is it better?
dhomes.5.3.lm <- lm(data=homesNA_5_3, DHOMES ~ DHOMES_1 + DHOMES_5 + 
                          DIRATE_1 + DIRATE_3)
summary(dhomes.5.3.lm)

# Correlogram of residuals
acfs <- acf(dhomes.5.3.lm$residuals, plot=FALSE); acfs
autoplot(acfs[2:23])


# AIC/BIC for ARDL(5,3)
modelCriteria(dh[6:218], dhomes.5.3.lm$fitted.values, 5, 5) # actually, lag is 5,3 ??
# AIC/BIC for AR(1,2)
modelCriteria(dh[3:218], dhomes.lm$fitted.values, 4, 2) # actually, 1,2 lags

# The AIC/BIC are lower for ARDL(5,3)
