setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
library(foreign)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a)
usa <- read.dta("usa.dta")
autoplot(ts(usa$gdp))   # constant with trend
autoplot(ts(usa$inf))   # constant NO trend

#  GDP - find out how many augmented terms to use 
g <- usa$gdp
t <- seq(1, 104)
g_1 <- c(NA, g[1:103])
dg <- c(NA, diff(g))
ddg <- c(NA, NA, diff(diff(g)))
dg_1 <- c(NA, dg[1:103])

# TRY 1
dg.lm <- lm(dg ~ g_1 + t)
autoplot(acf(dg.lm$residuals), lag.max=20, plot=FALSE) # still some autocorrs
# TRY 2
dg.lm <- lm(dg ~ g_1 + t + dg_1)
summary(dg.lm)
autoplot(acf(dg.lm$residuals), lag.max=20, plot=FALSE) # perfect



#  INF - find out how many augmented terms to use 
i <- usa$inf
i_1 <- c(NA, i[1:103])
di <- c(NA, diff(i))
ddi <- c(NA, NA, diff(diff(i)))
di_1 <- c(NA, di[1:103])
di_2 <- c(NA, NA, di[1:102])
di_3 <- c(NA, NA, NA, di[1:101])
di_4 <- c(NA, NA, NA, NA, di[1:100])
di_5 <- c(NA, NA, NA, NA, NA, di[1:99])
di_6 <- c(NA, NA, NA, NA, NA, NA, di[1:98])
di_7 <- c(NA, NA, NA, NA, NA, NA, NA, di[1:97])
di_8 <- c(NA, NA, NA, NA, NA, NA, NA, NA, di[1:96])

# TRY 1
di.lm <- lm(di ~ i_1)
autoplot(acf(di.lm$residuals), lag.max=20, plot=FALSE) # still some autocorrs
# TRY 2
di.lm <- lm(di ~ i_1 + di_1)
autoplot(acf(di.lm$residuals), lag.max=20, plot=FALSE) # still some autocorrs
# TRY 3
di.lm <- lm(di ~ i_1 + di_1 + di_2)
autoplot(acf(di.lm$residuals), lag.max=20, plot=FALSE) # still some autocorrs
# TRY 4
di.lm <- lm(di ~ i_1 + di_1+di_2+di_3+di_4+di_5+di_6+di_7+di_8)
autoplot(acf(di.lm$residuals), lag.max=80, plot=FALSE) # still some autocorrs
summary(di.lm)


## NOTE: you include as many lags as it takes residuals to not be
# autocorrelated (to not cross the significance line)


## TODO: why are taus different?
# Tau should be -1.961 > -3.41 --> nonstationary
dickeyFullerTest(usa$gdp, useTrend = TRUE, k = 1) 
# TAU should be -1.35 > -2.86 ---> nonstationary
dickeyFullerTest(usa$inf, useTrend = FALSE, k = 8)








## Part b) what is order of integration?
mean(dg, na.rm=T)
autoplot(ts(dg))   # --> floating around constant

mean(di, na.rm=T)  # --> no constant
autoplot(ts(di))


# GDP - check for how many augmentations
ddg.lm <- lm(ddg ~ dg)
autoplot(acf(ddg.lm$residuals, lag.max=20, plot=FALSE)) # so k = 0

# Their TAU = -5.228
dickeyFullerTest(diff(diff(g)), useTrend=FALSE, k = 0)


# INF - check for how many augmentation terms
ddi.lm <- lm(ddi ~ di + di_1)
autoplot(acf(ddi.lm$residuals, lag.max=20, plot=FALSE)) # so k = 1
# TODO - why does book say 7 terms since 1 is enough... ???

# Their TAU = -4.627 ... ??
dickeyFullerTest(diff(diff(i)), useTrend = F, k = 7)







## Part c) forecast and predict

# Why do the equations need to be estimated using G_1 = 0 and I_1 = 0 ? 