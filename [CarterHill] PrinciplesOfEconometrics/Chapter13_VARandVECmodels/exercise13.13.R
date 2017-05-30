setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part b) is there a long run relationship?   (cointegrating)
gfc <- read.dta("gfc.dta")
gfc$time <- seq(1, 60)

# there seems to be constant to so test "constant"
# y = leuro, x = lusa (why?)
cointegrationTest(gfc, type="constant", resid.lags = 0)



## Part c) So no long run (cointegrating). Is there short run relation (VAR) ?
gfc.diff <- data.frame(dleuro=diff(gfc$leuro), dlusa=diff(gfc$lusa))

# RESULT: constant is significant so keep it
var <- VAR(gfc.diff, type="const", p=1)
var
summary(var)

# RESULT: not all second lags are significant, so just use p =1
var <- VAR(gfc, type="const", p=2)
var
summary(var)

# FINAL MODEL
var <- VAR(gfc.diff, type="const", p=1)
var
summary(var)

# No serial correlation confirms that p = 1 and not p > 1...
e.res <- var$varresult$dleuro$residuals
u.res <- var$varresult$dlusa$residuals
df.res <- data.frame(t=seq(1, 58), e.res=e.res, u.res=u.res)

autoplot(acf(var$varresult$dleuro$residuals, lag.max = 20, plot = FALSE))
autoplot(acf(var$varresult$dlusa$residuals, lag.max = 20, plot = FALSE))

ggplot() + 
      geom_line(data=df.res, aes(x=t, y=e.res), lwd=1, colour="red") +
      geom_line(data=df.res, aes(x=t, y=u.res), lwd=1, colour="blue")
autoplot(ts(e.res), size=1, colour="red")
autoplot(ts(u.res), size=1, colour="blue")
