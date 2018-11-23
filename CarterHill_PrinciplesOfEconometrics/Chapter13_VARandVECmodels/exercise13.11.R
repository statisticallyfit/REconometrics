setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part b)  estimate VEC
sterling <- read.dta("sterling.dta")
sterling <- subset(sterling, select=c("sterling", "euro"))
sterling$time <- seq(1, 96)

# (1) Check EURO and STERLING are nonstationary
ggplot() + 
      geom_line(data=sterling, aes(x=time, y=sterling), lwd=1, colour="purple") + 
      geom_line(data=sterling, aes(x=time, y=euro), lwd=1, colour="salmon")

mean(sterling$euro) # so use constant and trend
e.df <- ur.df(sterling$euro, type="trend", lags = 0); e.df  #nonstationary
autoplot(acf(e.df@res, lag.max = 20, plot=FALSE))
s.df <- ur.df(sterling$sterling, type="trend", lags = 0); s.df  #nonstationary
autoplot(acf(s.df@res, lag.max = 20, plot=FALSE))


# (2) Check for cointegration
# seems to be a constant space between them so use "constant"
dickeyFuller.res <- cointegrationTest(sterling, type="constant", resid.lags=0)

# no autocorr in resids, so lag=0 is fine!
autoplot(acf(dickeyFuller.res, lag.max = 20, plot = FALSE))

# (3) Build a VEC model using diffed variables (since they are I(1))
sterling.diff <- data.frame(ds=diff(sterling$sterling), de=diff(sterling$euro))
vec.res <- VEC(sterling.diff, type="none", lag=1)
autoplot(acf(vec.res$y, lag.max = 20, plot=FALSE))
autoplot(acf(vec.res$x, lag.max = 20, plot=FALSE))


v <- suppressWarnings(VECM(ts(sterling.diff), lag=1, 
                           r=1, include="none", estim="2OLS"))
summary(v)

head(v$residuals)
## change chointeTest to return cointegration lm residuals to build my own VEC
cointeg.res <- cointegrationTest(sterling, type="constant", resid.lags=0)
e_1 <- cointeg.res[1:(length(cointeg.res) - 1)]; length(e_1)
ds <- sterling.diff$ds; length(ds)
de <- sterling.diff$de
ds_1 <- c(NA, ds[1:94])
de_1 <- c(NA, de[1:94])
vec.ds.lm <- lm(data=sterling.diff, ds ~ e_1 + ds_1 + de_1 + 0)
vec.de.lm <- lm(data=sterling.diff, de ~ e_1 + ds_1 + de_1 + 0)
# Both coeffs are negative --- why???
# Coeff on EURO eq is not significant, showing that Sterling shifts to 
# restore equilibrium, not EURO. ...
vec.ds.lm
vec.de.lm


## TODO - why does doing it manually give different answer than VECM? 





## Part c) Estimate a VAR model 
# THe lag terms aren't significant so don't include them!
var <- VAR(sterling.diff, p=2, type="const")
summary(var)
autoplot(acf(var$varresult$ds$residuals, lag.max = 20, plot = FALSE))
autoplot(acf(var$varresult$de$residuals, lag.max = 20, plot = FALSE))

# The intercepts aren't significant, so remove them!
var <- VAR(sterling.diff, p=1, type="const")
summary(var)

# This seems best
var <- VAR(sterling.diff, p=1, type="none")
summary(var)
autoplot(acf(var$varresult$ds$residuals, lag.max = 20, plot = FALSE))
autoplot(acf(var$varresult$de$residuals, lag.max = 20, plot = FALSE))
