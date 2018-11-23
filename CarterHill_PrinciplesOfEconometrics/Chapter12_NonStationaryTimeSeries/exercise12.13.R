setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
library(foreign)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) 
ukpi <- read.dta("ukpi.dta")
u <- ukpi$uk
e <- ukpi$euro
ukpi$time <- seq(1, 168)
ggplot() + 
      geom_line(data=ukpi, aes(x=time, y=euro), lwd=1, colour="blue") +
      geom_line(data=ukpi, aes(x=time, y=uk), lwd=1, colour="red")


# UK level test
# TAU = 0.9962 > -3.41 so nonstationary
ur <- ur.df(u, type="trend", lags=5); ur
autoplot(acf(ur@res))# How many augmentation terms? 



# UK first difference
autoplot(ts(du))
mean(du, na.rm = T) #constant, no trend
# TAU -13.56 < -2.86 so stationary so original is I(1)
ur <- ur.df(diff(u), type="drift", lags=0)
autoplot(acf(ur@res)) # eems more augmentations are needed, but book says nothing...


# EURO level test
autoplot(ts(e)) # trend and constant
# TAU -2.915 > -3.41 so nonstationary
ur <- ur.df(e, type="trend", lags=6); ur #so need 6 lags but still some more...
autoplot(acf(ur@res))


# EURO first diff
autoplot(ts(diff(e)))
mean(diff(e)) # constant
# TAU -11.48 < -2.86 so stationary so original is I(1)
ur <- ur.df(diff(e), type="drift", lags=0); ur
# Need more augmentation terms! What happens to the model if I include 30
# to make the autocorrelation go away? Is this overfitting?
autoplot(acf(ur@res)) 






## Part b) cointegrated?

cointegration.lm <- lm(u ~ e)
summary(cointegration.lm)
# TAU 0.179 > -3.37 so spuriously related
ur <- ur.df(cointegration.lm$residuals, type="none", lags=0); ur

# automatic test (I made)
cointegrationTest(u, e)


# Error correction model: 
u_1 <- c(NA, u[1:167])
e_1 <- c(NA, e[1:167])
de <- c(NA, diff(e))
du <- c(NA, diff(u))


# HELP - why such a different formula in solutions page 451?
past.lm <- lm(u_1 ~ e_1); summary(past.lm)
res <- c(NA, past.lm$residuals)
vec.lm <- lm(du ~ res + de)


# second model
second.lm <- lm(du ~ res + de)
summary(second.lm)



