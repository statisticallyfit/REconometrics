setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a)
texas <- read.dta("texas.dta")
autoplot(ts(texas$txnag))     # trend
autoplot(ts(texas$usnag))     # trend

t <- texas$txnag
t_1 <- c(NA, t[1:56]); length(t_1)
dt <- c(NA, diff(t)); length(dt)
dt_1 <- c(NA, dt[1:56]); length(dt_1)
trend <- seq(1:57)
t.lm <- lm(dt ~ t_1 + trend + dt_1)
autoplot(acf(t.lm$residuals, lag.max=20, plot=F))
# but it seems no augmentation is needed -- wrong regression to check in?

u <- texas$usnag
u_1 <- c(NA, u[1:56])
du <- c(NA, diff(u))
du_1 <- c(NA, du[1:56])
u.lm <- lm(du ~ u_1 + trend + du_1)
autoplot(acf(u.lm$residuals, lag.max=20, plot=F))   
# but it seems no augmentation is needed - is this the wrong regression?



# Doing dickey fuller test with trend and 1 augmentation
dickeyFullerTest(t, useTrend=TRUE, k=1) # nonstationary
dickeyFullerTest(u, useTrend=TRUE, k=1) # nonstationary






## Part b) check stationarity of the first differences
autoplot(ts(diff(t)))    # around constant
autoplot(ts(diff(u)))    # around constant
dickeyFullerTest(diff(t), useTrend = FALSE, k = 0) # nonstationary
dickeyFullerTest(diff(u), useTrend = FALSE, k = 0) # nonstationary





## Part c) cointegrated? Assume TXNAG and USNAG are I(1) unlike above
# (1) estimate the regression 
cointegrating.lm <- lm(t ~ u)
summary(cointegrating.lm)
# (2) dickey fuller for residuals stationarity
autoplot(ts(cointegrating.lm$residuals))    # is this a trend?
e <- cointegrating.lm$residuals; length(e)
de <- c(NA, diff(e))
e_1 <- c(NA, e[1:56])
de_1 <- c(NA, diff(de)); length(de_1)
e.lm <- lm(de ~ e_1)
# TODO: why include just one lagged diff? It needs 6 (?)
# TODO why when including de_1, acfs are the same??
autoplot(acf(e.lm$residuals)) 

mean(cointegrating.lm$residuals) # very close to zero so use dickeyFuller with
# no constant

# TODO: how to use no constant??
# their tau = -0.811 > -3.37 so nonstationary resids so 
# cointegration.lm is a spurious regression
dickeyFullerTest(cointegrating.lm$residuals, useTrend=FALSE, k = 1)






## Part d) are DU and DT related?
dudt.lm <- lm(dt ~ du)
summary(dudt.lm)  # p-value of du coeff is significant so they are related





## Part e) 
# why is cointegration long-run and regression short-run?
