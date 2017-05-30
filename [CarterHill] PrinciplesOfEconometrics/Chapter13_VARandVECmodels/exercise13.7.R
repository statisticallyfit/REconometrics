setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) - estimate a VEC model
vec <- read.dta("vec.dta")
vec <- data.frame(y=vec$y, x=vec$x)
cointegrationTest(vec, type="none")
res <- VEC(vec, type="none")
# the residuals should not be autocorrelated - they're not!
autoplot(acf(res$y, lag.max = 20, plot=FALSE), main="y ACFs")
autoplot(acf(res$x, lag.max = 20, plot=FALSE), main="x ACFs")

lm <- lm(data=vec, y ~ x + 0)
df <- data.frame(res=lm$residuals, time=seq(1,100))
# The cointegrating error
ggplot(data=df, aes(x=time, y=res)) + geom_line(lwd=1)

# Why does the cointegrating error need to be corrected by the VEC ? 