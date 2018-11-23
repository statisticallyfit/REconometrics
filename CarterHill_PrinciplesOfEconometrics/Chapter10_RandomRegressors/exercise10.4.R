setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a) b)
ivreg <- read.dta("ivreg1.dta")
x <- ivreg$x
e <- ivreg$e
meanX <- mean(x); meanX
meanE <- mean(e); meanE
y <- 1 + x + e
y.true <- 1 + x
y.fit <- lm(data=ivreg, y ~ x)
summary(y.fit)
df <- data.frame(y=y, yhat=y.fit$fitted.values, x=x, Ey=y.true)
ggplot(data=df, aes(x=x, y=y)) + 
      geom_point(shape=19) + 
      geom_line(data=df, aes(y=y.true), colour="blue", lwd=1)
# Observation: the data fall below regression line for x < 0 and above it
# for x > 0

# Part c)
summary(y.fit)
# The estimates are very close to the true parameters B1 = 1, B2 = 1
coefs <- summary(y.fit)$coeff; coefs
b1 <- coefs[1,1]; b1
s1 <- coefs[1,2]; s1
b2 <- coefs[2,1]; b2
s2 <- coefs[2,2]; s2
t1 <- (b1 - 1)/s1; t1
t2 <- (b2 - 1)/s2; t2
p1 <- 1 - pt(t1, df=74); p1
p2 <- 1 - pt(t2, df=74); p2



## Part d) plot y with yhat
head(df)
# Runs through center of correlated observations, so not a good estimation
# of the y.true regression function
ggplot(data=df, aes(x=x, y=y)) + 
      geom_point(shape=19) + 
      geom_line(data=df, aes(y=yhat), colour="blue", lwd=1)


## Part e) correlation matrix of x, e, ehat
dfCors <- data.frame(x=x, e=e, ehat=y.fit$residuals)
head(dfCors)
cor(dfCors)
# It says it is a characteristic of least squares, which makes zero
# correlation between x and e-hat. Understand better why.