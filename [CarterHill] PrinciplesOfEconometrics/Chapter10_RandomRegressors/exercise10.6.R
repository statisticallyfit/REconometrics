setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
ivreg <- read.dta("ivreg2.dta")
head(ivreg)
B1 <- 3; B2 <- 1
x <- ivreg$x  # rnorm(mean=0, sd=2), e = rnorm(mean=0, sd=1)
y <- ivreg$y  # = 3 + x + e
covXE <- 0.9
varX <- 2; varE <- 1
corXE <- covXE / sqrt(varX * varE); corXE
# this is the population correlation




## Part b)
e <- y - B1 - B2*x; e
cor(x, e)
# this is the sample correlation (? why)




## Part c) plot Ey = 3 + x, against y and x
y.true <- B1 + B2*x
df <- data.frame(x=x, y=y, Ey=y.true)
head(df)
# Data tends to fall above Ey (which is y.true) for x > 0 and below for x<0
ggplot(data=df, aes(x=x, y=y)) + 
      geom_point(shape=19) + 
      geom_line(data=df, aes(y=Ey), lwd=2, colour="blue")




## Part d) estimate in incremental samples
summary(lm(data=ivreg[1:10,], y ~ x))$coef
summary(lm(data=ivreg[1:20,], y ~ x))$coef
summary(lm(data=ivreg[1:100,], y ~ x))$coef
summary(lm(data=ivreg[1:500,], y ~ x))$coef
# The estimates do not get much closer to the true values B1=3 and B2=1
# because of the correlation between x and e




## Part e)
cor.df <- data.frame(z1=ivreg$z1, z2=ivreg$z2, x=x, e=e)
cor(cor.df)
# z1 is better as instrumental variable since correlation between
# x and z1 (0.6208) is greater than that between x and z2 (0.289)




## Part f) estimating IV estimates using N=10,20,50,500 with z1 instrument
ivreg2(form=y ~ x, data=ivreg[1:10,], endog="x", iv=c("z1"), digits=10)$results
ivreg2(form=y ~ x, data=ivreg[1:20,], endog="x", iv=c("z1"), digits=10)$results
ivreg2(form=y ~ x, data=ivreg[1:100,], endog="x", iv=c("z1"), digits=10)$results
ivreg2(form=y ~ x, data=ivreg[1:500,], endog="x", iv=c("z1"), digits=10)$results
# The IV estimates (slopes) are getting closer to the true values
# as sample size grows, showing that IV estimator is consistent

## TODO: what does it mean 'consistent'? How does consistency overcome
# correlation between x and e?




## Part g) estimating IV estimates as above with z2 instrument
ivreg2(form=y ~ x, data=ivreg[1:10,], endog="x", iv=c("z2"), digits=10)$results
ivreg2(form=y ~ x, data=ivreg[1:20,], endog="x", iv=c("z2"), digits=10)$results
ivreg2(form=y ~ x, data=ivreg[1:100,], endog="x", iv=c("z2"), digits=10)$results
ivreg2(form=y ~ x, data=ivreg[1:500,], endog="x", iv=c("z2"), digits=10)$results
# When sample size is low, estimates are far away from true values, because
# z2 is less correlated to x than is z1





## Part h) using z1 and z2
ivreg2(form=y~x, data=ivreg[1:10,], endog="x", iv=c("z1","z2"), digits=10)$results
ivreg2(form=y~x, data=ivreg[1:20,], endog="x", iv=c("z1","z2"), digits=10)$results
ivreg2(form=y~x, data=ivreg[1:100,], endog="x", iv=c("z1","z2"), digits=10)$results
ivreg2(form=y~x, data=ivreg[1:500,], endog="x", iv=c("z1","z2"), digits=10)$results
# Improvement for samples 100 and 500 over just using z1
