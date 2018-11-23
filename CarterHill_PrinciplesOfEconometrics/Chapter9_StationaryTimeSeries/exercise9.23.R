setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
infln <- read.dta("infln_wage.dta") # from 1970 Q2 to 2010 Q1
inf.ts <- ts(infln$inf, start=1970, frequency = 4)
inf.ts <- lag(inf.ts, -1); inf.ts
wgwth.ts <- ts(infln$wgwth, start=1970, frequency = 4)
wgwth.ts <- lag(wgwth.ts, -1); wgwth.ts

autoplot(inf.ts)
autoplot(wgwth.ts)



## Part b) estimate the model
inf.lm <- lm(data=infln, inf ~ wgwth)
summary(inf.lm)

# correlogram
autoplot(acf(inf.lm$residuals, plot=FALSE)[1:22])
# LM test for AR(2) errors
bgtest(inf.lm, order=2, type="Chisq")



## Part c) estimate the model with INF_1
i <- infln$inf; length(i)
w <- infln$wgwth; length(w)
infNA_1 <- data.frame(inf=i, inf_1=c(NA, i[1:159]), wg=w)
head(infNA_1); tail(infNA_1)

inf.1.lm <- lm(data=infNA_1, inf ~ inf_1 + wg)
summary(inf.1.lm)

# correlogram
autoplot(acf(inf.1.lm$residuals, plot=FALSE)[1:22])
# LM test for AR(2) errors
bgtest(inf.1.lm, order = 2, type="Chisq") # so there is autocorrelation
# LM test for AR(3) errors
bgtest(inf.1.lm, order = 3, type="Chisq") # so there is autocorrelation



## Part d) e) AR(2) and AR(3) LM tests for INF_1,INF_2,INF_3
# model 2
infNA_2 <- infNA_1
infNA_2$inf_2 <- c(NA, NA, i[1:158])
head(infNA_2); tail(infNA_2)
inf.2.lm <- lm(data=infNA_2, inf ~ inf_1 + inf_2 + wg)
summary(inf.2.lm)

autoplot(acf(inf.2.lm$residuals, plot=FALSE)[1:21])
# LM test AR(2) errors
bgtest(inf.2.lm, order=2, type="Chisq")
# LM test AR(3) errors
bgtest(inf.2.lm, order=3, type="Chisq")

# model 3
infNA_3 <- infNA_2
infNA_3$inf_3 <- c(NA, NA, NA, i[1:157])
head(infNA_3); tail(infNA_3)
inf.3.lm <- lm(data=infNA_3, inf ~ inf_1 + inf_2 + inf_3 + wg)
summary(inf.3.lm)

autoplot(acf(inf.3.lm$residuals, lag.max=100, plot=FALSE)[1:30])
# LM test AR(2) errors
bgtest(inf.3.lm, order=2, type="Chisq")
# LM test AR(3) errors
bgtest(inf.3.lm, order=3, type="Chisq")

# Conclusion: adding INF_2 does nothing to help serial correlation. 
# But adding INF_3 removes serial correlation at all lags

# this is the significance bound ........
1.96/sqrt(160)



## Part f) dropping inf_2
summary(lm(data=infNA_3, inf ~ inf_1 + inf_3 + wg))
# adding wg_1 raises AIC and BIC (not good) and error serial correlation