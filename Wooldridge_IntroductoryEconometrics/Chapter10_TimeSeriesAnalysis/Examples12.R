setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Wooldridge_Introductory Econometrics: A Modern Approach/Chapter10_TimeSeriesAnalysis")
rm(list=ls())

library('foreign')
library(ggfortify)


# Example 12.7
prminwge <- read.dta('prminwge.dta')

lm.10.3 <- lm(lprepop ~ lmincov + lusgnp + lprgnp + t, data=prminwge)
summary(lm.10.3)

# Finding HAC stderrors
x.lm <- lm(data=prminwge, lmincov ~ lusgnp + lprgnp + t)
rt <- x.lm$residuals #; length(rt)
ut <- lm.10.3$residuals
at.1 <- rt*ut
at.2 <- rt[2:38] * ut[2:38]
at.3 <- rt[3:38] * ut[3:38]
at_1 <- rt[1:37] * ut[1:37]
at_2 <- rt[1:36] * ut[1:36]
v <- sum(at.1^2) + (4/3)*sum(at.2*at_1) + (2/3)*sum(at.3*at_2); v
sumry <- summary(lm.10.3); sumry #using the residstderror from this regression
stdResid <- 0.0328
stdOLS <- sumry$coefficients[2,2]; stdOLS
stdHAC <- (stdOLS / stdResid)^2 * sqrt(v); stdHAC
