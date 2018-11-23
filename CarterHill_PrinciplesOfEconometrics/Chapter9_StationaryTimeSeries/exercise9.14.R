setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)

# ln(area)=y, ln(price)=x (lagged)
bangla <- read.dta("bangla.dta")
lnp <- log(bangla$p)
banglaData <- data.frame(lnA=log(bangla$a), 
                         lnP=lnp, 
                         lnP_1=c(NA, lnp[1:33]), 
                         lnP_2=c(NA, NA, lnp[1:32]), 
                         lnP_3=c(NA, NA, NA, lnp[1:31]), 
                         lnP_4=c(NA, NA, NA, NA, lnp[1:30]))
banglaData <- na.omit(banglaData)
head(banglaData)

area.ts <- ts(bangla$a)
autoplot(area.ts)
lnArea.ts <- ts(log(bangla$a))
autoplot(lnArea.ts)


# Estimate the model
bangla.lm <- lm(data=banglaData, lnA ~ lnP + lnP_1 + lnP_2 + lnP_3 + lnP_4)
summary(bangla.lm)



## Part b)
zt0 <- banglaData$lnP + banglaData$lnP_1 + banglaData$lnP_2 +
      banglaData$lnP_3 + banglaData$lnP_4
zt1 <- banglaData$lnP_1 + 2*banglaData$lnP_2 + 3*banglaData$lnP_3 +
      4*banglaData$lnP_4
zt0; zt1

alpha.lm <- lm(data=banglaData, lnA ~ zt0 + zt1)
summary(alpha.lm)
summary(bangla.lm)
