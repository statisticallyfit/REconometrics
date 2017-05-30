setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)

# ln(area)=y, ln(price)=x (not lagged)
bangla <- read.dta("bangla.dta")
lnp <- log(bangla$p)
lna <- log(bangla$a)
banglaNA <- data.frame(lnA=lna, lnA_1=c(NA, lna[1:33]), 
                       lnP=lnp, lnP_1=c(NA, lnp[1:33]))
#banglaNA <- na.omit(banglaNA)
head(banglaNA); tail(banglaNA)  

# Estimating the ARDL(1, 1) model
ardl.lm <- lm(data=banglaNA, lnA ~ lnP + lnA_1 + lnP_1)
summary(ardl.lm)

