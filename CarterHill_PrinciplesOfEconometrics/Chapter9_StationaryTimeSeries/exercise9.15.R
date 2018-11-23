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
banglaData <- data.frame(lnA=lna, lnP=lnp)
head(banglaData); tail(banglaData)

bangla.lm <- lm(data=banglaData, lnA ~ lnP)
summary(bangla.lm)

autoplot(acf(bangla.lm$residuals, lag.max=34, plot=FALSE))

# creating data.frame with NA
e <- bangla.lm$residuals
lnp <- banglaData$lnP  
lna <- banglaData$lnA
banglaNA <- data.frame(lnA=lna, lnA_1=c(NA, lna[1:33]), 
                          lnP=lnp, lnP_1=c(NA, lnp[1:33]), 
                          E=e, E_1=c(NA, e[1:33]))
#banglaNA <- na.omit(banglaNA)
head(banglaNA); tail(banglaNA)      

banglaZero <- data.frame(lnA=lna, lnA_1=c(0, lna[1:33]), 
                         lnP=lnp, lnP_1=c(0, lnp[1:33]), 
                         E=e, E_1=c(0, e[1:33]))
head(banglaZero); tail(banglaZero)


# Part b) LM test of autocorrelation (joint test of acf values)

# method (2) way
lagrange.lm <- lm(data=banglaZero, E ~ lnP + E_1)
summary(lagrange.lm)
# calculate R^2 by hand
sse <- sum( (banglaZero$E - lagrange.lm$fitted.values)^2 )
sst <- sum( (banglaZero$E - mean(banglaZero$E))^2  )
R2 <- 1 - sse/sst; R2
T <- 34
LM <- T * R2; LM
# now compare to chi-statistic and find p-value
# df for chi statistic is = number of lagged residuals that are included
df <- 1
chi.star <- qchisq(0.95, df); chi.star
p.value <- 1 - pchisq(LM, df); p.value   # reject the null! there is autocorrelation



# Part c) calculating HAC stderrors for confidence intervals

### ??? how to do it if you have only 1 predictor??? cannot use examples12.R
### (wooldridge) method


## TODO: did not work, estimates are not the same as in book!
# Part d) estimating an AR(1) model


## +++++ Wooldridge steps
# step 1
head(banglaData)
reg.lm <- lm(data=banglaData, lnA ~ lnP)
summary(reg.lm)
# step 2
e <- reg.lm$residuals; e; length(e)
errorNA_1 <- data.frame(E=e, E_1=c(NA, e[1:33]))
head(errorNA_1); tail(errorNA_1)
err.lm <- lm(data=errorNA_1, E ~ E_1); 
sumry <- summary(err.lm)
p <- sumry$coefficients[2]; p  # todo: p supposed to be 0.422 using wooldridge method
# step 3
head(banglaNA)
lnaDiffP <- banglaNA$lnA - p*banglaNA$lnA_1; lnaDiffP
lnpDiffP <- banglaNA$lnP - p*banglaNA$lnP_1; lnpDiffP
lm.3 <- lm(lnaDiffP ~ lnpDiffP)
summary(lm.3) # B2 should be -0.6944 not -0.69166
#++++++




# Part d), estimating an ARDL(1,1) model
ardl.lm <- lm(data=banglaNA, lnA ~ lnP + lnA_1 + lnP_1)
summary(ardl.lm)
head(banglaNA)


# Part e), are errors from ARDL(1, 1) serially correlated?
# Correlogram
autoplot(acf(ardl.lm$residuals, lag.max=34, plot=FALSE)) # nope...

# LM test
e <- ardl.lm$residuals; e; length(e)
banglaNA_1 <- banglaData
lna <- banglaData$lnA; lna
lnp <- banglaData$lnP; lnp
banglaNA_1$lnA_1 <- c(NA, lna[1:33])
banglaNA_1$lnP_1 <- c(NA, lnp[1:33])
banglaNA_1$E <- c(NA, e)
banglaNA_1$E_1 <- c(NA, NA, e[1:32])
head(banglaNA_1); tail(banglaNA_1)

lagrange.lm <- lm(data=banglaNA_1, E ~ lnP + E_1)
summary(lagrange.lm)
nona <- na.omit(banglaNA_1)
T <- nrow(nona); T
sse <- sum( (nona$E - lagrange.lm$fitted.values)^2 ); sse
sst <- sum( (nona$E - mean(nona$E))^2 ); sst
R2 <- 1 - sse/sst; R2
anova(lagrange.lm)
LM <- T * R2; LM
1 - pchisq(LM, 1)

# TODO: this pvalue is not the same as in book (0.423) - find out what's wrong
