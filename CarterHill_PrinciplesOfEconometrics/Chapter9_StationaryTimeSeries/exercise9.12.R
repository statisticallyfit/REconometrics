setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())
library(foreign)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


okun <- read.dta("okun.dta")
okun

g <- okun$g
okunDataNA <- data.frame(DU=c(NA, diff(okun$u)), 
                       G=g,  # G is same as G_0
                       G_1=c(NA, g[1:97]), 
                       G_2=c(NA, NA, g[1:96]), 
                       G_3=c(NA, NA, NA, g[1:95]),
                       G_4=c(NA, NA, NA, NA, g[1:94]),
                       G_5=c(NA, NA, NA, NA, NA, g[1:93]),
                       G_6=c(NA, NA, NA, NA, NA, NA, g[1:92]))
okunDataNA
okunData <- na.omit(okunDataNA)

# Make the models
okun.lm0 <- lm(data=okunData, DU ~ G)
okun.lm1 <- lm(data=okunData, DU ~ G + G_1)
okun.lm2 <- lm(data=okunData, DU ~ G + G_1 + G_2)
okun.lm3 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3)
okun.lm4 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4)
okun.lm5 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4 + G_5)
okun.lm6 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4 + G_5 + G_6)

# View model coefficients
okun.lm0
okun.lm1
okun.lm2
okun.lm3
okun.lm4
okun.lm5
okun.lm6


# Calculate AIC and BIC for all the models!

## SSE = sum(obs - pred)^2
## AIC = ln(SSE/N) + 2K/N
## BIC = ln(SSE/N) + Kln(N)/N

modelCriteria <- function(y, yhat, K, lag) {
      sse <- sum( (y - yhat)^2 )
      N <- length(y) # Must equal length of yhat
      aic <- log(sse/N) + 2*K/N
      bic <- log(sse/N) + K*log(N)/N
      aicc <- -(abs(aic) - 1 - log(2*pi))
      bicc <- -(abs(bic) - 1 - log(2*pi))
      cat("\nAIC:  ", aic)
      cat("\nAICc: ", aicc)
      cat("\nBIC:  ", bic)
      cat("\nBICc: ", bicc, "\n ")
      criteriaAndLag <- c(aic, bic, lag)
      return(invisible(criteriaAndLag)) # yaya this works!!!
}

# model 0
modelCriteria(okunData$DU, okun.lm0$fitted.values, 2, 0)
# model 1
modelCriteria(okunData$DU, okun.lm1$fitted.values, 3, 1)
# model 2
modelCriteria(okunData$DU, okun.lm2$fitted.values, 4, 2)
# model 3
modelCriteria(okunData$DU, okun.lm3$fitted.values, 5, 3)
# model 4
modelCriteria(okunData$DU, okun.lm4$fitted.values, 6, 4)
# model 5
modelCriteria(okunData$DU, okun.lm5$fitted.values, 7, 5)
# model 6
values <- modelCriteria(okunData$DU, okun.lm6$fitted.values, 8, 6)


## returns a vector of 2 elements that show the lags where AIC and BIC are minimum
# minimizedCriteria <- function()
