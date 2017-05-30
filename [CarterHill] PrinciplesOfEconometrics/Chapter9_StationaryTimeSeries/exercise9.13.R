setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


# Part a)
sales <- read.dta("ex9_13.dta")
sales
salesData <- data.frame(Date=seq(1, 157), Sales=sales$sales, Adv=sales$adv)
salesData

# how to start at Dec 28,2005 and end at Dec 25,2007?
sales.ts <- ts(sales$sales, start=2005, frequency = 52)
adv.ts <- ts(sales$adv, start=2005, frequency = 52)
autoplot(sales.ts)
autoplot(adv.ts)
ggplot(data=salesData, aes(x=Date, y=Sales)) + 
      geom_line() + 
      geom_hline(yintercept=mean(salesData$Sales), colour="blue", lwd=1)
ggplot(data=salesData, aes(x=Date, y=Adv)) +
      geom_line() + 
      geom_hline(yintercept=mean(salesData$Adv), colour="red", lwd=1)



# Part b)
a <- sales$adv
salesData <- data.frame(Date=seq(1, 157), 
                        S=sales$sales, 
                        A=a, 
                        A_1=c(NA, a[1:156]), 
                        A_2=c(NA, NA, a[1:155]), 
                        A_3=c(NA, NA, NA, a[1:154]), 
                        A_4=c(NA, NA, NA, NA, a[1:153]), 
                        A_5=c(NA, NA, NA, NA, NA, a[1:152]))
salesData <- na.omit(salesData)
head(salesData) # starting from t=6

sales.lm0 <- lm(data=salesData, S ~ A)
sales.lm1 <- lm(data=salesData, S ~ A + A_1)
sales.lm2 <- lm(data=salesData, S ~ A + A_1 + A_2)
sales.lm3 <- lm(data=salesData, S ~ A + A_1 + A_2 + A_3)
sales.lm4 <- lm(data=salesData, S ~ A + A_1 + A_2 + A_3 + A_4)
sales.lm5 <- lm(data=salesData, S ~ A + A_1 + A_2 + A_3 + A_4 + A_5)

# Calculate AIC and BIC for all the models!
## (source the Formulas.R file)

# model 0
modelCriteria(salesData$S, sales.lm0$fitted.values, 2, 0)
# model 1
modelCriteria(salesData$S, sales.lm1$fitted.values, 3, 1)
# model 2
modelCriteria(salesData$S, sales.lm2$fitted.values, 4, 2)
# model 3
modelCriteria(salesData$S, sales.lm3$fitted.values, 5, 3)
# model 4
modelCriteria(salesData$S, sales.lm4$fitted.values, 6, 4)
# model 5
modelCriteria(salesData$S, sales.lm5$fitted.values, 7, 5)


totalMultiplier(sales.lm0)
totalMultiplier(sales.lm1)
totalMultiplier(sales.lm2)
totalMultiplier(sales.lm3)
totalMultiplier(sales.lm4)
totalMultiplier(sales.lm5)

