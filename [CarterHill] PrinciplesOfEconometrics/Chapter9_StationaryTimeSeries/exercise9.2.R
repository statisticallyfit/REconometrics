setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())
library(foreign)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


# QUESTION 9.2

sales <- read.dta("ex9_2.dta")
head(sales)
salesData <- data.frame(SALES=sales$sales, ADV=sales$adv, 
                       ADV_1=c(NA, sales$adv[1:104]), 
                       ADV_2=c(NA, NA, sales$adv[1:103]))
salesData

sales.lm <- lm(data=salesData, SALES ~ ADV + ADV_1 + ADV_2)
summary(sales.lm)