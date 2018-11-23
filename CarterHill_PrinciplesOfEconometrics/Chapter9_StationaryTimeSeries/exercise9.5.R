setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")

# QUESTION 9.5 (correlogram for 5.a)
growth <- read.dta("growth47.dta")
growth
growth.ts <- ts(growth, start=1947, frequency = 4)
growth.ts <- lag(growth.ts, -1)
growth.ts

autoplot(growth.ts)
autoplot(acf(growth.ts, plot = FALSE))
