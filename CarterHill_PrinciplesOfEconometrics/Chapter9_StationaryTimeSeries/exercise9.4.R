setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")

# QUESTION 9.4 (correlogram from 4.b)
e <- c(0.28, -0.31, -0.09, 0.03, -0.37, -0.17, -0.39, -0.03, 0.03, 1.02)

e.ts <- ts(e, start=1, frequency = 1)
autoplot(e.ts)
autoplot(acf(e.ts, plot=FALSE)) # correlogram
e.acf <- acf(e.ts, plot = FALSE)
e.acf # checking r1, and r2



