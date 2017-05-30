setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
growth <- read.dta("growth47.dta") # from quarter 2, 1947 to quarter 3, 2009
g <- growth$g; length(g)
growthNA_2 <- data.frame(G=g, G_1=c(NA, g[1:249]), G_2=c(NA, NA, g[1:248]))
head(growthNA_2); tail(growthNA_2)

growthAR2.lm <- lm(data=growthNA_2, G ~ G_1 + G_2)
summary(growthAR2.lm)

# Correlogram of residuals
#autoplot(acf(growthAR2.lm$residuals, plot=FALSE))
acfs <- acf(growthAR2.lm$residuals, plot=FALSE, lag.max=247)
autoplot(acfs[2:50])

# Do the LM test
e <- growthAR2.lm$residuals; e
errorsNA_2 <- data.frame(G_1=growthNA_2$G_1, G_2=growthNA_2$G_2, 
                         E=c(NA, NA, e), 
                         E_1=c(NA, NA, NA, e[1:247]), 
                         E_2=c(NA, NA, NA, NA, e[1:246]))
head(errorsNA_2); tail(errorsNA_2)

lagrange.lm <- lm(data=errorsNA_2, E ~ G_1 + G_2 + E_1 + E_2)
summary(lagrange.lm)
T <- nrow(na.omit(growthNA_2)); T
LM <- T*0.02489; LM    # TODO: why isn't LM = 7.405
1 - pchisq(LM, df=241) # TODO: why isn't p-value=0.0247 ?





# Part b) repeat using AR(3) model
growthNA_3 <- growthNA_2
growthNA_3$G_3 <- c(NA, NA, NA, g[1:247])
head(growthNA_3); tail(growthNA_3)

growthAR3.lm <- lm(data=growthNA_3, G ~ G_1 + G_2 + G_3)
summary(growthAR3.lm)

# Correlogram of residuals
acfs <- acf(growthAR3.lm$residuals, plot=FALSE); acfs
autoplot(acfs[2:23])

# LM test
e <- growthAR3.lm$residuals; e; length(e)
errorsNA_3 <- data.frame(G_1=growthNA_3$G_1, 
                         G_2=growthNA_3$G_2, 
                         G_3=growthNA_3$G_3,
                         E=c(NA, NA, NA, e), 
                         E_1=c(NA, NA, NA, NA, e[1:246]), 
                         E_2=c(NA, NA, NA, NA, NA,  e[1:245]), 
                         E_3=c(NA, NA, NA, NA, NA, NA, e[1:244]))
head(errorsNA_3, 10); tail(errorsNA_3)

# Doing LM test with just 2 lags - how? 2 lags of E or 2 lags of G?
lagrange.lm <- lm(data=errorsNA_3, E ~ G_1 + E_1 + E_2)
s <- summary(lagrange.lm); s
R2 <- s$r.squared; R2
T <- nrow(na.omit(growthNA_3)); T
LM <- T * R2; LM #T*0.02288; LM     # TODO: why isn't LM = 0.916 ? 
# df = number of lagged errors
1 - pchisq(LM, df=2)  # TODO: why isnt' p-value = 0.632 ? 
head(errorsNA_3, 10)
