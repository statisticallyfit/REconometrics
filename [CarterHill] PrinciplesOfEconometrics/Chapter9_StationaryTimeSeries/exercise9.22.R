setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
consumption <- read.dta("consumptn.dta") #from 1960 Q1 to 2009 Q4
c <- consumption$congwth[4:200]; length(c)
i <- consumption$incgwth[4:200]; length(i)
consumption <- data.frame(congwth=c, incgwth=i); nrow(consumption)
c.ts <- ts(c, start=1960, frequency = 4); c.ts
i.ts <- ts(i, start=1960, frequency = 4); i.ts

autoplot(c.ts)
autoplot(i.ts)
# They have serial correlation, but seem stationary around their mean
# TODO: how to check the graph for serial correlation?



## Part b) estimate the model
con.lm <- lm(data=consumption, congwth ~ incgwth)
summary(con.lm)

# correlogram
autoplot(acf(con.lm$residuals, plot=FALSE)[1:22]) #2,6,10,11,16,22
# LM test
bgtest(con.lm, order=2, type="Chisq")
# major evidence of autocorrelation
# AIC/BIC
modelCriteria(c, con.lm$fitted.values, 2, 0)



## Part c) add to the model C_1 variable
length(c); length(i)
consNA_1 <- data.frame(C=c, C_1=c(NA, c[1:196]), I=i)
head(consNA_1); tail(consNA_1)

cons.1.lm <- lm(data=consNA_1, C ~ C_1 + I)
summary(cons.1.lm)

# Correlogram
autoplot(acf(cons.1.lm$residuals, plot=FALSE)[1:22])
# LM Test
bgtest(cons.1.lm, order=2, type="Chisq")
# AIC/BIC
nona <- na.omit(consNA_1)
modelCriteria(nona$C, cons.1.lm$fitted.values, 4, 1)



## Part d) estimating with C_2
consNA_2 <- data.frame(C=c, C_1=c(NA, c[1:196]), C_2=c(NA, NA, c[1:195]), I=i)
head(consNA_2); tail(consNA_2)

cons.2.lm <- lm(data=consNA_2, C ~ C_1 + C_2 + I)
summary(cons.2.lm)

# Correlogram
autoplot(acf(cons.2.lm$residuals, plot=FALSE)[1:22])
# LM test
bgtest(cons.2.lm, order=2, type="Chisq")
# AIC/BIC
nona <- na.omit(consNA_2)
modelCriteria(nona$C, cons.2.lm$residuals, 5, 2)



## Part e) adding INC_1 to the model
consNA_3 <- data.frame(C=c, C_1=c(NA, c[1:196]), 
                       C_2=c(NA, NA, c[1:195]), 
                       I=i, I_1=c(NA, i[1:196]))
head(consNA_3); tail(consNA_3)

cons.2.2.lm <- lm(data=consNA_3, C ~ C_1 + C_2 + I + I_1)
summary(cons.2.2.lm)

# Correlogram
autoplot(acf(cons.2.2.lm$residuals, plot=FALSE)[1:22])
# LM test
bgtest(cons.2.2.lm, order=2, type="Chisq")
bgtest(cons.2.2.lm, order=4, type="Chisq")
# AIC/BIC
nona <- na.omit(consNA_3)
modelCriteria(nona$C, cons.2.2.lm$residuals, 6, 2)



## Part g) drop C_1 
head(consNA_3)
cons.1.2.lm <- lm(data=consNA_3, C ~ C_2 + I + I_1)
summary(cons.1.2.lm)

# Correlogram
autoplot(acf(cons.1.2.lm$residuals, plot=FALSE)[1:22])
# LM test
bgtest(cons.1.2.lm, order=2, type="Chisq")
bgtest(cons.1.2.lm, order=3, type="Chisq")
bgtest(cons.1.2.lm, order=4, type="Chisq")
# AIC/BIC
nona <- na.omit(consNA_3)
modelCriteria(nona$C, cons.1.2.lm$residuals, 4, 2)
