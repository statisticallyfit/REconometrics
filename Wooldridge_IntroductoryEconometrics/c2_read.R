library(ggplot2)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/IntroductoryEconometrics:AModernApproach/data")


# Example 2.3
data <- read.table("CEOSAL1.raw")
ceo <- data.frame(cbind(data$V1, data$V4))
colnames(ceo) <- c("salary", "roe")
head(ceo)

summary(ceo)
ceoModel <- lm(data=ceo, salary ~ roe)
summary.lm(ceoModel)

ceoGraph <- ggplot(data=ceo, aes(x=roe, y=salary)) + 
      geom_point(shape=19) + 
      geom_smooth(method="lm", size=1)

c <- ceoModel$coefficients
ceoHat <- c[1] + c[2]*(ceo$roe);
resids <- ceo$salary - ceoHat

head(cbind(ceoModel$fitted.values, ceoHat))
head(cbind(ceoModel$residuals, resids))

# Straightened log model (this is salary to sales, not roe)
data <- read.table("CEOSAL1.raw")
logceo <- data.frame(logsalary=log(data$V1), logsales=log(data$V3))
head(logceo)

logceoModel <- lm(data=logceo, logsalary~logsales)
summary.lm(logceoModel)

logceoGraph <- ggplot(data=logceo, aes(x=logsales, y=logsalary)) + 
      geom_point(shape=19) + 
      geom_smooth(method="lm", size=1)
logceoGraph
ceoGraph

# original power equation is now: 
c <- logceoModel$coefficients
# e^yhat = y
original <- exp(c[1]) * exp(c[2])^(logceo$logsales); original

d <- data.frame(x=logceo$logsales, y=original)
ggplot(data=d, aes(x=x, y=y)) + geom_line()



# Example 2.4
data <- read.table("WAGE1.raw")
head(data)
wages <- data.frame(cbind(data$V1, data$V2))
colnames(wages) <- c("wage", "educ")
head(wages)

wageModel <- lm(data=wages, wage~educ) #hourly wage, years education
summary.lm(wageModel)

wageGraph <- ggplot(data=wages, aes(x=educ, y=wage)) + 
      geom_point(shape=19) + 
      geom_smooth(method="lm")
wageGraph


# Straightened log model
logwages <- data.frame(logwage=log(wages$wage), educ=wages$educ)
head(logwages)

logwageModel <- lm(data=logwages, logwage~educ)
summary.lm(logwageModel)

logwageGraph <- ggplot(data=logwages, aes(x=educ, y=logwage)) + 
      geom_point(shape=19) + 
      geom_smooth(method="lm")
logwageGraph
wageGraph

c <- logwageModel$coefficients
logwageHat <- c[1] + c[2]*(wages$educ)
logwageHat # this is the y-hat model
# e^yhat = y
original <- exp(c[1]) * exp(c[2])^(wages$educ)

# Graph of the increasing returns to wage by education
d <- data.frame(x=wages$educ, y=original)
ggplot(data=d, aes(x=x, y=y)) + geom_line()

# Compare the two models: 
anova(wageModel, logwageModel)
wageModel
logwageModel
anova(logwageModel, wageModel)
