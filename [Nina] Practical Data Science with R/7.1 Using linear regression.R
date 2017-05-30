library(ggplot2)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Nina_Practical Data Science with R")

load("data/psub.RData")
head(psub)

dtrain <- subset(psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)
model <- lm(log(PINCP, base=10) ~ AGEP + SEX + COW + SCHL, data=dtrain)
dtest$predLogPINCP <- predict(model, newdata=dtest)
dtrain$predLogPINCP <- predict(model, newdata=dtrain)

# Predictions
ggplot(data=dtest, aes(x=predLogPINCP, y=log(PINCP, base=10))) + 
      geom_point(color="grey", shape=19) +
      geom_smooth(aes(x=predLogPINCP, y=log(PINCP, base=10)), 
                  color="black", size=1) + 
      geom_line(aes(x=log(PINCP, base=10), y=log(PINCP, base=10)), 
                color="blue", linetype=2) + 
      scale_x_continuous(limits=c(4,5)) + 
      scale_y_continuous(limits=c(3.5, 5.5))

# Residuals
ggplot(data=dtest, aes(x=predLogPINCP, 
                       y=predLogPINCP - log(PINCP, base=10))) + 
      geom_point(color="grey", shape=19) + 
      geom_smooth(aes(x=predLogPINCP,
                      y=predLogPINCP - log(PINCP, base=10)), 
                  color="black", size=1)


# Model diagnostics
# R squared (model squared explained) = 1 - SSE/SST
rsq <- function(y, f){
      1 - sum((y - f)^2) / sum((y - mean(y))^2)
}
rsq(log(dtrain$PINCP, base=10), predict(model, newdata=dtrain))
rsq(log(dtest$PINCP, base=10), predict(model, newdata=dtest))

# Root mean square error (average width of data cloud around fit line)
rmse <- function(y, f) {
      sqrt(mean( (y - f)^2 ))
}
rmse(log(dtrain$PINCP, base=10), predict(model, newdata=dtrain))
rmse(log(dtest$PINCP, base=10), predict(model, newdata=dtest))


# Coefficients
coefficients(model)


# Summary
summary.lm(model)
# F-ratio = ratio of variance of residuals from linear model to 
# variance of residuals from constant model. 