library(ggplot2)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Nina_Practical Data Science with R")

load("data/NatalRiskData.rData")
head(sdata)

train <- sdata[sdata$ORIGRANDGROUP <= 5, ]
test <- sdata[sdata$ORIGRANDGROUP > 5, ]


complications <- c("ULD_MECO", "ULD_PRECIP", "ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")

y <- "atRisk"
x <- c("PWGT", "UPREVIS", "CIG_REC", "GESTREC3", "DPLURAL", 
       complications, riskfactors)
fmla <- paste(y, paste(x, collapse=" + "), sep=" ~ ")
print(fmla)

model <- glm(fmla, data=train, family=binomial(link="logit"))

# Need to specify "type="response"" otherwise predict() returns
# result of logit(y), need to put this response into sigmoid function
train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")


# Predictions grouped by known outcome
ggplot(data=train, aes(x=pred, color=atRisk, linetype=atRisk)) + 
      geom_density(size=1)



# Model trade-offs
library(ROCR)
#install.packages("ROCR")
library(grid)

predObj <- prediction(train$pred, train$atRisk)
precObj <- performance(predObj, measure="prec") #precision as function of threshold
recObj <- performance(predObj, measure="rec") # recall as function of threshold

precision <- (precObj@y.values)[[1]]
prec.x <- (precObj@x.values)[[1]]
recall <- (recObj@y.values)[[1]]
# xvalues (thresholds) are same in predObj and recObj

rocFrame <- data.frame(threshold=prec.x, precision=precision, recall=recall)
head(rocFrame)

#Function to plot many stacked plots on one page
nplot <- function(plist) {
      n <- length(plist)
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(n, 1)))
      vplayout = function(x, y) {
            viewport(layout.pos.row=x, layout.pos.col=y)
      }
      for (i in 1:n){
            print(plist[[i]], vp=vplayout(i, 1))
      }
}

# find rate of at-risk births in training set
pnull <- mean(as.numeric(train$atRisk))

#Plot enrichment rate as function of threshold
p1 <- ggplot(rocFrame, aes(x=threshold)) + 
      geom_line(aes(y=precision/pnull), size=1) + 
      coord_cartesian(xlim=c(0, 0.05), ylim=c(0, 10))

# Plot recall as function of threshold
p2 <- ggplot(rocFrame, aes(x=threshold)) + 
      geom_line(aes(y=recall), size=1) + 
      coord_cartesian(xlim=c(0, 0.05))

nplot(list(p1, p2))



# Have picked threshold, now evaluate model with confusion matrix
ctab.test <- table(pred=test$pred > 0.02, atRisk=test$atRisk)
ctab.test
precision <- ctab.test[2,2] / sum(ctab.test[2, ])
precision
recall <- ctab.test[2,2] / sum(ctab.test[ ,2])
recall
enrichmentRate <- precision / mean(as.numeric(test$atRisk))
enrichmentRate



# Advice from logistic model
coefficients(model)

# Calculating deviance residuals
pred <- predict(model, newdata=train, type="response")

# to return loglikelihoods for each data point
loglikComponents <- function (y, py){
      y*log(py) + (1-y)*log(1-py)
}

residDeviance <- sign(as.numeric(train$atRisk) - pred) * 
      sqrt(-2*loglikComponents(as.numeric(train$atRisk), pred))

summary(residDeviance)
