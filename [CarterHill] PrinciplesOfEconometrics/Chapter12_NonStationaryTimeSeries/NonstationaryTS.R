setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter12_NonStationaryTimeSeries")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
library(reshape2)
#install.packages("systemfit")
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm




## Estimating the models randwalk1 and randwalk2 in regression
spurious <- read.dta("spurious.dta")
rw1.lm <- lm(data=spurious, rw1 ~ rw2)
summary(rw1.lm)

# See? large R^2 and significant coefficients. But this is 
# spurious relationship because

df <- data.frame(t=seq(1, 700), rw1=spurious$rw1, rw2=spurious$rw2)
# Scatter plot
ggplot(data=df, aes(x=rw1, y=rw2)) + 
      geom_point(shape=19) + 
      ggtitle("Scatterplot of rw1 and rw2")
# Time series
melt.df <- melt(df, id="t")
head(melt.df); tail(melt.df)
ggplot(data=melt.df, aes(x=t, y=value, colour=variable)) + 
      geom_line(lwd=1) + 
      ggtitle("The rw1 and rw2 random walks")
