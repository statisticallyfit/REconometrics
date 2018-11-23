setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)




## Part a) 
uk <- read.dta("uk.dta")
cpi <- uk$ukcpi
cpi_1 <- c(NA, cpi[1:(length(cpi)-1)])
# calculate monthly inflation rate of consumer price index
inf <- 100 * (cpi - cpi_1) / cpi_1
head(inf)



## Part b) estimate TGARCH-in-mean
## TODO - help