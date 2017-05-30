setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
#install.packages("moments")




## Part a) 
term <- read.dta("term.dta")

# (1) check for stationarity
mean(term$r)
res <- dickeyFullerTest(term$r, type="drift", diffed.lags = 1)
autoplot(acf(res, lag.max = 620, plot = FALSE)) # how can it go past 651 lags?

# (2) estimate GARCH(1,1)
term.garch <- GARCH(term, p=1, q=1)




## Part b) estimate M_GARCH
term.garchinmean <- MGARCH11(term$r)

# TODO: why not the same? 

byd <- read.dta("byd.dta")
?garchFit
MGARCH11(byd$r)
MGARCH11(term$r) ## errors





## Part c) 
# financial econometric sense: garch-in-mean term 0.211sqrt(h) is 
# significant
# financial economic sense: the term is positive which is consistent
# with economic theory that when  volatility rises, so do returns. 