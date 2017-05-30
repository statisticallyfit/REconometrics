setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
library(quantmod)
library(ggfortify)
library(tseries)
rm(list=ls())


getSymbols("^FTSE")


# calculate the differences of logs of closing price
ftrt <- diff(log(Cl(FTSE)))
ft <- as.numeric(ftrt)
ft <- ft[!is.na(ft)] # removing first NA
head(ftrt)
autoplot(ts(ft)) # very clear volatility changes


## (1) Step 1 - check the time series is stationary
mean(ft)
res <- dickeyFullerTest(ft, type="none", diffed.lags = 7)
autoplot(acf(res, lag.max = 50, plot = FALSE))


## (2) Step 2 - fit a model to the data, in this case, ARIMA(p,d,q) model. 
ftfinal.aic <- Inf
ftfinal.arima <- ""
ftfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4){
      ftcurrent.aic <- AIC(arima(ft, order=c(p,d,q)))
      if (ftcurrent.aic < ftfinal.aic){
            ftfinal.aic <- ftcurrent.aic
            ftfinal.order <- c(p,d,q)
            ftfinal.arima <- arima(ft, order=ftfinal.order)
      }
}
ftfinal.order # 2 autoregressive params and 3 moving average params
ftfinal.aic
ftfinal.arima




## (3) Step 3 - do the residuals of this model show discrete white noise? 
autoplot(acf(ftfinal.arima$residuals, plot=FALSE)) # yes




## (4) Step 4 - do the squared residuals indicate conditional 
# heteroskedasticity in FTSE? 
autoplot(acf(ftfinal.arima$residuals^2, plot=FALSE)) # yes,due to sercorrelation. 




## (5) Step 5 - fit a GARCH model to the errors
## TODO: is this fitting garch to residuals or to actual data FTSE? 
## TODO: is p and q in garch(p,q) the same as in ARIMA(p,d,q)? 
ft.garch <- garch(ft, trace=FALSE)
summary(ft.garch)

ft.res <- ft.garch$residuals[-1] # the first is NA




## (6) Step 6 - test if the GARCH fit to the residuals is good. 
autoplot(acf(ft.res, plot=FALSE)) # yes, good
autoplot(acf(ft.res^2, plot=FALSE)) # yes, good since cond.heter. has been removed
