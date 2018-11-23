library(ggplot2)
library(ggfortify)
library("TTR")
library(car)

# 2.2 Reading Data
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
kings
kings.ts = ts(kings)
# freq=number of times per year the data was collected
kings.ts
autoplot(kings.ts)

# 2.3 Plotting Time Series
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
births.ts = ts(births, freq=12, start=c(1946,1))
births.ts
autoplot(births.ts)
#autoplot(acf(births.ts))

souvenir = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir
souvenir.ts = ts(souvenir, freq=12, start=c(1987, 1))
souvenir.ts
autoplot(souvenir.ts)
autoplot(acf(souvenir.ts))
# seasonal increases with time so must do log transform
logsouvenir.ts = log(souvenir.ts)
autoplot(logsouvenir.ts)


# 2.4 Decomposing TIme Series

# -- decomposing non-seasonal data

# do smoothing to find simple moving average of non seasonal additive time series
kingsSMA3.ts = SMA(kings.ts, n=3) # order=3
kingsSMA3.ts
autoplot(kings.ts)
autoplot(kingsSMA3.ts) # this has extracted the trend
# using higher order gives better smoothing, less randomness
kingsSMA8.ts = SMA(kings.ts, n=8)
autoplot(kingsSMA8.ts)

# -- decomposing seasonal data

autoplot(births.ts)
birthscomponents.ts = decompose(births.ts)
birthscomponents.ts$seasonal
autoplot(birthscomponents.ts)

# -- seasonally adjusting
birthseasonallyadjusted.ts = births.ts - birthscomponents.ts$seasonal
autoplot(birthseasonallyadjusted.ts)
autoplot(birthscomponents.ts$trend)



# 2.5 Forecasts using exponential smoothing

# -- Exponential smoothing for additive model, no trend, no seasonality

rain = scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip=1)
rain.ts = ts(rain, start=c(1813))
rain.ts
autoplot(rain.ts)
# additive model seems good since random fluctuations are constant over time
# so --> use simple exp smoothing to forecast
raintimeseriesforecast = HoltWinters(rain.ts, beta=F, gamma=F)
raintimeseriesforecast
# alpha = 0.024 is close to 0 ==> little weight is placed on recent observations when forecasting
raintimeseriesforecast$fitted # are the forecasts
# original rain.ts against the forecast
autoplot(raintimeseriesforecast)
# accuracy of forecasts - shown by SSE of forecasts
raintimeseriesforecast$SSE

HoltWinters(rain.ts, beta=F, gamma=F, l.start=23.56)


# can make forecasts for further time points:

library("forecast")

# to forecast rainfall 8 years after last year in rain.ts:
raintimeseriesforecast2 = forecast.HoltWinters(raintimeseriesforecast, h=8)
raintimeseriesforecast2
autoplot(raintimeseriesforecast2, size=1)

# residuals from forecasting
raintimeseriesforecast2$residuals


# --- TEST 1 --- whether this model is good:
# calculate a correlogram to see if errors are successively
# correlated. If they are, another model should be used. 
autoplot(acf(raintimeseriesforecast2$residuals, lag.max=20, plot=FALSE))

# the autocorrelation at lag 3 is touching the bounds - significant 
# evidence for nonzero correlations at lags 1-20?
Box.test(raintimeseriesforecast2$residuals, lag=20, type="Ljung-Box")
# high pvalue --> little evidence of nonzero autocorrelation

# --- TEST 2 --- whether this model is good:
# check if forecast errors are normal with mean 0 and constant variance

# is variance constant?
autoplot(raintimeseriesforecast2$residuals)

# is mean zero?
plotForecastErrors <- function(forecasterrors){
  # make red histogram of forecast errors
  mybinsize = IQR(forecasterrors)/4
  mymin = min(forecasterrors)*3
  mymax = max(forecasterrors)*3
  mybins = seq(mymin, mymax, by=mybinsize)
  
  residsXts = as.xts(raintimeseriesforecast2$residuals)
  resids = coredata(residsXts)
  ggplot(data=as.data.frame(resids), aes(resids)) + 
    geom_histogram(breaks=mybins, aes(y=..density..), fill="red") +
    stat_function(fun=dnorm, color="blue", size=1, arg=list(mean=0, sd=sd(forecasterrors)))
}

plotForecastErrors(raintimeseriesforecast2$residuals)



# -- 2.5.2 Exponential smoothing for additive model, with trend, no seasonality
# Then use Holt's exponential smoothing to make short term forecasts

# data using additive model with a trend but no seasonality
skirts = scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat", skip=5)
skirt.ts = ts(skirts, start=c(1866)) # from 1866 to 1911
skirt.ts
autoplot(skirt.ts)

skirttimeseriesforecasts = HoltWinters(skirt.ts, gamma=FALSE)
skirttimeseriesforecasts
"alpha ~ 1 (is high): current estimates are based on recent observations
beta ~ 1 (is high): slope b of trend is based on recent observations"
skirttimeseriesforecasts$SSE

# plot original time series and red forecast
autoplot(skirttimeseriesforecasts)
# l.start = first value, b.start=second-first value
autoplot(HoltWinters(skirt.ts, gamma=FALSE, l.start=608, b.start=9))

# make future predictions as before
skirttimeseriesforecasts2 = forecast.HoltWinters(skirttimeseriesforecasts, h=19) # 19 years past the last one
autoplot(skirttimeseriesforecasts2)

# Test 1 if model could be improved
autoplot(acf(skirttimeseriesforecasts2$residuals, lag.max=20, plot=F))
Box.test(skirttimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
# Test 2 if model could be improved
autoplot(skirttimeseriesforecasts2$residuals) # homoskedastic
plotForecastErrors(skirttimeseriesforecasts2$residuals) # yes, mean is 0


# 2.5.3 Holt Winters Exponential Smoothing for additive, trend, and seasonal model
# this method estimates level, slope, and seasonal component
# alpha = level estimate
# beta = slope b of trend
# gamma = seasonal estimate of component
logsouvenir.ts
souvenirtimeseriesforecasts = HoltWinters(logsouvenir.ts)
souvenirtimeseriesforecasts
autoplot(souvenirtimeseriesforecasts)

# forecast ahead
souvenirtimeseriesforecasts2 = forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
autoplot(souvenirtimeseriesforecasts2)

# Test 1 for autocorrelations
autoplot(acf(souvenirtimeseriesforecasts2$residuals,
             lag.max=20, plot=F))
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, 
         type="Ljung-Box")

# Test 2 for constant variance and normal data and mean 0
errors = coredata(as.xts(souvenirtimeseriesforecasts2$residuals))

autoplot(souvenirtimeseriesforecasts2$residuals) # homoskedastic

plotForecastErrors(souvenirtimeseriesforecasts2$residuals) # mean is 0 and no signal in resids since they are normal
shapiro.test(souvenirtimeseriesforecasts2$residuals) #NOT significantly non-normal
qplot(data=as.data.frame(errors), sample=errors)
