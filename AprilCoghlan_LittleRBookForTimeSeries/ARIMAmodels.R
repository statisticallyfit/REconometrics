library(ggplot2)
library(ggfortify)

# 2.6.1 If the data is not stationary (follows the mean) then 
# difference the data

# EXAMPLE 1
autoplot(skirt.ts)

skirtdiff.ts = diff(skirt.ts, differences = 1)
autoplot(skirtdiff.ts) # still not stationary, so diff again

skirtdiff2.ts = diff(skirt.ts, differences = 2)
autoplot(skirtdiff2.ts) # is stationary in mean and variance, so we use ARIMA(p, d=2, q) model


# EXAMPLE 2
autoplot(kings.ts)

kingsdiff.ts = diff(kings.ts, differences = 1)
autoplot(kingsdiff.ts)



# 2.6.2 Selecting a candidate ARIMA model

# choose the right ARIMA after data is stationary
# to find p and q, look at correlogram and partial correlogram of stationary ts

# EXAMPLE 1
autoplot(acf(kingsdiff.ts, lag.max=20, plot=FALSE)) # q
autoplot(pacf(kingsdiff.ts, lag.max=20, plot=FALSE)) # p
# FINAL MODEL: ARIMA(p=0,d=1,q=1)


# EXAMPLE 2
volcanodust = scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodust.ts = ts(volcanodust, start=c(1500)) # data is from 1500-1969
autoplot(volcanodust.ts)

autoplot(acf(volcanodust.ts, lag.max=20, plot=FALSE)) #q=3 (because after lag 3, the lags are within the bounds)
autoplot(pacf(volcanodust.ts, lag.max=20, plot=FALSE)) # p=2

# Either use ARMA(2,0) or ARMA(2,3) ==> ARIMA(2,0,0), or ARIMA(2,0,3)



# 2.6.3 Forecasting using an ARIMA model

# EXAMPLE 1
kingsarima = arima(kings.ts, order=c(0,1,1))
# theta is ma1 in the output = -0.7218

# forecasting using arima model
kingsarimaforecasts = forecast.Arima(kingsarima, h=5)
autoplot(kingsarimaforecasts)


# TEST 1 if model is good: 

#like in exponential smoothing, must see if forecast 
#errors are normal with constant variance and if there are successive 
#correlations
autoplot(acf(kingsarimaforecasts$residuals, lag.max=20, plot=F))
Box.test(kingsarimaforecasts$residuals, lag=20, type="Ljung-Box")
# ==> no evidence that model is bad

# TEST 2 if model is good: 

# normal and homoskedastic?
autoplot(kingsarimaforecasts$residuals) # roughly constant var and steady mean
shapiro.test(kingsarimaforecasts$residuals) # normal

plotForecastErrors(kingsarimaforecasts$residuals) # mean=0
mean(kingsarimaforecasts$residuals) # mean ~ 0
t.test(kingsarimaforecasts$residuals) # mean is close to 0

# So ARIMA(0,1,1) is a good model



# EXAMPLE 2
volcanodustarima = arima(volcanodust.ts, order=c(2,0,0))
volcanodustarima
# beta1 and beta 2 are ar1 and ar2

# forecast with arima
volcanoarimaforecasts = forecast.Arima(volcanodustarima, h=31)
volcanoarimaforecasts
autoplot(volcanoarimaforecasts)

# TEST 1 if model is good:
autoplot(acf(volcanoarimaforecasts$residuals, lag.max=20, plot=FALSE))
Box.test(volcanoarimaforecasts$residuals, lag=20, type="Ljung-Box")

# TEST 2 if model is good:
autoplot(volcanoarimaforecasts$residuals)
shapiro.test(volcanoarimaforecasts$residuals) # not that normal!

plotForecastErrors(volcanoarimaforecasts$residuals) # skewed right!
mean(volcanoarimaforecasts$residuals) # so mean is close to 0
t.test(volcanoarimaforecasts$residuals)
# Model must be improved