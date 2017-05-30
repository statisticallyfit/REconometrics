#install.packages("fpp")
#install.packages("GGally")
#install.packages("reshape")
#install.packages("caret")

library(fpp)
library(ggplot2)
library(ggfortify)
library(GGally)
library(reshape)
library(plyr)
library(caret)


getwd()
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/FPP")


melsyd
autoplot(melsyd[, "Economy.Class"], main="Economy class 
         passengers: Melbourne-Sydney", xlab="Year", 
         ylab="Thousands")
a10
autoplot(a10, ylab="$ million", xlab="Year",
         main="Antidiabetic drug sales")


#" TIME SERIES PATTERNS:

#Trend: long-term increase or decrease in data
#Seasonal pattern: have fixed/known length
#Cycle: has variable/unknown length;
#data rises and falls due to economic conditions

#* cycle is longer and more variable than seasonal"



# PLOTS FOR SEASONAL PATTERNS

# 1. Seasonal plot
seasonplot(a10, ylab="$ million", xlab="Year", 
           main="Seasonal Plot: antidiabetic drug sales", 
           year.labels=TRUE, year.labels.left=TRUE,
           col=rainbow(16), pch=19)

# 2. Seasonal subseries plots
monthplot(a10, ylab="$ million", xlab="Month", xaxt="n", 
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1, at=1:12, labels=month.abb, cex=0.8)


# TO SEE RELATION in CROSS-SECTIONAL DATA (not time series)

# 1. Scatter plot
head(fuel)
class(fuel)
ggplot(data=fuel, aes(x=jitter(fuel[,5]), y=jitter(fuel[,8]))) + 
  geom_point(size=3) + xlab("City mpg") + ylab("Carbon footprint")

# 2. Scatterplot matrices
# y value is given by variable on the row, and x-value is given by
# variable on the column. 
pairs(fuel[, -c(1:2, 4, 7)], pch=19)
ggpairs(fuel[, -c(1:2, 4, 7)])


# Statistics
fuel2 = fuel[fuel$Litres < 2, ]
summary(fuel2[, "Carbon"])
summary(fuel[, "Carbon"])
sd(fuel2[, "Carbon"])
sd(fuel[, "Carbon"])

# Autocorrelations
#ausbeer = read.table("data/beer.csv", dec=",", header=TRUE)
#ausbeer.ts = ts(ausbeer, start=1956, end=c(1995, 8), freq=12)
#ausbeer2.ts = window(ausbeer.ts, start=1992) #, end=2006-0.1)
beer2 = window(ausbeer, start=1992, end=2006-0.1)
lag.plot(beer2, lags=9, do.lines=FALSE)
autoplot(acf(beer2, plot=FALSE))

# White noise
set.seed(30)
x = ts(rnorm(50))
autoplot(x, main="White noise", xlab="Time", ylab="Normal values")

# Acf for white noise
autoplot(acf(x, plot=FALSE))




# 2.3. Simple forecasting methods

# y = contains the time series, h = forecast horizon

# Average method: 
    # meanf(y, h) 
  # for cross sectional and time series data
# Naive method: 
    # naive(y, h)
  # the forecast is the last observation in the time series
# Seasonal naive method: 
    # snaive(y, h)
  # forecast is the last observation from same season
  # of the year (meant for highly seasonal data)
# Drift method: 
    # rwf(y, h, drift=TRUE)
  # like extrapolating the average of the first and last 
  # observation (called drift) into the future

# Examples
beerfit1 = meanf(beer2, h=11)
beerfit2 = naive(beer2, h=11)
beerfit3 = snaive(beer2, h=11)

autoplot(beer2)
plot(beer2, main="Forecasts for quarterly beer production")
# @todo: how to add these data to autoplot?
lines(beerfit1$mean, col=4)
lines(beerfit2$mean, col=2)
lines(beerfit3$mean, col=3)
legend("topright", lty=1, col=c(4,2,3), 
       legend=c("Mean method", "Naive method", "Seasonal naive method"))


'# plot with ggplot2
beer.mat = matrix(as.vector(beer2), ncol=4, byrow=TRUE)
dates = matrix(rep(1992:2005, 4), ncol=4, byrow=FALSE)

beer.df = data.frame(character(), character())
for(i in 1:nrow(dates)){
  mat = cbind(dates[i,], beer.mat[i,])
  beer.df = rbind(beer.df, as.data.frame(mat))
}
beer.df = rename(beer.df, c("V1"="Date", "V2"="Quarter"))
beer.df

ggplot(beer.df, aes(Date, Quarter)) + geom_line()
'

# Forecasting applied to Dow Jones index
dj2 = window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 July 1994)", 
         ylab="", xlab="Day", xlim=c(2, 290))
lines(meanf(dj2, h=42)$mean, col=4)
lines(naive(dj2, h=42)$mean, col=2)
lines(rwf(dj2, drift=TRUE, h=42)$mean, col=3)
legend("topleft", lty=1, col=c(4,2,3),
       legend=c("Mean method", "Naive method", "Drift method"))
lines(dj)




# 2.4 Transformations

# Box cox transformations depend on lambda parameter: 
# wt = ln(yt) if lambda=0,  and (yt^lambda-1)/lambda otherwise
# lambda makes seasonality across series the same

autoplot(log(elec), ylab="Transformed electricity demand", 
         xlab="Year", main="Transformed monthly electricity demand")
title(main="log", line=-1) # log(lambda=0)

yt = elec
wt = (elec^lambda-1)/lambda
lambda = BoxCox.lambda(yt); lambda
autoplot(BoxCox(yt, lambda))
autoplot(wt) #the same

# Backtransforming: 
# yt = exp(wt) if lambda=0 and (lambda*wt + 1)^(1/lambda) otherwise
autoplot((lambda*wt + 1)^(1/lambda))
autoplot(yt) #the same as above


# Calendar Variation
monthdays = rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
monthdays[26 + (4*12)*(0:2)] = 29 # leap year
monthdays
autoplot(milk, main="Monthly milk production per cow", ylab="Pounds", 
         xlab="Years")
# Now remove variation due to differing month lengths
autoplot(milk/monthdays, main="Average milk production
         per cow per day", ylab="Pounds", xlab="Years")




# 2.5 Evaluating Forecast Accuracy

# ei = forecast error = yi - yi_forecast
# MAE (mean absolute error) = mean(|ei|), 
# RMSE (root mean squared error) = sqrt(mean(ei^2))
# MAPE (mean absolute % error) = mean(|100ei/yi|)

plot(beerfit1, plot.conf=FALSE, main="Forecasts for quarterly
     beer production")
lines(beerfit2$mean, col=2)
lines(beerfit3$mean, col=3)
lines(ausbeer)
legend("topright", lty=1, col=c(4,2,3), 
       legend=c("Mean method", "Naive method", "Seasonal naive method"))


beer3 = window(ausbeer, start=2006)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)


dj3 = window(dj, start=251)
accuracy(meanf(dj2, h=42), dj3)
accuracy(rwf(dj2, h=42), dj3)
accuracy(rwf(dj2, drift=TRUE, h=42), dj3)




# 2.6 Residual Diagnostics

dj2 = window(dj, end=250)
autoplot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)", xlab="Day")
res = residuals(naive(dj2), na.rm=T)
res = na.omit(res)
autoplot(res, main="Residuals from naive method", xlab="Day")
autoplot(acf(res, main="ACF of residuals from naive method", plot=FALSE))

# Portmanteau test: 
# Use lag=10 for non-seasonal data and lag=2m for seasonal data, 
# where m is period of seasonality
# Use lag = T/5 if those values are larger than T/5
# Smaller statistic = lower p-value = more normal residuals

Box.test(res, lag=10, fitdf=0)
Box.test(res, lag=10, fitdf=0, type="Lj")





# ---------------- EXERCISES ------------------
library(fma)

# 1a
autoplot(dole, main="Monthly total people on unemployed benefits
         in Australia (Jan 1956 - Jul 1992", xlab="Year")
# 1b
autoplot(usdeaths, main="Number of accidental US deaths", xlab="Year")
# transform: 
autoplot(diff(usdeaths))
autoplot(log(usdeaths))
# 1c
autoplot(bricksq, 
         main="Quarterly brick production in Australia", 
         xlab="Years")
lambda = BoxCox.lambda(bricksq); lambda
autoplot(BoxCox(bricksq, lambda))
# 2a --- 
autoplot(dowjones, main="Dow Jones Data")
# 2b
driftMethod = rwf(dowjones, drift=TRUE, h=10)
plot(dowjones)
lines(driftMethod$mean, col=3)

firstObs = dowjones[1]
lastObs = dowjones[length(dowjones)]

mean(c(firstObs, lastObs))
driftMethod$mean
# none of the means are equal! How to answer question 2c?

# 2d
dowjones
dowjones2 = window(dowjones, start=1, end=67)
djfit1 = meanf(dowjones2, h=10)
djfit2 = naive(dowjones2, h=10)
djfit3 = snaive(dowjones2, h=10)
djfit4 = rwf(dowjones2, drift=TRUE, h=10)

dowjones3 = window(dowjones, start=68)
accuracy(djfit1, dowjones3)[2, c(2,3,5,6)]
accuracy(djfit2, dowjones3)[2, c(2,3,5,6)]
accuracy(djfit3, dowjones3)[2, c(2,3,5,6)]
accuracy(djfit4, dowjones3)[2, c(2,3,5,6)]

# therefore, naive method is better

# 3a ---
autoplot(ibmclose, main="IBM stock daily closing prices", xlab="Day")
# 3b
values = c(5,6,8,-1,4,7,4,4,3,3,5,6,1,2,-9,10,3,3,4,4,-4,-4,-3,-3,18,34,2,-30)
record = values
getRemainingElements2 <- function(){
  for(i in length(values):1){
    for(j in 1:length(valuesToRemove)){
      if(valuesToRemove[j] == values[i]){
        values = c(values[1:(i-1)], values[(i+1):length(values)])
        j = length(valuesToRemove)
      }
    }
  }
  return(values)
}

getRemainingElements2()
record


#-----------
trainingSet = sample(ibmclose, size=round(0.20*length(ibmclose), 0), replace=FALSE)
uniqueTrainingSet = unique(trainingSet)
testSet = as.vector(ibmclose)
record = testSet
sum(!(record==ibmclose)) # check testSet is ibmclose at this point

getRemainingElements <- function(){
  for(i in length(testSet):1){
    for(j in 1:length(uniqueTrainingSet)){
      if(uniqueTrainingSet[j] == testSet[i]){
        testSet = c(testSet[1:(i-1)], testSet[(i+1):length(testSet)])
        j = length(uniqueTrainingSet)
      }
    }
  }
  return(testSet)
}

supposedToBeRemainderOfIBMNotInTrainingSet = getRemainingElements()
length(unique(supposedToBeRemainderOfIBMNotInTrainingSet))
length(uniqueTrainingSet)
length(unique(record))

length(supposedToBeRemainderOfIBMNotInTrainingSet)
length(trainingSet)
length(record)

# trying to split with caret package...
set.seed(3456)
trainIndex = createDataPartition(iris$Species, 
                                 list=FALSE, 
                                 times=1)
trainIndex
irisTrain = iris[trainIndex, ]
irisTest = iris[-trainIndex,]
irisTrain
irisTest

# how to create time slices for time series?

# 4a
autoplot(hsales, main="Sales of new one-family houses")
# 4b
# know how to split data into train/test...