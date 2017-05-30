setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learntimeseries/cowpertwaitcode")
getwd()

library(ggplot2)
library(ggfortify)
library(xts)

data(AirPassengers)
AP <- AirPassengers
AP

class(AP)
start(AP)
end(AP)
frequency(AP)

summary(AP)
autoplot(AP, size=1, ts.colour="blue", ts.linetype="dashed")
autoplot(AP, size=1, ylab="Passengers (1000's)")
AP

# remove seasonal effect to clarify trend
# for each year, add the monthly passengers and plot.
112+118+132+129+121+135+148+148+136+119+104+118
aggregate(AP)
autoplot(aggregate(AP), size=1, main="Aggregated Annual Series")

# cycle extracts the seasons for each item (year)
AP
cycle(AP)
# plot
boxplot(AP~cycle(AP))

# OR plot this way:
# NOTE: date symbols: www.statmethods.net/input/dates.html
df <- data.frame(date=as.Date(time(AP)), NumPassengers=as.matrix(AP))
graph = ggplot(df)+geom_boxplot(aes(x=format(date,"%m"),y=NumPassengers))+
  scale_x_discrete("Month",labels=unique(format(df$date,"%b")))
graph



# -------- Unemployment in Maine ---------------
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Maine.dat"
Maine.month = read.table(www, header=TRUE)
Maine.month
attach(Maine.month)
class(Maine.month)

# create a time series object
Maine.month.ts = ts(unemploy, start=c(1996, 1), freq=12)
Maine.month.ts
# add the month rates for each year
Maine.annual.ts = aggregate(Maine.month.ts)/12
Maine.annual.ts
#aggregate(Maine.month.ts)
#(67+67+64+59+52+48+48+40+42+44+50+50)/10

# Plot both time series
layout(1:1)
autoplot(Maine.month.ts, ylab="unemployed (%)")
autoplot(Maine.annual.ts, ylab="unemployed (%)")

Maine.Feb = window(Maine.month.ts, start=c(1996, 2), freq=TRUE)
Maine.Feb
Maine.Aug = window(Maine.month.ts, start=c(1996, 8), freq=TRUE)
Maine.Aug
Feb.ratio = mean(Maine.Feb)/mean(Maine.month.ts); Feb.ratio
Aug.ratio = mean(Maine.Aug)/mean(Maine.month.ts); Aug.ratio
# SO, unemployment is 22% higher in February compared to overall, 
# and 18% lower in August compared to overall

# Monthly unemployment rate for all US in Jan-Oct(1996-2006)
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/USunemp.dat"
US.month = read.table(www, header=TRUE)
attach(US.month)
US.month
US.month.ts = ts(USun, start=c(1996,1), end=c(2006,10), freq=12)
autoplot(US.month.ts, size=1, ylab="unemployed (%)")



# Multiple Time Series: Electricity, Beer, Chocolate Data
www <- 'http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat'
CBE = read.table(www, header=TRUE)
# Create time series objects for the data
Elec.ts = ts(CBE[, 3], start=1958, freq=12)
Elec.ts
Beer.ts = ts(CBE[,2], start=1958, freq=12)
Beer.ts
Choc.ts = ts(CBE[,1], start=1958, freq=12)
autoplot(cbind(Elec.ts, Beer.ts, Choc.ts), size=1)

# Study intersections
AP.elec = ts.intersect(AP, Elec.ts)
AP.elec
start(AP.elec); end(AP.elec)
AP.elec[1:3, ] # rows 1 -3, all columns
AP = AP.elec[,1]; Elec = AP.elec[,2]

# Plot to compare AP and Elec data in the intersecting time period
library(grid); library(gridExtra)
ap = autoplot(AP, ylab="Air Passengers / 1000's")
elec = autoplot(Elec, ylab="Electricity production / MkWh")
grid.arrange(ap, elec, nrow=2)

scatter = ggplot(data=as.data.frame(cbind(AP, Elec)), aes(x=as.vector(AP), y=as.vector(Elec))) + 
  xlab("Air Passengers / 1000's") +
  ylab("Electricity production / MWh") +
  labs(title="Intersection of Electricity and Air Passengers") +
  geom_point(shape=19) +
  geom_smooth(method=lm, size=1); scatter

cor(AP, Elec)
summary(lm(Elec~AP))



# Quarterly Exchange Rate GBP to NZ dollar
www <- 'http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/pounds_nz.dat'
Z = read.table(www, header=TRUE)
# means, quarterly periods of 3 months starting in Jan-March (1st quarter)
Z.ts = ts(Z, start=1991, freq=4)
Z.ts
autoplot(Z.ts, xlab="time (years)", ylab="Quarterly exchange rate in $NZ/pound", size=1)
# Subset into two trends (Cut the graph in half)
Z.92.96 = window(Z.ts, start=c(1992,1), end=c(1996,1))
Z.92.96
Z.96.98 = window(Z.ts, start=c(1996,1), end=c(1998,1))
Z.96.98
autoplot(Z.92.96, ylab="Exchange rate in $NZ/pound", xlab="Time (years)")
autoplot(Z.96.98, ylab="Exchange rate in $NZ/pound", xlab="Time (years)")



# Global temperature
www <- 'http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/global.dat'
Global = scan(www)
Global.ts = ts(Global, start=c(1856, 1), end=c(2005,12), freq=12)
Global.ts
Global.annual = aggregate(Global.ts, FUN=mean)
Global.annual # took the mean row-wise
autoplot(Global.ts)
autoplot(Global.annual)

# Study the upward trend from 1970-2005
New.series = window(Global.ts, start=c(1970, 1), end=c(2005,12))
New.series
New.time = time(New.series)
New.time
ggplot(data=as.data.frame(cbind(New.series, New.time)), 
       aes(x=New.time, y=New.series)) +
  geom_point() + geom_line() +
  geom_smooth(method=lm)




# Decomposition of series
autoplot(decompose(Elec.ts)) #method 1
autoplot(decompose(Elec.ts), type="mult")

# plot(stl(Elec.ts)) --- how to use stl()?
plot(decompose(Elec.ts)) # method 2
Elec.decom = decompose(Elec.ts, type="mult")
plot(Elec.decom)

Trend = Elec.decom$trend
Seasonal = Elec.decom$seasonal
ts.plot(cbind(Trend, Trend*Seasonal), lty=1:2)
