setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
library(reshape2)
#install.packages("rugarch")
library(rugarch)

# Tells which models to pick for garch: 
# http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html
# Asks if it's possible to estimate asymmetric GARCH? 
# https://stat.ethz.ch/pipermail/r-sig-finance/2011q3/008339.html



byd <- read.dta("byd.dta")

## Estimate the ARCH: 
# method 1
byd.arch <- ARCH(byd, p=1)

# method 2 - why slightly different? is this asymmetric? 
arch.spec <- ugarchspec(variance.model=list(garchOrder=c(1,0)),
                         mean.model=list(armaOrder=c(0, 0)))
arch.fit <- ugarchfit(spec=arch.spec, data=byd)
arch.fit@fit$matcoef




## Estimate the GARCH model

# method 1 - use garchFit() method from Library(fGarch)
garch <- GARCH(byd, p=1, q=1)

# method 2 - use rugarch (slightly different answers)
garch.spec <- ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                          mean.model=list(armaOrder=c(0, 0)))
garch.fit <- ugarchfit(spec=garch.spec, data=byd)
garch.fit@fit$matcoef



## Estimate the T-GARCH model

# method 1 
tgarch.spec <- ugarchspec(variance.model=list(model="fGarch", 
                                            submodel="TGARCH",
                                            garchOrder=c(1,1)), 
                        mean.model=list(armaOrder=c(0, 0)))
tgarch.fit <- ugarchfit(spec=tgarch.spec, data=byd)
tgarch.fit@fit$matcoef
?ugarchspec


# method 2
tgarch.spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(0,0)))
tgarch.fit <- ugarchfit(spec=tgarch.spec, data=byd)
tgarch.fit@fit$coef

# method 3 (different answer)
tgarchFit <- garchFit(~aparch(1,1), data=byd, 
                      delta=2, include.delta = F, trace=F)
tgarchFit@fit$matcoef

?garchFit




# Graph
autoplot(ts(byd$r), main="BYD Returns")
autoplot(ts(arch@h.t), main="Conditional Heteroskedasticity function")

d <- data.frame(time=seq(1,500), returns=byd$r, variance=arch@h.t)
ggplot(data=d, aes(x=time)) + 
      geom_line(aes(y=returns), color="orange") + 
      geom_line(aes(y=variance), color="purple") + 
      ggtitle("Returns and variance function")
d.melt <- melt(d, id="time"); head(melt)
ggplot(data=d.melt, aes(x=time, y=value, colour=variable)) + geom_line()

