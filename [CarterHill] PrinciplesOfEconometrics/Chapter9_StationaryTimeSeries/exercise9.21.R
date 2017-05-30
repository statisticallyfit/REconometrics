setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

#install.packages("nlts")
#install.packages("spdep")
#library(nlts)
#library(spdep)
library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
okun <- read.dta("okun.dta")
g <- okun$g; length(g)
u <- okun$u; length(u)
du <- diff(u); length(du)
okunNA_1 <- data.frame(DU=c(NA, du), DU_1=c(NA, NA, du[1:96]), 
                       G=g, G_1=c(NA, g[1:97]))
head(okunNA_1); tail(okunNA_1)

okun.lm <- lm(data=okunNA_1, DU ~ DU_1 + G + G_1)
summary(okun.lm)



## Part b) correlogram of residuals of okun.lm
# TODO: how many lags to calculate?
acfs <- acf(okun.lm$residuals, lag.max=100, plot=FALSE); acfs
autoplot(acfs[2:95])



## Part c) LM test of residual autocorrelation
e <- okun.lm$residuals; e; length(e)
errorsNA_4 <- data.frame(DU_1=c(NA, NA, du[1:96]), 
                         G=g, 
                         G_1=c(NA, g[1:97]),
                         E=c(NA, NA, e), 
                         E_1=c(NA, NA, NA, e[1:95]), 
                         E_2=c(NA, NA, NA, NA, e[1:94]), 
                         E_3=c(NA, NA, NA, NA, NA, e[1:93]), 
                         E_4=c(NA, NA, NA, NA, NA, NA, e[1:92]))
head(errorsNA_4, 10); tail(errorsNA_4)

# Lag 1
lagrange.1.lm <- lm(data=errorsNA_4, E ~ DU_1 + G + G_1 + E_1)
s <- summary(lagrange.1.lm); s
R2 <- s$r.squared; R2
T <- nrow(okunNA_1); T
LM <- T*R2; LM          # TODO: what am I doing wrong?

# Lag 4 
lagrange.4.lm <- lm(data=errorsNA_4, E ~ DU_1 + G + G_1 + E_1 + E_2 + E_3 + E_4)
s <- summary(lagrange.4.lm); s
R2 <- s$r.squared; R2
T <- nrow(okunNA_1); T
LM <- T*R2; LM          # TODO: what am I doing wrong?


pValues <- data.frame(CHISQ=c(NA), P=c(NA)); pValues
for(n in 1:70){
      bg <- bgtest(okun.lm, order=n, type="Chisq")
      if(n == 1) {
            pValues$CHISQ <- bg$statistic
            pValues$P <- bg$p.value
      } else {
            pValues <- rbind(pValues, c(bg$statistic, bg$p.value))
            #pValues <- rbind(pValues, bg$p.value)
      }      
}
pValues
any(pValues$P <= 0.05)
sum(pValues$P <= 0.05)
which(pValues$P <= 0.05)  # so autocorrelation at lags 8,10, 11, 12, 13, 17
pValues$P[which(pValues$P <= 0.05)]
pValues$CHISQ[which(pValues$P <= 0.05)]

head(pValues, 4)




## Part d) reestimate the equation with DU_2 and G_2 added separately, then together
okunNA_2 <- okunNA_1
okunNA_2$DU_2 <- c(NA, NA, NA, du[1:95])
okunNA_2$G_2 <- c(NA, NA, g[1:96])
head(okunNA_2); tail(okunNA_2)

summary(lm(data=okunNA_2, DU ~ DU_1 + DU_2 + G + G_1))
summary(lm(data=okunNA_2, DU ~ DU_1 +        G + G_1 + G_2))
summary(lm(data=okunNA_2, DU ~ DU_1 + DU_2 + G + G_1 + G_2))


# No, coefficients of DU_2 and G_2 are not significantly different than zero. 