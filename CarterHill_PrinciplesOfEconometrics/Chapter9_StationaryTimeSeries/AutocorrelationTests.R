#install.packages("fUnitRoots")
# This package's adfTest does not detrend the time series before testing, like
# the adf.test does

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())
library(ggfortify)
library(foreign)

www <- "http://www.econometrics.com/comdata/hill4/okun.dat"
okun <- read.table(www, header=TRUE)
head(okun)

# Method 1 of creating the time vector (hard way)
# make col1 - time
years <- seq(1985, 2009)
years <- years[2:24]

time <- rep(1985, 3)
for (y in years) {time <- append(time, rep(y, 4))}
time <- append(time, rep(2009, 3))
time # col1

# make col2 - Ut and its companions
U_1 <- c(NA, okun$U[1:97]); U_1
DU <- c(NA, diff(okun$U))
G_1 <- c(NA, okun$G[1:97]); G_1
G_2 <- c(NA, NA, okun$G[1:96]); G_2

okunData <- data.frame(Quarter=time, Ut=okun$U, DU=DU, Ut_1=U_1, Gt=okun$G, 
                       Gt_1=G_1, Gt_2=G_2)
head(okunData); tail(okunData)


# Method 2 of creating time vector (easy way)
# uses rolling shift provided by lag() method
okun.ts <- ts(okunData$DU, start=1985, frequency=4)
okun.ts <- lag(okun.ts, -1)
okun.ts
okun.ts <- na.omit(okun.ts)
okun.ts


# Calculate autocorrelation coefficients for Gt
g <- okunData$Gt 
c0 <- 1/98 * sum( (g[1:98] - mean(g)) * (g[1:98] - mean(g)) )
c1 <- 1/98 * sum( (g[1:97] - mean(g)) * (g[2:98] - mean(g)) )
c2 <- 1/98 * sum( (g[1:96] - mean(g)) * (g[3:98] - mean(g)) )
c3 <- 1/98 * sum( (g[1:95] - mean(g)) * (g[4:98] - mean(g)) )
c4 <- 1/98 * sum( (g[1:94] - mean(g)) * (g[5:98] - mean(g)) )
c5 <- 1/98 * sum( (g[1:93] - mean(g)) * (g[6:98] - mean(g)) )
c6 <- 1/98 * sum( (g[1:92] - mean(g)) * (g[7:98] - mean(g)) )

r1 <- c1/c0; r2 <- c2/c0; r3 <- c3/c0; r4 <- c4/c0
r5 <- c5/c0; r6 <- c6/c0
r1; r2; r3; r4; r5; r6

# Test significance of autocorrelation coefficients
# Z = (rk - 0) / sqrt(1/T)
z1 <- r1*sqrt(98); z1                 # z-statistic
t1 = r1 / sqrt((1 - r1^2)/(98-2)); t1 # t-statistic
cor.test(g[1:97], g[2:98])            # so autocorcoef is NOT same as corcoef

z2 <- r2*sqrt(98)
z3 <- r3*sqrt(98)
z4 <- r4*sqrt(98)
z5 <- r5*sqrt(98)
z6 <- r6*sqrt(98)

p1 <- 1 - pnorm(z1)
p2 <- 1 - pnorm(z2)
p3 <- 1 - pnorm(z3)
p4 <- 1 - pnorm(z4)
p5 <- 1 - pnorm(z5)
p6 <- 1 - pnorm(z6)
tbl <- data.frame(R=round(c(r1,r2,r3,r4,r5,r6), 5),
                  Z=round(c(z1, z2,z3,z4,z5,z6), 5), 
                  P=round(c(p1,p2,p3,p4,p5,p6), 5)); tbl




# =============================================================================

# METHODS TO TEST FOR AUTOCORRELATION OF ERRORS:

# 1. CORRELOGRAM (for acf to work, must be just 1 variable in .ts, alongside time)

# @todo: why when lag.max=12 does graph show only 3 lags?
# Equation: DU ~ Gt + Gt_1 + Gt_2
autoplot(acf(okun.ts, lag.max=12, plot=FALSE)) 
autoplot(acf(okun.ts, lag.max=98/2, plot=FALSE)) 
# WARNING: this is the acfs of Gt, not of y or residuals, so is not same as r1...
okun.acf <- acf(okun.ts, lag.max=46, plot=FALSE) #lag.max=98/2 (halfish num of obs)
okun.acf


# 2. LAGRANGE MULTIPLIER TEST (for error autocorrelation)

# set up the data, do lm
phillips <- read.dta("phillips_aus.dta")
i <- phillips$inf
d <- diff(phillips$u); length(d)
head(phillips); tail(phillips)

phillipsData <- data.frame(inf=i, 
                           #inf_1=c(NA, i[1:90]),
                           du=c(NA, d[1:90]))
                           #du_1=c(NA, NA, d[1:89])
#phillipsData <- na.omit(phillipsData)
head(phillipsData); tail(phillipsData)


phillips.lm <- lm(data=phillipsData, inf ~ du)
summary(phillips.lm)
summaryHAC(phillips.lm)

# adding errors, set up data with fewer rows due to the ommitted NAs
e <- phillips.lm$residuals; length(e); head(e); tail(e)

# creating data.frame of lag 1 errors
phillipsNA_1 <- phillipsData # the _1 means it contains only up to lag E_1
phillipsNA_1$E <- c(NA, e)
phillipsNA_1$E_1 <- c(NA, NA, e[1:89])
#phillipsNA_1 <- na.omit(phillipsNA_1)
head(phillipsNA_1); tail(phillipsNA_1)
nrow(phillipsNA_1)

# now create the dataframe holding up to lag 4 errors
phillipsNA_4 <- phillipsNA_1
phillipsNA_4$E_2 <- c(NA, NA, NA, e[1:88])
phillipsNA_4$E_3 <- c(NA, NA, NA, NA, e[1:87])
phillipsNA_4$E_4 <- c(NA, NA, NA, NA, NA, e[1:86])
#phillipsNA_4 <- na.omit(phillipsNA_4)
head(phillipsNA_4); tail(phillipsNA_4); nrow(phillipsNA_4)

## set up data set with zero in place of ommitted row
phillipsZero_1 <- data.frame(inf=phillips$inf, du=c(NA, diff(phillips$u)))
phillipsZero_1 <- na.omit(phillipsZero_1)
phillipsZero_1$E <- e
phillipsZero_1$E_1 <- c(0, e[1:89]) # see? put a zero
head(phillipsZero_1); tail(phillipsZero_1)

# now create the dataframe holding up to lag 4 errors
phillipsZero_4 <- phillipsZero_1
phillipsZero_4$E_2 <- c(0, 0, e[1:88])
phillipsZero_4$E_3 <- c(0, 0, 0, e[1:87])
phillipsZero_4$E_4 <- c(0, 0, 0, 0, e[1:86])
head(phillipsZero_4); tail(phillipsZero_4); nrow(phillipsZero_4)

# do the test

## method (1)
lagrange.lm <- lm(data=phillipsNA_1, E ~ du + E_1)
summary(lagrange.lm)    # df = N-2 = 89-2 = 87 (why df=86 in summary???)
T <- nrow(na.omit(phillipsNA_1)); T
LM <- T * 0.3102; LM # should be 27.6078

lagrange.lm <- lm(data=phillipsNA_4, E ~ du + E_1 + E_2 + E_3 + E_4)
summary(lagrange.lm)
T <- nrow(na.omit(phillipsNA_4)); T
LM <- T * 0.3882; LM # should be 33.3852 ~ 33.4 - good!

## method (2)
lagrange.lm <- lm(data=phillipsZero_1, E ~ du + E_1)
summary(lagrange.lm)
T <- nrow(phillipsZero_1)
LM <- T * 0.3066; LM # should be 27.594

lagrange.lm <- lm(data=phillipsZero_4, E ~ du + E_1 + E_2 + E_3 + E_4)
summary(lagrange.lm)
T <- nrow(phillipsZero_4)
LM <- T * 0.4075; LM # should be 36.7 - good!


# 3. T-TEST (for error autocorrelation)

## method (1)
ttest.lm <- lm(data=phillipsNA_1, inf ~ du + E_1)
summary(ttest.lm) # see? t-value is 6.219, df = N-2 = 88-2 = 86
anova(ttest.lm)   # see? F-value is 38.67

ttest.lm <- lm(data=phillipsNA_4, inf ~ du + E_1 + E_2 + E_3 + E_4)
summary(ttest.lm) # the book doesn't give these test results
anova(ttest.lm)

## method (2)
ttest.lm <- lm(data=phillipsZero_1, inf ~ du + E_1)
summary(ttest.lm)     # see? t-value is 6.202, df = N-2 = 89-2 = 87
anova(ttest.lm)       # see? F-value is 38.47

ttest.lm <- lm(data=phillipsZero_4, inf ~ du + E_1 + E_2 + E_3 + E_4)
summary(ttest.lm) # the book doesn't give these test results
anova(ttest.lm)



# 4. DURBIN-WATSON TEST (for error autocorrelation)




## -----------------------------------------------------------------------------

# Estimating an AR(1) model: 

# making the phillipsNA with lagged inf, du
phillips <- read.dta("phillips_aus.dta")
i <- phillips$inf; length(i)
u <- phillips$u
d <- diff(u); length(d)
phillipsNA_1_1 <- data.frame(inf=i, 
                             inf_1=c(NA, i[1:90]), 
                             du=c(NA, d), 
                             du_1=c(NA, NA, d[1:89]))
phillipsZero_1_1 <- data.frame(inf=i, 
                               inf_1=c(0, i[1:90]), 
                               du=c(0, d), 
                               du_1=c(0, 0, d[1:89]))
head(phillipsNA_1_1); tail(phillipsNA_1_1)
head(phillipsZero_1_1); tail(phillipsZero_1_1)

# estimate with + or - du_1 ? 
## WHY AREN'T THE ESTIMATES THE SAME AS IN BOOK??? (page 362)
# p on inf_1 must be [0.557 with std error 0.09]
ar.phil.lm <- lm(data=phillipsNA_1_1, inf ~ du + inf_1 + du_1)
summary(ar.phil.lm)


## +++++ Wooldridge steps
# step 1
reg.lm <- lm(data=phillipsNA_1_1, inf ~ du)
# step 2
e <- reg.lm$residuals; e; length(e)
errorNA_1 <- data.frame(E=e, E_1=c(NA, e[1:89]))
head(errorNA_1); tail(errorNA_1)
err.lm <- lm(data=errorNA_1, E ~ E_1); 
sumry <- summary(err.lm)
p <- sumry$coefficients[2]; p  # todo: p supposed to be 0.557 using wooldridge method
# step 3
head(phillipsNA_1_1)
infDiffP <- phillipsNA_1_1$inf - p*phillipsNA_1_1$inf_1; infDiffP
duDiffP <- phillipsNA_1_1$du - p*phillipsNA_1_1$du_1; duDiffP
lm.3 <- lm(infDiffP ~ duDiffP)
summary(lm.3) # B2 should be -0.6944 not -0.69166

# todo: why are estimates not the same? Why isn't p the same as 0.557 
# from (page 362) in carter hill