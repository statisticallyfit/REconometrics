setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Wooldridge_Introductory Econometrics: A Modern Approach/Chapter10_TimeSeriesAnalysis")

# from exercise 5: compare R^2 of these two models
barium <- read.dta('barium.dta')

# R, U mean restricted, unrestricted models
barium.lm.R <- lm(lchnimp ~ lchempi + lgas + lrtwex, data=barium)
barium.lm.U <- lm(lchnimp ~ lchempi + lgas + lrtwex + 
                      befile6 + affile6 + afdec6, data=barium)

nrow(barium)-6-1
nrow(barium)-3-1

R2.R <- summary(barium.lm.R)$r.sq # R2 from restricted model
R2.U <- summary(barium.lm.U)$r.sq # R2 from unrestricted model
R2.R
R2.U

# Make the F-statistic for joint significance test
N <- nrow(barium); N
Q <- 3 # number of restricted parametrs
K <- 6 # number of total parameters in unrestricted model
df.1 <- N - K - 1; df.1
df.2 <- Q; df.2
F.stat <- (R2.U - R2.R)/(1 - R2.U) * (df.1/df.2); F.stat
p.value <- 1 - pf(F.stat, df.1, df.2); p.value
F.crit <- qf(0.90, df.1, df.2); F.crit
# so do not reject the null
