setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Wooldridge_Introductory Econometrics: A Modern Approach/Chapter10_TimeSeriesAnalysis")
rm(list=ls())


library('foreign')
library(ggfortify)

#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/phillips.dta','phillips.dta',mode="wb")
#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/intdef.dta','intdef.dta',mode="wb")
#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/prminwge.dta','prminwge.dta',mode="wb")
#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta','fertil3.dta',mode="wb")
#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/barium.dta','barium.dta',mode="wb")
#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/fair.dta','fair.dta',mode="wb")
#download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/hseinv.dta','hseinv.dta',mode="wb")

# Example 10.1
phillips <- read.dta('phillips.dta')
head(phillips)
lm.10.1 <- lm(inf ~ unem, data=phillips)
summary(lm.10.1)

phillips.ts <- ts(phillips[,2], start=1948) # how to set names?
autoplot(lm.10.1)
autoplot(phillips.ts, ts.size = 1, ts.colour = "red")
#ggplot(phillips.ts, aes(year, unem)) + geom_line(lwd=1) + xlab("year") + ylab("unem")


# Example 10.2
intdef <- read.dta("intdef.dta")
lm.10.2 <- lm(i3 ~ inf + def, data=intdef)
summary(lm.10.2)

lm.10.2$residuals

intdef.ts <- ts(data.frame(intdef[,2:3], def=intdef[,6]), start=1948)
head(intdef.ts)
autoplot(intdef.ts, ts.size=1, ts.colour="blue")
# Or individual ggplots
#ggplot(intdef.ts, aes(year, i3)) + geom_line(lwd=1) + xlab("year") + ylab("i3")
#ggplot(intdef.ts, aes(year, inf)) + geom_line(lwd=1) + xlab("year") + ylab("inf")
#ggplot(intdef.ts, aes(year, def)) + geom_line(lwd=1) + xlab("year") + ylab("def")



# Example 10.3
prminwge <- read.dta('prminwge.dta')

lm.10.3 <- lm(lprepop ~ lmincov + lusgnp , data=prminwge)
summary(lm.10.3)

prminwge.ts <- ts(data.frame(lprepop=prminwge$lprepop, lmincov=prminwge$lmincov, 
                             lusgnp=prminwge$lusgnp), start=1950)
prminwge.ts
autoplot(prminwge.ts, ts.size=1)
autoplot(acf(lm.10.3$residuals, plot=FALSE))



# Example 10.4
fertil3 <- read.dta('fertil3.dta')
# view the data
fertil3.ts <- ts(data.frame(gfr=fertil3$gfr, pe=fertil3$pe, 
                            pe_1=fertil3$pe_1, pe_2=fertil3$pe_2, 
                            pill=fertil3$pill, ww2=fertil3$ww2), start=1913)
autoplot(fertil3.ts, ts.size=1, ts.colour="deeppink1")

# create the models
lm.10.4.1 <- lm(gfr ~ pe + ww2 + pill, data=fertil3)
summary(lm.10.4.1)

# multicollinearity between pe, pe1, pe2 makes it hard to estimate their coefs
lm.10.4.2 <- lm(gfr ~ pe + pe_1 + pe_2 + ww2 + pill, data=fertil3)
lm.10.4.2
summary(lm.10.4.2)

# Joint significance of pe values
lm.10.4.2res <- lm(gfr ~ ww2 + pill, data=fertil3, subset=(is.na(pe_2)==FALSE))
lm.10.4.2res #called the restricted model, and lm.10.4.2 is the unrestricted model
lm.10.4.2
anova(lm.10.4.2res, lm.10.4.2) # F = 3.973

# Joint significance of lagged pe values
lm.10.4.2res2 <- lm(gfr ~ pe + ww2 + pill, data=fertil3, subset=(is.na(pe_2)==FALSE))
lm.10.4.2res2
lm.10.4.2
anova(lm.10.4.2res2, lm.10.4.2) # F = 0.0534

# Standard error of the long run propensity (running the theta=LRP regression
# to get standard error of the coefficient on pe)
with(fertil3,
     {
      p1p0 <- pe_1-pe
      p2p1 <- pe_2-pe_1
      print(summary(lm(gfr ~ pe + p1p0 + p2p1 + ww2 + pill)))})
# so theta (LRP) coefficient (on pe) is significant. There is a 
# significant long-run effect of pe. 


# Observe that pe and pe_1 are autocorrelated since they have a correlation
r <- cor(fertil3$pe, fertil3$pe_1, use="complete.obs") ; r
r.test <- cor.test(fertil3$pe, fertil3$pe_1, use="complete.obs") ; r.test
# t-stat = (r - rho) / sqrt((1 - r^2)/(n-2)), here rho=0
tstat <- r /  sqrt((1 - r^2)/(71-2))
tstat; r.test$statistic
# covariance:
p <- fertil3$pe[2:72] 
p1 <- fertil3$pe_1[2:72]
cov(p, p1) / sqrt(var(p)*var(p1))  # is the same as correlation coefficient



# Example 10.5
barium <- read.dta('barium.dta')

lm.10.5 <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + 
                    affile6 + afdec6, data=barium)
summary(lm.10.5)




# Example 10.6
fair <- read.dta('fair.dta')

with(fair,{
      pg <- partyWH*gnews
      pi <- partyWH*inf
      lm.10.6 <- lm(demvote ~ partyWH + incum + pg + pi)
      summary(lm.10.6)
})
# Slightly different results since the book uses 20 observations to 1992.
# The results are the same when we drop the last observation.

# Now, like the book
with(fair[fair$year != 1996, ],{
      pg <- partyWH*gnews
      pi <- partyWH*inf
      lm.10.6 <- lm(demvote ~ partyWH + incum + pg + pi)
      summary(lm.10.6)
})





# Example 10.7
hseinv <- read.dta('hseinv.dta')
logInvpc.ts <- ts(hseinv$linvpc, start=1947, frequency = 1)
logPrice.ts <- ts(hseinv$lprice, start=1947, frequency = 1)
autoplot(logInvpc.ts, ts.size = 1)
autoplot(logPrice.ts, ts.size = 1)
autoplot(acf(logInvpc.ts, plot=FALSE), ts.size = 1)
autoplot(acf(logPrice.ts, plot=FALSE), ts.size = 1)

lm.10.7.1 <- lm(linvpc ~ lprice, data=hseinv)
summary(lm.10.7.1) # spurious regression

"
# Time trend is significant, while lprice coef is not, showing that other factors
# captured in time trend are causing the change in loginvpc, not at all the price. 
# Spurious regression occurred since invpc and price are trending upward over time."
lm.10.7.2 <- lm(linvpc ~ lprice + t, data=hseinv)
summary(lm.10.7.2) 



# Example 10.8
fertil3 <- read.dta('fertil3.dta')

"An example of when adding trend 't' makes a coefficient (pe_t) more significant.
But pill is not significant anymore."
lm.10.8.1 <- lm(gfr ~ pe + ww2 + pill + t, data=fertil3)
summary(lm.10.8.1)

"Using quadratic trend to account for the ups and downs of gfr."
lm.10.8.2 <- lm(gfr ~ pe + ww2 + pill + t + tsq, data=fertil3)
summary(lm.10.8.2)

anova(lm.10.8.1, lm.10.8.2) #so model2 is much better due to quadratic trend. 



# Example 10.9
prminwge <- read.dta('prminwge.dta')

lm.10.9 <- lm(lprepop ~ lmincov + lusgnp + t, data=prminwge)
summary(lm.10.9)

# Obtain results by detrending
with(prminwge, {
      yu <- lm(lprepop ~ t)$resid
      x1u <- lm(lmincov ~ t)$resid
      x2u <- lm(lusgnp ~ t)$resid
      summary(lm(yu ~ x1u + x2u - 1))
})



# Example 10.10
hseinv <- read.dta('hseinv.dta')

with(hseinv,{
      dtr.linvpc <- lm(linvpc ~ t)$resid
      summary(lm(dtr.linvpc ~ lprice + t))
})
# Thus, movements in lprice about its trend have no explanatory power
# R2=0.008) over movements in linvpc about its trend
# Consistent with low significance of lprice in this last regression. 



# Example 10.11
barium <- read.dta('barium.dta')

# using seasonal dummy variables
lm.10.11 <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 +
                   feb + mar + apr + may + jun + jul + 
                   aug + sep + oct + nov + dec, data=barium)
lm.10.11restricted <- lm(lchnimp ~ lchempi + lgas + lrtwex + 
                               befile6 + affile6 + afdec6, data=barium)
# F-test: are the coefficients on the months significantly different than 0?
anova(lm.10.11,lm.10.11restricted)
