setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter11_SimultaneousEquationsModels")
rm(list=ls())

library(lmtest)
library(dplyr)
library(foreign)
library(ggfortify)
#install.packages("systemfit")
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm





## Part a) 
mroz <- read.dta("mroz.dta")
mroz.dp <- tbl_df(mroz)

# summary stats for data with lfp == 1
mroz.dp %>%
      select(age, kidsl6, faminc, lfp) %>%
      filter(lfp == 1) %>%
      summarise(mean.age = mean(age), mean.kids = mean(kidsl6), 
                mean.fam = mean(faminc), 
                sd.age = sd(age), sd.kids = sd(kidsl6), sd.fam = sd(faminc))
# stats with data with lfp == 0
mroz.dp %>%
      select(age, kidsl6, faminc, lfp) %>%
      filter(lfp == 0) %>%
      summarise(mean.age = mean(age), mean.kids = mean(kidsl6), 
                mean.fam = mean(faminc), 
                sd.age = sd(age), sd.kids = sd(kidsl6), sd.fam = sd(faminc))


# Or do it this way
tapply(data=mroz, age, lfp == 1, mean)
tapply(data=mroz, faminc, lfp == 1, mean)
tapply(data=mroz, kidsl6, lfp == 1, mean)

# TODO - what is going on? why gives errors?
tapply(data=mroz, age, lfp == 1, sd)
tapply(data=mroz, faminc, lfp == 1, sd)
tapply(data=mroz, kidsl6, lfp == 1, sd)





## Part c) estimate supply hours equation
mroz.1 <- tbl_df(mroz) %>% 
      select(hours, faminc, wage, educ, age, kidsl6, kids618, exper, lfp) %>%
      filter(lfp == 1)
mroz.1$exper2 <- (mroz.1$exper)^2
head(mroz.1)

lnwage <- log(mroz.1$wage); length(lnwage)
nwifeinc <- with(mroz.1, faminc - wage * hours)
hours.lm <- lm(data=mroz.1, hours ~ lnwage + educ + age + kidsl6 + 
                     kids618 + nwifeinc)
summary(hours.lm)





## Part d) estimate reduced form lnwage equation
lnwage.reduced.lm <- lm(data=mroz.1, lnwage ~ educ + age + kidsl6 + kids618 +
                              nwifeinc + exper + exper2)

summary(lnwage.reduced.lm)

# Doing F-test of joint significance for IV vars exper, exper2
# These exogenous variables are absent from HOURS equation, making the
# HOURS equation identified. Checking "how well" it's identified: 
u.lm <- lnwage.reduced.lm
r.lm <- lm(data=mroz.1, lnwage ~ educ + age + kidsl6 + kids618 + nwifeinc)
anova(r.lm, u.lm) # instrumental vars exper and exper2 not strong enough
# if using the F > 10 rule since F = 8.2502





## Part f) estimating HOUSE with 2SLS not OLS
struct <- list(houseSupply = hours ~ lnwage + educ + age + kidsl6 + 
                     kids618 + nwifeinc)
inst <- ~ educ + age + kidsl6 + kids618 + nwifeinc + exper + exper2
hours.2sls <- systemfit(struct, inst=inst, method="2SLS", data=mroz.1)
summary(hours.2sls)
