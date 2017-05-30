setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")

homes <- read.dta("homes.dta")
homes
dirateVar <- diff(homes$irate)
dhomeVar <- diff(homes$homes)
homesData <- data.frame(homes=homes$homes, 
                        homes_1=c(NA, homes$homes[1:218]),
                        dhomes=c(NA, dhomeVar),
                        dhomes_1=c(NA, NA, dhomeVar[1:217]),
                        irate=homes$irate, 
                        irate_1=c(NA, homes$irate[1:218]), 
                        irate_2=c(NA, NA, homes$irate[1:217]), 
                        dirate=c(NA, dirateVar), 
                        dirate_1=c(NA, NA, dirateVar[1:217]), 
                        dirate_2=c(NA, NA, NA, dirateVar[1:216]))
head(homesData)

homes.lm <- lm(data=homesLastRowRemoved, dhomes ~ dirate_1)
summary(homes.lm)
nrow(homesData); nrow(homesLastRowRemoved)
head(homesData)

# TODO:
# which one gives same results as in book? Why not the same as in book?
homesLastRowRemoved <- homesData[ -nrow(homesData), ]
homesLastRowRemoved <- na.pass(homesLastRowRemoved) #[is.na(homesLastRowRemoved)] <- 0
tail(homesLastRowRemoved)
homesLastTwoRowsRemoved <- homesData[-218:-219,]
tail(homesLastTwoRowsRemoved)
homesFirstRowRemoved <- homesData[ -1, ]
homesFirstRowRemoved <- na.pass(homesFirstRowRemoved)
head(homesFirstRowRemoved)
