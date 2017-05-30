library(ggplot2)
library(rdatamarket)

#install.packages("rdatamarket")

getwd()
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/BasicDataAnalysisforTimeSerieswithR")


# Simulating data from model and estimate parameters
n = 50
x = c(1:n)
# simulate 50 random normal errors
error = rnorm(n, 0, 3)
y = 3 + 0.5*x + error
# ----- METHOD 1 to plot
g = ggplot(data=data.frame(x, y), 
       aes(x=x, y=y), 
       main="The line y = 3 + 0.5x, n=50") + 
  geom_point(shape=19)
g + geom_smooth(method="lm", size=1)
# ----- METHOD 2 use abline
g + geom_abline(intercept=fit$coefficients[1], 
              slope=fit$coefficients[2], 
              size=1, col="red")
# ----- METHOD 3
plot(x,y)
lines(x, fit$fitted.values)
# ----- METHOD 4
plot(x,y)
lines(x, fit$f)
# ----- METHOD 5
predict = fit$coeff[1] + fit$coeff[2]*x
plot(x, y)
lines(x, predict)


# Estimating parameters
fit = lm(y ~ x); fit
anova(fit)
summary.aov(fit)
summary.lm(fit)
summary(fit)
names(fit)
# SEb = (stdev of residuals) / sqrt((n-1) * (s_x^2))
# SEa = (stdev of residuals) * sqrt(1/n + xbar^2/((n-1)s_x^2))
b.std.error = sd(error) / sqrt((n-1) * var(x)); b.std.error
a.std.error = sd(error) * sqrt(1/n + (mean(x))^2/((n-1)*var(x))); a.std.error

# Check residuals
qplot(sample=error, stat='qq')
# or
plot(fit$fitted, fit$residuals)
abline(0, 0) #slope 0 and mean 0


# OLS the MATRIX way
# ---- METHOD 1 to create xMatrix
col1 = rep(1, n)
col2 = x
xMatrix = cbind(col1, col2)
xMatrix
# ---- METHOD 2 to create xMatrix
x = c(1:50)
xTemp = c(rep(1,n), x)
xMatrix = matrix(xTemp, ncol=2)
xMatrix
# solve(a, b) solves ax = b
newFit = solve(t(xMatrix) %*% xMatrix, t(xMatrix) %*% y)
newFit
summary.lm(fit)




# ------- EXERCISES
# 1
n = 50
x = c(1:n)
error = rnorm(n, 20, 10) # must experiment with the variance
y = 5 - 4*x + error

g = ggplot(data=data.frame(x, y), aes(x=x, y=y)) + 
  geom_point(shape=19)
g + geom_smooth(method="lm", size=1)

fit = lm(y ~ x); fit 
summary.lm(fit)
r = cor(x, y); r; r^2

qplot(sample=fit$residuals, stat='qq')
