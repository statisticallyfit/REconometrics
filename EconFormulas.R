source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/Util.R')

# library(DescTools) # consider using this also: epitools, vcd, TablesInR...


describe <- function(x) {
      library(tseries)
      invisible(cat("\nObs:          ", length(x), "\n"))
      invisible(cat("NAs:          ", sum(is.na(x)), "\n"))
      invisible(cat("min:          ", min(x), "\n"))
      invisible(cat("max:          ", max(x), "\n"))
      invisible(cat("\nmean:         ", mean(x), "\n"))
      invisible(cat("median:       ", median(x), "\n"))
      invisible(cat("std.dev:      ", sd(x), "\n"))
      invisible(cat("variance:     ", var(x), "\n"))
      invisible(cat("skewness:     ", skewness(x), "\n"))
      invisible(cat("kurtosis:     ", kurtosis(x), "\n"))
      
      j <- jarque.bera.test(x)
      s <- j$statistic
      p <- j$p.value
      invisible(cat("\nJarque-Bera:  ", j$statistic, "\n"))
      invisible(cat("p-value:      ", j$p.value, "\n\n"))
}



totalMultiplier <- function(lmObject){
      result <- sum(lmObject$coefficients) - lmObject$coefficients[1]
      return(result[[1]])
}

# K = number of parameters including intercept
modelCriteria <- function(y, yhat, K, lag) {
      sse <- sum( (y - yhat)^2 )
      N <- length(y) # Must equal length of yhat
      aic <- log(sse/N) + 2*K/N
      bic <- log(sse/N) + K*log(N)/N
      aicc <- -(abs(aic) - 1 - log(2*pi))
      bicc <- -(abs(bic) - 1 - log(2*pi))
      cat("\nAIC:  ", aic)
      cat("\nAICc: ", aicc)
      cat("\nBIC:  ", bic)
      cat("\nBICc: ", bicc, "\n ")
      criteriaAndLag <- c(aic, bic, lag)
      return(invisible(criteriaAndLag)) # yaya this works!!!
}



# copied this code (did not make)
ivreg2 <- function(form,endog,iv,data,digits=3){
      # library(MASS)
      # model setup
      r1 <- lm(form,data)
      y <- r1$fitted.values+r1$resid
      x <- model.matrix(r1)
      aa <- rbind(endog == colnames(x),1:dim(x)[2])  
      z <- cbind(x[,aa[2,aa[1,]==0]],data[,iv])  
      colnames(z)[(dim(z)[2]-length(iv)+1):(dim(z)[2])] <- iv  
      # iv coefficients and standard errors
      z <- as.matrix(z)
      pz <- z %*% (solve(crossprod(z))) %*% t(z)
      biv <- solve(crossprod(x,pz) %*% x) %*% (crossprod(x,pz) %*% y)
      sigiv <- crossprod((y - x %*% biv),(y - x %*% biv))/(length(y)-length(biv))
      vbiv <- as.numeric(sigiv)*solve(crossprod(x,pz) %*% x)
      res <- cbind(biv,sqrt(diag(vbiv)),biv/sqrt(diag(vbiv)),(1-pnorm(biv/sqrt(diag(vbiv))))*2)
      res <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
      rownames(res) <- colnames(x)
      colnames(res) <- c("Coef","S.E.","t-stat","p-val")
      # First-stage F-test
      y1 <- data[,endog]
      z1 <- x[,aa[2,aa[1,]==0]]
      bet1 <- solve(crossprod(z)) %*% crossprod(z,y1)
      bet2 <- solve(crossprod(z1)) %*% crossprod(z1,y1)
      rss1 <- sum((y1 - z %*% bet1)^2)
      rss2 <- sum((y1 - z1 %*% bet2)^2)
      p1 <- length(bet1)
      p2 <- length(bet2)
      n1 <- length(y)
      fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
      firststage <- c(fs)
      firststage <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),firststage)),ncol=length(firststage))
      colnames(firststage) <- c("First Stage F-test")
      # Hausman tests
      bols <- solve(crossprod(x)) %*% crossprod(x,y) 
      sigols <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y)-length(bols))
      vbols <- as.numeric(sigols)*solve(crossprod(x))
      sigml <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y))
      x1 <- x[,!(colnames(x) %in% "(Intercept)")]
      z1 <- z[,!(colnames(z) %in% "(Intercept)")]
      pz1 <- z1 %*% (solve(crossprod(z1))) %*% t(z1)
      biv1 <- biv[!(rownames(biv) %in% "(Intercept)"),]
      bols1 <- bols[!(rownames(bols) %in% "(Intercept)"),]
      # Durbin-Wu-Hausman chi-sq test:
      # haus <- t(biv1-bols1) %*% ginv(as.numeric(sigml)*(solve(crossprod(x1,pz1) %*% x1)-solve(crossprod(x1)))) %*% (biv1-bols1)
      # hpvl <- 1-pchisq(haus,df=1)
      # Wu-Hausman F test
      resids <- NULL
      resids <- cbind(resids,y1 - z %*% solve(crossprod(z)) %*% crossprod(z,y1))
      x2 <- cbind(x,resids)
      bet1 <- solve(crossprod(x2)) %*% crossprod(x2,y)
      bet2 <- solve(crossprod(x)) %*% crossprod(x,y)
      rss1 <- sum((y - x2 %*% bet1)^2)
      rss2 <- sum((y - x %*% bet2)^2)
      p1 <- length(bet1)
      p2 <- length(bet2)
      n1 <- length(y)
      fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
      fpval <- 1-pf(fs, p1-p2, n1-p1)
      #hawu <- c(haus,hpvl,fs,fpval)
      hawu <- c(fs,fpval)
      hawu <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),hawu)),ncol=length(hawu))
      #colnames(hawu) <- c("Durbin-Wu-Hausman chi-sq test","p-val","Wu-Hausman F-test","p-val")
      colnames(hawu) <- c("Wu-Hausman F-test","p-val")  
      # Sargan Over-id test
      ivres <- y - (x %*% biv)
      oid <- solve(crossprod(z)) %*% crossprod(z,ivres)
      sstot <- sum((ivres-mean(ivres))^2)
      sserr <- sum((ivres - (z %*% oid))^2)
      rsq <- 1-(sserr/sstot)
      sargan <- length(ivres)*rsq
      spval <- 1-pchisq(sargan,df=length(iv)-1)
      overid <- c(sargan,spval)
      overid <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),overid)),ncol=length(overid))
      colnames(overid) <- c("Sargan test of over-identifying restrictions","p-val")
      if(length(iv)-1==0){
            overid <- t(matrix(c("No test performed. Model is just identified")))
            colnames(overid) <- c("Sargan test of over-identifying restrictions")
      }
      full <- list(results=res, weakidtest=firststage, endogeneity=hawu, overid=overid)
      return(full)
}





# Written by John Fox to calculate HAC standard errors
# source: http://novicemetrics.blogspot.ro/2011/04/video-tutorial-on-robust-standard.html
summaryHAC <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
      
      if (!require(car)) stop("Required car package is missing.")
      
      type <- match.arg(type)
      V <- hccm(model, type=type)
      sumry <- summary(model)
      table <- coef(sumry)
      table[,2] <- sqrt(diag(V))
      table[,3] <- table[,1]/table[,2]
      table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
      
      sumry$coefficients <- table
      p <- nrow(table)
      hyp <- cbind(0, diag(p - 1))
      sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
      
      print(sumry)
      cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
      
      # the below is added by statisticallyfit
      ## returning HAC standard errors
      return(table[,2])
}



# Tests the vector time series data for stationarity 
# Input vector (v) of values
# Finds the t-(tau)statistic from regression deltaValue = laggedValue + error
# diffed.lags = number of diffed lags needed to reduce error autocorrelation
dickeyFullerTest <- 
      function(v, type=c("none", "drift", "trend"), diffed.lags){
      
      library(urca)
      
      ur <- ur.df(v, type=type, lags=diffed.lags)
      tau.statistic <- ur@teststat[1,1]
      critical.value <- ur@cval[1,2]
      
      cat("###################################################################\n")
      cat("####      Augmented Dickey Fuller Test for Stationarity       #####\n")
      cat("###################################################################\n")
      cat("                                                                   \n")
      cat(" Testing equation: \n")
      print(ur@testreg$coefficients)
      cat("\n")
      
      cat(" tau statistic:                         ", tau.statistic, "\n")
      cat(" critical value:                        ", critical.value, "\n")
      
      if (tau.statistic < critical.value)
            cat("\n Result: Stationary")
      else 
            cat("\n Result: Not enough evidence to reject non-stationarity")
      
      # returning the residuals from dickey fuller to test if autocorrelation
      # left and if so, the user must increase the lags. 
      return (invisible(ur@res))
}



#dickeyFullerStationarityTest <- function(v, useTrend=FALSE, k=0){
#      n <- length(v)
#      #v_1 <- c(NA, v[1:(n-1)])
#      #dv <- c(NA, diff(v))
#      #data <- data.frame(dv=dv, v_1=v_1)
#      
#      # Creating the diffed lags
#      data.diffedLag0 <- makeDiffedLags(v, from=0, to=k)
#      data.lag1 <- makeLags(v, from=1, to=1)
#      data <- cbind(data.lag1, data.diffedLag0)
#      if(useTrend)
#            data$trend <- seq(1:n)
#      #if(k != 0){
#      #      c <- ncol(data) + 1
#      #      for(i in 1:k){
#      #            diffedLag <- c(rep(NA, i), dv[1:(n-i)])
#      #            data[, c] <- diffedLag
#      #            c <- c + 1
#      #      }
#      #}
#      # Find the regression 
#      numCols <- ncol(data)
#      lm <- ""
#      if(numCols == 2){
#            lm <- lm(dv ~ v_1, data=data)     
#      }else {# error - just need to make sure to get right tau and right reg
#            lm <- lm(dv ~ ., data=data[ , 2:numCols])
#      }      
#      tau <- summary(lm)$coefficients[2,3]
#      return(tau)
#}


# v = dataframe with one column on values
# k from and to = from and to limits (lags 2 to 4, for example)
makeDiffedLags <- function(v, from, to) {
      n <- nrow(v)
      name <- names(v)
      vec <- v[,1]
      dv <- c(NA, diff(vec))
      data <- data.frame(dv=dv)
      #firstLabel <- paste("d", name, sep="")
      #data <- data.frame(firstLabel = dv)
      col <- ncol(data) + 1
      Vcounter <- 2
      for(i in from:to){
            diffedLag <- c(rep(NA, i), dv[1:(n-i)])
            actualLabel <- paste("d", name, "_", i, sep="")
            changeLabel <- paste("V", Vcounter, sep="")
            data[, col] <- diffedLag
            names(data)[names(data) == changeLabel] <- actualLabel
            col <- col + 1
            Vcounter <- Vcounter + 1
      }
      data$dv <- NULL
      return (data)
}

makeLags <- function(v, from, to){
      n <- nrow(v)
      name <- names(v)
      vec <- v[,1]
      data <- data.frame(v=vec)
      c <- ncol(data) + 1
      Vcounter <- 2
      for(i in from:to){
            lag <- c(rep(NA, i), vec[1:(n-i)])
            actualLabel <- paste(name, "_", i, sep="")
            changeLabel <- paste("V", Vcounter, sep="")
            data[, c] <- lag
            names(data)[names(data) == changeLabel] <- actualLabel
            c <- c + 1
            Vcounter <- Vcounter + 1
      }
      data$v <- NULL
      return (data)
}

# data[,1] = y, data[,2]=x holds the data vectors that are 
#     supposedly cointegrated
# returns the lagged residuals to make it useful for estimating VEC model
# CORRECTION: now returns residuals from dickey fuller test of 
#     cointegrating residuals to know how many lags to include
# Precondition: in data, y is first col, x is second, time is third
# type is either "none", "constant", or "trend"
# lags = number of lags to include in dickey fuller test of stationary 
# for the cointeg.lm's residuals
cointegrationTest <- function(data, type, resid.lags = 0){
      yName <- names(data)[1]
      xName <- names(data)[2]
      y <- data[,1]
      x <- data[,2]
      t <- 0
      if(type == "trend") {
            if (ncol(data) != 3) {
                  cat("\nERROR: Need to include a 'time' column", "\n")
                  return(invisible(0))
            } else {
                  t <- data[,3]
            }
      }
      
      cointeg.lm <- ""
      if (type == "none")          cointeg.lm <- lm(y ~ x + 0)
      else if (type == "constant") cointeg.lm <- lm(y ~ x)
      else if (type == "trend")    cointeg.lm <- lm(y ~ x + t)
      
      # from urca package
      ur <- ur.df(cointeg.lm$res, type="none", lags=resid.lags)
      testStatistic <- ur@teststat[1]
      df.resids <- ur@res
      
      cat("##################################################\n")
      cat("######         Cointegration Test          #######\n")
      cat("##################################################\n")
      cat("                                                  \n")
      cat(" Cointegrating equation: \n")
      
      if (type == "none"){
            b <- cointeg.lm$coefficients[[1]]
            cat(" ", yName, " = ", b, " (", xName, ")", "\n", sep="")
      }
      else if(type == "constant"){
            a <- cointeg.lm$coefficients[[1]]
            b <- cointeg.lm$coefficients[[2]]
            cat(" ", yName, " = ", a, " + ", b, " (", xName, ")", 
                "\n", sep="")
      } else if (type == "trend"){
            a <- cointeg.lm$coefficients[[1]]
            b <- cointeg.lm$coefficients[[2]]
            tCoef <- cointeg.lm$coefficients[[3]]
            cat(" ", yName, " = ", a, " + ", b, " (", xName, ")", 
                " + ", tCoef, " (t)","\n", sep="")
      }
      
      cat("                                                  \n")
      
      criticalValue <- 0
      if (type == "none")          criticalValue <- -2.76
      else if (type == "constant") criticalValue <- -3.37
      else if (type == "trend")    criticalValue <- -3.42
      cat(" test statistic:                        ", testStatistic, "\n")
      cat(" critical value:                        ", criticalValue, "\n")
      
      if (testStatistic > criticalValue)
            cat("\n\n Result: Not cointegrated -> spurious regression")
      else 
            cat("\n\n Result: Cointegrated")
      #return (invisible(cointeg.lm$residuals))
      return (invisible(df.resids))
}

# data must only have two columns (y first then x)
# type must either be: "none", "const", "trend", or "both"
# lag = numbre of lagged terms to incldue in the VEC model
VEC <- function(data, type, lag = 0){
      # if 3 cols, assume 3rd is time and delete it
      if(ncol(data) == 3) data[,3] <- NULL
      v <- suppressWarnings(VECM(ts(data), lag=lag, 
                            r=1, include=type, estim="2OLS"))
      print(summary(v))
      
      v$residuals <- data.frame(y=v$residuals[,1], x=v$residuals[,2])
      return(invisible(v$residuals))
}



# First part from: https://faculty.chicagobooth.edu/ruey.tsay/teaching/introTS/
# rtn = time series
# lags = number of lags to put into the error regression equation
# This answers the question: Do we need ARCH model? 
archTest <- function(rtn, order.lags){
      # Perform Lagrange Multiplier Test for ARCH effect of a time series
      # rtn: time series
      # m: selected AR order
      #
      y <- (rtn - mean(rtn))^2
      T <- length(rtn)
      atsq <- y[(order.lags + 1):T]
      x <- matrix(0, (T - order.lags), order.lags)
      for (i in 1:order.lags){
            x[,i] <- y[(order.lags + 1 - i):(T - i)]
      }
      md <- lm(atsq ~ x)
      # now this is my work: 
      R.squared <- summary(md)$r.squared
      LM.statistic <- (T - order.lags) * R.squared
      chi.critical <- qchisq(0.95, df=order.lags)
      
      cat("###########################################################\n")
      cat("####     Lagrange-Multiplier Test for ARCH effects     ####\n")
      cat("###########################################################\n")
      cat("                                                  \n")
      cat(" Testing equation: ", "\n\n")
      print(summary(md)$coeff)
      cat("\n\n")
      cat(" LM statistic:                        ", LM.statistic, "\n")
      cat(" chi-square critical value:           ", chi.critical, "\n")
      
      if (LM.statistic > chi.critical)
            cat("\n\n Result: There are ARCH effects")
      else 
            cat("\n\n Result: No evidence of ARCH effects")
      
      # do you need to test residuals to see if test worked well? 
      #return (invisible(md$residuals))
}




# this fits an ARCH(p) model
# Returns the error variance function which models the conditional
# heteroskedasticity in original time series. 
# data = data.frame
# p = num lags of error - represents AR(p) component
ARCH <- function(data, p){
      library(fGarch)
      formula <- formula(paste("~garch(", p, ",", 0, ")", sep=""))
      arch <- garchFit(data=data, formula, trace=F)
      print(arch@fit$matcoef)
      #autoplot(ts(arch@h.t), main="Conditional heteroskedasticity function")
      return(invisible(arch))
}



# this fits an GARCH(p,q) model
# Returns the error variance function which models the conditional
# heteroskedasticity in original time series. 
# data = data.frame
# p = num lags of error - represents AR(p) component
# q = num lags of variance - represents the MA(q) component
GARCH <- function(data, p, q){
      library(fGarch)
      formula <- formula(paste("~garch(", p, ",", q, ")", sep=""))
      garch <- garchFit(data=data, formula, trace=F)
      print(garch@fit$matcoef)
      return(invisible(garch))
}


## implement list: 
#### 1) VAR equivalent of VEC
#### 2) lagrange multiplier test for error autocorrelation
#### 3) TGARCH and TARCH
#### 4) GARCH-in-mean and ARCH-in-mean
