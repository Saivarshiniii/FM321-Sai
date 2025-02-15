#clear all
rm(list = ls())

.libPaths(c(.libPaths(), "C:/Users/tpd19wzu/Documents/R/MyLibrary"))
.libPaths(c(.libPaths(), "C:/Program Files/R/R-4.1.3/library"))

library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(timeSeries)
library(tseries)
library(roll)
library(car)
library(MASS)
library(extraDistr)
library(rugarch)
library(QRM)
library(dplyr)
library(reshape2)
library(lubridate)
library(moments)
library(zoo)
library(knitr)
library(rmarkdown)
library(rmgarch)

#setting working directory
setwd("C:/data/WRDS-FM321/INPUT")

#importing dataset
data<-read.csv("C:/data/WRDS-FM321/INPUT/10decdata.csv")

#creating adjusted prices
data$adjusted_prices<-data$PRC/data$CFACPR

#subsetting data on returns
BAC_ret<- data[data$PERMNO == 59408, c("date","RET")]
names(BAC_ret)[2]<- "BAC"
BP_ret<- data[data$PERMNO == 29890, c("date","RET")]
names(BP_ret)[2]<- "BP"
JNJ_ret<- data[data$PERMNO == 22111, c("date","RET")]
names(JNJ_ret)[2]<- "JNJ"

RET<- merge(BAC_ret, BP_ret, all=FALSE, by="date")
RET<- merge(RET, JNJ_ret, all=FALSE, by="date")
RET <- RET[!duplicated(RET$date), ]

date.ts <- ymd(RET$date)
RET$date <- as.Date(as.character(RET$date), format = "%Y%m%d")

RET$BAC <- as.numeric(RET$BAC)
RET$BP<- as.numeric(RET$BP)
RET$JNJ <- as.numeric(RET$JNJ)

# Backtesting VaR
#Time period = 10 years
stocks <- c("BAC","BP","JNJ")
p <- 0.01
lambda <- 0.94
value <- 1
T <- 2600
WE <- 500
WT <- 2600 - WE
Burn <- 30

# Initialize results list
results_10 <- list()
coefficients_10 <- list() #stores rolling GARCH and t-GARCH coefficients for all stocks for this period

# Main Loop for Stocks
for (stock in stocks) {
  y_stock <- tail(RET[[stock]], T)
  d_ts <- tail(date.ts, T)
  
  VaR_10 <- data.frame(
    Date = d_ts,
    y_stock = y_stock
  )
  colnames(VaR_10)[2] <- paste0("y_", stock)
  
  # Historical Simulation
  VaR_10[[paste0("HS_", stock)]] <- NA
  for (t in (WE + 1):T) {
    t1 <- t - WE
    t2 <- t - 1
    window <- VaR_10[[paste0("y_", stock)]][t1:t2]
    VaR_10[[paste0("HS_", stock)]][t] <- -sort(window)[WE * p] * value
  }
  
  # GARCH
  VaR_10[[paste0("GARCH_", stock)]] <- NA
  spec_garch <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )
  garch_coeffs_10 <- list()  #Stores rolling GARCH coefficients for this time period
  
  for (t in (WE + 1):T) {
    t1 <- t - WE
    t2 <- t - 1
    print(t)
    window <- VaR_10[[paste0("y_", stock)]][t1:t2]
    fit <- ugarchfit(spec = spec_garch, data = window, solver = "hybrid")
    s2 <- coef(fit)[1] + 
      coef(fit)[2] * tail(window, 1)^2 + 
      coef(fit)[3] * tail(fit@fit$var, 1)
    VaR_10[[paste0("GARCH_", stock)]][t] <- -value * qnorm(p, sd = sqrt(s2))
    garch_coeffs_10[[t]] <- coef(fit)
    print(garch_coeffs_10[[t]])
  }
  
  # Save GARCH coefficients
  coefficients_10[[paste0("GARCH_", stock,"_10")]] <- garch_coeffs_10
  
  # t-GARCH
  VaR_10[[paste0("tGARCH_", stock)]] <- NA
  spec_tgarch <- ugarchspec(
    variance.model = list(garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "std"
  )
  tgarch_coeffs_10 <- list()  # Stores rolling GARCH coefficients for this stock
  
  
  for (t in (WE + 1):T) {
    t1 <- t - WE
    t2 <- t - 1
    window <- VaR_10[[paste0("y_", stock)]][t1:t2]
    print(t)
    fit <- ugarchfit(spec = spec_tgarch, data = window, solver = "hybrid")
    df <- fit@fit$coef["shape"]
    omega <- coef(fit)[1]
    alpha <- coef(fit)[2]
    beta <- coef(fit)[3]
    s2 <- omega + 
      alpha * tail(window, 1)^2 + 
      beta * tail(fit@fit$var, 1)
    VaR_10[[paste0("tGARCH_", stock)]][t] <- -value * sqrt(s2) * qt(p, df = df) * sqrt((df - 2) / df)
    tgarch_coeffs_10[[t]] <- coef(fit)
    print(tgarch_coeffs_10[[t]])# Save coefficients
  }
  
  # Save t-GARCH coefficients
  coefficients_10[[paste0("tGARCH_", stock,"_10")]] <- tgarch_coeffs_10
  
  results_10[[stock]] <- VaR_10
}

#Time period = 10 years
stocks <- c("BAC","BP","JNJ")  # Example stocks
p <- 0.01
lambda <- 0.94
value <- 1
T <- 1400
WE <- 250
WT <- 1400 - WE
Burn <- 30

results_5 <- list()
coefficients_5 <- list() 

for (stock in stocks) {
  y_stock <- tail(RET[[stock]], T)
  d_ts <- tail(date.ts, T)
  
  VaR_5 <- data.frame(
    Date = d_ts,
    y_stock = y_stock
  )
  colnames(VaR_5)[2] <- paste0("y_", stock)
  
  # Historical Simulation
  VaR_5[[paste0("HS_", stock)]] <- NA
  for (t in (WE + 1):T) {
    t1 <- t - WE
    t2 <- t - 1
    window <- VaR_5[[paste0("y_", stock)]][t1:t2]
    VaR_5[[paste0("HS_", stock)]][t] <- -sort(window)[WE * p] * value
  }
  
  # GARCH
  VaR_5[[paste0("GARCH_", stock)]] <- NA
  spec_garch <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )
  garch_coeffs_5 <- list()
  
  for (t in (WE + 1):T) {
    t1 <- t - WE
    t2 <- t - 1
    print(t)
    window <- VaR_5[[paste0("y_", stock)]][t1:t2]
    fit <- ugarchfit(spec = spec_garch, data = window, solver = "hybrid")
    s2 <- coef(fit)[1] + 
      coef(fit)[2] * tail(window, 1)^2 + 
      coef(fit)[3] * tail(fit@fit$var, 1)
    VaR_5[[paste0("GARCH_", stock)]][t] <- -value * qnorm(p, sd = sqrt(s2))
    garch_coeffs_5[[t]] <- coef(fit)
    print(garch_coeffs_5[[t]])
  }
  
  coefficients_5[[paste0("GARCH_", stock,"_5")]] <- garch_coeffs_5
  
  # t-GARCH
  VaR_5[[paste0("tGARCH_", stock)]] <- NA
  spec_tgarch <- ugarchspec(
    variance.model = list(garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "std"
  )
  tgarch_coeffs_5 <- list()  
  
  
  for (t in (WE + 1):T) {
    t1 <- t - WE
    t2 <- t - 1
    window <- VaR_5[[paste0("y_", stock)]][t1:t2]
    print(t)
    fit <- ugarchfit(spec = spec_tgarch, data = window, solver = "hybrid")
    df <- fit@fit$coef["shape"]
    omega <- coef(fit)[1]
    alpha <- coef(fit)[2]
    beta <- coef(fit)[3]
    s2 <- omega + 
      alpha * tail(window, 1)^2 + 
      beta * tail(fit@fit$var, 1)
    VaR_5[[paste0("tGARCH_", stock)]][t] <- -value * sqrt(s2) * qt(p, df = df) * sqrt((df - 2) / df)
    tgarch_coeffs_5[[t]] <- coef(fit)
    print(tgarch_coeffs_5[[t]])
  }
  
  coefficients_5[[paste0("tGARCH_", stock,"_5")]] <- tgarch_coeffs_5
  
  results_5[[stock]] <- VaR_5
}

# Accessing individual stock dataframes from the huge results_5 and results_10 lists
BAC_5<-results_5[["BAC"]]
BAC_10<-results_10[["BAC"]]
BP_5<-results_5[["BP"]]
BP_10<-results_10[["BP"]]
JNJ_5<-results_5[["JNJ"]]
JNJ_10<-results_10[["JNJ"]]

#removing NULL (warm-up period) coefficients from all the sublists in the coefficients_5 and coefficients_10 lists
coefficients_10 <- lapply(coefficients_10, function(x) Filter(Negate(is.null), x))

# Converting sublists into dataframes
GARCH_BAC_10<-coefficients_10[["GARCH_BAC_10"]]
GARCH_BAC_10_df <- as.data.frame(do.call(rbind, GARCH_BAC_10))
dates_coeff <- tail(date.ts, nrow(GARCH_BAC_10_df))#extracting tail dates so that it can be attached to individual dataframes with GARCH and t-GARCH coefficients
GARCH_BAC_10_df$Date <- dates_coeff  

GARCH_BP_10<-coefficients_10[["GARCH_BP_10"]]
GARCH_BP_10_df <- as.data.frame(do.call(rbind, GARCH_BP_10))  # Ensure it becomes a data frame
GARCH_BP_10_df$Date <- dates_coeff 

GARCH_JNJ_10<-coefficients_10[["GARCH_JNJ_10"]]
GARCH_JNJ_10_df <- as.data.frame(do.call(rbind, GARCH_JNJ_10))
GARCH_JNJ_10_df$Date <- dates_coeff 

GARCH_BAC_5<-coefficients_5[["GARCH_BAC_5"]]
GARCH_BAC_5_df <- as.data.frame(do.call(rbind, GARCH_BAC_5))
dates_coef <- tail(date.ts, nrow(GARCH_BAC_5_df))
GARCH_BAC_5_df$Date <- dates_coef 

GARCH_BP_5<-coefficients_5[["GARCH_BP_5"]]
GARCH_BP_5_df <- as.data.frame(do.call(rbind, GARCH_BP_5))
GARCH_BP_5_df$Date <- dates_coef

GARCH_JNJ_5<-coefficients_5[["GARCH_JNJ_5"]]
GARCH_JNJ_5_df <- as.data.frame(do.call(rbind, GARCH_JNJ_5))
GARCH_JNJ_5_df$Date <- dates_coef


tGARCH_BAC_10<-coefficients_10[["tGARCH_BAC_10"]]
tGARCH_BAC_10_df <- as.data.frame(do.call(rbind, tGARCH_BAC_10)) 
dates_coeff <- tail(date.ts, nrow(GARCH_BAC_10_df))
tGARCH_BAC_10_df$Date <- dates_coeff  

tGARCH_BP_10<-coefficients_10[["tGARCH_BP_10"]]
tGARCH_BP_10_df <- as.data.frame(do.call(rbind, tGARCH_BP_10))
tGARCH_BP_10_df$Date <- dates_coeff 

tGARCH_JNJ_10<-coefficients_10[["tGARCH_JNJ_10"]]
tGARCH_JNJ_10_df <- as.data.frame(do.call(rbind, tGARCH_JNJ_10))
tGARCH_JNJ_10_df$Date <- dates_coeff 

tGARCH_BAC_5<-coefficients_5[["tGARCH_BAC_5"]]
tGARCH_BAC_5_df <- as.data.frame(do.call(rbind, tGARCH_BAC_5)) 
dates_coef <- tail(date.ts, nrow(GARCH_BAC_5_df))
tGARCH_BAC_5_df$Date <- dates_coef 

tGARCH_BP_5<-coefficients_5[["tGARCH_BP_5"]]
tGARCH_BP_5_df <- as.data.frame(do.call(rbind, tGARCH_BP_5)) 
tGARCH_BP_5_df$Date <- dates_coef

tGARCH_JNJ_5<-coefficients_5[["tGARCH_JNJ_5"]]
tGARCH_JNJ_5_df <- as.data.frame(do.call(rbind, tGARCH_JNJ_5))
tGARCH_JNJ_5_df$Date <- dates_coef

#Plotting VaR estimates along with returns for each stock over two time periods
dev.new()
plot(BAC_10$Date,BAC_10$y_BAC, type = 'l', main = "Fig.1-Returns and VaR estimates-BAC", xlab = "Date", ylab = "Returns/VaR")
lines(BAC_10$Date,BAC_10$HS_BAC, type = 's', col = "blue", lwd = 2)
lines(BAC_10$Date,BAC_10$GARCH_BAC, type = 's', col = "magenta", lwd = 2)
lines(BAC_10$Date,BAC_10$tGARCH_BAC,type='s',col="green",lwd=2)
lines(BAC_5$Date,-BAC_5$HS_BAC, type = 's', col = "blue", lwd = 2)
lines(BAC_5$Date,-BAC_5$GARCH_BAC, type = 's', col = "magenta", lwd = 2)
lines(BAC_5$Date,-BAC_5$tGARCH_BAC,type='s',col="green",lwd=2)
legend("bottomleft", legend = c("HS", "GARCH", "t-GARCH"),col = c("blue", "magenta", "green"),lwd = 2,lty = 1, cex = 0.8)

plot(BP_10$Date,BP_10$y_BP, type = 'l', main = "Fig.2-Returns and VaR estimates-BP", xlab = "Date", ylab = "Returns/VaR")
lines(BP_10$Date,BP_10$HS_BP, type = 's', col = "blue", lwd = 2)
lines(BP_10$Date,BP_10$GARCH_BP, type = 's', col = "magenta", lwd = 2)
lines(BP_10$Date,BP_10$tGARCH_BP,type='s',col="green",lwd=2)
lines(BP_5$Date,-BP_5$HS_BP, type = 's', col = "blue", lwd = 2)
lines(BP_5$Date,-BP_5$GARCH_BP, type = 's', col = "magenta", lwd = 2)
lines(BP_5$Date,-BP_5$tGARCH_BP,type='s',col="green",lwd=2)
legend("bottomleft", legend = c("HS", "GARCH", "t-GARCH"),col = c("blue", "magenta", "green"),lwd = 2,lty = 1, cex = 0.8)

plot(JNJ_10$Date,JNJ_10$y_JNJ, type = 'l', main = "Fig.3-Returns and VaR estimates-JNJ", xlab = "Date", ylab = "Returns/VaR")
lines(JNJ_10$Date,JNJ_10$HS_JNJ, type = 's', col = "blue", lwd = 2)
lines(JNJ_10$Date,JNJ_10$GARCH_JNJ, type = 's', col = "magenta", lwd = 2)
lines(JNJ_10$Date,JNJ_10$tGARCH_JNJ,type='s',col="green",lwd=2)
lines(JNJ_5$Date,-JNJ_5$HS_JNJ, type = 's', col = "blue", lwd = 2)
lines(JNJ_5$Date,-JNJ_5$GARCH_JNJ, type = 's', col = "magenta", lwd = 2)
lines(JNJ_5$Date,-JNJ_5$tGARCH_JNJ,type='s',col="green",lwd=2)
legend("bottomleft", legend = c("HS", "GARCH", "t-GARCH"),col = c("blue", "magenta", "green"),lwd = 2,lty = 1, cex = 0.8)

#time-series plot of Degrees of freedom parameter for BAC and BP
plot(BAC_10$Date,BAC_10$y_BAC, type = 'l', main = "Fig.4-Returns & tGARCH dof-BAC", xlab="Date", ylab = "Returns")
plot(tGARCH_BAC_10_df$Date,tGARCH_BAC_10_df$shape, type = 'l',xlab = "Date", ylab = "Degrees of Freedom")

plot(BP_10$Date,BP_10$y_BP, type = 'l', main = "Fig.5-Returns & tGARCH dof-BP", xlab="Date", ylab = "Returns")
plot(tGARCH_BP_10_df$Date,tGARCH_BP_10_df$shape, type = 'l',xlab = "Date", ylab = "Degrees of Freedom")

#Calculation of violation ratios and plotting results
T=2600
WE=500
WT=2100

V=BAC_10[(WE+1):T,]
for(i in c("HS_BAC","GARCH_BAC","tGARCH_BAC")){
  V[,i]=-V[,"y_BAC"]-V[,i]
  V[V[,i]<0,i]=0
  V[V[,i]>0,i]=1
}
colSums(V[,c("HS_BAC","GARCH_BAC","tGARCH_BAC")], na.rm = T)/(WT*p)
anyNA(V[, i])
matplot(V$Date,V[,c("HS_BAC","GARCH_BAC","tGARCH_BAC")],main="VaR violations-BAC-10yrs",xlab = "Date", ylab = "VaR Violations")
legend("bottomright", legend = c("1- HS_BAC", "2- GARCH_BAC", "3- tGARCH_BAC"), col = c("black", "red", "green"), lty = 1)

B=BP_10[(WE+1):T,]
for(i in c("HS_BP","GARCH_BP","tGARCH_BP")){
  B[,i]=-B[,"y_BP"]-B[,i]
  B[B[,i]<0,i]=0
  B[B[,i]>0,i]=1
}
colSums(B[,c("HS_BP","GARCH_BP","tGARCH_BP")], na.rm = T)/(WT*p)
anyNA(B[, i])
matplot(B$Date,B[,c("HS_BP","GARCH_BP","tGARCH_BP")],main="VaR violations-BP-10yrs",xlab = "Date", ylab = "VaR Violations")
legend("bottomright", legend = c("1- HS_BP", "2- GARCH_BP", "3- tGARCH_BP"), col = c("black", "red", "green"), lty = 1)

D=JNJ_10[(WE+1):T,]
for(i in c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")){
  D[,i]=-D[,"y_JNJ"]-D[,i]
  D[D[,i]<0,i]=0
  D[D[,i]>0,i]=1
}
colSums(D[,c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")], na.rm = T)/(WT*p)
anyNA(D[, i])
matplot(D$Date,D[,c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")],main="Fig.6- VaR violations-JNJ-10yrs",xlab = "Date", ylab = "VaR Violations")
legend("bottomright", legend = c("1- HS_JNJ", "2- GARCH_JNJ", "3- tGARCH_JNJ"), col = c("black", "red", "green"), lty = 1)


T=1400
WE=250
WT=1150
A=BAC_5[(WE+1):T,]
for(i in c("HS_BAC","GARCH_BAC","tGARCH_BAC")){
  A[,i]=-A[,"y_BAC"]-A[,i]
  A[A[,i]<0,i]=0
  A[A[,i]>0,i]=1
}
anyNA(A[, i])
colSums(A[,c("HS_BAC","GARCH_BAC","tGARCH_BAC")], na.rm = T)/(WT*p)
matplot(A$Date,A[,c("HS_BAC","GARCH_BAC","tGARCH_BAC")], main="VaR violations-BAC-5yrs",xlab = "Date", ylab = "Violations")
legend("bottomright", legend = c("1- HS_BAC", "2- GARCH_BAC", "3- tGARCH_BAC"), col = c("black", "red", "green"), lty = 1)

C=BP_5[(WE+1):T,]
for(i in c("HS_BP","GARCH_BP","tGARCH_BP")){
  C[,i]=-C[,"y_BP"]-C[,i]
  C[C[,i]<0,i]=0
  C[C[,i]>0,i]=1
}
colSums(C[,c("HS_BP","GARCH_BP","tGARCH_BP")], na.rm = T)/(WT*p)
anyNA(C[, i])
matplot(C$Date,C[,c("HS_BP","GARCH_BP","tGARCH_BP")],main="VaR violations-BP-5yrs",xlab = "Date", ylab = "VaR Violations")
legend("bottomright", legend = c("1- HS_BP", "2- GARCH_BP", "3- tGARCH_BP"), col = c("black", "red", "green"), lty = 1)

E=JNJ_5[(WE+1):T,]
for(i in c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")){
  E[,i]=-E[,"y_JNJ"]-E[,i]
  E[E[,i]<0,i]=0
  E[E[,i]>0,i]=1
}
colSums(E[,c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")], na.rm = TRUE)/(WT*p)
anyNA(E[, i])
matplot(E$Date,E[,c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")], main="Fig.7- VaR violations-JNJ-5yrs", xlab = "Date", ylab = "VaR Violations")
legend("bottomright", legend = c("1- HS_JNJ", "2- GARCH_JNJ", "3- tGARCH_JNJ"), col = c("black", "red", "green"), lty = 1)


#Coverage Test- for all three stocks but only for 10-year window
T=2600
WE=500
WT=2100
#looping over each stock's dataset to go over all three models
for(i in c("HS_BAC","GARCH_BAC","tGARCH_BAC")){
  # retrieving returns
  ra <- BAC_10[(WE + 1):T,2] 
  VaRa <- BAC_10[(WE + 1):T,i]
  #takes the value 1 if observed return(ra) is lesser than VaRa
  eta <- ra < - VaRa 
  v1 <- sum(eta)
  v0 <- length(eta) - v1
  #Probability of observed violations
  picap <- v1 / (v1 + v0)
  
  # likelihood of restricted model- violations according to significance level p
  a <- (1 - p)^v0 * p^v1 
  # likelihood of unrestricted model
  b <- (1 - picap)^v0 * picap^v1 
  
  LR <- 2 * (log(b / a))
  LR
  
  if (LR > qchisq(p = 1 - p, df = 1)) {
    print(i)
    print (LR)
    print(v1)
    print('null hypothesis H0 is rejected')
  } else {
    print(i)
    print (LR)
    print(v1)
    print('We cannot reject the null')
  }
}


for(i in c("HS_BP","GARCH_BP","tGARCH_BP")){
  ra <- BP_10[(WE + 1):T,2] 
  VaRa <- BP_10[(WE + 1):T,i]
  eta <- ra < - VaRa 
  v1 <- sum(eta)
  v0 <- length(eta) - v1
  picap <- v1 / (v1 + v0)

  a <- (1 - p)^v0 * p^v1 
  b <- (1 - picap)^v0 * picap^v1 
  
  LR <- 2 * (log(b / a))
  LR
  
  if (LR > qchisq(p = 1 - p, df = 1)) {
    print(i)
    print (LR)
    print(v1)
    print('null hypothesis H0 is rejected')
  } else {
    print(i)
    print (LR)
    print(v1)
    print('We cannot reject the null')
  }
}

for(i in c("HS_JNJ","GARCH_JNJ","tGARCH_JNJ")){
  ra <- JNJ_10[(WE + 1):T,2] 
  VaRa <- JNJ_10[(WE + 1):T,i]
  eta <- ra < - VaRa 
  v1 <- sum(eta)
  v0 <- length(eta) - v1
  picap <- v1 / (v1 + v0)
  
  a <- (1 - p)^v0 * p^v1 
  b <- (1 - picap)^v0 * picap^v1 
  
  LR <- 2 * (log(b / a))
  LR
  
  if (LR > qchisq(p = 1 - p, df = 1)) {
    print(i)
    print (LR)
    print(v1)
    print('null hypothesis H0 is rejected')
  } else {
    print(i)
    print (LR)
    print(v1)
    print('We cannot reject the null')
  }
}

#Independence Test- only Bank of America- over two periods
T=2600
WE=500
WT=2100

#looping it to through the violations matrices that were used to compute violation ratios previously
for(i in c("HS_BAC","GARCH_BAC","tGARCH_BAC")){
  V_ind <- V[[i]] 
  #modelling consecutive days violations by way of naming themnyesterday and today
  yesterday_V <- V_ind[-length(V_ind)]  
  today_V <- V_ind[-1] 
 
  v00 <- sum(yesterday_V == 0 & today_V == 0)  # No violation yesterday and today
  v01 <- sum(yesterday_V == 0 & today_V == 1)  # No violation yesterday, violation today
  v10 <- sum(yesterday_V == 1 & today_V == 0)  # Violation yesterday, no violation today
  v11 <- sum(yesterday_V == 1 & today_V == 1)  #Violation yesterday, Violation today
  
  # Estimated Transition Probabilities  
  rho00 <- v00 / (v00 + v01)  
  rho01 <- v01 / (v00 + v01)  
  rho10 <- v10 / (v10 + v11)  
  rho11 <- v11 / (v10 + v11) 

  # Null Transition Matrix  
   rho <- sum(V_ind) / WT # Overall violation probability  
rho_null <- matrix(c(1 - rho, rho,  
                     1 - rho, rho),  
                   nrow = 2, byrow = TRUE)  

# Estimated Transition Matrix  
rho_hat <- matrix(c(rho00, rho01,  
                    rho10, rho11),  
                  nrow = 2, byrow = TRUE)  

# Likelihood Under Null Hypothesis  
L_null <- (1 - rho)^(v00 + v10) * rho^(v01 + v11)  

# Likelihood Under Alternative Hypothesis  
L_alt <- (1 - rho01)^v00 * rho01^v01 * (1 - rho11)^v10 * rho11^v11  

# LR Test Statistic  
LR <- 2 * (log(L_alt) - log(L_null))  

# P-value<- Chi-squared distribution with 1 degree of freedom
p_value <- pchisq(LR, df = 1, lower.tail = FALSE)  


cat("Transition Matrix under Null Hypothesis:\n", rho_null, "\n")  
cat("Estimated Transition Matrix:\n", rho_hat, "\n")  
cat("Likelihood Ratio Test Statistic (LR):", LR, "\n")  
cat("P-value:", p_value, "\n")  

if (p_value < 0.05) {  
  cat("Reject the null hypothesis: Violations are not independent.\n")  
} else {  
  cat("Fail to reject the null hypothesis: Violations are independent.\n")  
}  
}


T=1400
WE=250
WT=1150
for(i in c("HS_BAC","GARCH_BAC","tGARCH_BAC")){
  A_ind <- A[[i]] 
  yesterday_A <- A_ind[-length(A_ind)]
  today_A <- A_ind[-1] 
  v00 <- sum(yesterday_A == 0 & today_A == 0)  
  v01 <- sum(yesterday_A == 0 & today_A == 1)  
  v10 <- sum(yesterday_A == 1 & today_A == 0)  
  v11 <- sum(yesterday_A == 1 & today_A == 1)
  
   
  rho00 <- v00 / (v00 + v01)  
  rho01 <- v01 / (v00 + v01)  
  rho10 <- v10 / (v10 + v11)  
  rho11 <- v11 / (v10 + v11) 
   
  
  rho <- sum(A_ind) / WT                      
  rho_null <- matrix(c(1 - rho, rho,  
                       1 - rho, rho),  
                     nrow = 2, byrow = TRUE)  
  
  
  rho_hat <- matrix(c(rho00, rho01,  
                      rho10, rho11),  
                    nrow = 2, byrow = TRUE)  
 
  
  L_null <- (1 - rho)^(v00 + v10) * rho^(v01 + v11)  
  
   
  L_alt <- (1 - rho01)^v00 * rho01^v01 * (1 - rho11)^v10 * rho11^v11  
  
  # LR Test Statistic  
  LR <- 2 * (log(L_alt) - log(L_null))  
  
  # P-value  
  p_value <- pchisq(LR, df = 1, lower.tail = FALSE)  
  
  # Output Results  
  cat("Transition Matrix under Null Hypothesis:\n", rho_null, "\n")  
  cat("Estimated Transition Matrix:\n", rho_hat, "\n")  
  cat("Likelihood Ratio Test Statistic (LR):", LR, "\n")  
  cat("P-value:", p_value, "\n")  
  
  if (p_value < 0.05) {  
    cat("Reject the null hypothesis: Violations are not independent.\n")  
  } else {  
    cat("Fail to reject the null hypothesis: Violations are independent.\n")  
  }  
}

#Plotting vilations to see clustering
plot(V$Date, V$HS_BAC, type = "s", col = "blue", 
     main = "Fig.8-VaR Violations Over Time-HS-BAC",
     ylab = "Violations (1 = Yes, 0 = No)", xlab = "Time", ylim = c(0, 1.1))
plot(V$Date, V$GARCH_BAC, type = "s", col = "blue", 
     main = "Fig.9-VaR Violations Over Time-GARCH-BAC",
     ylab = "Violations (1 = Yes, 0 = No)", xlab = "Time", ylim = c(0, 1.1))
plot(V$Date, V$tGARCH_BAC, type = "s", col = "blue", 
     main = "Fig.10-VaR Violations Over Time-tGARCH-BAC",
     ylab = "Violations (1 = Yes, 0 = No)", xlab = "Time", ylim = c(0, 1.1))


#Plotting alpha estimates over time for GARCH and t-GARCH- over different periods
plot(GARCH_BP_10_df$Date,GARCH_BP_10_df$beta1, type = 'l', main = "Fig.12-Beta estimates from GARCH and tGARCH-BP", ylim=c(0.1,1), xlab = "Date", ylab = "Beta estimate", lwd=2)
lines(tGARCH_BP_10_df$Date,tGARCH_BP_10_df$beta1, type = 'l', col = "blue", lwd = 1)
lines(GARCH_JNJ_5_df$Date,GARCH_BP_5_df$beta1, type = 's', col = "red", lwd = 1)
legend("bottomleft", legend = c("GARCH-10yrs", "tGARCH-10yrs","GARCH-5yrs"),col = c("black", "blue", "red"),lwd = 2,lty = 1, cex = 0.8)

#Plotting alpha estimates over time for GARCH and t-GARCH- over different periods
plot(GARCH_BP_10_df$Date,GARCH_BP_10_df$alpha1, type = 'l', main = "Fig.11-Alpha estimates from GARCH and tGARCH-BP", ylim = c(0,0.8),xlab = "Date", ylab = "Alpha estimate", lwd=2)
lines(tGARCH_BP_10_df$Date,tGARCH_BP_10_df$alpha1, type = 'l', col = "blue", lwd = 1)
lines(GARCH_BP_5_df$Date,GARCH_BP_5_df$alpha1, type = 's', col = "red", lwd = 1)
legend("topleft", legend = c("GARCH-10yrs", "tGARCH-10yrs","GARCH-5yrs"),col = c("black", "blue", "red"),lwd = 2,lty = 1, cex = 0.8)

