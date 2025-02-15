#clear all
rm(list = ls())

#loading already installed packages
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
library("knitr")
library(rmarkdown)

#setting working directory
setwd("C:/data/WRDS-FM321/INPUT")

#importing dataset
data<-read.csv("C:/data/WRDS-FM321/INPUT/finalprojectdata.csv")

#creating adjusted prices
data$adjusted_prices<-data$PRC/data$CFACPR

#subsetting data on adjusted price
XOM_prc<- data[data$PERMNO == 11850, c("date","adjusted_prices")]
names(XOM_prc)[2]<- "XOM"
CVX_prc<- data[data$PERMNO == 14541, c("date","adjusted_prices")]
names(CVX_prc)[2]<- "CVX"

#subsetting data on returns
XOM_ret<- data[data$PERMNO == 11850, c("date","RET")]
names(XOM_ret)[2]<- "XOM"
CVX_ret<- data[data$PERMNO == 14541, c("date","RET")]
names(CVX_ret)[2]<- "CVX"

#Prices dataset
PRICE<- merge(CVX_prc, XOM_prc, all=FALSE, by="date")
PRICE <- PRICE[!duplicated(PRICE$date), ]

date.ts <- ymd(PRICE$date)
PRICE$date <- as.Date(as.character(PRICE$date), format = "%Y%m%d")

PRICE$XOM <- as.numeric(PRICE$XOM)
PRICE$CVX <- as.numeric(PRICE$CVX)

#Plotting prices
dev.new()
plot(date.ts,PRICE$XOM, type = "l", main = "Prices of XOM and CVX", ylab = "Price", xlab = "Date", col = "red", ylim = c(0,185), xaxt = "n", yaxt = "n")
lines(date.ts, PRICE$CVX, col = "blue")
legend("bottomright",legend = c("XOM", "CVX"), col = c("red", "blue"), lty=1, bty="n")
axis(1, at = seq(min(date.ts), max(date.ts), by = 365), labels = format(seq(min(date.ts), max(date.ts), by = 365), "%Y"))
axis(2, at = seq(0, 185, by = 25))

#Returns dataset
RET<- merge(XOM_ret, CVX_ret, all=FALSE, by="date")
RET <- RET[!duplicated(RET$date), ]

date.ts <- ymd(RET$date)
RET$date <- as.Date(as.character(RET$date), format = "%Y%m%d")

RET$XOM <- as.numeric(RET$XOM)
RET$CVX <- as.numeric(RET$CVX)
ret<- as.numeric(ret)

plot(date.ts,RET$XOM, type = "l", main = "Return of XOM and CVX", ylab = "Price", xlab = "Date", col = "purple", xaxt = "n")
lines(date.ts, RET$CVX, col = "orange")
legend("bottomright",legend = c("XOM", "CVX"), col = c("red", "blue"), lty=1, bty="n")
axis(1, at = seq(min(date.ts), max(date.ts), by = 365), labels = format(seq(min(date.ts), max(date.ts), by = 365), "%Y"))

#Calculating test statistics
MeanPX<-mean(PRICE$XOM)
StdDevPX<-sd(PRICE$XOM)
SkewPX<-skewness(PRICE$XOM)
KurtPX<-kurtosis(PRICE$XOM)

MeanPC<-mean(PRICE$CVX)
StdDevPC<-sd(PRICE$CVX)
SkewPC<-skewness(PRICE$CVX)
KurtPC<-kurtosis(PRICE$CVX)

MeanRetX<-mean(RET$XOM)
StdDevRetX<-sd(RET$XOM)
SkewRetX<-skewness(RET$XOM)
KurtRetX<-kurtosis(RET$XOM)

MeanRetC<-mean(RET$CVX)
StdDevRetC<-sd(RET$CVX)
SkewRetC<-skewness(RET$CVX)
KurtRetC<-kurtosis(RET$CVX)

Summary_stat<- data.frame(
  Statistic = c("Average", "Standard Deviation", "Skewness", "Kurtosis"),
  `PRICE_XOM` = c(MeanPX, StdDevPX, SkewPX, KurtPX),
  `PRICE_CVX` = c(MeanPC, StdDevPC, SkewPC, KurtPC),
  `RET_XOM` = c(MeanRetX, StdDevRetX, SkewRetX, KurtRetX),
  `RET_CVX` = c(MeanRetC, StdDevRetC, SkewRetC, KurtRetC)
)

Summary_stat


#Histogram of returs
hist(RET$XOM, breaks = 30, col = "red", main = "Histogram of XOM Daily Returns",
     xlab = "Daily Returns (%)", ylab = "Frequency")
hist(RET$CVX, breaks = 30, col = "blue", main = "Histogram of XOM Daily Returns",
     xlab = "Daily Returns (%)", ylab = "Frequency", add=TRUE)
legend("bottomright",legend = c("XOM", "CVX"), col = c("red", "blue"), lty=1, bty="n")

#Calculating rolling correlation to prove non-linear dependencs
rolling_corr <- rollapply(
  data = RET[, c("XOM", "CVX")],
  width = 30,
  FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
  by.column = FALSE, align = "right"
)

#plotting rolling correlation
window_size <- 30
plot(RET$date[window_size:length(RET$date)], rolling_corr, type = "l", col = "darkgreen", lwd = 2,
     main = paste("Figure 5:",30, "-Month Rolling Correlation Between CVX and XOM"),
     xlab = "Date", ylab = "Correlation")

#comparing return distribution to normal distribution
jarque.bera.test(RET$CVX)
CV = qchisq(p = 0.95, df = 2)
CV
jarque.bera.test(RET$XOM)
CV = qchisq(p = 0.95, df = 2)
CV

#plotting autocorrelation of squared returns
acf((RET$XOM)^2, main = "Autocorrelation of squared returns")
acf((RET$CVX)^2, main = "Autocorrelation of returns")

#testing for autocorrelation of returns
Box.test((RET$XOM), lag = 20, type = "Ljung-Box")
print(paste('Critical Value is:', qchisq(p = 0.95, df = 20, lower.tail=TRUE)))
Box.test(RET$CVX, lag = 20, type = "Ljung-Box")
print(paste('Critical Value is:', qchisq(p = 0.95, df = 20, lower.tail=TRUE)))


summary_stats_function <- function(stock_returns) {
  # Compute the summary statistics
  stats <- c(
    Mean = mean(stock_returns, na.rm = TRUE),
    SD = sd(stock_returns, na.rm = TRUE),
    Kurtosis = kurtosis(stock_returns, na.rm = TRUE),
    Skewness = skewness(stock_returns, na.rm = TRUE)
  )
  return(stats)
}

ret <- RET[, -1]  # Exclude Date column if present

# Apply the summary function to each column (each stock) using lstocks_data <- data.frame(lapply(ret, summary_stats_function as.numeric(as.character(x))))
apply

# Convert the result into a data frame for better readability
summary_stats_df <- data.frame(stocks_data)
summary_stats_df$Stock <- rownames(summary_stats_df)
rownames(summary_stats_df) <- NULL

# View the result
print(summary_stats_df)