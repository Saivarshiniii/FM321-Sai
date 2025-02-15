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
data<-read.csv("C:/data/WRDS-FM321/INPUT/finaldata.csv")

#creating adjusted prices
data$adjusted_prices<-data$PRC/data$CFACPR

#subsetting data on returns
TSM_ret<- data[data$PERMNO == 85442, c("date","RET")]
names(TSM_ret)[2]<- "TSM"
HP_ret<- data[data$PERMNO == 32707, c("date","RET")]
names(HP_ret)[2]<- "HP"
INFY_ret<- data[data$PERMNO == 86776, c("date","RET")]
names(INFY_ret)[2]<- "INFY"
DLR_ret<- data[data$PERMNO == 90373, c("date","RET")]
names(DLR_ret)[2]<- "DLR"
MA_ret<- data[data$PERMNO == 91233, c("date","RET")]
names(MA_ret)[2]<- "MA"
NVDA_ret<- data[data$PERMNO == 86580, c("date","RET")]
names(NVDA_ret)[2]<- "NVDA"
HD_ret<- data[data$PERMNO == 66181, c("date","RET")]
names(HD_ret)[2]<- "HD"
BAC_ret<- data[data$PERMNO == 59408, c("date","RET")]
names(BAC_ret)[2]<- "BAC"
GOOGL_ret<- data[data$PERMNO == 90319, c("date","RET")]
names(GOOGL_ret)[2]<- "GOOGL"

TD_ret<- data[data$PERMNO == 83835, c("date","RET")]
names(TD_ret)[2]<- "TD"
TSLA_ret<- data[data$PERMNO == 93436, c("date","RET")]
names(TSLA_ret)[2]<- "TSLA"
AMZN_ret<- data[data$PERMNO == 84788, c("date","RET")]
names(AMZN_ret)[2]<- "AMZN"
JPM_ret<- data[data$PERMNO == 47896, c("date","RET")]
names(JPM_ret)[2]<- "JPM"
SATS_ret<- data[data$PERMNO == 92469, c("date","RET")]
names(SATS_ret)[2]<- "SATS"
CMCSA_ret<- data[data$PERMNO == 89525, c("date","RET")]
names(CMCSA_ret)[2]<- "CMCSA"
CVX_ret<- data[data$PERMNO == 14541, c("date","RET")]
names(CVX_ret)[2]<- "CVX"
PG_ret<- data[data$PERMNO == 18163, c("date","RET")]
names(PG_ret)[2]<- "PG"
PFE_ret<- data[data$PERMNO == 21936, c("date","RET")]
names(PFE_ret)[2]<- "PFE"

RET_ESG<- merge(TSM_ret, HP_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, INFY_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, DLR_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, MA_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, NVDA_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, HD_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, BAC_ret, all=FALSE, by="date")
RET_ESG<- merge(RET_ESG, GOOGL_ret, all=FALSE, by="date")
RET_nESG<- merge(TD_ret, TSLA_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, AMZN_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, JPM_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, SATS_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, CMCSA_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, CVX_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, PG_ret, all=FALSE, by="date")
RET_nESG<- merge(RET_nESG, PFE_ret, all=FALSE, by="date")

RET_nESG <- RET_nESG[!duplicated(RET_nESG$date), ]
RET_ESG <- RET_ESG[!duplicated(RET_ESG$date), ]

date.ts <- ymd(RET_ESG$date)
RET_ESG$date <- as.Date(as.character(RET_ESG$date), format = "%Y%m%d")
date.ts <- ymd(RET_nESG$date)
RET_nESG$date <- as.Date(as.character(RET_nESG$date), format = "%Y%m%d")

RET_ESG$TSM <- as.numeric(RET_ESG$TSM)
RET_ESG$HP<- as.numeric(RET_ESG$HP)
RET_ESG$INFY<- as.numeric(RET_ESG$INFY)
RET_ESG$DLR <- as.numeric(RET_ESG$DLR)
RET_ESG$MA<- as.numeric(RET_ESG$MA)
RET_ESG$NVDA<- as.numeric(RET_ESG$NVDA)
RET_ESG$HD <- as.numeric(RET_ESG$HD)
RET_ESG$BAC<- as.numeric(RET_ESG$BAC)
RET_ESG$GOOGL<- as.numeric(RET_ESG$GOOGL)

RET_nESG$TD <- as.numeric(RET_nESG$TD)
RET_nESG$TSLA<- as.numeric(RET_nESG$TSLA)
RET_nESG$AMZN<- as.numeric(RET_nESG$AMZN)
RET_nESG$JPM <- as.numeric(RET_nESG$JPM)
RET_nESG$SATS<- as.numeric(RET_nESG$SATS)
RET_nESG$CMCSA<- as.numeric(RET_nESG$CMCSA)
RET_nESG$CVX <- as.numeric(RET_nESG$CVX)
RET_nESG$PG<- as.numeric(RET_nESG$PG)
RET_nESG$PFE<- as.numeric(RET_nESG$PFE)

w_ESG <- rep(1 / 9, 9)
w_nESG<-rep(1 / 9, 9)
w<-rep(1 / 18, 18)

ESG_portfolio <- RET_ESG [, -1]* w_ESG
ESG_portfolio <- rowSums(ESG_portfolio)
ESG_portfolio<- data.frame(
  Date = RET_ESG[, 1],  # Keep the original date column
  ESG_portfolio =  ESG_portfolio
)
nESG_portfolio <- RET_nESG [, -1]* w_nESG
nESG_portfolio <- rowSums(nESG_portfolio)
nESG_portfolio<- data.frame(
  Date = RET_nESG[, 1],  # Keep the original date column
  nESG_portfolio =  nESG_portfolio
)
RET<- merge(RET_ESG, RET_nESG, all=FALSE, by="date")
Portfolio <- RET [, -1]* w
Portfolio <- rowSums(Portfolio)
Portfolio<- data.frame(
  Date = RET[, 1],  # Keep the original date column
  Portfolio =  Portfolio
)
Diversified_portfolio<-RET [, -1]* w
Diversified_portfolio <- rowSums(Diversified_portfolio)
Diversified_portfolio<- data.frame(
  Date = RET[, 1],  # Keep the original date column
  Diversified_portfolio =  Diversified_portfolio
)

ESG_portfolio$Cumulative_ESG <- cumprod(1 + ESG_portfolio$ESG_portfolio) - 1
nESG_portfolio$Cumulative_nESG <- cumprod(1 + nESG_portfolio$nESG_portfolio) - 1
Diversified_portfolio$Cumulative_ESG <- cumprod(1 + Diversified_portfolio$Diversified_portfolio) - 1


Diversified_portfolio$diversified_log<-log(1 + Diversified_portfolio$Diversified_portfolio)
ESG_portfolio$ESG_log <- log(1 + ESG_portfolio$ESG_portfolio)
nESG_portfolio$nESG_log <- log(1 + nESG_portfolio$nESG_portfolio)

#Descriptive Analysis
master_returns<-data.frame(RET_ESG$date, ESG_portfolio$ESG_portfolio,nESG_portfolio$nESG_portfolio,Diversified_portfolio$Diversified_portfolio)
descriptive_stats <- data.frame(
  Portfolio = c("High-ESG", "Low-ESG", "Diversified portfolio"),
  Mean_Return = apply(master_returns [, -1], 2, mean),
  Std_Dev = apply(master_returns[, -1], 2, sd),
  Skewness = apply(master_returns[, -1], 2, skewness),
  Kurtosis = apply(master_returns[, -1], 2, kurtosis),
  Max_Return = apply(master_returns[, -1], 2, max),
  Min_Return = apply(master_returns[, -1], 2, min)
)
print(descriptive_stats)
Diversified_portfolio$Cumulative_ESG <- as.numeric(Diversified_portfolio$Cumulative_ESG)
Diversified_portfolio$Date <- as.Date(Diversified_portfolio$Date, format = "%Y-%m-%d")

plot(ESG_portfolio$Date, ESG_portfolio$Cumulative_ESG, type = 'l',
     main = "Figure 3. Cumulative Returns Over Time",  
     xlab = "Date",
     ylab = "Returns",
     col = "blue",
     lwd = 2)
lines(nESG_portfolio$Date, nESG_portfolio$Cumulative_nESG,
      type = 'l',col="red", lwd=2)
lines(Diversified_portfolio$Date, Diversified_portfolio$Cumulative_ESG,
      type = 'l',col="green", lwd=2)
legend("topleft", legend = c("High ESG", "Low ESG", "Diversified Portfolio"),
       col = c("blue", "red","green"), lwd = 2, lty = c(1, 2))

#PORTFOLIO VOLATILITY USING DCC-EGARCH

#Comparing log-likelihood between standard GARCH and EGARCH
ret_esg <-log(1 + RET_ESG[, -1])
ret_nesg <-log(1 + RET_nESG[, -1])  
ret <-log(1 + RET[, -1])  
#Standard GARCH
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "norm")
fit <- ugarchfit(spec = garch_spec, data = ret_esg)

dcc_spec_garch <- dccspec(uspec = multispec(replicate(9, garch_spec)),  # Two assets
                    dccOrder = c(1, 1),  # DCC(1,1)
                    distribution = "mvnorm")
dcc_fit_esg <- dccfit(dcc_spec_garch, data = ret_esg)
dcc_fit_nesg <- dccfit(dcc_spec_garch, data = ret_nesg)

#DCC-EGARCH
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                   mean.model = list(armaOrder = c(0,0)),
                   distribution.model = "std")
fit <- ugarchfit(spec = egarch_spec, data =ret_esg)
dcc_spec_egarch <- dccspec(uspec = multispec(replicate(9, egarch_spec)),  # Two assets
                    dccOrder = c(1, 1),  # DCC(1,1)
                    distribution = "mvt")
dcc_fit_esg <- dccfit(dcc_spec_egarch, data = ret_esg)
dcc_fit_nesg <- dccfit(dcc_spec_egarch, data = ret_nesg)

#Computing portfolio volatility
  #Extracting Conditional Covariance Matrices
dcc_cov_esg <- rcov(dcc_fit_esg)     
dcc_cov_nesg <- rcov(dcc_fit_nesg)   

n_time <- dim(dcc_cov_esg)[3]        

  #Compute Portfolio Variance
portfolio_variance_esg <- sapply(1:n_time, function(t) {
  cov_matrix_esg <- dcc_cov_esg[, , t]
  t(w_ESG) %*% cov_matrix_esg %*% w_ESG
})

portfolio_variance_nesg <- sapply(1:n_time, function(t) {
  cov_matrix_nesg <- dcc_cov_nesg[, , t]
  t(w_nESG) %*% cov_matrix_nesg %*% w_nESG
})

  #Compute Portfolio Volatility
portfolio_volatility_esg <- sqrt(portfolio_variance_esg)
portfolio_volatility_nesg <- sqrt(portfolio_variance_nesg)

  #Plot portfolio volatility
plot(ESG_portfolio$Date,portfolio_volatility_esg , type = "l", col = "blue", lwd = 2,
     main = "Figure 4. Dynamic Portfolio Volatility-ESG",
     xlab = "Time", ylab = "Portfolio Volatility")
plot(nESG_portfolio$Date,portfolio_volatility_nesg , type = "l", col = "blue", lwd = 2,
     main = "Figure 5. Dynamic Portfolio Volatility-non ESG",
     xlab = "Time", ylab = "Portfolio Volatility")


#SPILLOVER EFFECT
#Within the portfolio
dcc_corr_esg <- rcor(dcc_fit_esg)
dcc_corr_nesg <- rcor(dcc_fit_nesg)
n_assets <- dim(dcc_corr_esg)[1]
n_timepoints <- dim(dcc_corr_esg)[3]

  #Averaging each periods correlation
avg_corr_esg <- sapply(1:n_timepoints, function(t) {
  # Extract the correlation matrix at time t
  corr_matrix_esg <- dcc_corr_esg[,,t]
  # Compute the average of off-diagonal elements
  mean(corr_matrix_esg[lower.tri(corr_matrix_esg)])
})

avg_corr_nesg <- sapply(1:n_timepoints, function(t) {
  # Extract the correlation matrix at time t
  corr_matrix_nesg <- dcc_corr_nesg[,,t]
  # Compute the average of off-diagonal elements
  mean(corr_matrix_nesg[lower.tri(corr_matrix_nesg)])
})
  #plotting Spillover with each portfolio
plot(RET_ESG$date, avg_corr_esg, type = 'l', col = 'blue', lwd = 2,
     main = "Figure 6. Within portfolio Spillover Measure- Average Correlation-ESG",
     xlab = "Time", ylab = "Average Correlation")

  #Add a mean line
mean_corr_esg <- mean(avg_corr_esg, na.rm = TRUE)
abline(h = mean_corr_esg, col = "red", lwd = 2, lty = 2)

legend("topleft", legend = c("DCC Correlation", "Mean Correlation"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))

plot(RET_nESG$date, avg_corr_esg, type = 'l', col = 'blue', lwd = 2,
     main = "Figure 7. Within portfolio Spillover Measure- Average Correlation-non ESG",
     xlab = "Time", ylab = "Average Correlation")

mean_corr_nesg <- mean(avg_corr_nesg, na.rm = TRUE)
abline(h = mean_corr_nesg, col = "red", lwd = 2, lty = 2)

legend("topleft", legend = c("DCC Correlation", "Mean Correlation"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))

#Between ESG and non-ESG portfolio
  #Combining log returns from ESG and non-ESG portfolio
ESGvsnESG<-merge(ESG_portfolio, nESG_portfolio, all=FALSE, by="Date")
esgvsnesg <- ESGvsnESG [, -1:-3]
esgvsnesg <- esgvsnesg [, -2]
esgvsnesg <- esgvsnesg [, -2]


# DCC-EGARCH Model for the ESG and non-ESG portfolios
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
dcc_spec_esgvsnesg <- dccspec(uspec = multispec(replicate(2, egarch_spec)),  # Two assets
                              dccOrder = c(1, 1),  # DCC(1,1)
                              distribution = "mvt")
dcc_fit_esgvsnesg <- dccfit(dcc_spec_esgvsnesg, data = esgvsnesg)

# Deriving the correlation vector from the fitted DCC model
dcc_corr_esgvsnesg<- rcor(dcc_fit_esgvsnesg)
correlation_vector <- dcc_corr_esgvsnesg[1, 2, ]  

# Plotting time-varying correlation between ESG and Non-ESG portfolios with mean line
plot(ESG_portfolio$Date, correlation_vector, type = 'l', 
     main = "Figure 8. Time-varying DCC Correlation between ESG and Non-ESG Portfolio",
     xlab = "Date", ylab = "DCC Correlation", lwd = 2)


mean_corr_esgvsnesg <- mean(correlation_vector, na.rm = TRUE)
abline(h = mean_corr_esgvsnesg, col = "red", lwd = 2, lty = 2)

legend("bottomleft", legend = c("DCC Correlation", "Mean Correlation"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2))

#volatility spillover
  #Covariance matrix
dcc_vol_esgvsnesg <- rcov(dcc_fit_esgvsnesg)
  #Initialize storage for volatility spillover
n_time <- dim(dcc_corr_esgvsnesg)[3]  
volatility_spillover_esgvsnesg <- numeric(n_time)  

  #Loop over each time step to calculate spillover
for (t in 1:n_time) {
  #volatility components for ESG and Non-ESG
  D_t_esg <- sqrt(dcc_vol_esgvsnesg[1, 1, t])  
  D_t_nesg <- sqrt(dcc_vol_esgvsnesg[2, 2, t])  
  
    #Correlation between ESG and Non-ESG at time t
  corr_t <- dcc_corr_esgvsnesg[1, 2, t]
  
    #Calculate the volatility spillover
  volatility_spillover_esgvsnesg[t] <- D_t_esg * corr_t * D_t_nesg
}

  #Plot the volatility spillover over time
plot(volatility_spillover_esgvsnesg, type = "l", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Volatility Spillover",
     main = "Figure 9. Volatility Spillover between ESG and Non-ESG Portfolios")

#VALUE-AT-RISK USING tGARCH
 
#ESG portfolio
T=2500
WE=750
Burn=30
WT=2000
value=1
p=0.01
ESG_log <- tail(ESG_portfolio$ESG_log, T)
d_ts <- tail(date.ts, T)

VaR<- data.frame(
  Date = d_ts,
  ESG_log = ESG_log 
)
VaR$ESG_VaR <- NA  
spec_tgarch <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"  
)
#rolling VaR
tgarch_coeffs_ESG <- list()
for (t in (WE + 1):T) {
  t1 <- t - WE
  t2 <- t - 1
  window <- ESG_log[t1:t2]
  print(t)
  fit <- ugarchfit(spec = spec_tgarch, data = window, solver = "hybrid")
  df <- fit@fit$coef["shape"]
  omega <- coef(fit)[1]
  alpha <- coef(fit)[2]
  beta <- coef(fit)[3]
  s2 <- omega + 
    alpha * tail(window, 1)^2 + 
    beta * tail(fit@fit$var, 1)
  VaR$ESG_VaR[t]<- -value * sqrt(s2) * qt(p, df = df) * sqrt((df - 2) / df)
  tgarch_coeffs_ESG [[t]]<- coef(fit)
  print(tgarch_coeffs_ESG[[t]])# Save coefficients
  Coefficients_ESG<- tgarch_coeffs_ESG[[t]]
}

  #Loop through each element of the list and extract coefficients
var_coefficients_ESG <- data.frame()
for (i in seq_along(tgarch_coeffs_ESG)) {
  coefficients <- tgarch_coeffs_ESG[[i]]
    var_coefficients_ESG <- rbind(var_coefficients_ESG, coefficients)
}
dates_coeff <- tail(date.ts, nrow(var_coefficients_ESG))
var_coefficients_ESG$Date <- dates_coeff 
colnames(var_coefficients_ESG) <- c("Omega","Alpha", "Beta", "s2","Date") 

#non-ESG Portfolio
T=2500
WE=750
Burn=30
WT=1750
value=1
p=0.01
nESG_log <- tail(nESG_portfolio$nESG_log, T)
d_ts <- tail(date.ts, T)
VaR$nESG_log<- nESG_log

VaR$nESG_VaR <- NA  # Initialize an empty vector to store VaR values
spec_tgarch <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"  # Student's t-distribution
)

tgarch_coeffs_nESG <- list()
#rolling VaR
for (t in (WE + 1):T) {
  t1 <- t - WE
  t2 <- t - 1
  window <- nESG_log[t1:t2]
  print(t)
  fit <- ugarchfit(spec = spec_tgarch, data = window, solver = "hybrid")
  df <- fit@fit$coef["shape"]
  omega <- coef(fit)[1]
  alpha <- coef(fit)[2]
  beta <- coef(fit)[3]
  s2 <- omega + 
    alpha * tail(window, 1)^2 + 
    beta * tail(fit@fit$var, 1)
  VaR$nESG_VaR[t]<- -value * sqrt(s2) * qt(p, df = df) * sqrt((df - 2) / df)
  tgarch_coeffs_nESG [[t]]<- coef(fit)
  print(tgarch_coeffs_nESG[[t]])# Save coefficients
  Coefficients_nESG<- tgarch_coeffs_nESG[[t]]
}

  #Extracting coefficients into seperate dataset
var_coefficients_nESG <- data.frame()
for (i in seq_along(tgarch_coeffs_nESG)) {
  # Extract coefficients at time t
  coefficients <- tgarch_coeffs_nESG[[i]]
  var_coefficients_nESG <- rbind(var_coefficients_nESG, coefficients)
}
dates_coeff <- tail(date.ts, nrow(var_coefficients_nESG))#extracting tail dates so that it can be attached to individual dataframes with GARCH and t-GARCH coefficients
var_coefficients_nESG$Date <- dates_coeff 
colnames(var_coefficients_nESG) <- c("Omega","Alpha", "Beta", "df","Date") 

#plotting tGARCH VaR along with actual returns
dev.new()
plot(VaR$Date, VaR$ESG_log, type = 'l', main = "Figure 10. Returns and VaR estimates", xlab = "Date", ylab = "Returns/VaR", ylim=c(-0.15,0.25))
lines(VaR$Date,VaR$ESG_VaR, type = 's', col = "magenta", lwd = 2)
lines(VaR$Date, VaR$nESG_log, type = 'l', main = "Figure 10. Returns and VaR estimates", xlab = "Date", ylab = "Returns/VaR",col='red')
lines(VaR$Date,VaR$nESG_VaR, type = 's', col = "blue", lwd = 2)

legend("topleft", legend = c("ESG_returns","non-ESG_returns","ESG_VaR", "non-ESG_VaR"),
       col = c("black","red","magenta", "blue"), lwd = 2, lty = c(1, 2))
var_coefficients_ESG$alphabeta<-var_coefficients_ESG$Alpha+var_coefficients_ESG$Beta
var_coefficients_nESG$nalphabeta<-var_coefficients_nESG$Alpha+var_coefficients_nESG$Beta


#Estimating VaR at p=0.01
p=0.01
# Fit the ESG model
ESG_var_fit <- ugarchfit(spec = egarch_spec, data = ESG_portfolio$ESG_log)
esg_volatility <- sigma(ESG_var_fit)
esg_volatility <- esg_volatility[822:1259]

# Extract coefficients for the ESG model
df <- ESG_var_fit@fit$coef["shape"]
omega <- as.numeric(ESG_var_fit@fit$coef[1])  # Correct extraction
alpha <- as.numeric(ESG_var_fit@fit$coef[2])  # Correct extraction
beta <- as.numeric(ESG_var_fit@fit$coef[3])   # Correct extraction

# Calculate volatility for the next period (T+1)
s2 <- omega + 
  alpha * tail(ESG_portfolio$ESG_portfolio, 1)^2 + 
  beta * tail(ESG_var_fit@fit$var, 1)

# Calculate VaR for ESG simulation
VaR_ESG <- -1000 * sqrt(s2) * qt(p = p, df = df) * sqrt((df - 2) / df)
VaR_ESG


# Fit the ESG model
nESG_var_fit <- ugarchfit(spec = egarch_spec, data = nESG_portfolio$nESG_log)
nesg_volatility <- sigma(nESG_var_fit)
nesg_volatility <- nesg_volatility[822:1259]

# Extract coefficients for the ESG model
df <- nESG_var_fit@fit$coef["shape"]
omega <- as.numeric(nESG_var_fit@fit$coef[1])  # Correct extraction
alpha <- as.numeric(nESG_var_fit@fit$coef[2])  # Correct extraction
beta <- as.numeric(nESG_var_fit@fit$coef[3])   # Correct extraction

# Calculate volatility for the next period (T+1)
s2 <- omega + 
  alpha * tail(nESG_portfolio$nESG_portfolio, 1)^2 + 
  beta * tail(nESG_var_fit@fit$var, 1)

# Calculate VaR for ESG simulation
VaR_nESG <- -1000 * sqrt(s2) * qt(p = p, df = df) * sqrt((df - 2) / df)
VaR_nESG

# Fit the ESG model
div_var_fit <- ugarchfit(spec = egarch_spec, data = Diversified_portfolio$diversified_log)
div_volatility <- sigma(div_var_fit)
div_volatility <- div_volatility[1009:1133]

# Extract coefficients for the ESG model
df <- nESG_var_fit@fit$coef["shape"]
omega <- as.numeric(nESG_var_fit@fit$coef[1])  # Correct extraction
alpha <- as.numeric(nESG_var_fit@fit$coef[2])  # Correct extraction
beta <- as.numeric(nESG_var_fit@fit$coef[3])   # Correct extraction

# Calculate volatility for the next period (T+1)
s2 <- omega + 
  alpha * tail(nESG_portfolio$nESG_portfolio, 1)^2 + 
  beta * tail(nESG_var_fit@fit$var, 1)

# Calculate VaR for ESG simulation
VaR_nESG <- -1000 * sqrt(s2) * qt(p = p, df = df) * sqrt((df - 2) / df)
VaR_nESG


#SHOCK 1
# Apply a regulatory shock for ESG portfolios
shock_start <- as.Date("2017-01-01")
shock_end <- as.Date("2017-06-30")  

ESG_portfolio$ESG_shock1 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                            ESG_portfolio$ESG_log * 1.05,  
                            ESG_portfolio$ESG_log)
nESG_portfolio$nESG_shock1 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                   nESG_portfolio$nESG_log * 0.85,  
                                   nESG_portfolio$nESG_log)



egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_1 <- ugarchfit(spec = egarch_spec, data =ESG_portfolio$ESG_log)
fit_2 <- ugarchfit(spec = egarch_spec, data =nESG_portfolio$nESG_log)
fit_3 <- ugarchfit(spec = egarch_spec, data =ESG_portfolio$ESG_shock1)
fit_4 <- ugarchfit(spec = egarch_spec, data =nESG_portfolio$nESG_shock1)
vol_esg <- sigma(fit_1)
vol_nesg <- sigma(fit_2)
vol_shock_esg <- sigma(fit_3)
vol_shock_nesg <- sigma(fit_4)

vol_esg<- vol_esg[882:1259]
vol_nesg<- vol_nesg[882:1259]
vol_shock_esg<- vol_shock_esg[882:1259]
vol_shock_nesg<- vol_shock_nesg[882:1259]

dates <- ESG_portfolio$Date[882:1259]
dev.new()
plot(dates,vol_esg, type = "l", main = "Figure 11.Conditional Volatility before and after shock 1", ylab = "Volatility", xlab = "Time", col="blue", lwd=2)
lines(dates,vol_nesg, type = "l", ylab = "Volatility", xlab = "Time",col="red", lwd=2)
lines(dates,vol_shock_esg, col='black', lwd=2)
lines(dates,vol_shock_nesg, col='green', lwd=2)

legend("topright", legend = c("ESG_vol","non-ESG_vol","ESG_vol_shocked", "non-ESG_vol_shocked"),
       col = c("blue","red","black", "green"), lwd = 2, lty = c(1, 2))


#SHOCK 2
RET_ESG$TSM_log <- log(1 + RET_ESG$TSM)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_TSM <- ugarchfit(spec = egarch_spec, data =RET_ESG$TSM_log)
RET_ESG$volatility_TSM <- sigma(fit_TSM)
RET_ESG$volatility_TSM_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                     RET_ESG$volatility_TSM* 3,  
                                     RET_ESG$volatility_TSM)
RET_ESG$TSM_shocke_returns<- rnorm(2768, mean = mean(RET_ESG$TSM_log), sd = RET_ESG$volatility_TSM_S2)


RET_ESG$HP_log <- log(1 + RET_ESG$HP)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_HP <- ugarchfit(spec = egarch_spec, data =RET_ESG$HP_log)
RET_ESG$volatility_HP<- sigma(fit_HP)
RET_ESG$volatility_HP_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                            RET_ESG$volatility_HP * 3,  
                            RET_ESG$volatility_HP)
RET_ESG$HP_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$HP_log), sd = RET_ESG$volatility_HP_S2)


RET_ESG$INFY_log <- log(1 + RET_ESG$INFY)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_INFY <- ugarchfit(spec = egarch_spec, data =RET_ESG$INFY_log)
RET_ESG$volatility_INFY <- sigma(fit_INFY)
RET_ESG$volatility_INFY_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                           RET_ESG$volatility_INFY* 3,  
                           RET_ESG$volatility_INFY)
RET_ESG$INFY_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$INFY_log), sd = RET_ESG$volatility_INFY_S2)


RET_ESG$DLR_log <- log(1 + RET_ESG$DLR)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_dlr <- ugarchfit(spec = egarch_spec, data =RET_ESG$DLR_log)
RET_ESG$volatility_DLR <- sigma(fit_dlr)
RET_ESG$volatility_DLR_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                    RET_ESG$volatility_DLR* 3,  
                                    RET_ESG$volatility_DLR)
RET_ESG$DLR_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$DLR_log), sd = RET_ESG$volatility_DLR_S2)


RET_ESG$MA_log <- log(1 + RET_ESG$MA)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_MA <- ugarchfit(spec = egarch_spec, data =RET_ESG$MA_log)
RET_ESG$volatility_MA <- sigma(fit_MA)
RET_ESG$volatility_MA_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                              RET_ESG$volatility_MA * 2,  
                                              RET_ESG$volatility_MA)
RET_ESG$MA_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$MA_log), sd = RET_ESG$volatility_MA_S2)


RET_ESG$NVDA_log <- log(1 + RET_ESG$NVDA)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_NVDA <- ugarchfit(spec = egarch_spec, data =RET_ESG$NVDA_log)
RET_ESG$volatility_NVDA <- sigma(fit_NVDA)
RET_ESG$volatility_NVDA_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                     RET_ESG$volatility_NVDA * 3,  
                                     RET_ESG$volatility_NVDA)
RET_ESG$NVDA_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$NVDA_log), sd = RET_ESG$volatility_NVDA_S2)


RET_ESG$HD_log <- log(1 + RET_ESG$HD)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_HD <- ugarchfit(spec = egarch_spec, data =RET_ESG$HD_log)
RET_ESG$volatility_HD <- sigma(fit_HD)
RET_ESG$volatility_HD_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                   RET_ESG$volatility_HD * 1.5,  
                                   RET_ESG$volatility_HD)
RET_ESG$HD_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$HD_log), sd = RET_ESG$volatility_HD_S2)


RET_ESG$BAC_log <- log(1 + RET_ESG$BAC)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_BAC <- ugarchfit(spec = egarch_spec, data =RET_ESG$BAC_log)
RET_ESG$volatility_BAC <- sigma(fit_BAC)
RET_ESG$volatility_BAC_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                    RET_ESG$volatility_BAC * 1.5,  
                                    RET_ESG$volatility_BAC)
RET_ESG$BAC_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$BAC_log), sd = RET_ESG$volatility_BAC_S2)


RET_ESG$GOOGL_log <- log(1 + RET_ESG$GOOGL)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_GOOGL <- ugarchfit(spec = egarch_spec, data =RET_ESG$GOOGL_log)
RET_ESG$volatility_GOOGL <- sigma(fit_GOOGL)
RET_ESG$volatility_GOOGL_S2 <- ifelse(RET_ESG$date >= shock_start & RET_ESG$date <= shock_end, 
                                      RET_ESG$volatility_GOOGL * 2,  
                                      RET_ESG$volatility_GOOGL)
RET_ESG$GOOGL_shocked_returns <- rnorm(2768, mean = mean(RET_ESG$GOOGL_log), sd = RET_ESG$volatility_GOOGL_S2)


RET_nESG$TD_log <- log(1 + RET_nESG$TD)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_TD <- ugarchfit(spec = egarch_spec, data =RET_nESG$TD_log)
RET_nESG$volatility_TD <- sigma(fit_TD)
RET_nESG$volatility_TD_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                   RET_nESG$volatility_TD * 2.2,  
                                   RET_nESG$volatility_TD)
RET_nESG$TD_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$TD_log), sd = RET_nESG$volatility_TD_S2)


RET_nESG$TSLA_log <- log(1 + RET_nESG$TSLA)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_TSLA <- ugarchfit(spec = egarch_spec, data =RET_nESG$TSLA_log)
RET_nESG$volatility_TSLA <- sigma(fit_TSLA)
RET_nESG$volatility_TSLA_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                      RET_nESG$volatility_TSLA * 2.5,  
                                      RET_nESG$volatility_TSLA)
RET_nESG$TSLA_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$TSLA_log), sd = RET_nESG$volatility_TSLA_S2)


RET_nESG$AMZN_log <- log(1 + RET_nESG$AMZN)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_AMZN <- ugarchfit(spec = egarch_spec, data =RET_nESG$AMZN_log)
RET_nESG$volatility_AMZN <- sigma(fit_AMZN)
RET_nESG$volatility_AMZN_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                      RET_nESG$volatility_AMZN * 2.2,  
                                      RET_nESG$volatility_AMZN)
RET_nESG$AMZN_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$AMZN_log), sd = RET_nESG$volatility_AMZN_S2)


RET_nESG$JPM_log <- log(1 + RET_nESG$JPM)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_JPM <- ugarchfit(spec = egarch_spec, data =RET_nESG$JPM_log)
RET_nESG$volatility_JPM <- sigma(fit_JPM)
RET_nESG$volatility_JPM_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                     RET_nESG$volatility_JPM * 2.5,  
                                     RET_nESG$volatility_JPM)
RET_nESG$JPM_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$JPM_log), sd = RET_nESG$volatility_JPM_S2)


RET_nESG$SATS_log <- log(1 + RET_nESG$SATS)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_SATS <- ugarchfit(spec = egarch_spec, data =RET_nESG$SATS_log)
RET_nESG$volatility_SATS <- sigma(fit_SATS)
RET_nESG$volatility_SATS_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                      RET_nESG$volatility_SATS * 2.5,  
                                      RET_nESG$volatility_SATS)
RET_nESG$SATS_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$SATS_log), sd = RET_nESG$volatility_SATS_S2)


RET_nESG$CMCSA_log <- log(1 + RET_nESG$CMCSA)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_CMCSA <- ugarchfit(spec = egarch_spec, data =RET_nESG$CMCSA_log)
RET_nESG$volatility_CMCSA <- sigma(fit_CMCSA)
RET_nESG$volatility_CMCSA_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                       RET_nESG$volatility_CMCSA * 2.2,  
                                       RET_nESG$volatility_CMCSA)
RET_nESG$CMCSA_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$CMCSA_log), sd = RET_nESG$volatility_CMCSA_S2)


RET_nESG$CVX_log <- log(1 + RET_nESG$CVX)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_CVX <- ugarchfit(spec = egarch_spec, data =RET_nESG$CVX_log)
RET_nESG$volatility_CVX <- sigma(fit_CVX)
RET_nESG$volatility_CVX_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                     RET_nESG$volatility_CVX * 2.5,  
                                     RET_nESG$volatility_CVX)
RET_nESG$CVX_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$CVX_log), sd = RET_nESG$volatility_CVX_S2)


RET_nESG$PG_log <- log(1 + RET_nESG$PG)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_PG <- ugarchfit(spec = egarch_spec, data =RET_nESG$PG_log)
RET_nESG$volatility_PG <- sigma(fit_PG)
RET_nESG$volatility_PG_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                    RET_nESG$volatility_PG * 2.2,  
                                    RET_nESG$volatility_PG)
RET_nESG$PG_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$PG_log), sd = RET_nESG$volatility_PG_S2)


RET_nESG$PFE_log <- log(1 + RET_nESG$PFE)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = "std")
fit_PFE <- ugarchfit(spec = egarch_spec, data =RET_nESG$PFE_log)
RET_nESG$volatility_PFE <- sigma(fit_PFE)
RET_nESG$volatility_PFE_S2 <- ifelse(RET_nESG$date >= shock_start & RET_nESG$date <= shock_end, 
                                     RET_nESG$volatility_PFE * 2.2,  
                                     RET_nESG$volatility_PFE)
RET_nESG$PFE_shocked_returns <- rnorm(2768, mean = mean(RET_nESG$PFE_log), sd = RET_nESG$volatility_PFE_S2)



Shock_ret_ESG <- data.frame(
  date = RET_ESG$date,  
  TSM = RET_ESG$TSM_shocke_returns,
  HP = RET_ESG$HP_shocked_returns,
  INFY = RET_ESG$INFY_shocked_returns,
  DLR = RET_ESG$DLR_shocked_returns,
  MA = RET_ESG$MA_shocked_returns,
  NVDA = RET_ESG$NVDA_shocked_returns,
  HD = RET_ESG$HD_shocked_returns,
  BAC = RET_ESG$BAC_shocked_returns,
  GOOGL = RET_ESG$GOOGL_shocked_returns
)
Shock_ret_nESG <- data.frame(
  date = RET_nESG$date,  
  TD = RET_nESG$TD_shocked_returns,
  TSLA = RET_nESG$TSLA_shocked_returns,
  AMZN = RET_nESG$AMZN_shocked_returns,
  JPM = RET_nESG$JPM_shocked_returns,
  SATS = RET_nESG$SATS_shocked_returns,
  CMCSA = RET_nESG$CMCSA_shocked_returns,
  CVX = RET_nESG$CVX_shocked_returns,
  PG = RET_nESG$PG_shocked_returns,
  PFE = RET_nESG$PFE_shocked_returns
)

volatilities_ESG <- data.frame(
  date = RET_ESG$date,  
  TSM = RET_ESG$volatility_TSM,
  HP = RET_ESG$volatility_HP,
  INFY = RET_ESG$volatility_INFY,
  DLR = RET_ESG$volatility_DLR,
  MA = RET_ESG$volatility_MA,
  NVDA = RET_ESG$volatility_NVDA,
  HD = RET_ESG$volatility_HD,
  BAC = RET_ESG$volatility_BAC,
  GOOGL = RET_ESG$volatility_GOOGL
)
Volatilities_ESG<-volatilities_ESG [,-1]

volatilities_nESG <- data.frame(
  date = RET_nESG$date,  
  TD = RET_nESG$volatility_TD,
  TSLA = RET_nESG$volatility_TSLA,
  AMZN = RET_nESG$volatility_AMZN,
  JPM = RET_nESG$volatility_JPM,
  SATS = RET_nESG$volatility_SATS,
  CMCSA = RET_nESG$volatility_CMCSA,
  CVX = RET_nESG$volatility_CVX,
  PG = RET_nESG$volatility_PG,
  PFE = RET_nESG$volatility_PFE
)

shock_volatilities_ESG <- data.frame(
  date = RET_ESG$date,  
  TSM = RET_ESG$volatility_TSM_S2,
  HP = RET_ESG$volatility_HP_S2,
  INFY = RET_ESG$volatility_INFY_S2,
  DLR = RET_ESG$volatility_DLR_S2,
  MA = RET_ESG$volatility_MA_S2,
  NVDA = RET_ESG$volatility_NVDA_S2,
  HD = RET_ESG$volatility_HD_S2,
  BAC = RET_ESG$volatility_BAC_S2,
  GOOGL = RET_ESG$volatility_GOOGL_S2
)

shock_volatilities_nESG <- data.frame(
  date = RET_nESG$date,  
  TD = RET_nESG$volatility_TD_S2,
  TSLA = RET_nESG$volatility_TSLA_S2,
  AMZN = RET_nESG$volatility_AMZN_S2,
  JPM = RET_nESG$volatility_JPM_S2,
  SATS = RET_nESG$volatility_SATS_S2,
  CMCSA = RET_nESG$volatility_CMCSA_S2,
  CVX = RET_nESG$volatility_CVX_S2,
  PG = RET_nESG$volatility_PG_S2,
  PFE = RET_nESG$volatility_PFE_S2
)


shock_ret_esg<-Shock_ret_ESG [,-1]
shock_ret_nesg<-Shock_ret_nESG [,-1]
vol_esg_shock<-shock_volatilities_ESG [,-1]
vol_nesg_shock<-shock_volatilities_nESG [,-1]


# Specify univariate GARCH for each series
Egarch_shock <- ugarchspec(
  variance.model = list(model = "eGARCH"),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)

# Multivariate DCC specification
dccspec_shock <- dccspec(
  uspec = multispec(replicate(9, Egarch_shock)), 
  dccOrder = c(1, 1),
  distribution = "mvt"
)


# Fit DCC-GARCH for ESG
fit_ESG_shock <- dccfit(dcc_spec_egarch, data = shock_ret_esg)

# Fit DCC-GARCH for non-ESG
fit_nESG_shock <- dccfit(dccspec_shock, data = shock_ret_nesg)
D_t_ESG <- diag(vol_esg_shock[t, ])
D_t_nESG <- diag(vol_nesg_shock[t, ])
R_t_ESG <- rcor(fit_ESG_shock)
R_t_nESG <- rcor(fit_nESG_shock)

n_timepoints <- dim(R_t_ESG)[3]
n_stocks <- dim(R_t_ESG)[1]

cov_matrix_ESG <- array(0, dim = c(n_stocks, n_stocks, n_timepoints))
cov_matrix_nESG <- array(0, dim = c(n_stocks, n_stocks, n_timepoints))

for (t in 1:n_timepoints) {
  D_t_ESG <- diag(vol_esg_shock[t, ])
  D_t_nESG <- diag(vol_nesg_shock[t, ])
  cov_matrix_ESG[,,t] <- D_t_ESG %*% R_t_ESG[,,t] %*% D_t_ESG
  cov_matrix_nESG[,,t] <- D_t_nESG %*% R_t_nESG[,,t] %*% D_t_nESG
}

# Initialize vectors to store portfolio volatilities
portfolio_volatility_shock_ESG <- numeric(dim(cov_matrix_ESG)[3])   
portfolio_volatility_shock_nESG <- numeric(dim(cov_matrix_nESG)[3]) 

# Compute portfolio volatilities over time
for (t in 1:dim(cov_matrix_nESG)[3]) {
  portfolio_volatility_shock_ESG[t] <- sqrt(t(w_ESG) %*% cov_matrix_ESG[, , t] %*% w_ESG)
  portfolio_volatility_shock_nESG[t] <- sqrt(t(w_nESG) %*% cov_matrix_nESG[, , t] %*% w_nESG)
}


vol_shock_esgvsnonesg <- data.frame(
  Date = RET_ESG$date,
  ESG = portfolio_volatility_shock_ESG,
  non_ESG = portfolio_volatility_shock_nESG
)
dev.new()
plot(dates,portfolio_volatility_esg[882:1259],type='l',ylim=c(0.003,0.022), main='Figure 12. Conditional Volatility before and after shock 2', ylab='Portfolio volatility', col='blue', lwd=2)
lines(dates,vol_shock_esgvsnonesg$ESG[882:1259],col='black',lwd=2)
lines(dates, portfolio_volatility_nesg[882:1259], col='red',lwd=2)
lines(dates,vol_shock_esgvsnonesg$non_ESG[882:1259],col='green',lwd=2)

legend("topleft", legend = c("ESG_vol","non-ESG_vol","ESG_vol_shocked", "non-ESG_vol_shocked"),
       col = c("blue","red","black", "green"), cex = 0.8,lwd = 2, box.lty = 'n' )


#CAPM
#importing dataset
data_FF<-read.csv("C:/data/WRDS-FM321/INPUT/FamaFrench.csv")
date.ts <- ymd(data_FF$X)
data_FF$Date <- as.Date(as.character(data_FF$X), format = "%Y%m%d")
FF_ESG<- merge(ESG_portfolio, data_FF, all=FALSE, by="Date")
FF_nESG<- merge(nESG_portfolio, data_FF, all=FALSE, by="Date")
FF_ESG <- FF_ESG[complete.cases(FF_ESG), ]
FF_nESG <- FF_nESG[complete.cases(FF_nESG), ]

FF_ESG$Mkt.RF <- FF_ESG$Mkt.RF / 100
FF_nESG$Rf <- FF_nESG$Rf / 100

# ESG Portfolio CAPM
capm_esg <- lm(FF_ESG$ESG_portfolio~ FF_ESG$Mkt.RF, data = FF_ESG)
summary(capm_esg)

# Non-ESG Portfolio CAPM
capm_nesg <- lm(FF_nESG$nESG_portfolio~ FF_nESG$Mkt.RF, data = FF_nESG)
summary(capm_nesg)


# ESG Portfolio Fama-French 5-Factor Model
ff5_esg <- lm(FF_ESG$ESG_portfolio~ FF_ESG$Mkt.RF + FF_ESG$SMB + FF_ESG$HML + FF_ESG$RMW + FF_ESG$CMA, data = FF_ESG)
summary(ff5_esg)

# Non-ESG Portfolio Fama-French 5-Factor Model
ff5_nesg <- lm(FF_nESG$nESG_portfolio~ FF_nESG$Mkt.RF + FF_nESG$SMB + FF_nESG$HML + FF_nESG$RMW + FF_nESG$CMA, data = FF_nESG)
summary(ff5_nesg)


