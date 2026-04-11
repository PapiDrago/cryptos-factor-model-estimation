# Clear the environment 
rm(list=ls())

# Setting as working directory the folder of this script
setwd("/Users/papidrago/Desktop/financial_data_science/cryptos-factor-model-estimation") 

# Loading the realt-time forecasting function
source("real-time_forecasting.R")

# Install packages 
# install.packages("ggplot2",dependencies=TRUE)
# install.packages("readxl",dependencies=TRUE)
# install.packages("corrplot",dependencies=TRUE)
#install.packages("dfms", depedencies=TRUE) # To estimate number of factor in a static factor model under
# information criteria. (Static factor model are a subset of 
# dynamical factor model). It handles dataset with missing data
#install.packages("xts", dependencies=TRUE) # General package to study time-series
# dataset
#install.packages("HDRFA", dependencies=TRUE) # it contains functions to estimate k
# with PCA and more, but also
# techinique to handle data in which
# outliers are frequent (robust PCA)
#install.packages("magrittr", dependencies=TRUE)
# Call libraries
#library(ggplot2)
library(readxl)
library(dfms)
#library(corrplot)
library(xts)
library(HDRFA)
library(magrittr)

# Import dataset
crypto_dataset<-read_excel("dataset1_cryptos.xlsx")


# Explore data
str(crypto_dataset) # Display the internal str ucture of an R object.

summary(crypto_dataset$ETH)
summary(crypto_dataset$BTC)
summary(crypto_dataset$XRP)
summary(crypto_dataset$LTC)
summary(crypto_dataset$XLM)


boxplot(crypto_dataset$ETH, main="Boxplot of ETH Asset Price", ylab="Price ($)")
boxplot.stats(crypto_dataset$ETH)
quantile(crypto_dataset$ETH)
outlier_count <- length(boxplot.stats(crypto_dataset$ETH)$out)
count <- length(crypto_dataset$ETH)
print((outlier_count/count)*100)
hist(crypto_dataset$ETH)

boxplot(crypto_dataset$BTC)
boxplot.stats(crypto_dataset$BTC)
quantile(crypto_dataset$BTC)
outlier_count <- length(boxplot.stats(crypto_dataset$BTC)$out)
count <- length(crypto_dataset$BTC)
print((outlier_count/count)*100)
hist(crypto_dataset$BTC)


boxplot(crypto_dataset$XRP)
boxplot.stats(crypto_dataset$XRP)
quantile(crypto_dataset$XRP)
outlier_count <- length(boxplot.stats(crypto_dataset$XRP)$out)
count <- length(crypto_dataset$XRP)
print((outlier_count/count)*100)
hist(crypto_dataset$XRP)


boxplot(crypto_dataset$LTC)




boxplot(crypto_dataset$XLM)



# a<-scan(text = "100 200 300 400 500 600 700 800 900 1000")
# boxplot(a)

# b<-scan(text = "100 200 300 400 500 500 500 500 500 500 500 600 700 800 900 1000")
# boxplot(a, b)

# c<-scan(text = "79.98 80.04 80.02 80.04 80.03 80.03 80.04 79.97
# 80.05 80.03 80.02 80.00 80.02")
# boxplot(c)


crypto_dataset<-crypto_dataset[-1]

log_crypto_dataset<-log(crypto_dataset)
summary(log_crypto_dataset)
hist(log_crypto_dataset$ETH)

var(crypto_dataset$ETH)
var(log_crypto_dataset$ETH)
mean(crypto_dataset$ETH)
mean(log_crypto_dataset$ETH)

log_ret_crypto_dataset<-as.data.frame(diff(as.matrix(log_crypto_dataset)))
summary(log_ret_crypto_dataset)
hist(log_ret_crypto_dataset$ETH)

results <- run_stock_watson_forecast(log_ret_crypto_dataset)
print(results$Relative_MSE)

cor(log_crypto_dataset)


# Install and load
#install.packages("crypto2")
library(crypto2)
library(dplyr)
library(tidyr)
install.packages("readr",dependencies=TRUE)
library(readr) # for fast CSV writing

# 1. Get a list of all coins available
all_coins <- crypto_list(only_active = TRUE)

# 2. Filter for the top N coins (e.g., top 30 by current rank)
top_100_coins <- all_coins[1:100, ]

# 3. Download daily historical OHLC data
# Note: This may take a few minutes depending on the number of coins
#raw_data <- crypto_history(coin_list = top_100_coins, 
                           #start_date = "2016-01-01", 
                           #end_date = "2019-09-30")

#write_csv(raw_data, "crypto_raw_historical_100.csv")

raw_crypto_data_100<-read_csv("crypto_raw_historical_100.csv")
#colSums(is.na(raw_crypto_data_100))

# 2. Transform the data
simplified_data <- raw_crypto_data_100 %>%
  # Keep only the columns we need
  select(time_open, symbol, close) %>%
  # Rename 'time_open' to 'Date' as requested
  rename(Date = time_open) %>%
  # Ensure we don't have duplicates for the same Date/Symbol
  distinct(Date, symbol, .keep_all = TRUE) %>%
  # Pivot: Move symbols from rows to columns
  # This creates: Date | BTC | ETH | XRP ...
  pivot_wider(names_from = symbol, values_from = close) %>%
  # Sort by Date chronologically
  arrange(Date)

# 3. Save to a new, simplified CSV
write_csv(simplified_data, "crypto_simplified_prices_100.csv")
colSums(is.na(simplified_data))


# Check the first few rows to confirm it looks correct
head(simplified_data)
















# Let's remove random-walkness (non-stationarity)
standardized_returns <- scale(log_ret_crypto_dataset) # Demean and standardize
standardized_returns <- as.data.frame(standardized_returns)
summary(standardized_returns)
plot(standardized_returns)

T_total <- nrow(standardized_returns)
N <- ncol(standardized_returns)

h <- 1  # h is the forecasting horizon

# 2. Define the Initial Training Split (e.g., first 1000 observations)
split_ratio <- 0.75
training_size_init <- floor(T_total * split_ratio)
# training_size_init is the number of observations (what we call typically T) in the starting training set
test_indices <- (training_size_init + 1):(T_total - h)
# I stop to T_total - h because eventually t 
forecasts <- numeric(length(test_indices))
actuals <- numeric(length(test_indices))

# Test per capire se queste percentuali delle 5 time series sono stazionarie?

# var(standardized_returns) 
#det(cov(standardized_returns))
# Determine the number of factors
ic<-ICr(standardized_returns, 3) # leggi la documentazione per capire effettivamente quali cruteri sono stati usati
                              # IC3 always overrestimate the number of common factors
                              # In general all of them overestimate
plot(ic) # This plots loss functions
limit <- floor(8*(min(5, T_total) / 100)^(1/4))
er<-PCA_FN(standardized_returns, 12) # eigenvalue ratio estimation of k differs from ic.
# The reason may be related to the fact that T>>N, I may try to work on it by considering more cryptos and less time

# is u_{i,t} correlated to u_{j,t}? Is cross-sectional correlation present? I think yes but how to test

estimated_terms<-PCA(as.matrix(standardized_returns), 4, constraint = "L") #hdrfa
#Estimating the loadings, as the eigenvectors associated to the k largest eigs of cov(x)
# and it estimates the common factors
F_hat <- estimated_terms$Fhat
F_hat <- as.data.frame(F_hat)
X_factors <- F_hat[1:(100 - 1), , drop = FALSE]

L_hat = estimated_terms$Lhat # phi_hat



#Estimate then common component and then make difference
fitted_values<-F_hat%*%t(L_hat)
fitted_values<-as.data.frame(fitted_values)


errors<-standardized_returns - fitted_values
squared_errors = (as.matrix(errors))^2
str(squared_errors)
V_k <- mean(squared_errors)
# Cerca magari sul paper un metodo affidabile per valutare la goodness of fit di un factor model
# poi prova a usare pure un metodo autoregressivo per predire l'errore idiosincratico

# Ora devi scegliere il numero di parametri da usare nel linear regression model. Non uso il lasso
# perche' aggiungerei un bias oltre all'attuetation bias introdotto dai generated regressors.

#factor_1<-F_hat[,1]
#lag1_factor_1_correlation<-acf(factor_1, 1, "correlation", demean=TRUE)

#returns_1<-crypto_dataset[,1]
#lag1_returns_1_correlation<-acf(returns_1, 1, "correlation", demean=TRUE) # Nota che se studio la correlazione delle x allora c'è. Perché?

#lag1_correlations <- apply(F_hat, 2, function(x) {
#  tmp_acf <- acf(x, plot = FALSE, lag.max = 1)
#  return(tmp_acf$acf[2])
#})
#names(lag1_correlations) <- c("Factor 1", "Factor 2", "Factor 3", "Factor 4")
#print(lag1_correlations)
#eigen(cov(standardized_returns))
