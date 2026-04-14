# Clear the environment 
rm(list=ls())

# Setting as working directory the folder of this script
setwd("/Users/papidrago/Desktop/financial_data_science/cryptos-factor-model-estimation") 

# Loading the realt-time forecasting function
source("real-time_forecasting.R")
#source("real-time_forecasting2.R")

# Install packages ì
# install.packages("ggplot2",dependencies=TRUE)
# install.packages("readxl",dependencies=TRUE)
# install.packages("corrplot",dependencies=TRUE)
#install.packages("dfms", depedencies=TRUE) # To estimate number of factor in a static factor model under
# information criteria. (Static factor model are a subset of 
# dynamical factor model). It handles dataset with missing data
#install.packages("xts", dependencies=TRUE) # General package to study time-series
# dataset
#install.packages("HDRFA", dependencies=TRUE) # it contains functions to estimate k
# with PCA and more, but also techinique to handle data in which outliers are frequent (robust PCA)
#install.packages("crypto2")
#install.packages("readr",dependencies=TRUE)

# Call libraries
library(readxl) # to use read_excel
library(dfms) # For ICr
library(HDRFA) # For PCA
library(crypto2) #To use crypto_list, crypto_history
library(dplyr) #To use %>%
library(tidyr) #To use pivot_wider
library(readr) # to use read_csv, write_csb ...
library(zoo) # ti use na.locf

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


# Clean the data
market_cap_floor <- 100000000  # $100 Million 
missing_threshold <- 0.25      # 25% Max Missing Data 
stablecoins <- c("USDT", "USDC", "TUSD", "PAX", "DAI", "USDS", "GUSD")

# 1. Filter out stablecoins
filtered_data <- raw_crypto_data_100 %>%
  filter(!(symbol %in% stablecoins))


# 2. Filter by Market Cap
# Note: In the paper, assets were included if they met the threshold [cite: 151]
# We'll keep coins that reached the $100M threshold at some point in your period
valid_symbols <- filtered_data %>%
  group_by(symbol) %>%
  summarize(max_cap = max(market_cap, na.rm = TRUE)) %>%
  filter(max_cap >= market_cap_floor) %>%
  pull(symbol)


filtered_data <- filtered_data %>%
  filter(symbol %in% valid_symbols)
str(filtered_data)
# 3. Pivot to Wide Format and check Missing Data (Density)
wide_prices <- filtered_data %>%
  select(time_open, symbol, close) %>%
  rename(Date = time_open) %>%
  # Ensure we don't have duplicates for the same Date/Symbol
  distinct(Date, symbol, .keep_all = TRUE) %>%
  pivot_wider(names_from = symbol, values_from = close) %>%
  arrange(Date)

# 4. Remove coins with more than 25% NAs 
n_obs <- nrow(wide_prices)
keep_cols <- colSums(is.na(wide_prices)) / n_obs <= missing_threshold
final_prices <- wide_prices[, keep_cols]
#colSums(is.na(log_final_prices))

write_csv(final_prices, "crypto_simplified_final_prices_100.csv")
final_prices<-read_csv("crypto_simplified_final_prices_100.csv")


final_prices<-final_prices[-1]

final_prices <- na.locf(final_prices, na.rm = FALSE)
colSums(is.na(final_prices))


log_final_prices<-log(final_prices)
summary(log_final_prices)
hist(log_final_prices$ETH)

log_ret_final_prices<-as.data.frame(diff(as.matrix(log_final_prices)))
summary(log_ret_final_prices)
hist(log_ret_final_prices$ETH)

colSums(is.na(log_final_prices))
#860 day of BCN is missing

colSums(is.na(log_ret_final_prices))
#859 and 860 day are missing

results_50 <- run_stock_watson_forecast(log_ret_final_prices)
print(results_50$Relative_MSE)

cor(log_final_prices)