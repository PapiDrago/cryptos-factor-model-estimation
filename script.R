# Clear the environment 
rm(list=ls())

# Setting as working directory the folder of this script
setwd("~/Desktop/financial_data_science/cryptos-factor-model-estimation")
# Loading the real-time forecasting function, implemented according Stock and Watson methodology,
# which also integrates the Trapani test to collect evidence about the presence of a common factor structure in the data.
source("real-time_forecasting.R")
#source("real-time_forecasting2.R")

# Bibliography

# Stock, J. H., & Watson, M. W. (2002). Forecasting Using Principal Components From a Large Number of Predictors
# https://doi.org/10.1198/016214502388618960

# Stock, J. H., & Watson, M. W. (2002). Macroeconomic Forecasting Using Diffusion Indexes.
# https://doi.org/10.1198/073500102317351921

# Trapani, L. (2018). A Randomized Sequential Procedure to Determine the Number of Factors.
# https://doi.org/10.1080/01621459.2017.1328359

# Bianchi Daniele, and Mykola Babiak. "A factor model for cryptocurrency returns." CERGE-EI Working Paper Series 710 (2021).

# Install packages
# install.packages("ggplot2",dependencies=TRUE)
# install.packages("readxl",dependencies=TRUE)
# install.packages("corrplot",dependencies=TRUE)
# install.packages("dfms", depedencies=TRUE) # To estimate number of factor in a static factor model under
# information criteria. (Static factor model are a subset of dynamical factor model).
# It handles dataset with missing data
# install.packages("xts", dependencies=TRUE) # General package to study time-series dataset
# install.packages("HDRFA", dependencies=TRUE) # it contains functions to estimate k
# with PCA and more, but also techinique to handle data in which outliers are frequent (robust PCA)
#install.packages("crypto2", dependencies=TRUE)
#install.packages("readr", dependencies=TRUE)

# Call libraries
library(readxl) # To use 'read_excel'
library(dfms) # To use 'ICr'
library(HDRFA) # To use 'PCA'
library(crypto2) #To use 'crypto_list', 'crypto_history'
library(dplyr) #To use '%>%'
library(tidyr) #To use 'pivot_wider'
library(readr) # To use 'read_csv', 'write_csv' ...
library(zoo) # To use 'na.locf'

# Import dataset
crypto_dataset<-read_excel("dataset1_cryptos.xlsx")


# Explore data
str(crypto_dataset) # Display the internal str ucture of an R object.

summary(crypto_dataset$ETH) # Display basic summary statistics
summary(crypto_dataset$BTC) # This is enough to see that the sample distributions
summary(crypto_dataset$XRP) # of crypto assets are not symmetric.
summary(crypto_dataset$LTC) # They are skewed to the right.
summary(crypto_dataset$XLM) # Data are not "well behaved". Variance is not meaningful.

hist(crypto_dataset$ETH) # Histograms give us an idea of the shape of the data
hist(crypto_dataset$BTC) # distributions. In our case we see long tails of values
hist(crypto_dataset$XRP) # to the right. Confirming our guess about the absence
hist(crypto_dataset$LTC) # symmetry.
hist(crypto_dataset$XLM)

# Boxplots provide us an equivalent point of view about the patterns in our data
boxplot(crypto_dataset$ETH, main="Boxplot of ETH Asset Price", ylab="Price ($)")
quantile(crypto_dataset$ETH)

boxplot(crypto_dataset$BTC, main="Boxplot of BTC Asset Price", ylab="Price ($)")
quantile(crypto_dataset$BTC)

boxplot(crypto_dataset$XRP, main="Boxplot of XRP Asset Price", ylab="Price ($)")
quantile(crypto_dataset$XRP)

boxplot(crypto_dataset$LTC, main="Boxplot of LTC Asset Price", ylab="Price ($)")
quantile(crypto_dataset$LTC)

boxplot(crypto_dataset$XLM, main="Boxplot of XLM Asset Price", ylab="Price ($)")
quantile(crypto_dataset$XLM)



cor(crypto_dataset[-1]) # Is there collinearity? In the case of crypyto assets, yes.
                        # Are cryptos driven by common features?

# Data pre-processing

crypto_dataset<-crypto_dataset[-1] # Removing the 'date' column

log_crypto_dataset<-log(crypto_dataset) # The natural logarithm of our data

log_ret_crypto_dataset<-as.data.frame(diff(as.matrix(log_crypto_dataset))) # The first difference between consecutive data observation

summary(log_ret_crypto_dataset)
cor(log_ret_crypto_dataset)

# With the previous statements we obtain log-returns. The statistical properties
# of log-returns are more tractable and comply with the hyphoteses we make
# when applying factor models. Now the distributions are more symmetric and
# the weight of outliers is diminished.

# Since we suspect that cryptos share common factors we estimate a foctor model
# in order to try to predict future log-returns.
# We are going to compare it with an autoregressive (AR) linear model in order to see
# if it is capable of capturing more than simply the previous history.

results <- run_stock_watson_forecast(log_ret_crypto_dataset)
print(results$Relative_MSE)
print(results$AR_parameter_history)
#print(results$Trapani_factor_number_history)

# A relative MSE greater than 1 means that the factor model performed poorer
# than the AR model, whereas a relative MSE smaller than 1 means that the 
# factor model was better.
# In our case choosing the factor model seems to make no difference with respect
# of choosing an AR model. Furthermore for 3 assets over 5
# the number of AR parameters is 0, meaning that is more convenient to predict
# the future by simply predict the sample mean.

# Now we try to see if the reason our factor model acted poorly was due to the
# small dataset, specifically to the the fact that the number of unit N was very
# small compared to the number of observation.

# To retrieve historical prices regarding more cryptos we use the package "crypto2"

#all_coins <- crypto_list(only_active = TRUE) # We obtain a list of all coins currently tradeable

#top_100_coins <- all_coins[1:100, ] # We retain just the first 100 coins

# Now we download all what we need (specifically the prices along
# the same daily period of the previous dataset) and we treat the data acquired
# to obtain a dataset with the same structure of the smaller one.

#raw_data <- crypto_history(coin_list = top_100_coins, 
                           #start_date = "2016-01-01", 
                           #end_date = "2019-09-30")

#write_csv(raw_data, "crypto_raw_historical_100.csv") # We store the data locally
                                                      # to avoid download them again
#raw_crypto_data_100<-read_csv("crypto_raw_historical_100.csv") 

# We clean the raw data inspired by Bianchi Daniele and Mykola Babiak paper:
"A factor model for cryptocurrency returns."

# We keep only the coins whose trading volume is greater than $100 million
# because we are interested in considering coins which are treaded a lot.
# We keep only the coins which have at max the 25% of missing data
# We filter-out stable coins since they are linked to actual coins and then they
# do not have the typical volatility of a crypto coin (we would like to model
# that volatility).
market_cap_floor <- 100000000  # 
missing_threshold <- 0.25      # 25% Max Missing Data 
stablecoins <- c("USDT", "USDC", "TUSD", "PAX", "DAI", "USDS", "GUSD")

filtered_data <- raw_crypto_data_100 %>%
  filter(!(symbol %in% stablecoins))

valid_symbols <- filtered_data %>%
  group_by(symbol) %>%
  summarize(max_cap = max(market_cap, na.rm = TRUE)) %>%
  filter(max_cap >= market_cap_floor) %>%
  pull(symbol)


filtered_data <- filtered_data %>%
  filter(symbol %in% valid_symbols)
#str(filtered_data)
wide_prices <- filtered_data %>%
  select(time_open, symbol, close) %>%
  rename(Date = time_open) %>%
  distinct(Date, symbol, .keep_all = TRUE) %>%
  pivot_wider(names_from = symbol, values_from = close) %>%
  arrange(Date)

n_obs <- nrow(wide_prices)
keep_cols <- colSums(is.na(wide_prices)) / n_obs <= missing_threshold
final_prices <- wide_prices[, keep_cols]
#colSums(is.na(log_final_prices))
# is.na(wide_prices) is a matrix which has TRUE (1) where there is a missing value in the original matrix
# colSums(is.na(wide_prices)) is a 1D vector so that each elements is the number of data missing in the
# corresponding column in the original matrix, this number then becomes a percentage, finally if it is
# smaller than our threshold then we obtain a 1 (TRUE) that is used to mantain the column (coin) in
# final matrix.


write_csv(final_prices, "crypto_simplified_final_prices_100.csv")
final_prices<-read_csv("crypto_simplified_final_prices_100.csv")


final_prices<-final_prices[-1]

final_prices <- na.locf(final_prices, na.rm = FALSE) # Possibly missing values present are replaced by the most recent non-NA prior to it
colSums(is.na(final_prices))


log_final_prices<-log(final_prices)

log_ret_final_prices<-as.data.frame(diff(as.matrix(log_final_prices)))

results_50 <- run_stock_watson_forecast(log_ret_final_prices)
print(results_50$Relative_MSE)
print(results_50$AR_parameter_history)
#print(results_50$Trapani_factor_number_history)

# The results are analogous to the previous one obtained with the smaller dataset.
# Now we ask whether a factor structure does actually exist in the data or not.
# To do so it has been implemented the Trapani test which uses hyphotesis testing
# to check if the K largest eigenvalues of the sample covariance matrix are actually
# diverging to infinity.

print(results$Trapani_factor_number_history)

# We can reject with 99 % confidence, at each step, the null hypthotesis. Thus
# we have a statistically strong evidence that an estimated factor model with this data
# is not able to discriminate the signal from the noise. There no exists a common factor structure in the data.


print(results_50$Trapani_factor_number_history)

# Here in the majority of the time windows the test judges as highly significant just one common factor.
# So with a fatter dataset a common factor structure starts to appear in the data.
# Unfortunately this is not enaugh to choose an estimated factor model instead of a simple AR model.

# In the following part of the script a slightly different version of the stock_watson simulation is used.
# The number of eigenvalues used in estimating the common factors is now decided by the Trpani test.
# We cannot appreciate any difference in terms of relative MSE since also the Bai and Ng IC return the same number of factors.
# Please note that in this case k_max according to the Schwert's rule is 5. But both information criteria
# and Trapani test agree on k_hat = 1.

source("real-time_forecasting2.R")

results_50_2 <- run_stock_watson_forecast(log_ret_final_prices)
print(results_50_2$Relative_MSE)
print(results_50_2$AR_parameter_history)
print(results_50_2$Trapani_factor_number_history)


print(results$Relative_MSE)
print(results_50$Relative_MSE)

