# Clear the environment 
rm(list=ls())

# Install packages 
# install.packages("ggplot2",dependencies=TRUE)
# install.packages("readxl",dependencies=TRUE)
# install.packages("corrplot",dependencies=TRUE)
install.packages("dfms", depedencies=TRUE) # To estimate number of factor in a static factor model under
                   # information criteria. (Static factor model are a subset of 
                   # dynamical factor model). It handles dataset with missing data
install.packages("xts", dependencies=TRUE) # General package to study time-series
                                           # dataset
install.packages("HDRFA", dependencies=TRUE) # it contains functions to estimate k
                                             # with PCA and more, but also
                                             # techinique to handle data in which
                                             # outliers are frequent (robust PCA)
install.packages("magrittr", dependencies=TRUE)
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

# Let's remove random-walkness (non-stationarity)
standardized_returns <- scale(log_ret_crypto_dataset) # Demean and standardize
standardized_returns <- as.data.frame(standardized_returns)
summary(standardized_returns)
plot(standardized_returns)

# Test per capire se queste percentuali delle 5 time series sono stazionarie?

# var(standardized_returns) 
det(cov(standardized_returns))

# Determine the number of factors
ic<-ICr(standardized_returns) # leggi la documentazione per capire effettivamente quali cruteri sono stati usati
                              # IC3 always overrestimate the number of common factors
                              # In general all of them overestimate
plot(ic) # This plots loss functions

er<-PCA_FN(standardized_returns, 12) # eigenvalue ratio estimation of k differs from ic
# is u_{i,t} correlated to u_{j,t}? Is cross-sectional correlation present? I think yes but how to test

estimated_terms<-PCA(as.matrix(standardized_returns), 4, constraint = "L") #hdrfa
#Estimating the loadings, as the eigenvectors associated to the k largest eigs of cov(x)
# and it estimates the common factors
F_hat = estimated_terms$Fhat
L_hat = estimated_terms$Lhat

#Estimate then common component and then make difference
estimated_common_components<-F_hat%*%t(L_hat)

errors = standardized_returns - estimated_common_components
# Cerca magari sul paper un metodo affidabile per valutare la goodness of fit di un factor model
# poi prova a usare pure un metodo autoregressivo per predire l'errore idiosincratico
summary((errors))
