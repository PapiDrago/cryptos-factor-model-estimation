# Clear the environment 
rm(list=ls())

# Install packages 
# install.packages("ggplot2",dependencies=TRUE)
# install.packages("readxl",dependencies=TRUE)
# install.packages("corrplot",dependencies=TRUE)

# Call libraries
#library(ggplot2)
library(readxl)
#library(corrplot)

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
