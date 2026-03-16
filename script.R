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

hist(crypto_dataset$ETH)

# boxplot(crypto_dataset$ETH, crypto_dataset$BTC)
boxplot(crypto_dataset$ETH)
boxplot(crypto_dataset$BTC)
boxplot(crypto_dataset$XRP)
boxplot(crypto_dataset$LTC)
boxplot(crypto_dataset$XLM)



a<-scan(text = "100 200 300 400 500 600 700 800 900 1000")
boxplot(a)

b<-scan(text = "100 200 300 400 500 600 700 800 900 1000 2000")
boxplot(a, b)