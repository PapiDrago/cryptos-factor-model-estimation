# Cryptocurrency Factor Forecasting & Structural Validation

This repository implements a real-time forecasting engine for cryptocurrency log-returns based on the **Stock & Watson (2002)** Diffusion Index framework. To ensure statistical integrity, the engine is augmented with the **Trapani (2018)** Randomized Sequential Procedure to validate the existence of a common factor structure within the data.

## Overview
Cryptos are financial assets that suffer from an extreme volatility: asset prices can change abruptly all of a sudden bringing the invested capital at risk.
The main goal of this project is to assess if a static factor model can be a good tool to predict cryptocurrency volatility.

The algorithm performs a recursive, real-time comparison between:
1.  **Static Factor Model:** Uses Principal Component Analysis (PCA) to extract unobservable common factors.
2.  **Autoregressive (AR) Model:** A univariate benchmark optimized via Bayesian Information Criterion (BIC) at every time step.

##  Key Features
- **Trapani Randomized Test:** Acts as a statistical "gatekeeper" using a randomized sequential test ($\alpha = 0.01$) to check if eigenvalues truly diverge, preventing factor overestimation.
- **Real-Time Simulation:** At each step of the simulation we act as if we want to predict the next occurence in time of the crypto log-return. We do so considering the data up to that point in time. Then we compare the prediction with the actual observation not considered in the dataset used for estimating both models.
- **Robust Data Cleaning:** Implements volume-based filtering, stablecoin exclusion, and missing-data thresholding (inspired by the Bianchi & Babiak methodology).
- **Comprehensive Logging:** Stores `k_trapani` and `p_history` for every iteration to analyze market regime changes.

## Project Structure
- `script.R`: The entry point for data loading, exploratory analysis, and simulation.
- `real-time_forecasting.R`: Contains the core `run_stock_watson_forecast` function.
- `trapani_factor_test.R`: The implementation of the Randomized Sequential Procedure.
- `dataset1_cryptos.xlsx`: Thinner dataset for initial testing.
- `crypto_simplified_final_prices_100.csv`: Fatter dataset for trying to capture common factors.

### Prerequisites
This code was tested using R 4.5.3 and the following packages:
```R
install.packages(c("readxl", "dfms", "HDRFA", "crypto2", "dplyr", "tidyr", "readr", "zoo"))
