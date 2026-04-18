# Cryptocurrency Factor Forecasting & Structural Validation

This repository implements a real-time forecasting engine for cryptocurrency log-returns based on the **Stock & Watson (2002)** Diffusion Index framework. To ensure statistical integrity, the engine is augmented with the **Trapani (2018)** Randomized Sequential Procedure to validate the existence of a common factor structure.

## 📊 Overview
The main goal of this project is to determine whether cryptocurrency markets are driven by latent common factors (like "market sentiment" or "Bitcoin dominance") or if their movements are primarily idiosyncratic.

The algorithm performs a recursive, real-time comparison between:
1.  **Dynamic Factor Model (DFM):** Uses Principal Component Analysis (PCA) to extract latent predictive factors.
2.  **Autoregressive (AR) Model:** A univariate benchmark optimized via Bayesian Information Criterion (BIC) at every time step.

## 🧬 Key Features
- **Trapani Randomized Test:** Acts as a statistical "gatekeeper" using a randomized sequential test ($\alpha = 0.01$) to check if eigenvalues truly diverge, preventing factor overestimation.
- **Real-Time Simulation:** Full recursive loops that update standardization parameters (Mean/SD) and model coefficients at each step to prevent **look-ahead bias**.
- **Robust Data Cleaning:** Implements volume-based filtering, stablecoin exclusion, and missing-data thresholding (inspired by the Bianchi & Babiak methodology).
- **Comprehensive Logging:** Stores `k_trapani` and `p_history` for every iteration to analyze market regime changes.

## 📂 Project Structure
- `main_script.R`: The entry point for data loading, exploratory analysis, and visualization.
- `real-time_forecasting.R`: Contains the core `run_stock_watson_forecast` function.
- `trapani_factor_test.R`: The implementation of the Randomized Sequential Procedure.
- `dataset1_cryptos.xlsx`: Sample dataset for initial testing.

## 🚀 Getting Started

### Prerequisites
You will need R 4.0+ and the following libraries:
```R
install.packages(c("readxl", "dfms", "HDRFA", "crypto2", "dplyr", "tidyr", "readr", "zoo"))