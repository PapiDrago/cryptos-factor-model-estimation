# Cryptocurrency Factor Models: Forecasting & Structural Validation

A real-time forecasting engine for cryptocurrency log-returns built on the **Stock & Watson (2002)** Diffusion Index framework, augmented with the **Trapani (2018)** Randomized Sequential Procedure to test whether a common factor structure actually exists in the data.

## Overview

Cryptocurrencies are highly volatile assets: prices can move abruptly, putting invested capital at risk. This project asks a focused question — **can a static factor model help forecast cryptocurrency log-returns, and is there a genuine common factor structure to exploit in the first place?**

The engine runs a recursive, out-of-sample comparison between two competing one-step-ahead forecasters:

1. **Static Factor Model** — extracts unobservable common factors via Principal Component Analysis (PCA) and forecasts each asset from them (Stock & Watson diffusion-index regression).
2. **Autoregressive (AR) benchmark** — a univariate model whose lag order is re-selected at every step via the Bayesian Information Criterion (BIC).

Predictive performance is compared through the **Relative MSE** of the factor model against the AR benchmark, and the existence of a factor structure is validated with the Trapani randomized test.

## How It Works

At each step of the real-time simulation, the engine behaves as if it were standing at time *t* trying to predict the next log-return, using only data observed up to *t*:

1. Estimate the number of factors $\hat{k}$ on the training window (Bai & Ng information criteria; search ceiling $k_{\max}$ set by Schwert's rule).
2. Extract the factors $\hat{F}_t$ by PCA on the standardized log-returns, with means and standard deviations computed from the training window **only**, to avoid look-ahead bias.
3. Fit a one-step-ahead OLS forecast per asset from $\hat{F}_t$, and a BIC-optimal AR forecast as benchmark.
4. Store both forecasts, expand the training window by one observation, and repeat.

Out-of-sample MSEs are accumulated across the test set and compared.

## Key Features

- **Trapani Randomized Test** — a statistical "gatekeeper" that tests, at level $\alpha = 0.01$, whether the leading eigenvalues genuinely diverge at rate $N$ (strong factors) rather than staying bounded (noise), guarding against factor overestimation by the information criteria.
- **Real-Time / Out-of-Sample Simulation** — every forecast uses only past information, and is compared against the truly held-out next observation; all training moments are computed in-window to prevent look-ahead bias.
- **Robust Data Cleaning** — volume-based filtering, stablecoin exclusion, and a missing-data threshold, following the spirit of the Bianchi & Babiak (2021) methodology.
- **Diagnostic Logging** — stores `k_trapani` and `p_history` at every iteration, so factor counts and AR lag orders can be tracked across time.

## Results

- **No forecasting edge.** Across both datasets the factor model's Relative MSE is $\approx 1$ — statistically indistinguishable from the AR benchmark. For 3 of 5 assets the BIC-optimal lag was $p = 0$, i.e. the best predictor was simply the historical mean.
- **Factor identification is highly $N$-dependent.** On the small panel ($N = 5$) the information criteria saturate the ceiling ($\hat{k} = 3 = k_{\max}$, a classic over-selection signature), but the Trapani test finds **no** diverging eigenvalue ($\hat{k} = 0$): no detectable common factor structure.
- **Structure emerges only with a wider cross-section.** On the expanded panel ($N = 29$) the Trapani test detects a single strong factor ($\hat{k} = 1$) — consistent with the theory, since factor strength is an $N \to \infty$ property — yet this still yields no predictive gain over the AR(0) benchmark.

## Project Structure

| File | Description |
|------|-------------|
| `script.R` | Entry point: data loading, exploratory analysis, and simulation driver. |
| `real-time_forecasting.R` | Core `run_stock_watson_forecast` function (recursive forecasting + benchmarking). |
| `trapani_factor_test.R` | Implementation of the Randomized Sequential Procedure. |
| `dataset1_cryptos.xlsx` | Small panel ($N = 5$) for initial testing. |
| `crypto_simplified_final_prices_100.csv` | Wider panel ($N = 29$ after cleaning) for capturing common factors. |

## Prerequisites

Tested with **R 4.5.3** and the following packages:

```r
install.packages(c(
  "readxl", "dfms", "HDRFA", "crypto2",
  "dplyr", "tidyr", "readr", "zoo"
))
```

| Package | Used for |
|---------|----------|
| `readxl`, `readr` | reading the `.xlsx` / `.csv` datasets |
| `dfms` | `ICr` — Bai & Ng information criteria for $\hat{k}$ |
| `HDRFA` | `PCA` — (robust) principal-component factor estimation |
| `crypto2` | downloading historical prices for the expanded panel |
| `dplyr`, `tidyr` | data wrangling / reshaping |
| `zoo` | handling missing values (`na.locf`) |

## Usage

From the repository root, in R:

```r
source("script.R")
```

`script.R` loads the data, runs the exploratory analysis, and calls `run_stock_watson_forecast()` on both panels, printing the Relative MSE, the AR lag history, and the Trapani factor-count history.

## References

- J. Bai and S. Ng (2002). *Determining the Number of Factors in Approximate Factor Models.* Econometrica, 70(1), 191–221.
- J. H. Stock and M. W. Watson (2002). *Forecasting Using Principal Components From a Large Number of Predictors.* Journal of the American Statistical Association, 97(460), 1167–1179.
- L. Trapani (2018). *A Randomized Sequential Procedure to Determine the Number of Factors.* Journal of the American Statistical Association, 113(523), 1341–1349.
- D. Bianchi and M. Babiak (2021). *A Factor Model for Cryptocurrency Returns.* CERGE-EI Working Paper Series 710.
