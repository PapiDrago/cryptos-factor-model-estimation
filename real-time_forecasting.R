# --- INITIAL SETUP ---
# data_log_returns: T x 5 matrix of raw log returns
# h: forecast horizon (e.g., 1)
# p_max: max lags for AR (e.g., 7)

run_stock_watson_forecast <- function(data_log_returns, h = 1, p_max = 7, k_max = 5) {
  
  T_total <- nrow(data_log_returns)
  N_assets <- ncol(data_log_returns)
  T1 <- floor(0.75 * T_total)
  
  k_max <- floor(8 * (min(N_assets, T_total) / 100)^(1/4))
  # Using Schwert's rule to compute the upper limit of number of common factors on which V(k) is computed when using ICr
  
  # Containers for results
  actuals <- c()
  forecasts_factor <- c()
  forecasts_ar <- c()
  
  # --- THE RECURSIVE LOOP ---
  for (t in T1:(T_total - h)) {
    
    # 1. SUBSET CURRENT VISIBLE DATA
    # We only use data up to index 't'
    D_train_t <- data_log_returns[1:t, ]
    
    # 2. STANDARDIZATION (For Factor Extraction Only)
    # We calculate mean/SD from training to avoid look-ahead bias
    train_means <- colMeans(D_train_t)
    train_sds <- apply(D_train_t, 2, sd) # Apply to all the cols the function sd from the package stats
    D_std_t <- scale(D_train_t, center = train_means, scale = train_sds)
    
    # 3. FACTOR EXTRACTION (PCA)
    ic <- ICr(D_std_t, k_max) # Estimating the number of common factors through eigenvalue thresholding
    # IC3 always overestimates the number of common factors
    # In general all of them overestimate
    # I noticed a discrepancy between professor's notes and IC expressions showed in the ICr (dfms) documentation
    estimated_factor_terms <- PCA(as.matrix(D_std_t), min(ic$r.star), constraint = "L")

    F_hat <- estimated_factor_terms$Fhat
    
    # 4. FACTOR MODEL OLS (Predicting Asset 1 as example)
    # Target (y): RAW log returns (unstandardized)
    # Features (X): Factors
    y_raw <- D_train_t[(1 + h):t, 1]
    X_factors <- F_hat[1:(t - h), , drop = FALSE] # "drop = FALSE" ensures that each row is treated as a data frame object
    
    model_factor <- lm(y_raw ~ X_factors) # A typical model has the form response ~ terms
    
    # Forecast Factor Model: Use F_hat at 'Today' (index t)
    latest_F <- as.data.frame(matrix(F_hat[t, ], nrow = 1)) # matrix creates a matrix from the given set of values
    colnames(latest_F) <- colnames(X_factors)
    pred_f <- predict(model_factor, newdata = latest_F)
    
    # 5. AR BENCHMARK MODEL (With BIC Selection)
    best_bic <- Inf
    best_p <- 0
    best_pred_ar <- 0
    
    # Fixed window start for fair BIC comparison
    obs_start <- p_max + 1
    
    for (p in 0:p_max) {
      # Prepare AR data
      y_ar <- D_train_t[(1 + h):t, 1] # It is redundant
      
      if (p == 0) {
        ar_mod <- lm(y_ar ~ 1) # ar_mod <- lm(D_train_t[(1 + h):t, 1] ~ 1)
      } else {
        # Create lag matrix
        lags_matrix <- embed(D_train_t[1:t, 1], p + 1) # If p = 0 it just returns D_train_t[1:t, 1]
        # Read the documentation at https://stat.ethz.ch/R-manual/R-devel/library/stats/html/embed.html
        # It is as if the headers of the p+1 columns were: lag_0,lag1, ..., lag_p-1, lag_p
        # For instance, considering AR(p), y_t+h+p will be regressed on the t-th row of lags_matrix considering cols from the second, corresponding to lag_1, to the last one
        y_shifted <- D_train_t[(p + h):t, 1]
        x_shifted <- lags_matrix[1:(nrow(lags_matrix) - h + 1), 2:(p + 1), drop = FALSE] # we could replace nrow(lags_matrix) with t
        # Please note that the last y to be predicted is y_t, which is regressed on y_t-h, y_t-h-1, ..., y_t-h-p+1
        # For this reason the last row to be included in x_shifted is the t-h+1^th because from the second column we
        # find the regressors required

        ar_mod <- lm(y_shifted ~ x_shifted)
      }
      
      # Calculate BIC on the slice [p_max to t-h]
      # We predict again on this slice to get a comparable RSS (same number of observations)
      slice_y <- D_train_t[(p_max + h):t, 1]
      resids <- residuals(ar_mod)
      # We take the last n_obs values of resids and compute RSS with them
      n_obs <- length(slice_y)
      rss <- sum(tail(resids, n_obs)^2)
      
      current_bic <- n_obs * log(rss / n_obs) + (p + 1) * log(n_obs) # Shouldn't be p+2 in order to count also the variance of AR irreducible error?
      
      if (current_bic < best_bic) {
        best_bic <- current_bic
        best_p <- p
        
        # Generate the forecast for this 'p'
        if (p == 0) {
          best_pred_ar <- predict(ar_mod, newdata = data.frame(1))[1] # data.frame(1) is just a 1 given to the model to let it predict the intercept
        } else {
          new_lags <- as.data.frame(matrix(D_train_t[(t - p + 1):t, 1], nrow = 1)) # Here we are transposing a column in a row
          # Reverse to match embed order if necessary, usually embed is [t, t-1, t-2]
          new_lags <- new_lags[, rev(seq_len(p)), drop = FALSE] # seq_len(p) generates [1, 2, ..., p], with rev() we reverse it, then we rearrange the order to imitate the order of lags_matrix cols (newest to oldest)
          colnames(new_lags) <- colnames(x_shifted)
          best_pred_ar <- predict(ar_mod, newdata = new_lags)
        }
      }
    }
    
    # STORE ALL RESULTS FOR THIS DAY
    actuals <- c(actuals, data_log_returns[t + h, 1])
    forecasts_factor <- c(forecasts_factor, pred_f)
    forecasts_ar <- c(forecasts_ar, best_pred_ar)
  }
  
  # --- FINAL EVALUATION ---
  mse_f <- mean((actuals - forecasts_factor)^2)
  mse_ar <- mean((actuals - forecasts_ar)^2)
  rel_mse <- mse_f / mse_ar
  
  return(list(
    Relative_MSE = rel_mse,
    Factor_MSE = mse_f,
    AR_MSE = mse_ar,
    Actuals = actuals,
    Forecasts_Factor = forecasts_factor
  ))
}

# Example usage:
# results <- run_stock_watson_forecast(my_crypto_data)
# print(results$Relative_MSE)