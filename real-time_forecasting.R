# --- INITIAL SETUP ---
# data_log_returns: T x N matrix of raw log returns
# h: forecast horizon (e.g., 1)
# p_max: max lags for AR (e.g., 7)

run_stock_watson_forecast <- function(data_log_returns, h = 1, p_max = 7, k_max = 5) {
  
  T_total <- nrow(data_log_returns)
  N_assets <- ncol(data_log_returns)
  T1 <- floor(0.75 * T_total)
  
  k_max <- floor(8 * (min(N_assets, T_total) / 100)^(1/4))
  # Using Schwert's rule to compute the upper limit of the number of common factors on which V(k) is computed when using ICr
  
  # Containers for results
  n_forecasts = T_total - (T1 + h) + 1 # it coincides with the number of iterations of the next for loop: at iteration t, we forecast y_t+h starting from t=T1
  # In actuals[1, i] there will be y_(T1+h)_i
  actuals <- matrix(NA, nrow = n_forecasts, ncol = N_assets)
  forecasts_factor <- matrix(NA, nrow = n_forecasts, ncol = N_assets)
  forecasts_ar <- matrix(NA, nrow = n_forecasts, ncol = N_assets)
  row_idx <- 1

  mse_f <- numeric(N_assets)
  mse_ar <- numeric(N_assets)
  rel_mse <- numeric(N_assets)
  
  # Real-time forecasting loop:
  # for each t we compute a forecast assuming not to have knowledge about the future (data)
  for (t in T1:(T_total - h)) {
    
    # We assume to have data up to index 't', we want to forecast y_t+h
    D_train_t <- data_log_returns[1:t, ]
    
    # In order to properly estimate the factors through PCA we need to standardize our data
    # We calculate mean/SD from training to avoid look-ahead bias
    train_means <- colMeans(D_train_t)
    train_sds <- apply(D_train_t, 2, sd) # Apply to all the cols the function sd from the package stats
    D_std_t <- scale(D_train_t, center = train_means, scale = train_sds)
    
    # We estimate the number of factors applying eigenvalue thresholding techinque
    ic <- ICr(D_std_t, k_max) # This functions minimizes 3 information criteria (IC) proposed by Bai and Ng (2002)
    # IC3 always overestimates the number of common factors. In general all of them overestimate
    # I noticed a discrepancy between professor's notes and IC expressions showed in the ICr (dfms) documentation
    estimated_factor_terms <- PCA(as.matrix(D_std_t), min(ic$r.star), constraint = "L") # We finally estimate the common factors.

    F_hat <- estimated_factor_terms$Fhat
    
    pred_f <- numeric(N_assets)
    best_pred_ar <- numeric(N_assets)
    
    for (i in 1:N_assets) {
      # We use OLS estimator to fit a linear model in order to forecast y_t+h
      y_raw <- D_train_t[(1 + h):t, i]
      X_factors <- F_hat[1:(t - h), , drop = FALSE] # "drop = FALSE" ensures that each row is treated as a data frame object instead of 'vector'
    
      model_factor <- lm(y_raw ~ X_factors) # A typical model has the form response ~ terms
    
      latest_F <- as.data.frame(matrix(F_hat[t, ], nrow = 1)) # matrix creates a matrix from the given set of values
      colnames(latest_F) <- colnames(X_factors)
      pred_f[i] <- predict(model_factor, newdata = latest_F) #y_t+h = beta_OLS * F_hat_t
    
      # We are going to compare our estimated factor model with an autoregressive model (AR)
      best_bic <- Inf
      best_p <- 0
      # We use Bayesian Information Criterion (BIC) to select the number of parameters of the AR model at time t
      # Fixed window start for fair BIC comparison
      obs_start <- p_max + h
    
      for (p in 0:p_max) {
      
        if (p == 0) {
            ar_mod <- lm(D_train_t[(1 + h):t, i] ~ 1)
        } else {
          # Create lag matrix
          lags_matrix <- embed(D_train_t[1:t, i], p + 1) # If p = 0 it just returns D_train_t[1:t, 1]
          # Read the documentation at https://stat.ethz.ch/R-manual/R-devel/library/stats/html/embed.html
          # It is as if the headers of the p+1 columns were: lag_0,lag1, ..., lag_p-1, lag_p
          # For instance, considering AR(p), y_t+h+p will be regressed on the t-th row of lags_matrix considering cols from the second, corresponding to lag_1, to the last one
          y_shifted <- D_train_t[(p + h):t, i]
          x_shifted <- lags_matrix[1:(nrow(lags_matrix) - h + 1), 2:(p + 1), drop = FALSE]
          # Please note that the last y to be predicted is y_t, which is regressed on y_t-h, y_t-h-1, ..., y_t-h-p+1
          # For this reason the last row to be included in x_shifted is the t-h+1^th because from the second column we
          # find the regressors required

          ar_mod <- lm(y_shifted ~ x_shifted)
        }
      
        # Calculate BIC on the slice [p_max to t-h]
        # We consider this slice to get a comparable RSS (same observations)
        slice_y <- D_train_t[obs_start:t, i]
        resids <- residuals(ar_mod)
        # We take the last n_obs values of resids and compute RSS with them
        n_obs <- length(slice_y)
        rss <- sum(tail(resids, n_obs)^2)
      
        current_bic <- n_obs * log(rss / n_obs) + (p + 1) * log(n_obs) # Theoretically should be p+2 in order to count also the variance of AR irreducible error
      
        if (current_bic < best_bic) {
          best_bic <- current_bic
          best_p <- p
        
          # Generate the forecast for this 'p'
          if (p == 0) {
            best_pred_ar[i] <- predict(ar_mod, newdata = data.frame(1))[1] # data.frame(1) is just a 1 given to the model to let it predict the intercept
          } else {
            new_lags <- as.data.frame(matrix(D_train_t[(t - p + 1):t, i], nrow = 1)) # Here we are transposing a column in a row
            new_lags <- new_lags[, rev(seq_len(p)), drop = FALSE]
            # seq_len(p) generates [1, 2, ..., p], with rev() we reverse it, then we rearrange the order to imitate the order of lags_matrix cols (newest to oldest)
            colnames(new_lags) <- colnames(x_shifted)
            best_pred_ar[i] <- predict(ar_mod, newdata = new_lags)
          }
        }
      }
    
      # We append the results computed at time t
      actuals[row_idx, i] <- data_log_returns[t + h, i]
      forecasts_factor[row_idx, i] <- pred_f[i]
      forecasts_ar[row_idx, i] <- best_pred_ar[i]

    }
    row_idx <- row_idx + 1
  }
  
  # Finally we compare the factor model with the AR model using relative MSE
  fmse_f <- colMeans((actuals - forecasts_factor)^2)
  mse_ar <- colMeans((actuals - forecasts_ar)^2)
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