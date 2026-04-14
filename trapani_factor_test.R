trapani_factor_test <- function(estimated_eigenvalues, N, T_total, u_vec, weights_vec, alpha = 0.05, R = 400) {
  # N = cross-sectional sample size, T_total = time sample size
  # estimated_eigenvalues = vector of eigenvalues
  # weights_vec contain the values of F(u)
  if (length(u_vec) != length(weights_vec)) {
    return(-1)
  }
  
  eigenvalues <- sort(estimated_eigenvalues, decreasing = TRUE)
  
  beta <- log(N) / log(T_total) 
  
  if (beta <= 0.5) {
    delta <- 0.01
  } else {
    delta <- 1.01 * (1 - 1 / (2 * beta)) 
  }
  
  if (N <= T_total) {
    avg_eigs = mean(eigenvalues)
    phi <- exp(N^(-delta) * (estimated_eigenvalues / avg_eigs))
  }
  
  estimated_k <- 0
  
  
  for (p in 0:((length(estimated_eigenvalues))-1)) {
    if (N > T_total) {
      eigs_sum = sum(eigenvalues[(p+1):length(eigenvalues)])
      avg_eigs = eigs_sum / N
      phi_p <- exp(N^(-delta) * (estimated_eigenvalues[p+1] / avg_eigs))
    } else {
      phi_p = phi[p+1]
    }
    # Generate artificial sample xi ~ i.i.d. N(0,1) (Step 1)
    xi <- rnorm(R, mean = 0, sd = 1)
    
    theta_squared_values <- sapply(u_vec, function(u) {
      
      zeta_seq <- as.numeric(sqrt(phi_p) * xi <= u)
      
      theta <- (2 / sqrt(R)) * sum(zeta_seq - 0.5)
      
      return(theta^2)
    })
    
    test_statistic <- sum(theta_squared_values * weights_vec)
    
    # Under H0, test_statistic follows a chi-squared distribution with df=1
    critical_value <- qchisq(1 - alpha, df = 1)
    
    if (test_statistic > critical_value) {
      # REJECT H0: The eigenvalue does not diverge
      break 
    } else {
      # FAIL TO REJECT H0: The eigenvalue diverges (a valid factor)
      estimated_k <- p+1
    }
  }
  
  return(estimated_k)
}