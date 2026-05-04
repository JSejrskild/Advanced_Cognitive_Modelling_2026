#

prototype_agent <- function(
    stimuli, # matrix: trials (rows) x features (cols)
    cat_true, # vector: true categories (0 or 1 : Friendly or Dangerous)
    init_sigma, # a single init constant
    init_mu, 
    q_val, 
    r_val,
    seed
  ){
  # ----
  # The prototype agent goes through a sequence of a learning loop for each trial.
  # 1. Add Q to both sigmas (variance/uncertainty for each prototype category)
  # 2. Decide: calculate choice
  # 3. Respond: Generate response
  # 4. Receive feedback: Observe true category (correct / uncorrect)
  # 5. Update: Apply the kalman gain to the correct "prototype" category
  
  # ----
  n_trials   <- nrow(stimuli)
  n_features <- ncol(stimuli)
  
  mu0_init <- rep(init_mu, n_features) # mu0 = Non-Dangerous
  mu1_init <- rep(init_sigma, n_features) # mu1 = Dangerous
  
  # setup parameter lists for each prototype category
  prototype_cat_0 <- list(mu = mu0_init, sigma = diag(init_sigma, n_features))
  prototype_cat_1 <- list(mu = mu1_init, sigma = diag(init_sigma, n_features))
  # create identity matrices
  r_matrix        <- diag(r_val, n_features)
  q_matrix        <- diag(q_val, n_features)
  
  response_probs <- numeric(n_trials) # empty vector?
  
  log_sum_exp <- function(v) {
    m <- max(v)
    m + log(sum(exp(v - m)))
  }
  
  for (i in seq_len(n_trials)) {
    cat("Trial", i, "\n")
    
    current_obs <- as.numeric(stimuli[i, ]) # take the current observation
    
    # === Prediction step ===
    # Reflects potential drift in category location since last trial.
    # When q_value = 0 this reduces to the static-target filter.
    prototype_cat_0$sigma <- prototype_cat_0$sigma + q_matrix
    prototype_cat_1$sigma <- prototype_cat_1$sigma + q_matrix
    
    # === Decision ===
    cov_cat_0 <- prototype_cat_0$sigma + r_matrix
    cov_cat_1 <- prototype_cat_1$sigma + r_matrix
    #
    
    # Try-Catch block -> Assigns -Inf to log_prob_X if dmvnorm fails
    log_prob_0 <- tryCatch(
      mvtnorm::dmvnorm(current_obs, mean = prototype_cat_0$mu,
                       sigma = cov_cat_0, log = TRUE),
      error = function(e) -Inf
    )
    log_prob_1 <- tryCatch(
      mvtnorm::dmvnorm(current_obs, mean = prototype_cat_1$mu,
                       sigma = cov_cat_1, log = TRUE),
      error = function(e) -Inf
    )
    
    if (!is.finite(log_prob_0) && !is.finite(log_prob_1)) {
      prob_cat_1 <- 0.5
    } else if (!is.finite(log_prob_0)) {
      prob_cat_1 <- 1.0
    } else if (!is.finite(log_prob_1)) {
      prob_cat_1 <- 0.0
    } else {
      # if log_probs are NOT -Inf
      # Compute the probability of prototype category 1
      prob_cat_1 <- exp(log_prob_1 - log_sum_exp(c(log_prob_0, log_prob_1)))
    }
    response_probs[i] <- pmax(1e-9, pmin(1 - 1e-9, prob_cat_1)) # Check to make sure prob is not 0 or 1
    
    # === Update (only update correct prototype)
    true_cat <- cat_true[i]
    
    if (true_cat == 1) {
      upd <- multivariate_kalman_update(prototype_cat_1$mu, prototype_cat_1$sigma,
                                        current_obs, r_matrix)
      prototype_cat_1$mu    <- upd$mu
      prototype_cat_1$sigma <- upd$sigma
    } else {
      upd <- multivariate_kalman_update(prototype_cat_0$mu, prototype_cat_0$sigma,
                                        current_obs, r_matrix)
      prototype_cat_0$mu    <- upd$mu
      prototype_cat_0$sigma <- upd$sigma
    }
  }
  
  tibble(
    prob_cat1 = response_probs,
    sim_response = rbinom(n_trials, 1, response_probs)
  )
}

# Function to make subject schedule

multivariate_kalman_update <- function(mu_prev,      # previous mean vector
                                       sigma_prev,    # previous covariance matrix
                                       observation,   # observed feature vector
                                       r_matrix) {    # observation noise matrix
  # -----
  # One update step of a multivariate Kalman filter.
  # Returns updated mean vector, covariance matrix, and Kalman gain.
  # -----
  
  mu_prev     <- as.numeric(mu_prev)
  observation <- as.numeric(observation)
  sigma_prev  <- as.matrix(sigma_prev)
  r_matrix    <- as.matrix(r_matrix)
  
  n_dim <- length(mu_prev)
  I     <- diag(n_dim)
  
  # Kalman gain: K = Sigma_prev * (Sigma_prev + R)^{-1}
  S     <- sigma_prev + r_matrix
  S_inv <- tryCatch(
    solve(S),
    error = function(e) {
      warning("Matrix inversion failed; using pseudo-inverse.", call. = FALSE)
      MASS::ginv(S)
    }
  )
  K <- sigma_prev %*% S_inv
  
  # Update mean
  innovation <- observation - mu_prev
  mu_new     <- as.numeric(mu_prev + K %*% innovation)
  
  # Update covariance — Joseph form for numerical stability
  IK        <- I - K
  sigma_new <- IK %*% sigma_prev %*% t(IK) + K %*% r_matrix %*% t(K)
  
  # Enforce symmetry (floating-point can introduce tiny asymmetry)
  sigma_new <- (sigma_new + t(sigma_new)) / 2
  
  list(mu = mu_new, sigma = sigma_new, k = K)
}