#Function for updating the protorype

multivariate_kalman_update <- function(mu_prev,      # previous mean vector
                                       sigma_prev,    # previous covariance matrix
                                       observation,   # observed feature vector
                                       r_matrix) {    # observation noise matrix
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

#function for simulating kalman agent
prototype_kalman <- function(r_value,
                             q_value,
                             obs,
                             cat_one,
                             initial_mu         = NULL,
                             initial_sigma_diag = 10.0,
                             quiet              = TRUE) {
  n_trials   <- nrow(obs)
  n_features <- ncol(obs)
  
  if (is.null(initial_mu)) {
    musafe_init <- rep(2.5, n_features)
    mudanger_init <- rep(2.5, n_features)
  } else {
    musafe_init <- initial_mu[[1]]
    mudanger_init <- initial_mu[[2]]
  }
  
  prototype_safe <- list(mu = musafe_init, sigma = diag(initial_sigma_diag, n_features))
  prototype_danger <- list(mu = mudanger_init, sigma = diag(initial_sigma_diag, n_features))
  r_matrix        <- diag(r_value, n_features)
  q_matrix        <- diag(q_value, n_features)
  
  response_probs <- numeric(n_trials)
  
  log_sum_exp <- function(v) {
    m <- max(v)
    m + log(sum(exp(v - m)))
  }
  
  for (i in seq_len(n_trials)) {
    if (!quiet && i %% 20 == 0) cat("Trial", i, "\n")
    
    current_obs <- as.numeric(obs[i, ])
    
    # ── Prediction step: add process noise to both prototypes ──────────────
    # Reflects potential drift in category location since last trial.
    # When q_value = 0 this reduces to the static-target filter.
    prototype_safe$sigma <- prototype_safe$sigma + q_matrix
    prototype_danger$sigma <- prototype_danger$sigma + q_matrix
    
    # ── Decision ─────────────────────────────────────────────────────────────
    cov_safe <- prototype_safe$sigma + r_matrix
    cov_danger <- prototype_danger$sigma + r_matrix
    
    log_prob_safe <- tryCatch(
      mvtnorm::dmvnorm(current_obs, mean = prototype_safe$mu,
                       sigma = cov_safe, log = TRUE),
      error = function(e) -Inf
    )
    log_prob_danger <- tryCatch(
      mvtnorm::dmvnorm(current_obs, mean = prototype_danger$mu,
                       sigma = cov_danger, log = TRUE),
      error = function(e) -Inf
    )
    
    if (!is.finite(log_prob_safe) && !is.finite(log_prob_danger)) {
      prob_cat_danger <- 0.5
    } else if (!is.finite(log_prob_safe)) {
      prob_cat_danger <- 1.0
    } else if (!is.finite(log_prob_danger)) {
      prob_cat_danger <- 0.0
    } else {
      prob_cat_danger <- exp(log_prob_danger - log_sum_exp(c(log_prob_safe, log_prob_danger)))
    }
    response_probs[i] <- pmax(1e-9, pmin(1 - 1e-9, prob_cat_danger))
    
    # ── Update (measurement update for the correct category only) ────────────
    true_cat <- cat_one[i]
    if (true_cat == 1) {
      upd <- multivariate_kalman_update(prototype_danger$mu, prototype_danger$sigma,
                                        current_obs, r_matrix)
      prototype_danger$mu    <- upd$mu
      prototype_danger$sigma <- upd$sigma
    } else {
      upd <- multivariate_kalman_update(prototype_safe$mu, prototype_safe$sigma,
                                        current_obs, r_matrix)
      prototype_safe$mu    <- upd$mu
      prototype_safe$sigma <- upd$sigma
    }
  }
  
  tibble(
    prob_danger    = response_probs,
    sim_response = rbinom(n_trials, 1, response_probs)
  )
}
