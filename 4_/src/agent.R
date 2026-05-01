#prototype agent

prototype_agent <- function(id, stimuli, n_trials, init_sigma, init_mu, q_val, r_val, seed){
  schedule <- make_subject_schedule(stimuli, )
}




#function to make subject schedule

make_subject_schedule <- function(stimulus_info, n_blocks, seed) {
  set.seed(seed)
  n_stim <- nrow(stimulus_info)
  
  sequence <- unlist(lapply(seq_len(n_blocks), function(b) {
    sample(stimulus_info$stimulus, n_stim, replace = FALSE)
  }))
  
  tibble(
    trial_within_subject = seq_along(sequence),
    block                = rep(seq_len(n_blocks), each = n_stim),
    stimulus_id          = sequence
  ) |>
    left_join(stimulus_info, by = c("stimulus_id" = "stimulus")) |>
    rename(category_feedback = category_true) |>
    dplyr::select(trial_within_subject, block, stimulus_id,
                  height, position, category_feedback)
}






# One update step of a multivariate Kalman filter.
# Returns updated mean vector, covariance matrix, and Kalman gain.
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