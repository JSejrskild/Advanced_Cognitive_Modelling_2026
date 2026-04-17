# Agent for simulation WBA and PBA

WBA_agent_f <- function(n_trials, alpha, beta, ws, wd, choice_1){
  
  # Choice 1  - Beta-Binomial
  #probability <- rbeta(n_trials, alpha, beta)
  #choice_1    <- rbinom(n_trials, size = 7, prob = probability) + 1
  
  # Draw feedback (social/group rating)
  pool            <- init_feedback_pool(n_trials)
  feedback_result <- draw_feedback(choice_1, pool)
  group_rating    <- feedback_result$group_rating
  feedback_draw   <- feedback_result$feedback_draw
  
  # WBA equation
  alpha_post <- pmax(0.5 + wd * choice_1 + ws * group_rating, 1e-6)
  beta_post  <- pmax(0.5 + wd * (8 - choice_1) + ws * (8 - group_rating), 1e-6)
  
  
  # Choice 2 
  probability_post <- rbeta(n_trials, alpha_post, beta_post)
  choice_2         <- rbinom(n_trials, size = 7, prob = probability_post) + 1
  
  #Calculate change for df
  change <- choice_2 - choice_1
  
  # Return as dataframe - cause nice :)
  results <- data.frame(
    trial    = 1:n_trials,
    choice_1 = choice_1,
    group_rating = group_rating,
    choice_2 = choice_2,
    feedback = feedback_draw,
    change = change
  )
  
  return(results)
}


init_feedback_pool <- function(n_trials){ # Ensuring that the feedback is evenly distributed and only the experimental values
  values <- c(-3, -2, 0, 2, 3)
  pool   <- sample(rep(values, length.out = n_trials))
  return(pool)
}

draw_feedback <- function(choice_1_vec, pool){ #Using the pool of values to draw feedback for each trial
  
  n              <- length(choice_1_vec)
  feedback_draws <- numeric(n)
  feedback       <- choice_1_vec  # initialize to choice_1 so fallback is safe
  
  for(i in 1:n){ # Taking a viable feedback from the pool and removing it from the pool afterwards, if no viable left choose to give the same as choice_1
    choice_i <- choice_1_vec[i]
    valid_feedback_draw <- which(choice_i + pool >= 1 & choice_i + pool <= 8)
    
    if(length(valid_feedback_draw) == 0){
      feedback_draws[i] <- 0
      feedback[i]       <- choice_i
      
    } else {
      feedback_draw_idx <- valid_feedback_draw[sample(length(valid_feedback_draw), 1)]
      feedback_draws[i] <- pool[feedback_draw_idx]
      feedback[i]       <- choice_i + feedback_draws[i]
      pool              <- pool[-feedback_draw_idx]
    }
  }
  
  return(list(
    feedback_draw = feedback_draws,
    group_rating  = feedback,
    pool          = pool
  ))
}


PBA_agent_f <- function(n_trials, alpha, beta, ws, wd, choice_1){
    
    # Choice 1  - Beta-Binomial
    #probability <- rbeta(n_trials, alpha, beta)
    #choice_1    <- rbinom(n_trials, size = 7, prob = probability) + 1
    
    # Draw feedback (social/group rating)
    pool            <- init_feedback_pool(n_trials)
    feedback_result <- draw_feedback(choice_1, pool)
    group_rating    <- feedback_result$group_rating
    feedback_draw   <- feedback_result$feedback_draw
    
    # Ensure that the weights are proportional and therefor just one p 
    p = wd / (wd + ws)
    
    # PBA equation
    alpha_post <- pmax(0.5 + p * choice_1 + (1-p) * group_rating, 1e-6)
    beta_post  <- pmax(0.5 + p * (8 - choice_1) + (1-p) * (8 - group_rating), 1e-6)
    
    # Choice 2 
    probability_post <- rbeta(n_trials, alpha_post, beta_post)
    choice_2         <- rbinom(n_trials, size = 7, prob = probability_post) + 1
    
    #Calculate change for df
    change <- choice_2 - choice_1
    
    # Return as dataframe - cause nice :)
    results <- data.frame(
      trial    = 1:n_trials,
      choice_1 = choice_1,
      group_rating = group_rating,
      choice_2 = choice_2,
      feedback = feedback_draw,
      change = change
    )
    
    return(results)
  
}

