
RandomAgent_f <- function(n_trials, rate = 0.5, noise = 0, returnList = FALSE) {
  # Input validation
  if (!is.numeric(rate) || rate < 0 || rate > 1) {
    stop("Rate must be a probability between 0 and 1.")
  }
  if (!is.numeric(noise) || noise < 0 || noise > 1) {
    stop("Noise must be a probability between 0 and 1.")
  }
  
  # Generate base choices according to the rate
  choices <- rbinom(n_trials, size = 1, prob = rate)
  
  # Apply noise: identify trials where noise occurs
  noise_trials <- rbinom(n_trials, size = 1, prob = noise) == 1
  
  # Replace choices with random 50/50 on noise trials
  if (any(noise_trials)) {
    choices[noise_trials] <- rbinom(sum(noise_trials), size = 1, prob = 0.5)
  }
  
  if (returnList) {
    return(list(choice=choices))
  } else {
    return(choices)
  }
}

WSLSAgent_f <- function(prevChoice, feedback, noise = 0) {
  # Input validation
  if (!prevChoice %in% c(0, 1)) stop("Previous choice must be 0 or 1.")
  if (!feedback %in% c(0, 1)) stop("Feedback must be 0 or 1.")
  if (!is.numeric(noise) || noise < 0 || noise > 1) stop("Noise must be a probability between 0 and 1.")
  
  # Core WSLS logic:
  # If feedback is 1 (win), stay: choice = prevChoice
  # If feedback is 0 (loss), shift: choice = 1 - prevChoice
  choice <- ifelse(feedback == 1, prevChoice, 1 - prevChoice)
  
  # Apply noise if specified
  if (noise > 0 && runif(1) < noise) {
    # Override with a random 50/50 choice
    choice <- sample(c(0, 1), 1)
  }
  
  return(list(choice=choice))
}

RLAgent_f <- function(prevRate, learningRate, feedback, noise = 0) {
  # Input validation
  if (!is.numeric(prevRate) || prevRate < 0 || prevRate > 1) stop("Previous rate must be between 0 or 1.")
  if (!is.numeric(learningRate) || learningRate < 0 || learningRate > 1) stop("Learning rate must be between 0 or 1.")
  if (!feedback %in% c(0, 1)) stop("Feedback must be 0 or 1.")
  if (!is.numeric(noise) || noise < 0 || noise > 1) stop("Noise must be a probability between 0 and 1.")
  
  # RW Equation
  currentRate = prevRate + learningRate * (feedback - prevRate)
  
  # Choice function
  choice = rbinom(1, size = 1, prob = currentRate)
  
  
  # NOISE
  # Apply noise if specified
  if (noise > 0 && runif(1) < noise) {
    # Override with a random 50/50 choice
    choice <- sample(c(0, 1), 1)
  }
  
  return(list(choice = choice,
              currentRate = currentRate))
}

# Reinforcement Learning + Random Mixture model agent
RLRAgent_f <- function(
    prevRate,
    prevChoice,
    learningRate,
    feedback,
    winStreak = 0,
    lossStreak = 0,
    losing=FALSE,
    noise = 0,
    kSwitch = 3
) {
  # Input validation
  if (!is.numeric(prevRate) || prevRate < 0 || prevRate > 1) stop("Previous rate must be between 0 or 1.")
  if (!is.numeric(learningRate) || learningRate < 0 || learningRate > 1) stop("Learning rate must be between 0 or 1.")
  if (!feedback %in% c(0, 1)) stop("Feedback must be 0 or 1.")
  if (!is.numeric(noise) || noise < 0 || noise > 1) stop("Noise must be a probability between 0 and 1.")
  
  # Increase win/loss streak count
<<<<<<< HEAD
  if (feedback == feedback) {
=======
  if (feedback == 1) {
>>>>>>> origin/main
    winStreak <- winStreak + 1
    lossStreak <- 0
  } else {
    lossStreak <- lossStreak + 1
    winStreak <- 0
  }
  
  # Toggle/Untoggle 'losing' if either streak reaches kSwitch
  if ((losing && winStreak >= kSwitch) || (!losing && lossStreak >= kSwitch)) {
    losing <- !losing
  }
  
  # Call Random if in "losing" state else Reinforcement learning
  if (losing) {
    result <- RandomAgent_f(1, rate = 0.5, noise = noise, returnList=TRUE)
  } else {
    result <- RLAgent_f(prevRate, learningRate, feedback, noise)
  }
  
  # Extract results
  choice <- result$choice
  currentRate <- ifelse(losing, 0.5, result$currentRate)
  
  # Return the results
  return(list(
    choice = choice,
    currentRate = currentRate,
    winStreak = winStreak,
    lossStreak = lossStreak,
    losing = losing
  ))
}
