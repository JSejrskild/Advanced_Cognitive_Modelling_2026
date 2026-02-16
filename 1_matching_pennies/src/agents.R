
RandomAgent_f <- function(n_trials, rate = 0.5, noise = 0) {
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
  
  return(choices)
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
  
  return(choice)
}

RLAgent_f <- function(prevRate, learningRate, feedback, noise = 0) {
  # Input validation
  if (!is.numeric(prevRate) || prevRate < 0 || prevRate > 1) stop("Previous rate must be 0 or 1.")
  if (!is.numeric(learningRate) || learningRate < 0 || learningRate > 1) stop("Learning rate must be 0 or 1.")
  if (!feedback %in% c(0, 1)) stop("Feedback must be 0 or 1.")
  if (!is.numeric(noise) || noise < 0 || noise > 1) stop("Noise must be a probability between 0 and 1.")
  
  # RW Equation
  currentRate = prevRate + learningRate * ( feedback - prevRate)
  
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

BadSportAgent_f <- function() {
  
}

