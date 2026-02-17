
print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# import internal
source("src/agents.R")
#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr')
# Detect cores
cores <- detectCores() # detect how many cpu cores
print(paste0("All CPU Cores: ", cores))
nWorkers <- cores - 2 # free two of them
# Set seed
set.seed(2001)

n_trials <- 120
n_agents <- 100

# build function

RL_vs_WSLS <- function(n_trials, learningRate, noise){
  
  self_choice <- rep(NA, n_trials)
  self_rate <- rep(NA, n_trials)
  other <- rep(NA, n_trials)
  win <- rep(NA, n_trials)
  
  
  self_rate[1] <- 0.5
  self_choice[1] <- rbinom(1, 1, self_rate[1])
  other[1] <- rbinom(1, 1, 0.5)
  
  for (i in 2:n_trials){
    # Get feedback for other (WSLS) win or lose
    if (other[i-1] == self_choice[i-1]){
      feedback = 0}
    else {feedback = 1}
    
    # Update choice for WSLS
    other[i] <- WSLSAgent_f(prevChoice = other[i-1], feedback = feedback, noise = noise)
    
    # Update choice and rate for RL
    choicenrate <- RLAgent_f(prevrate = self_rate[i-1], feedback = other[i-1], learningRate = learningRate, noise = noise)
    
    self_choice[i] <- choicenrate$choice
    self_rate[i] <- choicenrate$rate
    
    # Save wins for RL agent
    win[i-1] <- 1-feedback
  }
  
  # Save varialbes in tibble
  df <- tibble(self_choice, self_rate, other, trial = seq(n_trials), win)
  
  return(df)
}

# from chatten

# set future plan 

future::plan(future::multisession, workers = nWorkers)

simulate_RL_WSLS_parallel <- function(n_agents, n_trials){
  
  # Build testing grid
  grid <- tidyr::crossing(
    learningRate = seq(0, 1, 0.1),
    noise        = seq(0, 1, 0.1),
    agent        = seq_len(n_agents)
  )
  
  out <- furrr::future_pmap_dfr(
    grid,
    function(learningRate, noise, agent){
      
      temp <- RL_vs_WSLS(
        n_trials      = n_trials,
        learningRate  = learningRate,
        noise         = noise
      )
      
      dplyr::mutate(
        temp,
        agent = agent,
        learningRate = learningRate,
        noise = noise
      )
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  return(out)
}

#my own code, not parallel

simulate_RL_WSLS <- function(n_agents, n_trials){
  
  results <- list()
  counter <- 1
  
  for (learningRate in seq(0, 1, 0.1)){
    for (noise in seq(0, 1, 0.1)){
      
      for (agent in 1:n_agents){
        
        temp <- RL_vs_WSLS(
          n_trials = n_trials,
          learningRate = learningRate,
          noise = noise
        )
        
        # Save agent data
        temp$agent <- agent
        temp$learningRate <- learningRate
        temp$noise <- noise
        
        results[[counter]] <- temp
        counter <- counter + 1
      }
    }
  }
  
  # Combine all runs into one dataframe
  dplyr::bind_rows(results)
}







