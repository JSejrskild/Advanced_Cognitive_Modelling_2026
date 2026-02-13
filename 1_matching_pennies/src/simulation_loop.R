
print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# import internal
source("src/agents.R")
#import packages
install.packages('pacman')
pacman::p_load('tidyverse')

n_trials <- 120
n_agents <- 100

self_choice <- rep(NA, n_trials)
self_rate <- rep(NA, n_trials)
other <- rep(NA, n_trials)
otherwin <- rep(NA, n_trials)


self_rate[1] <- 0.5
self_choice[1] <- rbinom(1, 1, self_rate[1])
other[1] <- rbinom(1, 1, 0.5)

for (i in 2:n_trials){
  # Get feedback for other (WSLS) win or lose
  if (other[i-1] == self_choice[i-1]){
    feedback = 0}
  else {feedback = 1}

    # Update choice for WSLS
  other[i] <- WSLSAgent_f(prevChoice = other[i-1], feedback = feedback, noise = 0)
  
    # Update choice and rate for RL
  choicenrate <- RLAgent_f(prevrate = self_rate[i-1], feedback = other[i-1], learningRate = 0.1, noise = 0)
  
  self_choice[i] <- choicenrate$choice
  self_rate[i] <- choicenrate$rate
  
  otherwin <- feedback
}

df <- tibble(self_choice, self_rate, other, trial = seq(n_trials), feedback)

