
print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse")
# import internal
source("src/agents.R")

n_trials <- 120
n_agents <- 100

learningRate <- 0.1
noise <- 0


RL_vs_RL <- function(n_trials, learning_rate, noise=0, initRateA, initRateB) {
  RLchoicesA <- rep(NA, n_trials)
  RLchoicesB <- rep(NA, n_trials)
  rateA <- rep(NA, n_trials)
  rateB <- rep(NA, n_trials)
  
  rateA[1] <- initRateA
  rateB[1] <- initRateB
  RLchoicesA[1] <- RandomAgent_f(1, rate = rateA[1])
  RLchoicesB[1] <- RandomAgent_f(1, rate = rateB[1])
  
  for (i in 2:n_trials){
    outputA <- RLAgent_f(rateA[i-1], learningRate, feedback = RLchoicesB[i-1], noise)
    RLchoicesA[i] <- outputA$choice
    rateA[i] <- outputA$currentRate
    
    outputB <- RLAgent_f(rateB[i-1], learningRate, feedback = RLchoicesA[i-1], noise)
    RLchoicesB[i] <- outputB$choice
    rateB[i] <- outputB$currentRate
  }
  
  temp <- tibble(
    trial = seq(n_trials), 
    choiceA = RLchoicesA, 
    choicesB = RLchoicesB, 
    rateA = rateA,
    rateB = rateB,
    learningRate, 
    noise
    )
}

RL_vs_RL(n_trials = n_trials, learning_rate = learningRate, initRateA = 0.5, initRateB = 0.5)


