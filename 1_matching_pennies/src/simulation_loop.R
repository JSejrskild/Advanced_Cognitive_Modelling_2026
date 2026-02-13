
print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse", "purrr")
# import internal
source("src/agents.R")

n_trials <- 120
n_agents <- 100

learningRate <- 0.1
noise <- 0


RL_vs_RL <- function(n_trials, learningRate, noise=0, initRateA=0.5, initRateB=0.5) {
  RLchoicesA <- rep(NA, n_trials)
  RLchoicesB <- rep(NA, n_trials)
  rateA <- rep(NA, n_trials)
  rateB <- rep(NA, n_trials)
  
  rateA[1] <- initRateA
  rateB[1] <- initRateB
  RLchoicesA[1] <- RandomAgent_f(1, rate = rateA[1])
  RLchoicesB[1] <- RandomAgent_f(1, rate = rateB[1])
  
  for (i in 2:n_trials){
    outputA <- RLAgent_f(rateA[i-1], learningRate=learningRate, feedback = RLchoicesB[i-1], noise)
    RLchoicesA[i] <- outputA$choice
    rateA[i] <- outputA$currentRate
    
    outputB <- RLAgent_f(rateB[i-1], learningRate=learningRate, feedback = RLchoicesA[i-1], noise)
    RLchoicesB[i] <- outputB$choice
    rateB[i] <- outputB$currentRate
  }
  
  temp <- tibble(
    trial = seq(n_trials), 
    choicesA = RLchoicesA, 
    choicesB = RLchoicesB, 
    rateA = rateA,
    rateB = rateB,
    learningRate = learningRate, 
    noise = noise
    )
  
  temp <- temp %>% 
    mutate(
      cumulativeRateA = cumsum(choicesA) / seq_along(choicesA),
      cumulativeRateB = cumsum(choicesB) / seq_along(choicesB)
    )
  
  return(temp)
}

result <- RL_vs_RL(n_trials = n_trials, learningRate = learningRate, initRateA = 0.5, initRateB = 0.5)

ggplot(result, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA, colour = "blue")) +
  geom_line(aes(y=cumulativeRateB, colour = "red")) + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic()
  

# -----------------------------------------------------
# ---- Loop across different parameter combinations ---
# -----------------------------------------------------

learningRateList <- seq(from = 0, to = 1, by = 0.1)
noiseList <- seq(from = 0, to = 1, by = 0.1)

combinations <- expand.grid(learningRate = learningRateList, noise = noiseList)

results <- combinations %>% 
  pmap_dfr(~ RL_vs_RL(
    n_trials = n_trials,
    learningRate = ..1,
    noise = ..2
  ))

unique(results$learningRate)
unique(results$noise)

ggplot(results, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA, color = "blue")) +
  geom_line(aes(y=cumulativeRateB, color = "red")) + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic() + 
  facet_grid(learningRate ~ noise)

ggplot(results, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA, color = noise)) +
  scale_color_gradient(low="green",high="red") + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic() + 
  facet_wrap(~learningRate)


# -------------------------------------------
# ---- Running Parallel across 100 agents ---
# -------------------------------------------


