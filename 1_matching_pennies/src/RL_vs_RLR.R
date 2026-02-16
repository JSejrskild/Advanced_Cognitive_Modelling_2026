print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse", "purrr", "ggnewscale")
# import internal
source("src/agents.R")

n_trials <- 120
n_agents <- 100

# RL vs RL-R
RL_vs_RLR <- function(n_trials, learningRate, noise=0, initRateA=0.5, kSwitch=3) {
  RLchoicesA <- rep(NA, n_trials)
  rateA <- rep(NA, n_trials)
  winStreakA <- rep(NA, n_trials)
  lossStreakA <- rep(NA, n_trials)
  losingA <- rep(NA, n_trials)
  
  
  RLchoicesB <- rep(NA, n_trials)
  
  # Initial Values
  ## RLR
  rateA[1] <- initRateA
  RLchoicesA[1] <- RandomAgent_f(1, rate = rateA[1])
  winStreakA[1] <- 0
  lossStreakA[1] <- 0
  losingA[1] <- FALSE
  ## WSLS
  RLchoicesB[1] <- RandomAgent_f(1, rate = 0.5)
  
  for (i in 2:n_trials){
    # RLR Agent A picks choice
    outputA <- RLRAgent_f(
      prevRate = rateA[i-1],
      prevChoice = RLchoicesA[i-1],
      learningRate=learningRate, 
      feedback = RLchoicesB[i-1], 
      noise = noise,
      kSwitch = kSwitch,
      winStreak = winStreakA[i-1],
      lossStreak = lossStreakA[i-1],
      losing = losingA[i-1]
    )
    
    RLchoicesA[i] <- outputA$choice
    rateA[i] <- outputA$currentRate
    winStreakA[i] <- outputA$winStreak
    lossStreakA[i] <- outputA$lossStreak
    losingA[i] <- outputA$losing
    
    # WSLS Agent B picks Choice
    outputB <- WSLSAgent_f(prevChoice = RLchoicesB[i-1], feedback = RLchoicesA[i-1], noise = noise)
    RLchoicesB[i] <- outputB$choice
  }
  
  temp <- tibble(
    trial = seq(n_trials), 
    choicesA = RLchoicesA, 
    choicesB = RLchoicesB,
    rateA = rateA,
    winStreakA = winStreakA,
    losingA = losingA,
    lossStreakA = lossStreakA,
    learningRate = learningRate, 
    noise = noise,
    initRateA = initRateA,
    
  )
  
  temp <- temp %>% 
    mutate(
      cumulativeRateA = cumsum(choicesA) / seq_along(choicesA),
      cumulativeRateB = cumsum(choicesB) / seq_along(choicesB)
    )
}

result <- RL_vs_RLR(
  n_trials = n_trials, 
  learningRate = 0.1, 
  noise = 0,
  initRateA = 0.5, 
  kSwitch=3
)

ggplot(result, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA), color="blue") +
  geom_line(aes(y=cumulativeRateB), color="red") + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic()
