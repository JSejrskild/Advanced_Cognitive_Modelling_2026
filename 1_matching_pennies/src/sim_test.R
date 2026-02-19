
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
    noise = noise,
    initRateA = initRateA,
    initRateB = initRateB
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
initRateAList <- seq(from = 0, to = 0.5, by = 0.1)
initRateBList <- seq(from = 0, to = 0.5, by = 0.1)

combinations <- expand.grid(
  learningRate = learningRateList, 
  noise = noiseList, 
  initRateA = initRateAList, 
  initRateB = initRateBList
  )

results <- combinations %>% 
  pmap_dfr(~ RL_vs_RL(
    n_trials = n_trials,
    learningRate = ..1,
    noise = ..2,
    initRateA = ..3,
    initRateB = ..4
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

ggplot(results, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA), color = "blue", alpha = 0.2) +
  geom_line(aes(y=cumulativeRateB), color = "red", alpha=0.2) + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic() + 
  facet_wrap(~learningRate)

ggplot(results, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA), color = "blue", alpha = 0.2) +
  geom_line(aes(y=cumulativeRateB), color = "red", alpha=0.2) + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic() + 
  facet_wrap(~noise)

# Adding different initial starting rates

learningRateList <- seq(from = 0, to = 1, by = 0.2)
initRateAList <- seq(from = 0, to = 0.5, by = 0.1)
initRateBList <- seq(from = 0, to = 0.5, by = 0.1)

combinations <- expand.grid(
  learningRate = learningRateList,
  initRateA = initRateAList, 
  initRateB = initRateBList
)

results <- combinations %>% 
  pmap_dfr(~ RL_vs_RL(
    n_trials = n_trials,
    learningRate = ..1,
    noise = .1,
    initRateA = ..2,
    initRateB = ..3
  ))

ggplot(results, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA, color=learningRate), alpha=0.2) +
  scale_color_gradient(low="red", high="darkblue")+
  new_scale_color() +
  geom_line(aes(y=cumulativeRateB, color=learningRate), alpha=0.2) +
  scale_color_gradient(low="yellow", high="darkgreen") +
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic() + 
  facet_grid(initRateA ~ initRateB)

# --- Competition 2
# RL vs RL-R
RL_vs_RLR <- function(n_trials, learningRate, noise=0, initRateA=0.5, kSwitch=3) {
  RLchoicesA <- rep(NA, n_trials)
  rateA <- rep(NA, n_trials)
  winStreakA <- rep(NA, n_trials)
  lossStreakA <- rep(NA, n_trials)
  losingA <- rep(NA, n_trials)

  
  RLchoicesB <- rep(NA, n_trials)
  
  rateA[1] <- initRateA
  
  RLchoicesA[1] <- RandomAgent_f(1, rate = rateA[1])
  RLchoicesB[1] <- RandomAgent_f(1, rate = rateB[1])
  
  for (i in 2:n_trials){
    # RLR Agent A picks choice
    outputA <- RLRAgent_f(
      prevRate = rateA[i-1],
      prevChoice = RLchoicesA[i-1],
      learningRate=learningRate, 
      feedback = RLchoicesB[i-1], 
      noise = noise,
      kSwitch = kSwitch,
      winStreakA = winStreakA[i-1],
      lossStreakA = lossStreakA[i-1],
      losingA = losingA[i-1]
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
  learningRate = learningRate, 
  noise = 0,
  initRateA = 0.5, 
  kSwitch=3
  )

  
# -------------------------------------------
# ---- Running Parallel across 100 agents ---
# -------------------------------------------




