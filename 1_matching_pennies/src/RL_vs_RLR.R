print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future")
# import internal
source("src/agents.R")
# Detect cores
cores <- detectCores() # detect how many cpu cores
print(paste0("All CPU Cores: ", cores))
nWorkers <- cores - 2 # free two of them
# Set seed
set.seed(271)

n_trials <- 120
n_agents <- 100

# Build RL vs RL-Random ("Bad Loser") Competition
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
    kSwitch = kSwitch
  )
  
  temp <- temp %>% 
    mutate(
      cumulativeRateA = cumsum(choicesA) / seq_along(choicesA),
      cumulativeRateB = cumsum(choicesB) / seq_along(choicesB)
    )
}

# Run and return data for N Trials
result <- RL_vs_RLR(
  n_trials = n_trials, 
  learningRate = 0.1, 
  noise = 0,
  initRateA = 0.5, 
  kSwitch=3
)

# ------------------------------------
# - Loop over parameter combinations -
# ------------------------------------
learningRateList <- seq(from = 0, to = 1, by = 0.1)
noiseList <- seq(from = 0, to = 1, by = 0.1)
kSwitchList <- seq(from = 1, to = 5, by = 1)

combinations <- expand.grid(
  learningRate = learningRateList, 
  noise = noiseList, 
  kSwitchList = kSwitchList
)

results <- combinations %>% 
  pmap_dfr(~ RL_vs_RLR(
    n_trials = n_trials,
    learningRate = ..1,
    noise = ..2,
    initRateA = 0.5,
    kSwitch = ..3
  ))

#-----------------------------------
#- Run 100 agent pairs in parallel -
#-----------------------------------

plan(multisession, workers = nWorkers) # prepare parrallel processing

# Create a data frame with all combinations and agent IDs
agent_combinations <- combinations %>%
  slice(rep(row_number(), each = n_agents)) %>%
  mutate(agent_id = rep(1:n_agents, nrow(combinations)))

simulate_agent_pair <- function(learningRate, noise, kSwitch, agent_id) {
  RL_vs_RLR(
    n_trials = n_trials,
    learningRate = learningRate,
    noise = noise,
    initRateA = 0.5,
    kSwitch = kSwitch
  ) %>%
    mutate(agent_id = agent_id)
}

resultsPar <- agent_combinations %>%
  future_pmap_dfr(
    ~ simulate_agent_pair(
      learningRate = ..1,
      noise = ..2,
      kSwitch = ..3,
      agent_id = ..4
    ),
    .options = furrr_options(seed = TRUE)
  )

plan(sequential) # reverse to sequential processing

# Save results
filepath <- "data/RL_vs_RLR.csv"
write.csv(resultsPar, filepath)
