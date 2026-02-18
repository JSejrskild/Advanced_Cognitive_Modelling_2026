print(getwd())
#setwd("../1_matching_pennies")
setwd("../")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future")
# import internal
source("src/agents.R")
# Detect cores
cores <- detectCores() # detect how many cpu cores
print(paste0("All CPU Cores: ", cores))
#nWorkers <- cores - 2 # free two of them
nWorkers <- 30 # free two of them
# Set seed
set.seed(271)

n_trials <- 120
n_agents <- 100

# Build RL vs WSLS
RL_vs_WSLS <- function(n_trials, learningRate, noise=0, initRateA=0.5) {
  RLchoicesA <- rep(NA, n_trials)
  rateA <- rep(NA, n_trials)
  winA <- rep(NA, n_trials)
  winB <- rep(NA, n_trials)
  
  
  RLchoicesB <- rep(NA, n_trials)
  
  # Initial Values
  ## RL
  rateA[1] <- initRateA
  RLchoicesA[1] <- RandomAgent_f(1, rate = rateA[1])
  ## WSLS
  RLchoicesB[1] <- RandomAgent_f(1, rate = 0.5)
  # decide on first wins/loss
  winA[1] <- ifelse(RLchoicesA[1]==RLchoicesB[1], 1, 0)
  winB[1] <- ifelse(RLchoicesA[1]==RLchoicesB[1], 0, 1)
  
  for (i in 2:n_trials){
    # RL Agent A picks choice
    outputA <- RLAgent_f(
      prevRate = rateA[i-1],
      learningRate=learningRate, 
      feedback = winA[i-1], 
      noise = noise
    )
    
    RLchoicesA[i] <- outputA$choice
    rateA[i] <- outputA$currentRate
  
    
    # WSLS Agent B picks Choice
    outputB <- WSLSAgent_f(prevChoice = RLchoicesB[i-1], feedback = winB[i-1], noise = 0)
    RLchoicesB[i] <- outputB$choice
    
    # decide win/loss
    winA[i] <- ifelse(RLchoicesA[i]==RLchoicesB[i], 1, 0)
    winB[i] <- ifelse(RLchoicesA[i]==RLchoicesB[i], 0, 1)
  }
  
  temp <- tibble(
    trial = seq(n_trials), 
    choicesA = RLchoicesA, 
    choicesB = RLchoicesB,
    winA = winA,
    winB = winB,
    rateA = rateA,
    learningRate = learningRate, 
    noise = noise,
    initRateA = initRateA
  )
  
  temp <- temp %>% 
    mutate(
      cumulativeRateA = cumsum(choicesA) / seq_along(choicesA),
      cumulativeRateB = cumsum(choicesB) / seq_along(choicesB), 
      cumulativeWinA = cumsum(winA) / seq_along(winA),
      cumulativeWinB = cumsum(winB) / seq_along(winB)
    )
}

# Run and return data for N Trials
result <- RL_vs_WSLS(
  n_trials = n_trials, 
  learningRate = 0.1, 
  noise = 0,
  initRateA = 0.5
)

# ------------------------------------
# - Loop over parameter combinations -
# ------------------------------------
learningRateList <- seq(from = 0, to = 1, by = 0.1)
noiseList <- seq(from = 0, to = 1, by = 0.1)

combinations <- expand.grid(
  learningRate = learningRateList, 
  noise = noiseList
)

results <- combinations %>% 
  pmap_dfr(~ RL_vs_WSLS(
    n_trials = n_trials,
    learningRate = ..1,
    noise = ..2,
    initRateA = 0.5
  ))

#-----------------------------------
#- Run 100 agent pairs in parallel -
#-----------------------------------

plan(multisession, workers = nWorkers) # prepare parrallel processing

# Create a data frame with all combinations and agent IDs
agent_combinations <- combinations %>%
  slice(rep(row_number(), each = n_agents)) %>%
  mutate(agent_id = rep(1:n_agents, nrow(combinations)))

simulate_agent_pair <- function(learningRate, noise, agent_id) {
  RL_vs_WSLS(
    n_trials = n_trials,
    learningRate = learningRate,
    noise = noise,
    initRateA = 0.5
  ) %>%
    mutate(agent_id = agent_id)
}

resultsPar <- agent_combinations %>%
  future_pmap_dfr(
    ~ simulate_agent_pair(
      learningRate = ..1,
      noise = ..2,
      agent_id = ..3
    ),
    .options = furrr_options(seed = TRUE)
  )

plan(sequential) # reverse to sequential processing


# Save results to csv
filepath <- "data/RL_vs_WSLS.csv"
write.csv(resultsPar, filepath)
