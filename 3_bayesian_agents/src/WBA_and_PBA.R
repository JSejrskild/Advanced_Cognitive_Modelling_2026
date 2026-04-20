# This is the simulation for 2 agents playing the experiment.
# The two agents are: WBA and PBA

# The variables:
# ID: Participant ID
# 1_rating: The first rating the participant gives (1-8)
# g_rating: The group rating feedback given after 1st rating (1-8) 
# 2_rating: The second rating the participant gives (1-8) <- This is what we want to predict
# Feedback: How far is the g_rating from the 1_rating (-3, -2, 0, 2, 3)
# Change: How much did the 2_rating change from the 1_rating (-7 to 7) but probably in the middle 
# Condition: Pre or Peri depending on whether the data was aquired before or during the pandemic

pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs")

print(here()) # work dir
new_wd <- here('3_bayesian_agents') # root/path
setwd(new_wd)
print(list.files("."))
# imports

source("src/agents.R")

# Set seed
set.seed(1702)

# Set global variable(s)
n_trials <- 80 # This is based on the data from cogsci-pre as they should match

# Simulation of the task - 5 scenarios
alpha <- c(1, 1, 1, 1, 1)   # kept neutral — alpha/beta only affect choice_1
beta  <- c(1, 1, 1, 1, 1)   
ws <- c(1, 10, 1,  10, 5)
wd <- c(1,  1, 10, 10, 1)

results_list <- list()

for(i in 1:5){
  
  # Ensure both models have the same 1st choice so they are comparable
  probability <- rbeta(n_trials, alpha[i], beta[i])
  choice_1_shared <- rbinom(n_trials, 7, probability) + 1
  
  # Run WBA
  wba <- WBA_agent_f(
    n_trials = n_trials,
    alpha = alpha[i],
    beta  = beta[i],
    ws    = ws[i],
    wd    = wd[i],
    choice_1 = choice_1_shared
  )
  
  wba$agent <- "WBA"
  wba$scenario <- i
  
  # Run PBA
  pba <- PBA_agent_f(
    n_trials = n_trials,
    alpha = alpha[i],
    beta  = beta[i],
    ws    = ws[i],
    wd    = wd[i],
    choice_1 = choice_1_shared
  )
  
  pba$agent <- "PBA"
  pba$scenario <- i
  
  # Store combined
  results_list[[i]] <- rbind(wba, pba)
}

# TODO:Should we save in a combined df or two seperate?
results_df <- do.call(rbind, results_list)

dir_create("data")
write_csv(results_df, "data/results_df.csv")

# ------------------------------------------
# Fitting the model in stan
# -








# Save the model fits



