# This is the simulation for 2 agents playing the experiment.
# With an extensive GRID SEARCH of parameter values drawn across a range of values
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
#new_wd <- ("/work/JohanneSejrskildRejsenhus#9686/Advanced_Cognitive_Modelling_2026/3_bayesian_agents")
setwd(new_wd)
print(list.files("."))
# imports

source("src/agents.R")

# Set seed
set.seed(1702)

# Set global variable(s)
n_trials <- 80 # This is based on the data from cogsci-pre as they should match

# Instead of4 scenarios we draw each parameter value from distributions 
# and run this n times to see if we can recover different combinations 



results_list <- list()

alpha <- 1
beta <- 1
n_sims <- 100
ws <- runif(n_sims,min=0.1,max=20)
wd <- runif(n_sims,min=0.1,max=20)

for(i in 1:n_sims){
  # Ensure both models have the same 1st choice so they are comparable
  probability <- rbeta(n_trials, alpha, beta)
  choice_1_shared <- rbinom(n_trials, 7, probability) + 1
  
  # Run WBA
  wba <- WBA_agent_f(
    n_trials = n_trials,
    alpha = alpha,
    beta  = beta,
    ws    = ws[i],
    wd    = wd[i],
    choice_1 = choice_1_shared
  )
  
  wba$agent <- "WBA"
  wba$scenario <- i
  
  # Run PBA
  pba <- PBA_agent_f(
    n_trials = n_trials,
    alpha = alpha,
    beta  = beta,
    ws    = ws[i],
    wd    = wd[i],
    choice_1 = choice_1_shared
  )
  
  # Add a columns with the ws[i] abd wd[i] values repeated
  wba <- wba %>% 
    mutate(
      ws=ws[i],
      wd=wd[i],
      sim_id=i
    )
  pba <- pba %>% 
    mutate(
      ws=ws[i],
      wd=wd[i],
      sim_id=i
    )
  
  pba$agent <- "PBA"
  pba$scenario <- i
  
  # Store combined
  results_list[[i]] <- rbind(wba, pba)
}

# TODO:Should we save in a combined df or two seperate?
results_df <- do.call(rbind, results_list)

dir_create("data")
write_csv(results_df, "data/results_recov.csv")




