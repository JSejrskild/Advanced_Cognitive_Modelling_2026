# This is a script to preliminary plot the simulation of the 5 scenearios
# that we have simulated PBA ans WBA agents. 

print(getwd())
setwd('/work/ACM_2026/Advanced_Cognitive_Modelling_2026/3_bayesian_agents')
print(list.files("."))

# imports
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future",
               "dplyr", "tidyr", "ggplot2", "patchwork")

# Set seed
set.seed(1702)

results_df <- read.csv("data/results_df.csv")