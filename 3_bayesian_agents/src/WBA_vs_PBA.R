# This is the simulation for 2 agents playing the experiment. 
# The two agents are: WBA and PBA

# The variables:
# ID: Participant ID
# 1_rating: The first rating the participant gives (0-8)
# g_rating: The group rating feedback given after 1st rating (0-8) 
# 2_rating: The second rating the participant gives (0-8) <- This is what we want to predict
# Feedback: How far is the g_rating from the 1_rating (-3, -2, 0, 2, 3)
# Change: How much did the 2_rating change from the 1_rating (-7 to 7) but probably in the middle 
# Condition: Pre or Peri depending on whether the data was aquired before or during the pandemic


print(getwd())
setwd('/work/ACM_2026/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models')
print(list.files("."))
# imports
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future")










