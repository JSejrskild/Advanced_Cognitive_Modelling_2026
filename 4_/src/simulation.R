#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr')

#create the stimuli

#the stimuli are conceptualized as 5 dimensional vectors of 0s and 1s (5 features, binary values)

#there are 32 possible stimuli, all 32 stimuli are presented in randomized order, 
#in three iterations (stimuli 1-32 in random order, stimuli 1-32 in a new random order, 
#stimuli 1-32 in a new random order).

#generate the different possibilites

# Generate all combinations

stimuli <- expand.grid(rep(list(c(0, 1)), 5))

#give columns names

colnames(stimuli) <- c("eyes", "legs", "colors", "spots", "arms")

# add dangerous 

stimuli <- stimuli %>%
  mutate(dangerous = ifelse(spots == 1 & eyes == 1, 1, 0))

# shuffle stimuli data

stimuli <- stimuli %>% sample_frac(1)
