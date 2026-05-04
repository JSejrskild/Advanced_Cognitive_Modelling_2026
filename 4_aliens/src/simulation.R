#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr')

#create the stimuli

#the columns we need:

# subject: 1-n
# condition: 2 (individuals not groups)
# session: 1 (for simplicity)
# trial: 1-32
# response: 0 or 1 (the guess)
# dangerous: 0 or 1 (this is the true label)
# correct: 0 or 1 (this is whether the guess was correct or not)
#stimuli: all features in one ( 5x 0 or 1's) 
# Eyes: 0 or 1
# legs: 0 or 1
# colors: 0 or 1
# spots: 0 or 1
# arms: 0 or 1

#the stimuli are conceptualized as 5 dimensional vectors of 0s and 1s (5 features, binary values)

#there are 32 possible stimuli, all 32 stimuli are presented in randomized order, 
#in three iterations (stimuli 1-32 in random order, stimuli 1-32 in a new random order, 
#stimuli 1-32 in a new random order).

# generate the different possibilites
n_subjects <- 20

# Generate all combinations
stimuli <- expand.grid(rep(list(c(0, 1)), 5))

#give columns names
colnames(stimuli) <- c("eyes", "legs", "colors", "spots", "arms")

# Add dangerous column
# Add column wih stim_number
stimuli <- stimuli %>%
  mutate(dangerous = ifelse(spots == 1 & eyes == 1, 1, 0),
         stimulus = seq(1:32))

# Function to create shuffled stimuli for one subject
create_subject_stimuli <- function(subject_id) {
  sessions <- 3
  # Shuffle stimuli for this subject
  shuffled_stimuli <- stimuli %>% sample_frac(1)
  
  # Create 3 copies with session column
  session_stimuli <- lapply(1:sessions, function(session) {
    shuffled_stimuli %>%
      mutate(session = session, subject = subject_id)
  })
  
  # Combine all sessions for this subject
  bind_rows(session_stimuli)
}

# Generate stimuli for all subjects
all_stimuli <- bind_rows(lapply(1:n_subjects, create_subject_stimuli))

# View the result
head(all_stimuli)
