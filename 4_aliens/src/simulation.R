#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr','here','fs')
print(getwd())
workdir <- here("4_aliens")
cat("Workdir:", workdir)
setwd(workdir)
source("src/agent.R")
# setup dirs
data_dir <- here(workdir, "data")
dir_create(output_dir, recurse = TRUE)

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

generate_subjects_stimuli <- function(n_subjects, sessions=1){
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
    # Shuffle stimuli for this subject
    shuffled_stimuli <- stimuli %>% sample_frac(1)
    
    # Create 3 copies with session column
    session_stimuli <- lapply(1:sessions, function(session) {
      shuffled_stimuli %>%
        mutate(session = session, subject = subject_id,
               trial = seq(1:32))
    })
    
    # Combine all sessions for this subject
    bind_rows(session_stimuli)
  }
  
  # Generate stimuli for all subjects
  all_stimuli <- bind_rows(lapply(1:n_subjects, create_subject_stimuli))
  return(all_stimuli)
}

simulate_all_subjects <- function(
    all_stimuli,
    simconfig
) {
  
  n_subjects <- length(unique(all_stimuli$subject))
  results_list <- vector("list", n_subjects)
  
  # Helper:
  # - scalar -> reuse for everyone
  # - vector/list length n_subjects -> index by subject
  get_subject_param <- function(x, i, n_subjects, param_name) {
    
    # list input
    if (is.list(x)) {
      
      if (length(x) == 1) {
        return(x[[1]])
      }
      
      if (length(x) == n_subjects) {
        return(x[[i]])
      }
      
      stop(
        paste0(
          param_name,
          " must have length 1 or n_subjects"
        )
      )
    }
    
    # atomic vector / scalar input
    if (length(x) == 1) {
      return(x)
    }
    
    if (length(x) == n_subjects) {
      return(x[i])
    }
    
    stop(
      paste0(
        param_name,
        " must have length 1 or n_subjects"
      )
    )
  }
  
  for (i in seq_len(n_subjects)) {
    
    # subset subject data
    subject_stimuli <- all_stimuli %>%
      filter(subject == i)
    
    stimuli_matrix <- subject_stimuli %>%
      select(eyes, legs, colors, spots, arms) %>%
      as.matrix()
    
    cat_true_vec <- subject_stimuli %>%
      pull(dangerous)
    
    # subject-specific params
    init_sigma <- get_subject_param(
      simconfig$init_sigma,
      i,
      n_subjects,
      "init_sigma"
    )
    
    init_mu <- get_subject_param(
      simconfig$init_mu,
      i,
      n_subjects,
      "init_mu"
    )
    
    q_val <- get_subject_param(
      simconfig$q_val,
      i,
      n_subjects,
      "q_val"
    )
    
    r_val <- get_subject_param(
      simconfig$r_val,
      i,
      n_subjects,
      "r_val"
    )
    
    # run simulation
    sim_result <- prototype_agent(
      stimuli = stimuli_matrix,
      cat_true = cat_true_vec,
      init_sigma = init_sigma,
      init_mu = init_mu,
      q_val = q_val,
      r_val = r_val,
      seed = simconfig$seed
    )
    
    cat("Successfully ran sim for subject", i, "\n")
    
    combined_result <- cbind(
      subject_stimuli,
      as.data.frame(sim_result)
    ) %>%
      mutate(
        correct = as.integer(dangerous == sim_response),
        performance = cumsum(correct) / row_number(),
        
        # store actual values used
        init_sigma = init_sigma,
        init_mu = init_mu,
        q_val = q_val,
        r_val = r_val
      )
    
    results_list[[i]] <- combined_result
  }
  
  final_results <- bind_rows(results_list)
  
  return(final_results)
}

n_subjects <- 20
set.seed(212)
r_values <- rlnorm(n_subjects, -0.3, 0.6)
q_values <- rlnorm(n_subjects, -0.3, 0.6)


simulation_config <- list(
  r_value = r_values,
  q_value = q_values,
  init_mu = 2.5,
  init_sigma = 2.5,
  seed = 129
)

all_stimuli <- generate_subjects_stimuli(n_subjects)

results <- simulate_all_subjects(all_stimuli, simconfig = simulation_config)

# save as csv
filepath <- here(data_dir, "simdata.csv")
write_csv(results, filepath)
