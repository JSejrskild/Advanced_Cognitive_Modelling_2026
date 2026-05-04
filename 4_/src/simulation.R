#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr')

#load the agent code
source('agent.R')

#define features
features <- c('eyes', 'legs', 'colors', 'spots', 'arms')

#create the stimuli

# generate the different possibilites

generate_stimuli <- function(){

stimuli <- expand.grid(rep(list(c(0, 1)), 5))

#give columns names

colnames(stimuli) <- features

# shuffle stimuli data

stimuli <- stimuli %>% sample_frac(1)

# add dangerous 
# Add column wih stim_number


stimuli <- stimuli %>%
  mutate(dangerous = ifelse(spots == 1 & eyes == 1, 1, 0),
         trial = seq(1:32))

return(stimuli)

}



#simulate agents

simulate_prototype_agent <- function(agent_id, r_value, q_value, stimuli){
  
  obs <- stimuli[features]
  cat_one <- stimuli$dangerous
  
  result <- prototype_kalman(
    r_value            = r_value,
    q_value            = q_value,
    obs                = obs,
    cat_one            = cat_one,
    initial_mu         = NULL,
    initial_sigma_diag = 10.0,
    quiet              = TRUE
  )
  
  stimuli |>
    mutate(
      agent_id      = agent_id,
      r_value_true  = r_value,
      q_value_true  = q_value,
      log_r_true    = log(r_value),
      log_q_true    = log(q_value),
      prob_danger   = result$prob_danger,
      sim_response  = result$sim_response,
      correct       = as.integer(dangerous == sim_response)
    ) |>
    group_by(agent_id) |>
    mutate(performance = cumsum(correct) / row_number()) |>
    ungroup()
}

simulate_many_agents <- function(parameter_values, seed){
  
  set.seed(seed)
  
  parameter_values <- parameter_values %>%
    mutate(agent_id = row_number())
  
  map_dfr(seq_len(nrow(parameter_values)), function(i){
    
    stimuli <- generate_stimuli()
    
    simulate_prototype_agent(
      agent_id = parameter_values$agent_id[i],
      r_value  = parameter_values$r_value[i],
      q_value  = parameter_values$q_value[i],
      stimuli  = stimuli
    )
  })
}


parameter_values <- tibble(
  r_value = c(0.5, 1, 1.5, 2),
  q_value = c(0.01, 0.1, 0.15, 0.2)
)

sim_data <- simulate_many_agents(
  parameter_values = parameter_values,
  seed = 2001)
