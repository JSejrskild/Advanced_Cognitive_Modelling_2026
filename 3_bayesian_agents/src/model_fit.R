
# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs",
               "cmdstanr", "posterior")
# set working dir
print(here())
workdir <- here('3_bayesian_agents') # root/path
#setwd(workdir)
workdir <- setwd('/work/JohanneSejrskildRejsenhus#9686/Advanced_Cognitive_Modelling_2026/3_bayesian_agents')
print(list.files("."))

# === Setup input data and IO ===
sim_fpath <- "data/results_df.csv"
sim_data <- read_csv(sim_fpath)

output_dir <- here(workdir, "output")
print(output_dir)

setup_stan_data_WBA <- function(df){
  
  stan_data <- list(
    t = length(df$trial),
    choice_1 = df$choice_1 - 1,
    group_rating = df$group_rating - 1,
    choice_2 = df$choice_2 - 1,
    rho_alpha = 2,
    rho_beta = 2,
    kappa_log_mu = 2,
    kappa_log_sd = 0.5
  )
  
  return(stan_data)
  
}

setup_stan_data_PBA <- function(df){
  
  stan_data <- list(
    t = length(df$trial),
    choice_1 = df$choice_1 - 1,
    group_rating = df$group_rating - 1,
    choice_2 = df$choice_2 - 1,
    p_alpha = 2,
    p_beta = 2
  )
  
  return(stan_data)
  
}

fit_model <- function(model_label, data, i_scenario){
  
  # Where to save model fit
  modelfit_name <- paste0(model_label,"_scenario", i_scenario, "_modelfit.rds")
  modelfit_path <- here(output_dir, modelfit_name)
  
  model_name <- paste0(model_label, ".stan")
  stanmodel_path <- here(workdir, "src", model_name)
  model <- cmdstan_model(stanmodel_path)
  
  # Stan data — select based on model label
  stan_data <- if (model_label == "WBA") {
    setup_stan_data_WBA(data)
  } else if (model_label == "PBA") {
    setup_stan_data_PBA(data)
  } else {
    stop(paste0("Unknown model: ", model_label))
  }
  
  # === Fit model ===
  model_fit <- model$sample(
    data = stan_data,  
    seed = 1702,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    refresh = 500
  )
  
  print(paste0("Saving model fit to: ", modelfit_path))
  if (!dir.exists(dirname(modelfit_path))) {
    dir.create(dirname(modelfit_path), recursive = TRUE)
  }
  model_fit$save_object(file = modelfit_path)
  print("Saved model fit!")
}

model_labels <- c("WBA","PBA")

for(label in model_labels){
  for( i in 1:length(unique(sim_data$scenario))){
    
    scenario_data <- sim_data %>% 
      filter(scenario==i, agent==label)
    
    fit_model(model_label = label, data = scenario_data, i_scenario = i)
  }
}