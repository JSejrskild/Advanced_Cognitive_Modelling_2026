# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs",
               "cmdstanr", "posterior")
# set working dir
print(here())
workdir <- here('3_bayesian_agents') # root/path
setwd(workdir)
#workdir <- setwd('/work/JohanneSejrskildRejsenhus#9686/Advanced_Cognitive_Modelling_2026/3_bayesian_agents')
print(list.files("."))

# === Setup input data and IO ===
datapath <- here(workdir, "data/cogsci_clean.csv")
data <- read_csv(datapath)
n_trials <- length(unique(data$trial))

# Setup functions
setup_stan_data <- function(df){
  
  n_trials <- length(df$ID[df$ID == 1])
  all_ids <- length(unique(df$ID))
  
  stan_data <- list(
    t = n_trials,
    subject_ids = all_ids,
    choice_1 = df$FirstRating,
    group_rating = df$GroupRating,
    choice_2 = df$SecondRating,
  )
  
  return(stan_data)
}

# MODEL FITTING
fit_model <- function(model_label, data){
  
  # Where to save model fit
  modelfit_name <- paste0(model_label, "_precogsci_modelfit.rds")
  modelfit_path <- here(output_dir, modelfit_name)
  
  model_name <- paste0(model_label, ".stan")
  stanmodel_path <- here(workdir, "src", model_name)
  model <- cmdstan_model(stanmodel_path)
  
  # Stan data — select based on model label
  # stan_data <- if (model_label == "WBA") {
  #   setup_stan_data_WBA(data)
  # } else if (model_label == "PBA") {
  #   setup_stan_data_PBA(data)
  # } else {
  #   stop(paste0("Unknown model: ", model_label))
  # }
  
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
    
  fit_model(model_label = label, data = scenario_data, i_scenario = i)
  
}