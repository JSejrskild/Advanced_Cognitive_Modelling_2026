#import packages
# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs",
               "cmdstanr", "posterior")
print(getwd())
workdir <- here("4_aliens")
#workdir <- setwd("/work/JohanneSejrskildRejsenhus#9686/Advanced_Cognitive_Modelling_2026/4_aliens")
cat("Workdir:", workdir)
setwd(workdir)

# setup dirs
output_dir <- here(workdir, "output")
dir_create(output_dir, recurse = TRUE)

#Setup input data
sim_fpath <- "output/simdata.csv"
sim_data <- read_csv(sim_fpath)



# Setup stan data
setup_stan_data_prototype_sim <- function(df){
  
  observation <- as.matrix(df[, c(1, 4)])
  
  stan_data <- list(
    ntrials = nrow(observation),
    nfeatures = ncol(observation),
    
    cat_dangerous = df$correct,
    y = df$sim_response,
    
    obs = observation,
    
    initial_mu_cat0 = c(2.5, 2.5),
    initial_mu_cat1 = c(2.5, 2.5),
    
    initial_sigma_diag = 10.0,
    
    prior_logr_mean = 0,
    prior_logr_sd = 1,
    
    prior_logq_mean = 0,
    prior_logq_sd = 1
  )
  
  return(stan_data)
}

fit_model <- function(data, subject, data_type) {
  
  modelfit_name <- paste0(subject, "_subject", data_type, "_modelfit.rds")
  modelfit_path <- here(output_dir, modelfit_name)
  
  model_name <- paste0("prototype.stan")
  stanmodel_path <- here(workdir, "src", model_name)
  model <- cmdstan_model(stanmodel_path)
  
  stan_data <- if (data_type == "sim_data") {
    setup_stan_data_prototype_sim(data)
  } else if (data_type == "emp_data") {
    setup_stan_data_prototype_emp(data)
  } else {
    stop(paste0("Unknown data type: ", data_type))
  }
  
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


for (i in 1:length(unique(sim_data$subject))) {
  
  subject_data <- sim_data %>% filter(subject == i)
  
  fit_model(data = subject_data, subject = i, data_type = "sim_data")

}

  
print(unique(sim_data$subject))
print(length(unique(sim_data$subject)))
  