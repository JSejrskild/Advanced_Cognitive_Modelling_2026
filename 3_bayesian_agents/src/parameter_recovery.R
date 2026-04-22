
# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs",
               "cmdstanr", "posterior", "patchwork", "bayesplot", "progressr")
# set workdir
{
  print(paste0("Repository is currently opened in root dir: ", here()))
  repo_root <- "Advanced_Cognitive_Modelling_2026"
  target <- "3_bayesian_agents"
  
  if (grepl(paste0(repo_root, "$"), here::here())) {
    workdir <- here::here(target)  # root/path
  } else if (grepl(paste0(target, "$"), here::here())) {
    workdir <- here::here()
  } else {
    warning(paste("Please open the folder root in either the parent", repo_root, "OR", target))
  }
  setwd(workdir)
  print(list.files("."))
}

# === Parameter Recovery ===

data_path <- here(workdir, "data/results_recov.csv")
print(data_path)

sim_data <- read_csv(data_path)

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

max_posterior_density <- function(posterior) {
  # posterior: a numeric vector of posterior samples
  # Returns the value with the highest density (posterior mode)
  
  # Use density() to estimate the density
  dens <- density(posterior)
  
  # Find the x value with the highest density
  mode <- dens$x[which.max(dens$y)]
  
  return(mode)
}

fit_model <- function(full_df, model_label, i_sim){
  
  model_name <- paste0(model_label, ".stan")
  stanmodel_path <- here(workdir, "src", model_name)
  model <- cmdstan_model(stanmodel_path)
  
  data <- full_df %>% 
    filter(agent == model_label, sim_id == i_sim)
  
  print(paste("Length of subset data: ", nrow(data)))
  
  stan_data <- if (model_label == "WBA") {
    setup_stan_data_WBA(data)
  } else if (model_label == "PBA") {
    setup_stan_data_PBA(data)
  } else {
    stop(paste0("Unknown model: ", model_label))
  }
  
  # --- TRY BLOCK ---
  result <- tryCatch({
    
    model_fit <- model$sample(
      data = stan_data,  
      seed = 1702,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 2000,
      refresh = 500
    )
    
    draws_df <- as_draws_df(model_fit$draws())
    
    if (model_label == "WBA") {
      list(
        ws_hat = max_posterior_density(draws_df$ws),
        wd_hat = max_posterior_density(draws_df$wd),
        ws_true = data$ws[1],
        wd_true = data$wd[1],
        sim_id = data$sim_id[1],
        agent = model_label,
        success = TRUE
      )
    } else {
      ws_true <- data$ws[1]
      wd_true <- data$wd[1]
      p_true <- wd_true / (wd_true + ws_true)
      
      list(
        p_hat = max_posterior_density(draws_df$p),
        p_true = p_true,
        sim_id = data$sim_id[1],
        agent = model_label,
        success = TRUE
      )
    }
    
  }, error = function(e) {
    
    message(paste("Model failed for sim:", i_sim, "agent:", model_label))
    message(e$message)
    
    # --- RETURN NA STRUCTURE ---
    if (model_label == "WBA") {
      list(
        ws_hat = NA,
        wd_hat = NA,
        ws_true = data$ws[1],
        wd_true = data$wd[1],
        sim_id = i_sim,
        agent = model_label,
        success = FALSE
      )
    } else {
      ws_true <- data$ws[1]
      wd_true <- data$wd[1]
      p_true <- wd_true / (wd_true + ws_true)
      
      list(
        p_hat = NA,
        p_true = p_true,
        sim_id = i_sim,
        agent = model_label,
        success = FALSE
      )
    }
  })
  
  return(result)
}

subset <- sim_data %>% 
  filter(agent=="PBA",sim_id==9)

sim_fit <- fit_model(sim_data, "PBA", i_sim=9)

# Fit all simulated parameter combinations
agents <- c("PBA", "WBA")
n_sims <- length(unique(sim_data$sim_id))
grid <- expand.grid(
  sim_id = 1:n_sims,
  model_label = agents
)

# Run grid of model fits
results <- pmap_dfr(
  grid,
  ~ fit_model(
    full_df = sim_data,
    model_label = ..2,
    i_sim = ..1
  )
)

# save the parameter recovery
results_path <- here(workdir,"data/param_recov.csv")
write_csv(results, results_path)

# Run in Parallel (NOT Used but saved for later)
# nWorkers <- 2
# plan(multisession, workers = nWorkers) # prepare parrallel processing
# handlers(global = TRUE)
# with_progress({
#   
#   p <- progressor(steps = nrow(grid))
#   
#   par_results <- future_pmap(
#     grid,
#     function(sim_id, model_label) {
#       fit_model(sim_data, model_label, sim_id)
#     }
#   )
# })
# 
# plan(sequential)
