# set working dir
print(getwd())
target_dir <- "/Users/peli/Projects/Repositories/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models"
if (basename(getwd()) != "2_matching_pennies_stan_models") {
  setwd(target_dir)
}
setwd(target_dir)
print(getwd()) # !WD root should be 2_matching_pennies_stan_models
print(list.files("."))
workdir <- getwd()
# imports
pacman::p_load("tidyverse", "cmdstanr", "here", "posterior", "bayesplot", "patchwork", "future", "progressr")

fpath <- "data/RL_vs_biased.csv"
simdata <- read_csv(fpath)

outputdir <- paste0(workdir,"/output")
datadir <- paste0(workdir,"/data")
model_file <- paste0(outputdir, "/rlmodel_sense.rds")

rlmodelpath <- "src/RL_model.stan"
print(rlmodelpath)
rlmodel <- cmdstan_model(rlmodelpath) # create the stan model object

# create a grid of prior combinations
prior_grid <- tidyr::expand_grid(
  prior_mu = c(-1.5,0,1.5),
  prior_sd = seq(0.5, 4.0, length.out = 2),
  learning_rate = round(seq(0.1, .3, by = 0.1), digits = 1),
  agent_id = round(seq(1, 5, by = 1), digits = 0)
)

fit_prior_sense_procedure <- function(learning_rate, agent_id, prior_mu, prior_sd){
  # extract simulation dataframe
  data_subset <- simdata %>% 
    select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>% 
    filter(agent_id==agent_id, learningRate == learning_rate, noise == 0)
  
  initialV <- 0.5
  
  sdata <- list(
    t = nrow(data_subset),
    choice = data_subset$choicesA,
    feedback = data_subset$choicesB,
    initialV = initialV,
    alpha_prior_mu = prior_mu,
    alpha_prior_sd = prior_sd
  )
  
  fit_rl <- rlmodel$sample( #set configurations
    data=sdata,
    seed=231,
    chains= 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    refresh = 500
  )
  
  draws_df <- fit_rl$draws("alpha", format = "df")
  
  result <- tibble(
    learning_rate = learning_rate,
    agent_id = agent_id,
    alpha_post = draws_df$alpha,
    prior_mu = prior_mu,
    prior_sd = prior_sd
  )
  return(result)
}


future::plan(future::multisession, workers = 6) # set parrallel workers

with_progress({
  p <- progressor(along = 1:nrow(prior_grid))  # one step per row
  
  draws_list <- furrr::future_pmap(
    prior_grid,
    function(...) {
      p()  # update progress for this row
      fit_prior_sense_procedure(...)  # run your actual function
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
})

# Collapse the list of dataframes into a single tidy dataframe
sensitivity_draws <- dplyr::bind_rows(draws_list)

sum(is.na(sensitivity_draws))

sense_draws_path <- paste0(datadir, "/sensitivity_draws.rds")
print(sense_draws_path)
saveRDS(sensitivity_draws, file = sense_draws_path)

# Shut down the parallel backend to free up RAM
future::plan(future::sequential)


# --------------------------------------------
