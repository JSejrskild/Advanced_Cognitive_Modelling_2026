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
pacman::p_load("tidyverse", "cmdstanr", "here")

fpath <- "data/RL_vs_biased.csv"
simdata <- read_csv(fpath)

outputdir <- paste0(workdir,"/output")
model_file <- paste0(outputdir, "/rlmodel_fit.rds")

RUN_MODEL_FIT = TRUE # if false we read in previously saved model file

if (RUN_MODEL_FIT){
  # === SET INPUT DATA ===
  # Maybe choose some specific data or loop across it.
  testdata <- simdata %>% 
    select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>% 
    filter(learningRate == 0.6, noise == 0)
  
  inspect_data <- simdata %>% 
    select(agent_id, trial, choicesA, choicesB, learningRate, noise, winB, ) %>% 
    filter(learningRate == 0.6, noise == 0)
  
  initialV <- 0.5
  
  # setup stan data structure
  sdata <- list(
    t = length(testdata$choicesA),
    choice = testdata$choicesA,
    feedback = testdata$choicesB,
    initialV = initialV,
    alpha_prior_mu = 0,
    alpha_prior_sd = 1.5
  )
  
  # === FIT MODEL ===
  rlmodelpath <- "src/RL_model.stan"
  print(rlmodelpath)
  
  rlmodel <- cmdstan_model(rlmodelpath) # create the stan model object
  
  fit_rl <- rlmodel$sample( # set configuations
    data=sdata,
    seed=231,
    chains= 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    refresh = 500
  )
  
  # === INSPECT FIT ===
  
  fit_rl$summary("alpha") # check alpha posterior
  
  # === SAVE FIT ===
  
  # Save the fitted model object
  print(paste0("Saving model fit to: ", model_file))
  if (!dir.exists(dirname(model_file))) {
    dir.create(dirname(model_file), recursive = TRUE)
  }
  fit_rl$save_object(file=model_file)
  print("Saved model fit!")
} else {
  # === LOAD SAVED FIT ===
  fit_rl <- readRDS(model_file)
}

# --------------------------------------------

# === MCMC DIAGNOSITCS ===
fit_summary <- fit_rl$summary() # check full posterior summary (DOES NOT WORK, too much data?)
diagnostic_summary <- fit_rl$diagnostic_summary()
diagnostic_summary

# 1. Check for Divergent Transitions (Zero Tolerance)
divergences <- sum(diagnostic_summary$num_divergent)
if (divergences > 0) {
  stop(paste("CRITICAL FAILURE:", divergences, "divergences detected. The posterior geometry is pathological. Reparameterization required. Do not interpret these results."))
}

# 2. Check E-BFMI (Energy-Bayesian Fraction of Missing Information)
# Threshold is typically 0.3. Values below this indicate poor exploration.
min_ebfmi <- min(diagnostic_summary$ebfmi)
if (min_ebfmi < 0.3) {
  warning(paste("WARNING: Low E-BFMI detected (Min =", round(min_ebfmi, 3), 
                "). The sampler may struggle to explore the tails of the posterior."))
}

# 3. Check for Convergence (Rhat < 1.01)
max_rhat <- max(fit_summary$rhat, na.rm = TRUE)
if (max_rhat > 1.01) {
  stop(paste("CRITICAL FAILURE: Chains did not converge. Max Rhat =", round(max_rhat, 3)))
}

# 4. Check Effective Sample Size (ESS > 400 for both bulk and tail)
min_ess_bulk <- min(fit_summary$ess_bulk, na.rm = TRUE)
min_ess_tail <- min(fit_summary$ess_tail, na.rm = TRUE)
if (min_ess_bulk < 400 || min_ess_tail < 400) {
  warning("WARNING: Insufficient Effective Sample Size. Autocorrelation is too high. Run longer chains.")
}


