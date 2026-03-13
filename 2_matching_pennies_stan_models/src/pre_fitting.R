# set working dir
print(getwd())
target_dir <- "C:/Users/anelo/OneDrive/Documents/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models"
if (basename(getwd()) != "2_matching_pennies_stan_models") {
  setwd(target_dir)
}
setwd(target_dir)
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse", "cmdstanr", "here")

fpath <- "data/RL_vs_biased.csv"
simdata <- read_csv(fpath)

# Maybe choose some specific data or loop across it.
testdata <- simdata %>% 
  select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>% 
  filter(learningRate == 0.5, noise == 0)

inspect_data <- simdata %>% 
  select(agent_id, trial, choicesA, winB, learningRate, noise, winB, ) %>% 
  filter(learningRate == 0.5, noise == 0)

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

rlmodelpath <- "src/RL_model.stan"
print(rlmodelpath)
outputdir <- "output"
model_file <- here(outputdir, "RL_fit.rds")

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

# Save the fitted model object
#fit_rl$save_object(file= model_file)
#fit_rl$summary()
fit_rl$draws("alpha")|> posterior::summarise_draws()


