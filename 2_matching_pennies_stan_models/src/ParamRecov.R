# WORKFLOW - Parameter recovery
# 0. Import packages etc.
# 1. Import our simulated data
# 2. Fit model to simulated data (many iterations)
# 3. Compare true vs. inferred parameters
# 4. Interpret results

# Packages
install.packages("pacman")
pacman::p_load(rstan, bayesplot, loo, posterior, brms, tidyverse, cmdstanr)

# Import simulated data
data <- read_csv("data/RL_vs_WSLS.csv")
print(colnames(data))



# 