
# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs",
               "cmdstanr", "posterior")
# set working dir
print(here())
workdir <- here('3_bayesian_agents') # root/path
#setwd(workdir)
workdir <- '/work/JohanneSejrskildRejsenhus#9686/Advanced_Cognitive_Modelling_2026/3_bayesian_agents'
setwd(workdir)
print(list.files("."))
stan_dir <- file.path(workdir, "src")
output_dir <- file.path(workdir, "figures")

