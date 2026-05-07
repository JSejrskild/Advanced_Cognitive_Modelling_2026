#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr','here','fs',
               'posterior','ggplot2','tibble','tidyr','readr','gtools')
print(getwd())
workdir <- here("4_aliens")
cat("Workdir:", workdir)
setwd(workdir)
source("src/simulation.R")
# setup dirs
output_dir <- here(workdir, "output")
dir_create(output_dir, recurse = TRUE)

#Load draws

fit_list <- list.files("output/")

sim_fit_list <- fit_list[grepl("^[0-9]+_subjectsim_data_modelfit\\.rds$", fit_list)] 
sim_fit_list <- mixedsort(sim_fit_list)

emp_fit_list <- fit_list[grepl("^[0-9]+_subjectemp_data_modelfit\\.rds$", fit_list)] 
emp_fit_list <- mixedsort(emp_fit_list)

sim_draws_list <- list()
emp_draws_list <- list()

for (file in sim_fit_list) {
  fit <- readRDS(file.path("output/",file))
  sim_draws_list[[file]] <- fit$draws(format = "df")
}

for (file in emp_fit_list) {
  fit <- readRDS(file.path("output/",file))
  emp_draws_list[[file]] <- fit$draws(format = "df")
}

#Compute choice from p

make_ppc_choices_emp <- function(draws_df) {
  
  p_cols <- paste0("p[", 1:104, "]")
  
  p_mat <- as.matrix(draws_df[, p_cols])
  
  choice_mat <- ifelse(p_mat < 0.5, 0, 1)
  
  colnames(choice_mat) <- paste0("choice[", 1:104, "]")
  
  cbind(
    draws_df[, c(".chain", ".iteration", ".draw")],
    as.data.frame(choice_mat)
  )
}

emp_ppc_list <- lapply(emp_draws_list, make_ppc_choices_emp)


make_ppc_choices_sim <- function(draws_df) {
  
  p_cols <- paste0("p[", 1:32, "]")
  
  p_mat <- as.matrix(draws_df[, p_cols])
  
  choice_mat <- ifelse(p_mat < 0.5, 0, 1)
  
  colnames(choice_mat) <- paste0("choice[", 1:32, "]")
  
  cbind(
    draws_df[, c(".chain", ".iteration", ".draw")],
    as.data.frame(choice_mat)
  )
}

sim_ppc_list <- lapply(sim_draws_list, make_ppc_choices_sim)


