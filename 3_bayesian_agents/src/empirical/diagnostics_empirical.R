# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs",
               "cmdstanr", "posterior", "patchwork", "bayesplot")
# Set working dir
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
# Internal imports
source("src/modules/diagnose.R")

color_scheme_set("viridis")

# === Validating Model Fit ===
output_dir <- here(workdir, "output")
print(output_dir)

# define a helper function
make_regex_pars <- function(bases, ids) {
  ids_regex <- paste(ids, collapse = "|")
  
  unlist(lapply(bases, function(b) {
    paste0(b, "\\[(", ids_regex, ")\\]")
  }))
}

make_slices <- function(ids, n_slices) {
  split(ids, cut(ids, breaks = n_slices, labels = FALSE))
}

# Define the function to validate model fit
validate_model_fit <- function(agent, output_dir, figures_dir) {
  
  modelfit_path <- here::here(output_dir, paste0(agent, "_precogsci_modelfit.rds"))
  print(paste0("Running model validation procedure for: ", modelfit_path))
  
  if (!file.exists(modelfit_path)) {
    warning(paste("File not found:", modelfit_path))
    return(NULL)
  }
  
  fit_object <- readRDS(modelfit_path)
  draws_df <- as_draws_df(fit_object$draws())
  
  #diagnose_fit(fit_object)
  
  make_slices <- function(ids, n_slices) {
    split(ids, cut(ids, breaks = n_slices, labels = FALSE))
  }
  
  # We create 3 
  subject_slices <- make_slices(1:45, 3)
  
  for (i in seq_along(subject_slices)) {
    print(paste("Creating trace plots for subjects:", i))
    
    subset_ids <- subject_slices[[i]]
    
    # Select the parameters to plot in trace plots
    if (agent == "PBA") {
      pars <- c("p")
    } else {
      pars <- c("rho", "kappa", "wd", "ws")
    }
    
    for (par in pars){
      regex_pars <- make_regex_pars(
        bases = par,
        ids = subset_ids
      )
      print(paste("This is the regex_pars:",regex_pars)) # DEBUG Statement
      
      trace_plot <- bayesplot::mcmc_trace(
        draws_df,
        regex_pars = regex_pars
      ) +
      ggplot2::labs(title = paste0(agent,"| Par: ", par, " | Subjects ", min(subset_ids), "-", max(subset_ids)))
      
      ggsave(
        filename = here::here(
          figures_dir,
          paste0(agent, "_par_", par, "_subjects_", min(subset_ids), "_to_", max(subset_ids), "_trace.png")
        ),
        plot = trace_plot,
        width = 8.27,
        height = 11.7,
        units = "in",
        device = "png",
        create.dir = TRUE
      )
    }
  }
}

figures_dir <- here(workdir, "figures") # figures dir

# List all files in the output_dir with full paths
agents <- c("PBA", "WBA")

for (agent in agents){
  validate_model_fit(agent, output_dir = output_dir, figures_dir = figures_dir)
}
