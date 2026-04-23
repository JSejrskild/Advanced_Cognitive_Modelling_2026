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

# List all files in the output_dir with full paths
agents <- c("PBA", "WBA")
scenarios <- seq(1, 5)

# Create the agent-scenario combinatory grid
combinations <- expand.grid(
  agent = agents,
  scenario = scenarios
)

# Define the function to validate model fit
validate_model_fit <- function(agent, scenario, output_dir) {
  modelfit_path <- here::here(output_dir, sprintf("%s_scenario%d_modelfit.rds", agent, scenario))
  print(paste0("Running model validation procedure for: ", modelfit_path))
  
  if (!file.exists(modelfit_path)) {
    warning(paste("File not found:", modelfit_path))
    return(NULL)
  }
  
  fit_object <- readRDS(modelfit_path)
  draws_df <- as_draws_df(fit_object$draws())
  
  # Select parameters based on agent
  if (agent == "PBA") {
    pars <- "p"
  } else {
    pars <- c("rho", "kappa", "wd", "ws")
  }
  
  # Generate trace plot
  trace_plot <- bayesplot::mcmc_trace(
    draws_df,
    pars = pars,
    facet_args = list(ncol = 1, strip.position = "left")
  ) +
    labs(title = paste0(agent, ", Scenario ", scenario, ": ", paste(pars, collapse = ", ")))
  
  # Return a named list
  list(
    plot = trace_plot,
    agent = agent,
    scenario = scenario
  )
}

# Loop over combinations and validate each model fit
results <- combinations %>%
  pmap(~ validate_model_fit(..1, ..2, output_dir))

# Split results by agent
create_combined_trace_plot <- function(agent_tag, dir){
  "
  This function collects all trace plots for each agent and combines them into a wrapped plot and saves to /figures.
  "
  plots <- results[sapply(results, function(x) !is.null(x) && x$agent == agent_tag)] %>%
    map("plot")
  
  combined_plots <- patchwork::wrap_plots(plots, ncol = 2)
  
  ggsave(
    filename = here(figures_dir, paste(agent_tag, "_trace_plots.pdf")),
    plot = combined_plots,
    width = 8.27,
    height = 11.7,
    units = "in",
    device = "pdf",
    create.dir = TRUE
  )
}

figures_dir <- here(workdir, "figures") # figures dir

for (agent in agents){
  create_combined_trace_plot(agent,figures_dir)
}

