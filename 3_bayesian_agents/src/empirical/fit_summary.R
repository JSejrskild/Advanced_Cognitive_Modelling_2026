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

output_dir <- here(workdir, "output")
print(output_dir)
figures_dir <- here(workdir, "figures")

summarise_fit <- function(agent){
  
  modelfit_path <- here::here(output_dir, paste0(agent, "_precogsci_modelfit.rds"))
  print(paste0("Summarising model fit for: ", modelfit_path))
  
  if (!file.exists(modelfit_path)) {
    warning(paste("File not found:", modelfit_path))
    return(NULL)
  }
  if (agent == "PBA") {
    pars <- c("p")
  } else {
    pars <- c("rho", "kappa", "wd", "ws")
  }
  
  fit_object <- readRDS(modelfit_path)
  
  draws_df <- fit_object$draws(
    format = "df",
    variables = pars
  )
  
  summary <- summarise_draws(draws_df) %>%
    mutate(
      param = sub("\\[.*", "", variable),
      index = as.integer(gsub(".*\\[|\\]", "", variable))
    )
  print(summary)
  
  return(summary)
}

par_summary_scatter <- function(summary, agent){
  
  ggplot(summary, aes(x = sd, y = mean, color = param)) +
    geom_point(alpha = 0.7) +
    facet_wrap(~param, scales = "free") +
    labs(
      title = paste0(agent, " | Parameter summaries"),
      x = "SD",
      y = "Mean"
    ) +
    theme_minimal()
}

par_summary_errors <- function(summary, agent){
  
  ggplot(summary, aes(x = index, y = mean, color = param, group = param)) +
    
    geom_errorbar(
      aes(ymin = mean - sd, ymax = mean + sd),
      position = position_jitterdodge(
        jitter.width = 0.2,
        dodge.width = 0.5
      ),
      alpha = 0.4,
      width = 0.2
    ) +
    
    geom_point(
      size = 1.5,
      alpha = 0.8,
      position = position_jitterdodge(
        jitter.width = 0.2,
        dodge.width = 0.5
      )
    ) +
    
    labs(
      title = paste0(agent, " | Parameter summaries"),
      x = "Index",
      y = "Posterior mean (± SD)"
    ) +
    
    theme_minimal() +
    theme(legend.position = "bottom")
}

create_summary <- function( plot_type=c("scatter","error_bar") ){
  plot_type <- match.arg(plot_type) # Check whether plot_type is one of the options
  
  agents <- c("PBA", "WBA")
  for(agent_label in agents){
    summary <- summarise_fit(agent = agent_label)
    
    if (plot_type=="scatter"){
      plot <- par_summary_scatter(summary, agent_label)
    }else{
      plot <- par_summary_errors(summary, agent_label)
    }
    
    plotname <- paste0(agent_label, "_param_summary_", plot_type, ".png")
    
    ggsave(
      filename = here::here(
        figures_dir,
        plotname
      ),
      plot = plot,
      width = 10,
      height = 8,
      units = "in",
      device = "png",
      create.dir = TRUE
    )
  }
}

create_summary(plot_type = "scatter")
create_summary(plot_type = "error_bar")