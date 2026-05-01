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

# === Plot Parameter Recovery ===
data_path <- here(workdir, "data/param_recov.csv")
data <- read_csv(data_path)

pba_plot <- data %>%
  filter(agent == "PBA") %>%
  ggplot(aes(x = p_true, y = p_hat)) +
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # True line (y = x)
  labs(x = "p_true", y = "p_hat", title = "Parameter Recovery: PBA") +
  ylim(0,1) +
  theme_minimal()

{
  p1 <- data %>%
    filter(agent == "WBA") %>%
    ggplot(aes(x = wd_true, y = wd_hat)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "wd_true", y = "wd_hat", title = "WBA: wd_true vs. wd_hat") +
    xlim(0,20) + 
    ylim(0,2.2) +
    theme_minimal()
  
  # Plot for ws_true vs. ws_hat
  p2 <- data %>%
    filter(agent == "WBA") %>%
    ggplot(aes(x = ws_true, y = ws_hat)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "ws_true", y = "ws_hat", title = "WBA: ws_true vs. ws_hat") +
    xlim(0,20) + 
    ylim(0,2.2) +
    theme_minimal()
  
  p3 <- data %>%
    filter(agent == "WBA") %>%
    ggplot(aes(x = rho_true, y = rho_hat)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "rho_true", y = "rho_hat", title = "WBA: rho_true vs. rho_hat") +
    xlim(0,1) + 
    ylim(0,1) +
    theme_minimal()
  
  p4 <- data %>%
    filter(agent == "WBA") %>%
    ggplot(aes(x = kappa_true, y = kappa_hat)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "kappa_true", y = "kappa_hat", title = "WBA: kappa_true vs. kappa_hat") +
    xlim(0,20) + 
    ylim(0,3.3) + 
    theme_minimal()
  
  # Combine plots side by side
  wba_plots <- (p1 + p2) / (p3 + p4)
}

figures_dir <- here(workdir, "figures")
ggsave(
  filename = here(figures_dir, paste0("PBA", "_parameter_recovery.png")),
  plot = pba_plot,
  width = 8,
  height = 5,
  units = "in",
  device = "png",
  create.dir = TRUE
)
ggsave(
  filename = here(figures_dir, paste0("WBA", "_parameter_recovery.png")),
  plot = wba_plots,
  width = 8,
  height = 5,
  units = "in",
  device = "png",
  create.dir = TRUE
)
