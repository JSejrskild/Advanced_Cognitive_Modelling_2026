# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs", "cmdstanr",
               "posterior", "patchwork", "bayesplot", "priorsense", "glue")

# Set working dir
{
  print(paste0("Repository is currently opened in root dir: ", here()))
  repo_root <- "Advanced_Cognitive_Modelling_2026"
  target <- "4_aliens"
  
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

# set figures dir
figures_dir <- here(workdir, "figures")
cat("figure_dir", figures_dir)
dir_create(figures_dir)
output_dir <- here(workdir, "output")

sim_data_path <- here(workdir, "data", "simdata.csv")
sim_data <- read_csv(sim_data_path)

#Data list

id <- 10
pattern <- here(output_dir, "{id}_subjectsim_data_modelfit.rds")
filepath <- glue(pattern)
fit_object <- read_rds(filepath)
df <- as_draws_df(fit_object)
df$q_prior_pred

# 1. Posterior prediction 
all_plots <- list()
n_subjects <- 20
for(id in 1:n_subjects){
  
  filepattern <- here(output_dir, "{id}_subjectsim_data_modelfit.rds")
  subject_filepath <- glue(filepattern)
  
  cat("Running for:", subject_filepath, "\n")
  
  fit_object <- read_rds(subject_filepath)
  df <- as_draws_df(fit_object)
  
  posterior_r <- df$log_r
  posterior_q <- df$log_q
  
  prior_r <- df$r_prior_pred
  prior_q <- df$q_prior_pred
  
  true_r <- sim_data %>%
    filter(subject == id) %>%
    pull(r_val)
  
  true_q <- sim_data %>%
    filter(subject == id) %>%
    pull(q_val)
  
  # ----- R parameter plot -----
  r_df <- tibble(
    value = c(prior_r, posterior_r),
    type = c(
      rep("prior", length(prior_r)),
      rep("posterior", length(posterior_r))
    )
  )
  
  r_plot <- ggplot(r_df, aes(x = value, fill = type)) +
    geom_density(alpha = 0.4) +
    geom_vline(
      xintercept = true_r,
      linetype = "dashed",
      color = "black",
      alpha = 0.7
    ) +
    scale_fill_manual(values = c(
      "prior" = "#d73027",
      "posterior" = "#4575b4"
    )) +
    theme_bw() +
    labs(
      title = paste0("log_r (true = ", round(true_r, digits = 2),")"),
      x = "log_r",
      y = "Density",
      fill = ""
    )
  
  # ----- Q parameter plot -----
  q_df <- tibble(
    value = c(prior_q, posterior_q),
    type = c(
      rep("prior", length(prior_q)),
      rep("posterior", length(posterior_q))
    )
  )
  
  q_plot <- ggplot(q_df, aes(x = value, fill = type)) +
    geom_density(alpha = 0.4) +
    geom_vline(
      xintercept = true_q,
      linetype = "dashed",
      color = "black",
      alpha = 0.7
    ) +
    scale_fill_manual(values = c(
      "prior" = "#d73027",
      "posterior" = "#4575b4"
    )) +
    theme_bw() +
    labs(
      title = paste("log_q (true = ", round(true_q, digits = 2),")"),
      x = "log_q",
      y = "Density",
      fill = ""
    )
  
    # --- Combined plot ---
  subject_plot <- (r_plot + q_plot +
                     plot_layout(guides = "collect")) +
    theme(legend.position = "right") +
    plot_annotation(title = paste0("Subject ", id))
  
  all_plots[[id]] <- subject_plot
  
  ggsave(
    filename = here(figures_dir, paste0("prior_pred_sub-", id, ".png")),
    plot = all_plots[[id]],
    width = 10,
    height = 6
  )
}

final_plot <- wrap_plots(all_plots, ncol = 4) +
  plot_layout(guides = "collect")

ggsave(
  filename = here(figures_dir, "all_subjects_prior_posterior.pdf"),
  plot = final_plot,
  width = 24,
  height = 30
)