pacman::p_load("tidyverse", "purrr", "patchwork", "ggplot2", "here", "fs", "bayesplot", "posterior")

workdir    <- setwd("/work/JohanneSejrskildRejsenhus#9686/Advanced_Cognitive_Modelling_2026/4_aliens")
output_dir <- here(workdir, "output")
plots_dir  <- here(workdir, "plots")

dir_create(plots_dir, recurse = TRUE)


load_subject_draws <- function(subject_id, data_type) {
  path <- here(output_dir, paste0(subject_id, "_subject", data_type, "_modelfit.rds"))
  if (!file.exists(path)) return(NULL)
  readRDS(path)$draws(c("log_r", "log_q"))
}


plot_traceplots <- function(subjects, data_type) {
  label          <- if (data_type == "sim_data") "Simulated" else "Empirical"
  grid_ncol      <- if (data_type == "sim_data") 4 else 5
  grid_nrow      <- if (data_type == "sim_data") 5 else 6
  individual_dir <- here(plots_dir, paste0("traceplots_individual_", data_type))
  dir_create(individual_dir, recurse = TRUE)
  
  walk(subjects, function(i) {
    fit <- readRDS(here(output_dir, paste0(i, "_subject", data_type, "_modelfit.rds")))
    p <- bayesplot::mcmc_trace(
      fit$draws(c("log_r", "log_q")),
      facet_args = list(ncol = 2, labeller = label_parsed)
    ) +
      ggtitle(paste0(label, " — Subject ", i)) +
      theme_minimal(base_size = 11)
    ggsave(here(individual_dir, paste0("subject_", i, "_traceplot.png")), p,
           width = 10, height = 4, dpi = 150)
  })
  
  all_draws <- map(subjects, load_subject_draws, data_type = data_type) %>% compact()
  
  subject_plots <- imap(all_draws, function(draws, idx) {
    bayesplot::mcmc_trace(
      draws,
      facet_args = list(ncol = 2, labeller = label_parsed)
    ) +
      ggtitle(paste0("Subject ", subjects[idx])) +
      theme_minimal(base_size = 7) +
      theme(plot.title = element_text(size = 7, face = "bold"))
  })
  
  combined <- wrap_plots(subject_plots, ncol = grid_ncol, nrow = grid_nrow) +
    plot_annotation(
      title    = paste0(label, " — Trace plots all subjects (log_r, log_q)"),
      subtitle = "Chains should look like hairy caterpillars",
      theme    = theme(plot.title = element_text(size = 14, face = "bold"))
    )
  
  ggsave(here(plots_dir, paste0("traceplots_combined_", data_type, ".png")),
         combined, width = 6 * grid_ncol, height = 4 * grid_nrow, dpi = 150, limitsize = FALSE)
}


sim_subjects <- list.files(output_dir, pattern = "_subjectsim_data_modelfit.rds") %>%
  sub("_subjectsim_data_modelfit.rds", "", .) %>%
  as.integer()

emp_subjects <- list.files(output_dir, pattern = "_subjectemp_data_modelfit.rds") %>%
  sub("_subjectemp_data_modelfit.rds", "", .) %>%
  as.integer()

plot_traceplots(subjects = sim_subjects, data_type = "sim_data")
plot_traceplots(subjects = emp_subjects, data_type = "emp_data")