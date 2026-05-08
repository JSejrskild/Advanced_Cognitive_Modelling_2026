# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs", "cmdstanr",
               "posterior", "patchwork", "bayesplot", "priorsense", "glue")

# Set working dir
{
  print(paste0("Repository is currently opened in root dir: ", here()))
  repo_root <- "Advanced_Cognitive_Modelling_2026"
  target <- "4_aliens"
  
  if (grepl(paste0(repo_root, "$"), here::here())) {
    workdir <- here::here(target)
  } else if (grepl(paste0(target, "$"), here::here())) {
    workdir <- here::here()
  } else {
    warning(paste("Please open the folder root in either the parent", repo_root, "OR", target))
  }
  setwd(workdir)
  print(list.files("."))
}

# set dirs
figures_dir <- here(workdir, "figures")
data_dir    <- here(workdir, "data")
output_dir  <- here(workdir, "output")
dir_create(figures_dir)

sim_data <- read_csv(here(data_dir, "simdata.csv"))

# We want to plot bot log and true space
make_prior_posterior_plot <- function(plot_data, space, fit_object_tag, figures_dir) {
  
  if (space == "log") {
    pd <- plot_data %>%
      transmute(
        subject,
        r_prior = log(r_prior_nat),
        r_post  = r_post_log,
        r_true  = r_true_log,
        q_prior = log(q_prior_nat),
        q_post  = q_post_log,
        q_true  = q_true_log
      )
    space_label <- "log space"
  } else {
    pd <- plot_data %>%
      transmute(
        subject,
        r_prior = r_prior_nat,          # prior already in natural space
        r_post  = exp(r_post_log),
        r_true  = exp(r_true_log),
        q_prior = q_prior_nat,
        q_post  = exp(q_post_log),
        q_true  = exp(q_true_log)
      )
    space_label <- "natural space"
  }
  
  r_long <- pd %>%
    select(subject, r_prior, r_post, r_true) %>%
    pivot_longer(cols = c(r_prior, r_post), names_to = "type", values_to = "value") %>%
    mutate(parameter = if_else(space == "log", "log_r", "r"), true = r_true) %>%
    filter(!is.na(value))
  
  q_long <- pd %>%
    select(subject, q_prior, q_post, q_true) %>%
    pivot_longer(cols = c(q_prior, q_post), names_to = "type", values_to = "value") %>%
    mutate(parameter = if_else(space == "log", "log_q", "q"), true = q_true) %>%
    filter(!is.na(value))
  
  vline_data <- bind_rows(r_long, q_long) %>%
    distinct(subject, parameter, true) %>%
    filter(!is.na(true))
  
  plot_long <- bind_rows(r_long, q_long) %>%
    mutate(type = recode(type,
                         r_prior = "prior", r_post = "posterior",
                         q_prior = "prior", q_post = "posterior"))
  
  final_plot <- ggplot(plot_long, aes(x = value, fill = type)) +
    geom_density(alpha = 0.4) +
    facet_grid(rows = vars(subject), cols = vars(parameter), scales = "free") +
    scale_fill_manual(values = c("prior" = "#d73027", "posterior" = "#4575b4")) +
    theme_bw() +
    labs(title = paste(fit_object_tag, "—", space_label),
         x = "value", y = "density", fill = "")
  
  if (nrow(vline_data) > 0) {
    final_plot <- final_plot +
      geom_vline(data = vline_data,
                 aes(xintercept = true, linetype = "True Value"),
                 color = "black") +
      scale_linetype_manual(
        name = "",
        values = c("True Value" = "dashed"),
        guide = guide_legend(override.aes = list(color = "black"))
      )
  }
  
  ggsave(
    filename = here(figures_dir, paste0(fit_object_tag, "_prior_posterior_", space, ".png")),
    plot = final_plot, width = 10, height = 14
  )
}

prior_posterior_update <- function(n_subjects, fit_object_tag) {
  
  plot_data <- map_dfr(1:n_subjects, function(id) {
    
    subject_filepath <- glue(here(output_dir, "{id}_{fit_object_tag}_data_modelfit.rds"))
    cat("Loading:", subject_filepath, "\n")
    
    tryCatch({
      fit_object <- readRDS(subject_filepath)
      df <- as_draws_df(fit_object)
      
      if (fit_object_tag == "subjectsim") {
        sim_sub <- sim_data %>% filter(subject == id)
        r_true_log <- sim_sub$r_val[1]   # stored as log already
        q_true_log <- sim_sub$q_val[1]
      } else {
        r_true_log <- NA
        q_true_log <- NA
      }
      
      tibble(
        subject     = id,
        r_prior_nat = df$r_prior,      # exp(Normal) from Stan GQ
        r_post_log  = df$log_r,        # log space posterior
        r_true_log  = r_true_log,
        q_prior_nat = df$q_prior,
        q_post_log  = df$log_q,
        q_true_log  = q_true_log
      )
    }, error = function(e) {
      message("Skipping subject ", id, " — ", e$message)
      return(NULL)
    })
  })
  
  if (nrow(plot_data) == 0) {
    warning("No subjects loaded for tag: ", fit_object_tag)
    return(invisible(NULL))
  }
  
  make_prior_posterior_plot(plot_data, space = "log",  fit_object_tag, figures_dir)
  make_prior_posterior_plot(plot_data, space = "true", fit_object_tag, figures_dir)
}

# Plot
fit_tags <- c("subjectsim", "subjectemp")

for (tag in fit_tags) {
  if (tag == "subjectsim") {
    data <- read_csv(here(data_dir, "simdata.csv"))
  } else if (tag == "subjectemp") {
    data <- read_csv(here(data_dir, "AlienData.csv"))
  }
  n_subjects <- length(unique(data$subject))
  prior_posterior_update(n_subjects, tag)
}