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
data_dir <- here(workdir, "data")
dir_create(figures_dir)
output_dir <- here(workdir, "output")

path <- here(output_dir, "26_subjectemp_data_modelfit.rds")
object <- read_rds(path)
draws <- as_draws_df(object)
hist(draws$log_q_prior)
hist(exp(draws$log_q))
hist(draws$log_r)

sim_data <- read_csv(here(data_dir, "simdata.csv"))
simdata$log_q
hist(simdata$r_val)

# 1. Posterior prediction 
prior_posterior_update_facet <- function(n_subjects, fit_object_tag){
  
  plot_data <- map_dfr(1:n_subjects, function(id) {
    
    filepattern <- here(output_dir, paste0("{id}_", fit_object_tag, "_data_modelfit.rds"))
    subject_filepath <- glue(filepattern)
    
    cat("Loading:", subject_filepath, "\n")
    
    # Try to load the file, return NULL if it fails
    tryCatch({
      fit_object <- readRDS(subject_filepath)
      df <- as_draws_df(fit_object)
    }, error = function(e) {
      message("Skipping ", subject_filepath, " due to error: ", e$message)
      return(NULL)  # Return NULL to skip this iteration
    })
    
    # If loading failed, return NULL
    if (is.null(fit_object) || is.null(df)) {
      return(NULL)
    }
    
    sim_sub <- sim_data %>% filter(subject == id)
    
    tibble(
      subject = id,
      
      # r
      r_prior = df$log_r_prior,
      r_post  = df$log_r,
      r_true  = sim_sub$log_r[1],
      
      # q
      q_prior = df$log_q_prior,
      q_post  = df$log_q,
      q_true  = sim_sub$log_q[1]
    )
  })
  
  r_long <- plot_data %>%
    select(subject, r_prior, r_post, r_true) %>%
    pivot_longer(cols = c(r_prior, r_post),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(parameter = "log_r",
           true = r_true)
  
  
  q_long <- plot_data %>%
    select(subject, q_prior, q_post, q_true) %>%
    pivot_longer(cols = c(q_prior, q_post),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(parameter = "log_q",
           true = q_true)
  
  plot_long <- bind_rows(r_long, q_long) %>%
    mutate(type = recode(type,
                         r_prior = "prior",
                         r_post  = "posterior",
                         q_prior = "prior",
                         q_post  = "posterior"))
  
  final_plot <- ggplot(plot_long, aes(x = value, fill = type)) +
    geom_density(alpha = 0.4) +
    geom_vline(aes(xintercept = true, linetype = "True Value"), color = "black") +
    facet_grid(rows = vars(subject), cols = vars(parameter), scales = "free_x") +
    scale_fill_manual(values = c(
      "prior" = "#d73027",
      "posterior" = "#4575b4"
    )) +
    scale_linetype_manual(
      name = "",          # Empty name to keep it clean, or use "Reference"
      values = c("True Value" = "dashed"),
      guide = guide_legend(override.aes = list(color = "black")) # Ensure line is black in legend
    ) +
    theme_bw() +
    labs(
      x = "value",
      y = "density",
      fill = ""
    )
  
  ggsave(
    filename = here(figures_dir, paste0(fit_object_tag, "_all_subjects_prior_posterior_faceted.png")),
    plot = final_plot,
    width = 10,
    height = 14
  )
}

prior_posterior_update_seperate <- function(n_subjects, fit_object_tag){
  
  plot_data <- map_dfr(1:n_subjects, function(id) {
    
    filepattern <- here(output_dir, paste0("{id}_", fit_object_tag, "_data_modelfit.rds"))
    subject_filepath <- glue(filepattern)
    
    cat("Loading:", subject_filepath, "\n")
    
    fit_object <- tryCatch({
      readRDS(subject_filepath)
    }, error = function(e) {
      message("Skipping ", subject_filepath, " due to error: ", e$message)
      return(NULL)
    })
    
    if (is.null(fit_object)) {
      return(NULL)
    }
    
    df <- as_draws_df(fit_object)
    
    sim_sub <- sim_data %>% filter(subject == id)
    
    tibble(
      subject = id,
      
      # r
      r_prior = df$log_r_prior,
      r_post  = df$log_r,
      r_true  = sim_sub$log_r[1],
      
      # q
      q_prior = df$log_q_prior,
      q_post  = df$log_q,
      q_true  = sim_sub$log_q[1]
    )
  })
  
  # ----- LOOP OVER SUBJECTS -----
  
  unique_subjects <- unique(plot_data$subject)
  
  for(subj in unique_subjects){
    
    subj_data <- plot_data %>%
      filter(subject == subj)
    
    # ----- r parameter -----
    
    r_long <- subj_data %>%
      select(r_prior, r_post, r_true) %>%
      pivot_longer(
        cols = c(r_prior, r_post),
        names_to = "type",
        values_to = "value"
      ) %>%
      mutate(
        type = recode(type,
                      r_prior = "prior",
                      r_post = "posterior")
      )
    
    r_plot <- ggplot(r_long, aes(x = value, fill = type)) +
      geom_density(alpha = 0.4) +
      geom_vline(
        xintercept = unique(subj_data$r_true),
        linetype = "dashed",
        color = "black"
      ) +
      scale_fill_manual(values = c(
        "prior" = "#d73027",
        "posterior" = "#4575b4"
      )) +
      theme_bw() +
      labs(
        title = paste("Subject", subj, "- log_r"),
        x = "value",
        y = "density",
        fill = ""
      )
    
    # ----- q parameter -----
    
    q_long <- subj_data %>%
      select(q_prior, q_post, q_true) %>%
      pivot_longer(
        cols = c(q_prior, q_post),
        names_to = "type",
        values_to = "value"
      ) %>%
      mutate(
        type = recode(type,
                      q_prior = "prior",
                      q_post = "posterior")
      )
    
    q_plot <- ggplot(q_long, aes(x = value, fill = type)) +
      geom_density(alpha = 0.4) +
      geom_vline(
        xintercept = unique(subj_data$q_true),
        linetype = "dashed",
        color = "black"
      ) +
      scale_fill_manual(values = c(
        "prior" = "#d73027",
        "posterior" = "#4575b4"
      )) +
      theme_bw() +
      labs(
        title = paste("Subject", subj, "- log_q"),
        x = "value",
        y = "density",
        fill = ""
      )
    
    # combined plots
    q_r_plot <- q_plot + r_plot
    
    ggsave(
      filename = here(
        figures_dir,
        paste0(fit_object_tag, "_subject_", subj, "_prior-posterior-update.png")
      ),
      plot = q_r_plot,
      width = 6,
      height = 4
    )
  }
}

# --- Run Prior Posterior Update ---
fit_tags <- c("subjectsim", "subjectemp")

for(tag in fit_tags){
  if(tag=="subjectsim"){
   data <- read_csv(here(data_dir, "simdata.csv"))
  } else if (tag=="subjectemp"){
    data <- read_csv(here(data_dir, "AlienData.csv"))
  }
  n_subjects <- length(unique(data$subject))
  prior_posterior_update_facet(n_subjects, tag)
  prior_posterior_update_seperate(n_subjects, tag)
}
