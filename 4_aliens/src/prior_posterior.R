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
plot_data <- map_dfr(1:n_subjects, function(id) {
  
  filepattern <- here(output_dir, "{id}_subjectsim_data_modelfit.rds")
  subject_filepath <- glue(filepattern)
  
  cat("Loading:", subject_filepath, "\n")
  
  fit_object <- read_rds(subject_filepath)
  df <- as_draws_df(fit_object)
  
  sim_sub <- sim_data %>% filter(subject == id)
  
  tibble(
    subject = id,
    
    # r
    r_prior = df$r_prior_pred,
    r_post  = df$log_r,
    r_true  = log(sim_sub$r_val[1]),
    
    # q
    q_prior = df$q_prior_pred,
    q_post  = df$log_q,
    q_true  = log(sim_sub$q_val[1])
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
  filename = here(figures_dir, "all_subjects_prior_posterior_faceted.png"),
  plot = final_plot,
  width = 10,
  height = 14
)