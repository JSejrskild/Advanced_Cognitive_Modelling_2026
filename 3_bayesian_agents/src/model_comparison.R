# load packages
pacman::p_load("tidyverse", "purrr", "parallel", "furrr", "future", "dplyr", "tidyr", "ggplot2", "here", "fs", "cmdstanr",
                "posterior", "patchwork", "bayesplot", "priorsense")
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

#Data list
fit_wba <- list(
  wba1 = readRDS("output/WBA_scenario1_modelfit.rds"),
  wba2 = readRDS("output/WBA_scenario2_modelfit.rds"),
  wba3 = readRDS("output/WBA_scenario3_modelfit.rds"),
  wba4 = readRDS("output/WBA_scenario4_modelfit.rds")
)

fit_pba <- list(
  pba1 = readRDS("output/PBA_scenario1_modelfit.rds"),
  pba2 = readRDS("output/PBA_scenario2_modelfit.rds"),
  pba3 = readRDS("output/PBA_scenario3_modelfit.rds"),
  pba4 = readRDS("output/PBA_scenario4_modelfit.rds")
)

df <- read.csv("data/results_df.csv")
stan_data_pba <- list(
  pba1 = df %>% filter(agent == "PBA", scenario == 1),
  pba2 = df %>% filter(agent == "PBA", scenario == 2),
  pba3 = df %>% filter(agent == "PBA", scenario == 3),
  pba4 = df %>% filter(agent == "PBA", scenario == 4)
)
stan_data_wba <- list(
  wba1 = df %>% filter(agent == "WBA", scenario == 1),
  wba2 = df %>% filter(agent == "WBA", scenario == 2),
  wba3 = df %>% filter(agent == "WBA", scenario == 3),
  wba4 = df %>% filter(agent == "WBA", scenario == 4)
)

# 1. Posterior prediction 





# 2. prior posterior updates
#PBA: parameter p
pba_prior <- as_draws_df(fit_pba$pba1$draws("p_prior")) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  dplyr::mutate(type = "prior")

pba_post <- as_draws_df(fit_pba$pba1$draws("p")) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  dplyr::mutate(type = "posterior")

pba_df <- bind_rows(pba_prior, pba_post)

pba_plot <- ggplot(pba_df, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(
    "prior" = "#d73027",
    "posterior" = "#4575b4"
  )) +
  theme_bw() +
  labs(
    title = "PBA: Prior vs Posterior",
    x = "p",
    y = "Density",
    fill = ""
  )

# WBA
wba_prior <- as_draws_df(fit_wba$wba1$draws(c("rho_prior", "kappa_prior"))) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(
    parameter = str_remove(parameter, "_prior"),
    type = "prior"
  )

wba_post <- as_draws_df(fit_wba$wba1$draws(c("rho", "kappa"))) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(type = "posterior")

wba_df <- bind_rows(wba_prior, wba_post) %>%
  filter(is.finite(value))

wba_plot <- ggplot(wba_df, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  
  facet_wrap(~parameter, scales = "free") +
  
  scale_fill_manual(values = c(
    "prior" = "#d73027",
    "posterior" = "#4575b4"
  )) +
  
  theme_bw() +
  labs(
    title = "WBA: Prior vs Posterior",
    x = "Parameter value",
    y = "Density",
    fill = ""
  )

# Save the plots
ggsave(
  filename = "figures/pba_prior_posterior.png",
  plot = pba_plot,
  width = 6,
  height = 4,
  dpi = 300
)
ggsave(
  filename = "figures/wba_prior_posterior.png",
  plot = wba_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# 3. prior sensitivity
sim_data <- read.csv("data/results_df.csv")

# Parameters per model
model_params <- list(
  WBA = c("rho", "kappa"),
  PBA = c("p")
)

scenarios <- unique(sim_data$scenario)

results <- imap(model_params, function(params, label) {
  map(scenarios, function(i) {
    
    modelfit_name <- paste0(label, "_scenario", i, "_modelfit.rds")
    modelfit_path <- here::here(target, "output", modelfit_name)
    
    if (!file.exists(modelfit_path)) {
      warning(paste0("Fit not found: ", modelfit_path))
      return(NULL)
    }
    
    fit <- readRDS(modelfit_path)
    
    # --- sensitivity ---
    sensitivity <- priorsense::powerscale_sensitivity(
      fit,
      variable = params
    )
    
    # --- plot ---
    ps <- powerscale_sequence(fit)
    
    p <- powerscale_plot_dens(ps, variables = params) +
      labs(
        title    = paste0("Prior sensitivity: ", label, " — Scenario ", i),
        subtitle = expression("Stronger deformation = greater dependence on the prior")
      ) +
      theme(
        strip.background = element_rect(fill = "grey95", colour = NA),
        strip.text       = element_text(face = "bold"),
        legend.position  = "bottom"
      )
    
    plot_name <- paste0("priorsens_", label, "_scenario", i, ".png")
    plot_path <- here::here(target, "figures", plot_name)
    
    ggsave(plot_path, plot = p, width = 8, height = 5, dpi = 300)
    message("Saved: ", plot_path)
    
    return(list(
      plot = p,
      sensitivity = sensitivity,
      model = label,
      scenario = i
    ))
  })
})


sens_table <- results |>
  flatten() |>
  compact() |>
  map_dfr(function(x) {
    as.data.frame(x$sensitivity) |>
      mutate(
        model = x$model,
        scenario = x$scenario
      )
  })

sens_table <- sens_table |>
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

tables_by_model <- sens_table |>
  group_split(model) |>
  setNames(unique(sens_table$model))

print(tables_by_model)