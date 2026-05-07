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
fit_pba <- readRDS('output/PBA_precogsci_modelfit.rds')
fit_wba <- readRDS('output/WBA_precogsci_modelfit.rds')

# 1. Posterior prediction 





# 2. prior posterior updates
#PBA: parameter p
pba_prior <- as_draws_df(fit_pba$draws("p_prior")) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    parameter = gsub("_prior", "", parameter),  # 👈 THIS is the fix
    type = "prior"
  )

pba_post <- as_draws_df(fit_pba$draws("p")) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  dplyr::mutate(type = "posterior")

pba_df <- bind_rows(pba_prior, pba_post)

#Subset
pba_subset <- pba_df %>%
  dplyr::mutate(id = as.integer(gsub("p\\[|\\]", "", parameter))) %>%
  dplyr::filter(id %in% c(1, 2, 3))  # 3 participants

#Plotting
pba_plot <- ggplot(pba_df, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~parameter, scales = "free") +
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

#Subset plotting
pba_subset_plot <- ggplot(pba_subset, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ id, scales = "free") +
  scale_fill_manual(values = c(
    "prior" = "#d73027",
    "posterior" = "#4575b4"
  )) +
  theme_bw() +
  labs(
    title = "PBA: Prior vs Posterior (3 participants)",
    x = "p",
    y = "Density",
    fill = ""
  )

# WBA
wba_prior <- as_draws_df(fit_wba$draws(c("rho_prior", "kappa_prior"))) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(
    parameter = str_remove(parameter, "_prior"),
    type = "prior"
  )

wba_post <- as_draws_df(fit_wba$draws(c("rho", "kappa"))) %>%
  dplyr::select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(type = "posterior")

wba_df <- bind_rows(wba_prior, wba_post) %>%
  filter(is.finite(value))

# Subset 
wba_subset <- wba_df %>%
  dplyr::mutate(
    param = gsub("\\[.*\\]", "", parameter),
    id = as.integer(gsub(".*\\[|\\]", "", parameter))
  ) %>%
  dplyr::filter(id %in% c(1, 2, 3))

# Plotting
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

# Subset plot
wba_subset_plot <- ggplot(wba_subset, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  facet_grid(param ~ id, scales = "free") +
  xlim(0,10) +
  scale_fill_manual(values = c(
    "prior" = "#d73027",
    "posterior" = "#4575b4"
  )) +
  theme_bw() +
  labs(
    title = "WBA: Prior vs Posterior (3 participants)",
    x = "Parameter value",
    y = "Density",
    fill = ""
  )

pba_subset_plot
wba_plot
# Save the plots
ggsave(
  filename = "figures/empirical_pba_prior_posterior.png",
  plot = pba_plot,
  width = 6,
  height = 4,
  dpi = 300
)
ggsave(
  filename = "figures/empirical_pba_prior_posterior.png",
  plot = pba_subset_plot,
  width = 6,
  height = 4,
  dpi = 300
)
ggsave(
  filename = "figures/empirical_wba_prior_posterior.png",
  plot = wba_plot,
  width = 6,
  height = 4,
  dpi = 300
)
ggsave(
  filename = "figures/empirical_wba_prior_posterior_subset.png",
  plot = wba_subset_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# 3. prior sensitivity
empirical_data <- read.csv("data/cogsci_clean.csv")


# PBA
pba_sens <- priorsense::powerscale_sensitivity(
  fit_pba,
  variable = "p"
)

pba_ps <- priorsense::powerscale_sequence(fit_pba)

pba_plot <- powerscale_plot_dens(pba_ps, variables = "p") +
  labs(
    title = "Prior sensitivity: PBA (empirical)",
    subtitle = "Stronger deformation = greater dependence on the prior"
  ) +
  theme_bw()

ggsave(
  filename = "figures/empirical_pba_prior_sense.png",
  plot = pba_plot,
  width = 6,
  height = 4,
  dpi = 300
)


#WBA
wba_sens <- priorsense::powerscale_sensitivity(
  fit_wba,
  variable = c("rho", "kappa")
)

wba_ps <- priorsense::powerscale_sequence(fit_wba)

wba_plot <- powerscale_plot_dens(wba_ps, variables = c("rho", "kappa")) +
  labs(
    title = "Prior sensitivity: WBA (empirical)",
    subtitle = "Stronger deformation = greater dependence on the prior"
  ) +
  theme_bw()

ggsave(
  filename = "figures/empirical_wba_prior_sense.png",
  plot = wba_plot,
  width = 6,
  height = 4,
  dpi = 300
)

#Table
sens_table <- bind_rows(
  as.data.frame(pba_sens) %>%
    mutate(model = "PBA"),
  as.data.frame(wba_sens) %>%
    mutate(model = "WBA")
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

