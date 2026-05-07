# Model Recovery

pacman::p_load(
  "tidyverse", "purrr", "parallel", "furrr", "future",
  "dplyr", "tidyr", "ggplot2", "here", "fs", "cmdstanr",
  "posterior", "patchwork", "bayesplot", "priorsense", "loo"
)

{
  print(paste0("Repository is currently opened in root dir: ", here()))
  repo_root <- "Advanced_Cognitive_Modelling_2026"
  target <- "3_bayesian_agents"
  
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

sim_data <- read.csv("data/results_df.csv")

true_models <- sim_data %>%
  distinct(scenario, agent) %>%
  rename(true_model = agent)

fits <- list(
  scen1 = list(
    PBA = readRDS("output/PBA_scenario1_modelfit.rds"),
    WBA = readRDS("output/WBA_scenario1_modelfit.rds")
  ),
  scen2 = list(
    PBA = readRDS("output/PBA_scenario2_modelfit.rds"),
    WBA = readRDS("output/WBA_scenario2_modelfit.rds")
  ),
  scen3 = list(
    PBA = readRDS("output/PBA_scenario3_modelfit.rds"),
    WBA = readRDS("output/WBA_scenario3_modelfit.rds")
  ),
  scen4 = list(
    PBA = readRDS("output/PBA_scenario4_modelfit.rds"),
    WBA = readRDS("output/WBA_scenario4_modelfit.rds")
  )
)

get_loo <- function(fit) {
  loo::loo(fit$draws("log_lik", format = "matrix"))
}

compare_scenario <- function(scenario_fits, label) {
  
  pba_loo <- get_loo(scenario_fits$PBA)
  wba_loo <- get_loo(scenario_fits$WBA)
  
  comp <- loo::loo_compare(pba_loo, wba_loo)
  
  weights <- loo::loo_model_weights(
    list(PBA = pba_loo, WBA = wba_loo),
    method = "stacking"
  )
  
  winner <- names(which.max(weights)) # Which model has the highes weight
  
  list(
    PBA = pba_loo,
    WBA = wba_loo,
    comparison = comp,
    weights = weights,
    winner = winner
  )
}

results <- imap(fits, compare_scenario)

recovery_table <- imap_dfr(results, function(res, scen) {
  tibble(
    scenario = scen,
    predicted_model = res$winner,
    PBA_weight = res$weights["PBA"],
    WBA_weight = res$weights["WBA"]
  )
})

recovery_table <- recovery_table %>%
  mutate(scenario = as.integer(gsub("scen", "", scenario)))

recovery_table <- recovery_table %>%
  left_join(true_models, by = "scenario")

recovery_accuracy <- mean(
  recovery_table$predicted_model == recovery_table$true_model
)

recovery_table
recovery_accuracy

table(
  True = recovery_table$true_model,
  Predicted = recovery_table$predicted_model
)

# pretty plot 

ml_scenario_summary <- recovery_table %>%
  select(scenario, PBA_weight, WBA_weight) %>%
  pivot_longer(
    cols = c(PBA_weight, WBA_weight),
    names_to = "Model",
    values_to = "weight"
  ) %>%
  mutate(
    Model = recode(Model,
                   PBA_weight = "PBA",
                   WBA_weight = "WBA"),
    Scenario = factor(scenario)
  )

# I want the scenarios to have better names for the plot 
ml_scenario_summary <- ml_scenario_summary %>%
  mutate(
    Scenario = factor(Scenario,
                      levels = c(1, 2, 3, 4),
                      labels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"))
  )

p_weight <- ggplot(ml_scenario_summary,
                   aes(x = Model, y = weight, fill = Model)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(aes(label = round(weight, 2)),
            vjust = -0.5, fontface = "bold") +
  facet_wrap(~ Scenario) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  labs(
    title = "Model Recovery via Stacking Weights",
    y = "Weight",
    x = NULL
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = "figures/model_recovery.png",
  plot = p_weight,
  width = 8,
  height = 6,
  dpi = 300
)