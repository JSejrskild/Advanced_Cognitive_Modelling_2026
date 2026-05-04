library(ggplot2)
library(dplyr)
library(purrr)

# folder where plots should go
out_dir <- "figures"

scenarios <- unique(posterior_predictive_check$scenario)

#posterior plot per scenario

walk(scenarios, function(scen) {
  
  p <- posterior_predictive_check |>
    filter(scenario == scen) |>
    ggplot(aes(x = obs_mean, y = pred_mean, color = factor(group_rating))) +
    
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    
    facet_wrap(~ choice_1) +
    
    labs(
      title = paste("PPC:", scen),
      subtitle = "One plot for each value of choice_1",
      x = "Observed mean choice_2",
      y = "Predicted mean choice_2",
      color = "Group rating"
    ) +
    
    coord_equal(xlim = c(1, 8), ylim = c(1, 8)) +
    theme_bw()
  
  print(p)
  
  ggsave(
    filename = file.path(out_dir, paste0("ppc_", scen, ".png")),
    plot = p,
    width = 8,
    height = 6
  )
})

#posterior lot per agent type

posterior_predictive_check <- posterior_predictive_check |>
  mutate(agent_type = ifelse(grepl("^WBA", scenario), "WBA", "PBA"))

agent_groups <- c("WBA", "PBA")

walk(agent_groups, function(agent_now) {
  
  p <- posterior_predictive_check |>
    filter(agent_type == agent_now) |>
    ggplot(aes(
      x = obs_mean,
      y = pred_mean,
      color = factor(group_rating),
      shape = scenario
    )) +
    
    geom_point(size = 1.5) +
    
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    
    facet_wrap(~ choice_1) +
    
    labs(
      title = paste("PPC:", agent_now, "(all scenarios combined)"),
      subtitle = "Each point = one scenario × group_rating condition",
      x = "Observed mean choice_2",
      y = "Predicted mean choice_2",
      color = "Group rating",
      shape = "Scenario"
    ) +
    
    coord_equal(xlim = c(1, 8), ylim = c(1, 8)) +
    theme_bw()
  
  print(p)
  
  ggsave(
    filename = file.path(out_dir, paste0("ppc_", agent_now, "_combined.png")),
    plot = p,
    width = 10,
    height = 6
  )
})

# Summary table

ppc_summary_table <- posterior_predictive_check |>
  mutate(
    # 1. Coverage: is observed value inside interval?
    covered = obs_mean >= lower & obs_mean <= upper,
    
    # 2. Error (signed)
    error = pred_mean - obs_mean,
    
    # 3. Absolute error (usually more interpretable)
    abs_error = abs(error),
    
    # Credible interval
    credible_interval = upper - lower
  )

model_summary <- ppc_summary_table |>
  mutate(Model = ifelse(grepl("^WBA", scenario), "WBA", "PBA")) |>
  group_by(Model) |>
  summarise(
    coverage_rate = mean(covered),
    mean_abs_error = mean(abs_error),
    mean_error = mean(error),
    n = n(),
    credible_interval = mean(credible_interval),
  
    .groups = "drop"
  )

# Prior predictive plot

prior_predictive_check <- prior_predictive_check |>
  mutate(agent_type = ifelse(grepl("^WBA", scenario), "WBA", "PBA"))

agent_groups <- c("WBA", "PBA")

walk(agent_groups, function(agent_now) {
  
  p_prior <- prior_predictive_check |>
    filter(agent_type == agent_now) |>
    ggplot(aes(x = pred)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 60,
      fill = "#2166AC",
      alpha = 0.7
    ) +
    labs(
      title = paste("Predicted choice_2 values given priors:", agent_now),
      x = "Predicted choice_2",
      y = "Density"
    ) +
    theme_bw()
  
  print(p_prior)
  
  ggsave(
    filename = file.path(out_dir, paste0("prior_pred_", agent_now, ".png")),
    plot = p_prior,
    width = 10,
    height = 6
  )
})
