library(ggplot2)
library(dplyr)
library(purrr)

# folder where plots should go
out_dir <- "figures"

#posterior plot per subject

#posterior plot per choice one for all pba


pba1 <- posterior_predictive_check_emp_pba |>
  ggplot(aes(x = obs_mean, y = pred_mean, color = factor(feedback))) +
  
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, linetype = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  facet_wrap(~ choice_1) +
  
  labs(
    title = paste("Posterior predictive check for PBA model"),
    subtitle = "One plot for each value of choice_1",
    x = "Observed mean choice_2",
    y = "Predicted mean choice_2",
    color = "Feedback"
  ) +
  
  coord_equal(xlim = c(1, 8), ylim = c(1, 8)) +
  theme_bw()

print(pba1)

ggsave(
  filename = file.path(out_dir, ("empirical_ppc_pba.png")),
  plot = pba1,
  width = 8,
  height = 6
)

#posterior plot for all pba


pba2 <- posterior_predictive_check_emp_pba |>
  ggplot(aes(x = obs_mean, y = pred_mean, color = factor(feedback))) +
  
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, linetype = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  labs(
    title = paste("Posterior predictive check for PBA model"),
    x = "Observed mean choice_2",
    y = "Predicted mean choice_2",
    color = "Feedback"
  ) +
  
  coord_equal(xlim = c(1, 8), ylim = c(1, 8)) +
  theme_bw()

print(pba2)

ggsave(
  filename = file.path(out_dir, ("allpred_empirical_pba.png")),
  plot = pba2,
  width = 8,
  height = 6
)

#posterior plot per choice one for all wba


wba1 <- posterior_predictive_check_emp_wba |>
  ggplot(aes(x = obs_mean, y = pred_mean, color = factor(feedback))) +
  
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, linetype = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  facet_wrap(~ choice_1) +
  
  labs(
    title = paste("Posterior predictive check for WBA model"),
    subtitle = "One plot for each value of choice_1",
    x = "Observed mean choice_2",
    y = "Predicted mean choice_2",
    color = "Feedback"
  ) +
  
  coord_equal(xlim = c(1, 8), ylim = c(1, 8)) +
  theme_bw()

print(wba1)

ggsave(
  filename = file.path(out_dir, ("empirical_ppc_wba.png")),
  plot = wba1,
  width = 8,
  height = 6
)

#posterior plot for all pba


wba2 <- posterior_predictive_check_emp_wba |>
  ggplot(aes(x = obs_mean, y = pred_mean, color = factor(feedback))) +
  
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, linetype = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  labs(
    title = paste("Posterior predictive check for PBA model"),
    x = "Observed mean choice_2",
    y = "Predicted mean choice_2",
    color = "Group rating"
  ) +
  
  coord_equal(xlim = c(1, 8), ylim = c(1, 8)) +
  theme_bw()

print(wba2)

ggsave(
  filename = file.path(out_dir, ("allpred_empirical_wba.png")),
  plot = wba2,
  width = 8,
  height = 6
)

# Summary table pba

ppc_summary_table_pba <- posterior_predictive_check_emp_pba |>
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

model_summary_pba <- ppc_summary_table_pba |>
  group_by(subject_id) |>
  summarise(
    coverage_rate = mean(covered),
    mean_abs_error = mean(abs_error),
    mean_error = mean(error),
    n = n(),
    credible_interval = mean(credible_interval),
    
    .groups = "drop"
  )

# Summary table wba

ppc_summary_table_wba <- posterior_predictive_check_emp_wba |>
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

model_summary_wba <- ppc_summary_table_wba |>
  group_by(subject_id) |>
  summarise(
    coverage_rate = mean(covered),
    mean_abs_error = mean(abs_error),
    mean_error = mean(error),
    n = n(),
    credible_interval = mean(credible_interval),
    
    
    .groups = "drop"
  )

model_summary_wba |>
  summarise(
    coverage_rate = mean(coverage_rate),
    mean_abs_error = mean(mean_abs_error),
    mean_error = mean(mean_error),
    sd = sd(credible_interval),
    credible_interval = mean(credible_interval),
    
    .groups = "drop"
  )

model_summary_pba |>
  summarise(
    coverage_rate = mean(coverage_rate),
    mean_abs_error = mean(mean_abs_error),
    mean_error = mean(mean_error),
    sd = sd(credible_interval),
    credible_interval = mean(credible_interval),
    
    .groups = "drop"
  )
