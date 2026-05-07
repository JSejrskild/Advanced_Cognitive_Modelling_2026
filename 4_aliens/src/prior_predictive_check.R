#Prior Predictive Check

#import packages
pacman::p_load('tidyverse','purrr','parallel','furrr','future','dplyr','here','fs')
print(getwd())
workdir <- here("4_aliens")
cat("Workdir:", workdir)
setwd(workdir)
source("src/simulation.R")
# setup dirs
output_dir <- here(workdir, "output")
dir_create(output_dir, recurse = TRUE)

n_ppc_samples <- 500

ppc_log_r  <- rnorm(n_ppc_samples, 0, 1)
ppc_log_q  <- rnorm(n_ppc_samples, -2, 1)
ppc_r      <- exp(ppc_log_r)
ppc_q      <- exp(ppc_log_q)
ppc_sched  <- make_subject_schedule(stimulus_info, n_blocks, seed = 999)

prior_pred_curves <- results

ppc_summary <- prior_pred_curves |>
  group_by(trial) |>
  summarise(
    q05 = quantile(cum_acc, 0.05), q25 = quantile(cum_acc, 0.25),
    q50 = median(cum_acc),
    q75 = quantile(cum_acc, 0.75), q95 = quantile(cum_acc, 0.95),
    .groups = "drop"
  )

ggplot(ppc_summary, aes(x = trial)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#009E73", alpha = 0.20) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#009E73", alpha = 0.40) +
  geom_line(aes(y = q50), color = "#006D4E", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = "Prior Predictive Check: Kalman Filter Prototype Model",
    subtitle = "Ribbons: 50% and 90% prior predictive intervals\nPriors: log(r) ~ Normal(0, 1), log(q) ~ Normal(-2, 1)",
    x = "Trial", y = "Cumulative Accuracy"
  )