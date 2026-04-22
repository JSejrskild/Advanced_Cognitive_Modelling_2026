# set working dir
print(getwd())
target_dir <- "/Users/peli/Projects/Repositories/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models"

print(getwd())
print(list.files("."))
workdir <- getwd()

pacman::p_load("tidyverse", "cmdstanr", "here", "posterior", "bayesplot", "patchwork")

fpath <- "data/RL_vs_biased_continous.csv"
simdata <- read_csv(fpath) %>%
  filter(noise == 0)

outputdir <- paste0(workdir, "/output")
rlmodelpath <- "src/RL_model.stan"
print(rlmodelpath)
rlmodel <- cmdstan_model(rlmodelpath)

all_results     <- tibble()
all_agent_preds <- tibble()
trace_plots     <- list()
bad_count       <- 0

trial_subsets <- c(30, 60, 90, 120)
agent_ids     <- unique(simdata$agent_id)

# RECOVERY LOOP
for (n_subset in trial_subsets) {
  print(paste("============== Trial Subset:", n_subset, "=============="))
  
  for (agent in agent_ids) {
    print(paste("=== Agent ID:", agent))
    
    testdata <- simdata %>%
      select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>%
      filter(agent_id == agent) %>%
      slice(1:n_subset)
    
    if (nrow(testdata) == 0) {
      print(paste("No data for agent", agent))
      bad_count <- bad_count + 1
      next
    }
    if (nrow(testdata) != n_subset) {
      print(paste("Wrong number of trials for agent", agent, ":", nrow(testdata)))
      bad_count <- bad_count + 1
      next
    }
    
    true_lr  <- unique(testdata$learningRate)
    initialV <- 0.5
    
    sdata <- list(
      t        = n_subset,
      choice   = testdata$choicesA,
      feedback = testdata$choicesB,
      initialV = initialV,
      alpha_prior_mu = 0,
      alpha_prior_sd = 1.5
    )
    
    # === FIT MODEL ===
    fit_rl <- rlmodel$sample(
      data = sdata,
      seed = 231,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 2000,
      refresh = 500
    )
    
    fit_rl$summary("alpha")
    
    draws <- as_draws_df(fit_rl$draws())
    alpha_prior      <- draws$alpha_prior
    alpha_post       <- draws$alpha
    mean_alpha_prior <- mean(alpha_prior)
    mean_alpha_post  <- mean(alpha_post)
    
    agent_results <- tibble(
      learning_rate = true_lr,
      agent_id      = agent,
      n_trials_used = n_subset,
      alpha_prior   = alpha_prior,
      alpha_post    = alpha_post
    )
    
    all_results <- bind_rows(all_results, agent_results)
    
    # Prior/posterior predictive fractions
    frac_preds <- function(pred_var_name) {
      pred <- as_draws_matrix(fit_rl$draws(pred_var_name))
      true <- testdata$choicesA
      matches <- pred == true
      matches_numeric <- matches * 1
      fraction_per_trial <- colSums(matches_numeric) / nrow(matches_numeric)
      return(fraction_per_trial)
    }
    
    prior_pred_per_trial <- frac_preds("choice_prior_pred")
    post_pred_per_trial  <- frac_preds("choice_post_pred")
    
    agent_preds <- tibble(
      learning_rate        = true_lr,
      agent_id             = agent,
      n_trials_used        = n_subset,
      prior_pred_per_trial = prior_pred_per_trial,
      post_pred_per_trial  = post_pred_per_trial
    )
    
    all_agent_preds <- bind_rows(all_agent_preds, agent_preds)
    
    # MCMC trace plot
    trace_plot <- ggplot(draws, aes(.iteration, alpha, group = .chain, color = .chain)) +
      geom_line() +
      labs(title = paste("α =", round(true_lr, 3), "| trials =", n_subset)) +
      theme_classic() +
      scale_color_gradient(low = "purple", high = "orange")
    
    trace_plots[[paste(agent, n_subset, sep = "_")]] <- trace_plot
  }
}

print(paste0("There were ", bad_count, " bad paramrecovery runs."))

posterior_means <- all_results %>%
  group_by(agent_id, n_trials_used) %>%
  summarize(
    true_learning_rate = unique(learning_rate),
    mean_alpha_prior   = mean(alpha_prior),
    mean_alpha_post    = mean(alpha_post),
    .groups = "drop"
  )

# === MCMC DIAGNOSTICS ===
diagnostics <- function(fit_object){
  fit_summary        <- fit_object$summary()
  diagnostic_summary <- fit_object$diagnostic_summary()
  
  divergences <- sum(diagnostic_summary$num_divergent)
  if (divergences > 0) {
    stop(paste("CRITICAL FAILURE:", divergences, "divergences detected. The posterior geometry is pathological. Reparameterization required. Do not interpret these results."))
  }
  
  min_ebfmi <- min(diagnostic_summary$ebfmi)
  if (min_ebfmi < 0.3) {
    warning(paste("WARNING: Low E-BFMI detected (Min =", round(min_ebfmi, 3),
                  "). The sampler may struggle to explore the tails of the posterior."))
  }
  
  max_rhat <- max(fit_summary$rhat, na.rm = TRUE)
  if (max_rhat > 1.01) {
    stop(paste("CRITICAL FAILURE: Chains did not converge. Max Rhat =", round(max_rhat, 3)))
  }
  
  min_ess_bulk <- min(fit_summary$ess_bulk, na.rm = TRUE)
  min_ess_tail <- min(fit_summary$ess_tail, na.rm = TRUE)
  if (min_ess_bulk < 400 || min_ess_tail < 400) {
    warning("WARNING: Insufficient Effective Sample Size. Autocorrelation is too high. Run longer chains.")
  }
}

diagnostics(fit_rl)

# === TRACE PLOTS ===
combined_trace_plots <- patchwork::wrap_plots(trace_plots, ncol = 3) +
  plot_annotation(title = "MCMC Trace Plots across all agents and trial subsets")

print(combined_trace_plots)

ggsave(
  file.path(workdir, "output", "all_trace_plots.png"),
  plot = combined_trace_plots,
  width = 25, height = 20, units = "cm", dpi = 300
)

# === VALIDATION PLOTS ===

# Single agent prior-posterior across trial subsets
pick <- all_results %>%
  filter(agent_id == 1)

ggplot(pick) +
  geom_density(aes(alpha_post, fill = "Posterior"), alpha = 0.6) +
  geom_density(aes(alpha_prior, fill = "Prior"), alpha = 0.6) +
  geom_vline(xintercept = unique(pick$learning_rate), linetype = "dashed", color = "black", linewidth = 1.2) +
  scale_fill_manual(values = c("Posterior" = "blue", "Prior" = "red")) +
  labs(
    title = paste("Prior-Posterior | Agent 1 | True LR =", round(unique(pick$learning_rate), 3)),
    x = "Alpha (Learning rate)",
    y = "Density",
    fill = "Distribution"
  ) +
  theme_classic() +
  facet_wrap(~n_trials_used, labeller = label_both)

# Parameter recovery by trial subset
param_recov_by_trials <- ggplot(posterior_means, aes(x = true_learning_rate, y = mean_alpha_post)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  labs(
    title = "Parameter Recovery by Number of Trials",
    x = "True learning rate",
    y = "Mean alpha posterior"
  ) +
  theme_classic() +
  facet_wrap(~n_trials_used, labeller = label_both)

print(param_recov_by_trials)

ggsave(
  file.path(workdir, "output", "parameter_recovery_by_trials.png"),
  plot = param_recov_by_trials,
  width = 25, height = 20, units = "cm", dpi = 300
)

# === PRIOR-POSTERIOR PREDICTIVE ===
all_agent_preds %>%
  group_by(agent_id, learning_rate, n_trials_used) %>%
  mutate(trial = 1:n()) %>%
  ungroup() %>%
  ggplot(aes(x = trial, y = prior_pred_per_trial, col = learning_rate)) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  geom_point() +
  scale_color_viridis_c() +
  labs(
    title = "Prior Predictive Check by Number of Trials",
    x = "Trial",
    y = "Fraction correct (prior)",
    color = "True learning rate"
  ) +
  theme_classic() +
  facet_wrap(~n_trials_used, labeller = label_both)