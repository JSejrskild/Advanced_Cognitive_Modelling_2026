# set working dir
print(getwd())
#target_dir <- "/Users/peli/Projects/Repositories/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models"
target_dir <- "/work/ACM_2026/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models"
if (basename(getwd()) != "2_matching_pennies_stan_models") {
  setwd(target_dir)
}
setwd(target_dir)
print(getwd()) # !WD root should be 2_matching_pennies_stan_models
print(list.files("."))
workdir <- getwd()
# imports
pacman::p_load("tidyverse", "cmdstanr", "here", "posterior", "bayesplot", "patchwork")

fpath <- "data/RL_vs_biased.csv"
simdata <- read_csv(fpath)

outputdir <- paste0(workdir,"/output")
model_file <- paste0(outputdir, "/rlmodel_fit.rds")

rlmodelpath <- "src/RL_model.stan"
print(rlmodelpath)
rlmodel <- cmdstan_model(rlmodelpath) # create the stan model object

LRs <- round(seq(0.1, 1, by = 0.1), digits = 1)
agent_ids <- round(seq(1, 100, by = 1), digits = 0)
ntrials <- 120
param_recov_result <- tibble()
class(LRs)
#class(one_agent_id)

all_results <- list()
all_agent_preds <- list()
trace_plots <- list()
bad_count <- 0
  
# RECOVERY LOOP
for (i in LRs){
  print(paste("============== Learning Rate: ", i, "=============="))
  for (agent in agent_ids) {
    print(paste("=== Agent ID: ", agent))
    testdata <- simdata %>% 
        select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>% 
        filter(agent_id==agent, learningRate == i, noise == 0)
    
    if (nrow(testdata) == 0) {
      print(paste("No data for learning rate", i))
      bad_count = bad_count + 1
      next
    }
    if (nrow(testdata) != ntrials* length(unique(testdata$agent_id))) {
      print(paste("Wrong number of trials for LR", i, ":", nrow(testdata)))
      bad_count = bad_count + 1
      next
    }
    
    initialV <- 0.5
    
      # setup stan data structure
    sdata <- list(
      t = ntrials,
      choice = testdata$choicesA,
      feedback = testdata$choicesB,
      initialV = initialV,
      alpha_prior_mu = 0,
      alpha_prior_sd = 1.5
    )
    
    # === FIT MODEL ===
    fit_rl <- rlmodel$sample( #set configurations
      data=sdata,
      seed=231,
      chains= 4,
      parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 2000,
      refresh = 500
    )
    
    # === INSPECT FIT ===
    
    fit_rl$summary("alpha") # check alpha posterior
    
    draws <- as_draws_df(fit_rl$draws())
    alpha_prior <- draws$alpha_prior
    alpha_post <- draws$alpha
    mean_alpha_prior <- mean(alpha_prior)
    mean_alpha_post <- mean(alpha_post)
    
    # create a combined dataframe with prior-posterior for each learning rate
    agent_results <- tibble(
      learning_rate = i,
      agent_id = agent,
      alpha_prior = alpha_prior,
      alpha_post = alpha_post
    )
    
    # Append to the global results dataframe
    all_results <- bind_rows(all_results, agent_results)
    
    # Create the prior fractions
    frac_preds <- function(pred_var_name) {
      pred <- as_draws_matrix(fit_rl$draws(pred_var_name))
      dim(pred)
      true <- testdata$choicesA
      matches <- pred == true
      matches_numeric <- matches * 1
      fraction_per_trial <- colSums(matches_numeric) / nrow(matches_numeric)
      return(fraction_per_trial)
    }
    
    prior_pred_per_trial <- frac_preds("choice_prior_pred")
    post_pred_per_trial <- frac_preds("choice_post_pred")
    
    agent_preds <-  tibble(
      learning_rate = i,
      agent_id = agent,
      prior_pred_per_trial = prior_pred_per_trial,
      post_pred_per_trial = post_pred_per_trial
    )
    
    all_agent_preds <- bind_rows(all_agent_preds, agent_preds)
    
    # create some MCMC trace plots
    trace_plot <- ggplot(draws, aes(.iteration, alpha, group = .chain, color = .chain)) +
      geom_line() +
      labs(title = paste("α = ", i)) +
      theme_classic() + 
      scale_color_gradient(low = "purple", high= "orange")
    
    trace_plots[[as.character(i)]] <- trace_plot
    
  }
}
print(paste0("There were ", bad_count, " bad paramrecovery runs."))

posterior_means <- all_results %>%
  group_by(learning_rate, agent_id) %>%
  summarize(
    mean_alpha_prior = mean(alpha_prior),
    mean_alpha_post = mean(alpha_post),
    .groups = "drop"
  )

# --------------------------------------------

# === MCMC DIAGNOSITCS ===
diagnostics <- function(fit_object){
  fit_summary <- fit_object$summary() # check full posterior summary (DOES NOT WORK, too much data?)
  diagnostic_summary <- fit_object$diagnostic_summary()
  diagnostic_summary
  
  # 1. Check for Divergent Transitions (Zero Tolerance)
  divergences <- sum(diagnostic_summary$num_divergent)
  if (divergences > 0) {
    stop(paste("CRITICAL FAILURE:", divergences, "divergences detected. The posterior geometry is pathological. Reparameterization required. Do not interpret these results."))
  }
  
  # 2. Check E-BFMI (Energy-Bayesian Fraction of Missing Information)
  # Threshold is typically 0.3. Values below this indicate poor exploration.
  min_ebfmi <- min(diagnostic_summary$ebfmi)
  if (min_ebfmi < 0.3) {
    warning(paste("WARNING: Low E-BFMI detected (Min =", round(min_ebfmi, 3), 
                  "). The sampler may struggle to explore the tails of the posterior."))
  }
  
  # 3. Check for Convergence (Rhat < 1.01)
  max_rhat <- max(fit_summary$rhat, na.rm = TRUE)
  if (max_rhat > 1.01) {
    stop(paste("CRITICAL FAILURE: Chains did not converge. Max Rhat =", round(max_rhat, 3)))
  }
  
  # 4. Check Effective Sample Size (ESS > 400 for both bulk and tail)
  min_ess_bulk <- min(fit_summary$ess_bulk, na.rm = TRUE)
  min_ess_tail <- min(fit_summary$ess_tail, na.rm = TRUE)
  if (min_ess_bulk < 400 || min_ess_tail < 400) {
    warning("WARNING: Insufficient Effective Sample Size. Autocorrelation is too high. Run longer chains.")
  }
}

diagnostics(fit_rl)

# create combined trace plots
combined_trace_plots <- patchwork::wrap_plots(trace_plots, ncol = 3) 

combined_trace_plots <- combined_trace_plots + 
  plot_annotation(title = "MCMC Trace Plots across all Learning Rates (model fits)")

print(combined_trace_plots)

# Save it
ggsave(
  file.path(workdir, "output", "all_trace_plots.png"), 
  plot = combined_trace_plots, 
  width = 25, height = 20, units = "cm", dpi = 300
)

# === Validation PLOTS ===

# Plot one prior-posterior:
pick <- all_results %>% 
  filter(learning_rate == 0.3)
ggplot(pick) +
  geom_density(aes(alpha_post, fill = "Posterior"), alpha = 0.6) +
  geom_density(aes(alpha_prior, fill = "Prior"), alpha = 0.6) +
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "black", linewidth = 1.2) +
  scale_fill_manual(values = c("Posterior" = "blue", "Prior" = "red")) +
  labs(
    title = "Prior-Posterior (Alpha, learning rate)",
    x = "Alpha (Learning rate)",
    y = "Density",
    fill = "Distribution"
  ) +
  theme_classic()

# Plot posterior means against true values
all_results

param_recov_allagents <- ggplot(posterior_means, aes(x=learning_rate, y=mean_alpha_post)) +
  geom_point() + 
  geom_abline(intercept=0, slope=1, linetype=2) +
  geom_smooth(method="lm", se=T, formula = "y ~ x") +
  labs(
    title = "Parameter Recovery (Alpha, learning rate)",
    x = "Alpha (learning rate)",
    y = "Mean alpha posterior"
  ) +
  theme_classic()

ggsave(
  file.path(workdir, "output", "parameter_recovery_allagents.png"), 
  plot = param_recov_allagents, 
  width = 25, height = 20, units = "cm", dpi = 300
)

# Prior-Predictive
sum(is.na(all_agent_preds))
all_agent_preds %>% 
  mutate(trial = rep(1:120, times = n_distinct(agent_id)*n_distinct(learning_rate))) %>% 
  ggplot(aes(x = trial, y = prior_pred_per_trial, col=learning_rate)) +
  geom_smooth(method="lm", se=T, formula = "y ~ x") +
  geom_point() + 
  facet_wrap(~learning_rate)
  


### Posterior Predictive Check ###

# Extract posterior predictive simulations from fit_rl
h_post_rep <- fit_rl$draws("choice_post_pred", format = "matrix")

# Calculate the test statistic T(y) for the actual empirical data
observed_sum <- sum(testdata$choicesA)

# Calculate T(y_rep) for each replicated dataset
post_rep_sums <- rowSums(h_post_rep)

# Bundle into tibble
ppc_df <- tibble(
  Replicated_Sum = post_rep_sums
)

# Posterior Predictive Check plot
ppc_posterior_ggplot <- ggplot(ppc_df, aes(x = Replicated_Sum)) +
  geom_histogram(fill = "skyblue", color = "white", alpha = 0.7, binwidth = 1)+
  geom_vline(xintercept = observed_sum, color = "black", linetype = "dashed", linewidth = 1) +
  ggtitle("Posterior Predictive Check: Total 'Right' Choices") +
  xlab(paste("Total Number of 'Right' Choices (out of", ntrials, "trials)")) +
  ylab("Frequency (Replicated Datasets)") +
  annotate(
    "text", x = observed_sum, y = Inf,
    label = "Empirical data",
    vjust = 2, hjust = if(observed_sum > ntrials/2) 1.1 else -0.1,
    size = 4, fontface = "bold"
  ) +
  theme_classic()

ppc_posterior_ggplot


## Posterior Predictive Check w. Pior behind ##

# Prepare prior data (grey, opaque, background)
prior_summary <- all_agent_preds %>%
  mutate(trial = rep(1:120, times = n_distinct(agent_id) * n_distinct(learning_rate))) %>%
  group_by(trial, learning_rate) %>%
  summarise(
    mean_pred = mean(prior_pred_per_trial, na.rm = TRUE),
    se = sd(prior_pred_per_trial, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  )

# Prepare posterior data (coloured, foreground)
post_summary <- all_agent_preds %>%
  mutate(trial = rep(1:120, times = n_distinct(agent_id) * n_distinct(learning_rate))) %>%
  group_by(trial, learning_rate) %>%
  summarise(
    mean_pred = mean(post_pred_per_trial, na.rm = TRUE),
    se = sd(post_pred_per_trial, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  )

combined_pred_plot <- ggplot() +
  # Prior: grey, behind, opaque
  geom_ribbon(data = prior_summary,
              aes(x = trial, ymin = mean_pred - ci, ymax = mean_pred + ci),
              fill = "grey60", alpha = 0.15) +
  geom_line(data = prior_summary,
            aes(x = trial, y = mean_pred),
            color = "black", alpha = 0.5, linewidth = 0.6, linetype = "dashed") +
  # Posterior: coloured, foreground
  geom_ribbon(data = post_summary,
              aes(x = trial, ymin = mean_pred - ci, ymax = mean_pred + ci, fill = mean_pred),
              alpha = 0.2) +
  geom_line(data = post_summary,
            aes(x = trial, y = mean_pred, color = mean_pred)) +
  scale_color_gradient(low = "red", high = "green") +
  scale_fill_gradient(low = "red", high = "green") +
  facet_wrap(~learning_rate) +
  ylim(0.45, 0.8) +
  labs(
    title = "Prior (grey) vs Posterior Prediction Accuracy",
    x = "Trial (out of 120)",
    y = "Prediction Accuracy",
    color = "Posterior Mean %",
    fill = "Posterior Mean %"
  ) +
  theme_classic()

print(combined_pred_plot)

ggsave(
  file.path(workdir, "output", "prior_posterior_pred_plot.png"),
  plot = combined_pred_plot,
  width = 25, height = 20, units = "cm", dpi = 300
)





