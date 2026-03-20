# set working dir
print(getwd())
target_dir <- "/Users/peli/Projects/Repositories/Advanced_Cognitive_Modelling_2026/2_matching_pennies_stan_models"
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

  # === SET INPUT DATA ===
  # Maybe choose some specific data or loop across it

rlmodel <- cmdstan_model(rlmodelpath) # create the stan model object

inspect <- simdata %>% 
  select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>% 
  filter(agent_id == 23, learningRate == 0.7, noise == 0)

LRs <- round(seq(0.1, 1, by = 0.1), digits = 1)
agents <- seq(0,100)
ntrials <- 120
param_recov_result <- tibble()
class(LRs)
class(one_agent_id)

aresults <- list()
trace_plots <- list()
bad_count <- 0
  
# RECOVERY LOOP

for (i in LRs){
  print(paste("============== Learning Rate: ", i, "=============="))
  testdata <- simdata %>% 
      select(agent_id, trial, choicesA, choicesB, learningRate, noise) %>% 
      filter(learningRate == i, noise == 0)
  
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
  rlmodelpath <- "src/RL_model.stan"
  print(rlmodelpath)
  
  fit_rl <- rlmodel$sample( # set configuations
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
  
  # create a combined dataframe with prior-posterior for each learning rate
  results[[as.character(i)]] <- tibble(
    learning_rate = i,
    alpha_prior = alpha_prior,
    alpha_post = alpha_post
  )
  
  # create some MCMC trace plots
  trace_plot <- ggplot(draws, aes(.iteration, alpha, group = .chain, color = .chain)) +
    geom_line() +
    labs(title = paste("α = ", i)) +
    theme_classic() + 
    scale_color_gradient(low = "purple", high= "orange")
  
  trace_plots[[as.character(i)]] <- trace_plot
  
}
print(paste0("There were ", bad_count, " bad paramrecovery runs."))

final_results <- bind_rows(results)

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
pick <- final_results %>% 
  filter(learning_rate == 0.3)
ggplot(pick) +
  geom_density(aes(alpha_post, fill = "Posterior"), alpha = 0.6) +
  geom_density(aes(alpha_prior, fill = "Prior"), alpha = 0.6) +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "black", linewidth = 1.2) +
  scale_fill_manual(values = c("Posterior" = "blue", "Prior" = "red")) +
  labs(
    title = "Prior-Posterior (Alpha, learning rate)",
    x = "Alpha (Learning rate)",
    y = "Density",
    fill = "Distribution"
  ) +
  theme_classic()

# Plot our prior-posterior predictive
plot_data <- final_results %>%
  pivot_longer(
    cols = c(alpha_post, alpha_prior),
    names_to = "Distribution",
    values_to = "alpha_value"
  ) %>%
  # Make the names look nice for the legend
  mutate(Distribution = ifelse(Distribution == "alpha_post", "posterior", "prior"))

lr_param_recov_plot1 <- ggplot(plot_data) +
  # Now we only need ONE density layer!
  geom_density(aes(x = alpha_value, fill = Distribution), alpha = 0.6) +
  geom_vline(aes(xintercept = learning_rate), linetype = "dashed", color = "black", linewidth = 1.2) +
  scale_fill_manual(values = c("posterior" = "lightblue", "prior" = "orange")) +
  labs(
    title = "Prior-Posterior (Alpha, learning rate)",
    x = "Alpha (Learning rate)",
    y = "Density",
    fill = "Distribution"
  ) +
  theme_classic() +
  # Use scales = "free_y" so if one posterior is very narrow and tall, 
  # it doesn't squash the other plots flat.
  facet_wrap(~learning_rate, scales = "free_y")

plotpath <- file.path(workdir, "output", "lr_param_recov_plot1.png")
ggsave(plotpath, plot=lr_param_recov_plot1, 
       width = 20,
       height = 14,
       units = "cm",
       dpi=300
       )
  
# ==== Plot Posterior Predictive ====