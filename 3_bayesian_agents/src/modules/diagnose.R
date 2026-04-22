diagnose_fit <- function(fit_object){
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

plot_trace <- function(draws_df, parameter, title) {
  trace_plot <- ggplot(draws_df, aes(.iteration, !!sym(parameter), group = .chain, color = .chain)) +
    geom_line() +
    labs(title = title) +
    theme_classic() +
    scale_color_gradient(low = "purple", high = "orange")
  
  return(trace_plot)
}