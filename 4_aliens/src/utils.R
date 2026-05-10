print_posterior_summaries <- function(data, fit_object_tag) {
  
  # Helper function
  summarize_param <- function(x) {
    c(
      min  = min(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      max  = max(x, na.rm = TRUE)
    )
  }
  
  # Helper for scalar true values
  summarize_true <- function(x) {
    c(
      min  = x[1],
      mean = x[1],
      max  = x[1]
    )
  }
  
  # -----------------------------
  # LOG SPACE SUMMARY
  # -----------------------------
  
  log_summary_df <- data.frame(
    parameter = c(
      "R Prior", "R Posterior", "R True",
      "Q Prior", "Q Posterior", "Q True"
    ),
    rbind(
      summarize_param(log(data$r_prior)),
      summarize_param(log(data$r_post)),
      summarize_true(log(data$r_true)),
      
      summarize_param(log(data$q_prior)),
      summarize_param(log(data$q_post)),
      summarize_true(log(data$q_true))
    ),
    row.names = NULL
  )
  
  # -----------------------------
  # NATURAL SPACE SUMMARY
  # -----------------------------
  
  natural_summary_df <- data.frame(
    parameter = c(
      "R Prior", "R Posterior", "R True",
      "Q Prior", "Q Posterior", "Q True"
    ),
    rbind(
      summarize_param(data$r_prior),
      summarize_param(data$r_post),
      summarize_true(data$r_true),
      
      summarize_param(data$q_prior),
      summarize_param(data$q_post),
      summarize_true(data$q_true)
    ),
    row.names = NULL
  )
  
  # =====================================================
  # PRINTING
  # =====================================================
  
  cat("\n")
  cat("=====================================================\n")
  cat(" Posterior Parameter Summary\n")
  cat("-----------------------------------------------------\n")
  cat(" Fit Object :", fit_object_tag, "\n")
  cat("=====================================================\n\n")
  
  # LOG SPACE
  cat("LOG SPACE\n")
  cat("-----------------------------------------------------\n")
  
  print(
    format(
      log_summary_df,
      digits = 4,
      justify = "left"
    ),
    row.names = FALSE
  )
  
  cat("\n")
  
  # NATURAL SPACE
  cat("NATURAL SPACE\n")
  cat("-----------------------------------------------------\n")
  
  print(
    format(
      natural_summary_df,
      digits = 4,
      justify = "left"
    ),
    row.names = FALSE
  )
  
  cat("\n=====================================================\n")
}