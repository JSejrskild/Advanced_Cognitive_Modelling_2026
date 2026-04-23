pacman::p_load('dplyr','posterior','ggplot2','tibble','tidyr','readr','purrr')

sim_list <- list(
  WBAscenario1 = results_df |> filter(scenario == "1", agent == "WBA"),
  WBAscenario2 = results_df |> filter(scenario == "2", agent == "WBA"),
  WBAscenario3 = results_df |> filter(scenario == "3", agent == "WBA"),
  WBAscenario4 = results_df |> filter(scenario == "4", agent == "WBA"),
  PBAscenario1 = results_df |> filter(scenario == "1", agent == "PBA"),
  PBAscenario2 = results_df |> filter(scenario == "2", agent == "PBA"),
  PBAscenario3 = results_df |> filter(scenario == "3", agent == "PBA"),
  PBAscenario4 = results_df |> filter(scenario == "4", agent == "PBA")
)

fit_list <- list(
  WBAscenario1 = WBA_scenario1_modelfit,
  WBAscenario2 = WBA_scenario2_modelfit,
  WBAscenario3 = WBA_scenario3_modelfit,
  WBAscenario4 = WBA_scenario4_modelfit,
  PBAscenario1 = PBA_scenario1_modelfit,
  PBAscenario2 = PBA_scenario2_modelfit,
  PBAscenario3 = PBA_scenario3_modelfit,
  PBAscenario4 = PBA_scenario4_modelfit)

# Generalized Extraction Function for Posteriors
extract_sum_predictive_data <- function(fit, data, scenario_name, target_var = "posterior_pred") {
  obs_df <- tibble(
    obs_id = seq_len(nrow(data)),
    choice_1  = data$choice_1,
    group_rating  = data$group_rating,
    choice_2 = data$choice_2
  )
  
  pred_samples <- posterior::as_draws_df(fit$draws(target_var)) |>
    dplyr::select(-.chain, -.iteration) |> 
    tidyr::pivot_longer(
      cols = dplyr::starts_with(target_var), 
      names_to = "obs_id", 
      values_to = "pred"
    ) |>
    dplyr::mutate(obs_id = readr::parse_number(obs_id))
  
  obs_summary <- obs_df |>
    dplyr::group_by(choice_1, group_rating) |>
    dplyr::summarise(obs_mean = mean(choice_2), .groups = "drop")
  
  pred_summary <- pred_samples |>
    dplyr::left_join(obs_df, by = "obs_id") |>
    dplyr::group_by(.draw, choice_1, group_rating) |>
    dplyr::summarise(pred_mean_draw = mean(pred), .groups = "drop") |>
    dplyr::group_by(choice_1, group_rating) |>
    dplyr::summarise(
      pred_mean = median(pred_mean_draw),
      lower  = quantile(pred_mean_draw, 0.025),
      upper  = quantile(pred_mean_draw, 0.975),
      .groups = "drop"
    ) |>
    dplyr::left_join(obs_summary, by = c("choice_1", "group_rating")) |>
    dplyr::mutate(
      scenario = scenario_name
    )
  
  return(pred_summary)
}

# Generalized Extraction Function for Posteriors
extract_draws <- function(fit, data, scenario_name, target_var = "posterior_pred") {
  obs_df <- tibble(
    obs_id = seq_len(nrow(data)),
    choice_1  = data$choice_1,
    group_rating  = data$group_rating,
    choice_2 = data$choice_2
  )
  
  pred_samples <- posterior::as_draws_df(fit$draws(target_var)) |>
    dplyr::select(-.chain, -.iteration) |> 
    tidyr::pivot_longer(
      cols = dplyr::starts_with(target_var), 
      names_to = "obs_id", 
      values_to = "pred"
    ) |>
    dplyr::mutate(obs_id = readr::parse_number(obs_id),
                  scenario = scenario_name
    )
  
  return(pred_samples)
}

# Generalized Extraction Function for Posteriors
extract_prior_pred <- function(fit, data, scenario_name, target_var = "prior_pred") {
  obs_df <- tibble(
    obs_id = seq_len(nrow(data)),
    choice_1  = data$choice_1,
    group_rating  = data$group_rating,
    choice_2 = data$choice_2
  )
  
  prior_pred_samples <- posterior::as_draws_df(fit$draws(target_var)) |>
    dplyr::select(-.chain, -.iteration) |> 
    tidyr::pivot_longer(
      cols = dplyr::starts_with(target_var), 
      names_to = "obs_id", 
      values_to = "pred"
    ) |>
    dplyr::mutate(obs_id = readr::parse_number(obs_id),
                  scenario = scenario_name
    )
  
  return(prior_pred_samples)
}

# Extract POSTERIOR predictive checks
posterior_predictive_check <- imap_dfr(fit_list, function(fit, scenario) {
  extract_sum_predictive_data(
    fit = fit, 
    data = sim_list[[scenario]], 
    scenario_name = scenario,
    target_var = "posterior_pred"
  )
})

# Extract draws

predictive_draws <- imap_dfr(fit_list, function(fit, scenario) {
  extract_draws(
    fit = fit, 
    data = sim_list[[scenario]], 
    scenario_name = scenario,
    target_var = "posterior_pred"
  )
})

prior_predictive_check <- imap_dfr(fit_list, function(fit, scenario) {
  extract_prior_pred(
    fit = fit, 
    data = sim_list[[scenario]], 
    scenario_name = scenario,
    target_var = "prior_pred"
  )
})
