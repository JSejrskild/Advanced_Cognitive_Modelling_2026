pacman::p_load('dplyr','posterior','ggplot2','tibble','tidyr','readr','purrr')

data <- data %>% drop_na()

extract_sum_empirical_data_feedback <- function(fit, data, target_var = "posterior_pred") {

obs_df <- tibble(
  obs_id = seq_len(nrow(data)),
  subject_id = data$ID,
  choice_1 = data$FirstRating,
  feedback = data$Feedback,
  choice_2 = data$SecondRating
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
  dplyr::group_by(subject_id, choice_1, feedback) |>
  dplyr::summarise(obs_mean = mean(choice_2), .groups = "drop")

pred_summary <- pred_samples |>
  dplyr::left_join(obs_df, by = "obs_id") |>
  dplyr::group_by(.draw, subject_id, choice_1, feedback) |>
  dplyr::summarise(pred_mean_draw = mean(pred), .groups = "drop") |>
  dplyr::group_by(subject_id, choice_1, feedback) |>
  dplyr::summarise(
    pred_mean = median(pred_mean_draw),
    lower = quantile(pred_mean_draw, 0.025),
    upper = quantile(pred_mean_draw, 0.975),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    obs_summary,
    by = c("subject_id", "choice_1", "feedback")
  )

return(pred_summary)
}

## Posterior predictive check empirical

posterior_predictive_check_emp_pba <- extract_sum_empirical_data_feedback(
  fit = PBA_precogsci_modelfit,
  data = data,
  target_var = "posterior_pred"
)

posterior_predictive_check_emp_wba <- extract_sum_empirical_data_feedback(
  fit = WBA_precogsci_modelfit,
  data = data,
  target_var = "posterior_pred"
)
