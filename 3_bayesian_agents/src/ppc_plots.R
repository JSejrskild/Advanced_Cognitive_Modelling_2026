library(ggplot2)
library(dplyr)
library(purrr)

# folder where plots should go
out_dir <- "figures"

scenarios <- unique(posterior_predictive_check$Scenario)

walk(scenarios, function(scen) {
  
  p <- posterior_predictive_check |>
    filter(Scenario == scen) |>
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