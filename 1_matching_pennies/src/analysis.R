print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse")

filepath <- "data/RL_vs_RLR.csv"
raw <- read_csv(filepath)

#pick some parameter values for simplicity
filtered <- raw %>%
  filter(learningRate == 0.1, noise == 0)

ggplot(filtered, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA, group=agent_id), color = "blue", alpha=0.2) +
  geom_line(aes(y=cumulativeRateB, group=agent_id), color = "red", alpha=0.2) + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic()

# I can try to maybe do a mean of all agent cumulativeRates
filteredMean <- raw %>%
  filter(learningRate == 0.1, noise == 0) %>%
  group_by(trial) %>%
  summarise(
    meanCumulativeRateA = mean(cumulativeRateA),
    meanCumulativeRateB = mean(cumulativeRateB),
    .groups = "drop"
  )

ggplot(filteredMean, aes(trial)) + 
  geom_line(aes(y=meanCumulativeRateA), color = "blue", alpha=0.2) +
  geom_line(aes(y=meanCumulativeRateB), color = "red", alpha=0.2) +
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic()