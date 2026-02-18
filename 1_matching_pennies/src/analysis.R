print(getwd())
setwd("../1_matching_pennies")
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse")

filepath <- "data/RLR_vs_WSLS.csv"
raw <- read_csv(filepath)

raw <- raw %>% 
  mutate(
    cumWinA = cumsum(winA) / seq_along(winA),
    cumWinB = cumsum(winB) / seq_along(winB)
 )

#pick some parameter values for simplicity
filtered <- raw %>%
  filter(learningRate == 0.1, noise == 0)

ggplot(filtered, aes(trial)) + 
  geom_line(aes(y=cumulativeRateA, group=agent_id), color = "blue", alpha=0.2) +
  geom_line(aes(y=cumulativeRateB, group=agent_id), color = "red", alpha=0.2) + 
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic()

# 
ggplot(filtered, aes(x = trial, group = agent_id)) + 
  geom_line(aes(y = cumulativeWinA), color = "blue", alpha = 0.2) +
  geom_line(aes(y = cumulativeWinB), color = "red", alpha = 0.2) + 
  geom_hline(yintercept = 0.5) + 
  ylim(0, 1) + 
  theme_classic()

# I can try to maybe do a mean of all agent cumulativeRates
filtered2 <- raw %>%
  filter(noise == 0)

ggplot(filtered2, aes(x=trial, color = as.factor(learningRate)
                         )) + 
  geom_line(aes(y=cumulativeWinA)) +
  geom_hline(yintercept=0.5) + 
  ylim(0,1) + 
  theme_classic()