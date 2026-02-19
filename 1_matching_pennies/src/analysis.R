# set working dir
print(getwd())
target_dir <- "../Advanced_Cognitive_Modelling_2026/1_matching_pennies"
if (basename(getwd()) != "1_matching_pennies") {
  setwd(target_dir)
}
print(getwd())
print(list.files("."))
# imports
pacman::p_load("tidyverse")


#################################################
###          RLR vs WSLS (PLOTTING)         #####
#################################################


filepath <- "data/RLR_vs_WSLS.csv"
raw <- read_csv(filepath)

raw <- raw %>% 
  mutate(
    cumWinA = cumsum(winA) / seq_along(winA),
    cumWinB = cumsum(winB) / seq_along(winB)
 )


# I can try to maybe do a mean of all agent cumulativeRates
filtered2 <- raw %>%
  filter(noise == 0)

rlr_avr_performance <- ggplot(filtered2, aes(x=trial,y=cumulativeWinA, 
                         group=learningRate, color = as.factor(learningRate)
                         )) + 
  stat_summary(fun=mean, geom="line") +
  geom_hline(yintercept=0.5) + 
  ylim(0, 1) + 
  theme_classic() + 
  labs(
    title = "Average 'RL-R' performance vs. WSLS",
    subtitle = "A very bad loser needs a high learning rate to win",
    x = "Trial Number",
    y = "Average Proportion Wins",
    color = "Own Learning Rate"
  ) +
  facet_wrap(~ kSwitch)

ggsave("output/avg_rlr_performance_by_learning.png", rlr_avr_performance, width = 12)


## FACET WRAP - by noise

#pick some parameter values for simplicity
filtered <- raw %>%
  filter(kSwitch == 3)

rlr_avr_performance_noise <- ggplot(filtered, aes(x=trial,y=cumulativeWinA, 
                                             group=learningRate, color = as.factor(learningRate)
)) + 
  stat_summary(fun=mean, geom="line") +
  geom_hline(yintercept=0.5) + 
  ylim(0, 1) + 
  theme_classic() + 
  labs(
    title = "Average 'RL-R' performance vs. WSLS",
    subtitle = "The more noise the more it says in equilibrium",
    x = "Trial Number",
    y = "Average Proportion Wins",
    color = "Own Learning Rate"
  ) +
  facet_wrap(~ noise)

ggsave("output/avg_rlr_performance_by_learning_by_noise.png", rlr_avr_performance_noise, width = 12)


#################################################
###          RL vs WSLS (PLOTTING)          #####
#################################################

filepath <- "data/RL_vs_WSLS.csv"
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

rl_avr_performance <- ggplot(filtered2, aes(x=trial,y=cumulativeWinA, 
                      group=learningRate, color = as.factor(learningRate)
)) + 
  stat_summary(fun=mean, geom="line") +
  geom_hline(yintercept=0.5) + 
  ylim(0, 1) + 
  theme_classic() + 
  labs(
    title = "Average 'RL' performance vs. WSLS",
    subtitle = "Extreme learning rates causes bad performance",
    x = "Trial Number",
    y = "Average Proportion Wins",
    color = "Own Learning Rate"
  ) 

ggsave("output/avg_rl_performance_by_learning.png", rl_avr_performance, width = 12)

## FACET WRAP -  by noise

rl_avr_performance_noise <- ggplot(raw, aes(x=trial,y=cumulativeWinA, 
                                            group=learningRate, color = as.factor(learningRate)
)) + 
  stat_summary(fun=mean, geom="line") +
  geom_hline(yintercept=0.5) + 
  ylim(0, 1) + 
  theme_classic() + 
  labs(
    title = "Average 'RL' performance vs. WSLS",
    subtitle = "The more noise the more it says in equilibrium",
    x = "Trial Number",
    y = "Average Proportion Wins",
    color = "Own Learning Rate"
  ) + 
  facet_wrap(~ noise)

ggsave("output/avg_rl_performance_by_learning_and_noise.png", rl_avr_performance_noise, width = 12)


