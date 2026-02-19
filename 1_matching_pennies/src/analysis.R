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

# cumulativeWin Rate of RL-R subplotted by kSwitch
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

# One winning agent pair dynamics in 
single_agent <- 10
filtered3 <- raw %>%
  filter(kSwitch==3, learningRate==1, noise==0, agent_id==single_agent) %>% 
  mutate(losingA = ifelse(losingA==TRUE, 1, 0) )

# zoom in on dynamics?
rlr_winner_dynamic_zoom <- ggplot(filtered3, aes(x = trial)) + 
  
  geom_line(aes(y = choicesA, color = "Choice RL-R"), linetype = "dotted", alpha = .8) +
  geom_line(aes(y = choicesB, color = "Choice WSLS"), linetype = "dotted", alpha = .8) +
  geom_point(aes(y = winA, color = "Win/Loss (RL-R)")) +
  geom_line(aes(y = cumulativeWinA, color = "Cumulative Win (RL-R)")) +
  geom_line(aes(y = losingA, color = "'Losing' mode (RL-R)"), linetype = "dashed") +
  
  geom_hline(yintercept = 0.5) +
  ylim(0, 1) +
  xlim(0, 30) +
  theme_classic() +
  
  scale_color_manual(
    name = "Legend",
    values = c(
      "Choice RL-R" = "blue",
      "Choice WSLS" = "red",
      "Win/Loss (RL-R)" = "green",
      "Cumulative Win (RL-R)" = "darkgreen",
      "'Losing' mode (RL-R)" = "orange"
    )
  ) +
  labs(
    title = "Agent pair 10: RL-R vs. WSLS",
    subtitle = "Example of RL-R winning game behaviour (zoomed in, 30 of 120 trials)",
    x = "Trial Number",
    y = "Average Proportion Wins"
  )
rlr_winner_dynamic_zoom

ggsave("output/rlr_winner_dynamic_zoom.png", rlr_winner_dynamic_zoom, width = 12)

# Then zooming out sampling 9 pseudo-random agent-pairs and showing all trials
filtered4 <- raw %>%
  filter(kSwitch==3, learningRate==1, noise==0) %>% 
  mutate(losingA = ifelse(losingA==TRUE, 1, 0) )

set.seed(271)
sampled_ids <- filtered4 %>%
  distinct(agent_id) %>%
  pull(agent_id) %>%
  sample(9)

sampled_filtered <- filtered4 %>%
  filter(agent_id %in% sampled_ids)

rlr_winner_sampled <- ggplot(sampled_filtered, aes(x = trial)) + 
  geom_line(aes(y = choicesA, color = "Choice RL-R"),
            linetype = "dotted", alpha = .8) +
  geom_line(aes(y = choicesB, color = "Choice WSLS"),
            linetype = "dotted", alpha = .8) +
  geom_point(aes(y = winA, color = "Win/Loss (RL-R)")) +
  geom_line(aes(y = cumulativeWinA, color = "Cumulative Win (RL-R)")) +
  geom_line(aes(y = losingA, color = "'Losing' mode (RL-R)"),
            linetype = "dashed") +
  geom_hline(yintercept = 0.5) +
  ylim(0, 1) +
  xlim(0, 25) +
  theme_classic() +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Choice RL-R" = "blue",
      "Choice WSLS" = "red",
      "Win/Loss (RL-R)" = "green",
      "Cumulative Win (RL-R)" = "darkgreen",
      "'Losing' mode (RL-R)" = "orange"
    )
  ) +
  labs(
    title = "9 Randomly sampled agent pairs RL-R vs WSLS simulations",
    subtitle = "Examples of RL-R winning game behaviour (120 of 120 trials)",
    x = "Trial Number",
    y = "Average Proportion Wins"
  ) +
  facet_wrap(~ agent_id)

rlr_winner_sampled

ggsave("output/rlr_winner_dynamic_samples.png", rlr_winner_sampled, width = 12)

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

# Cumulative win rate for RL agent, noise=0, colored by learning
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


