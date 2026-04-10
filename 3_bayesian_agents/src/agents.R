# Agents for 3rd Assignment

WBA_agent_f <- function(n_trials, N_sample, alpha, beta, ws, wd){
  
  # Make a choice number 1 by sampling from the beta_binomial
  probablity = rbeta(N_sample, alpha, beta)
  choice_1 = rbinom(N_sample, size = 7, prob = probablity)
  
  # call group_feedback 
  
  
  # Make choice number 2 by adding the feedback
  alpha_post = 0.5 + wd * k1 + ws *k2
  beta_post = 0.5 + wd *(n_trials - k1) + ws * (n_trials - k2)
   
}
  
Group_feedback_f <- function(choice_1){
  # Sørg for at det tal der bliver lagt til/taget fra choice_1 er ligeligt fordelt over alle n_trials
  
  
}
  
  
#PBA_agent_f <-
  
