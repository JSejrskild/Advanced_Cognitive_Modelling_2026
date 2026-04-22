# Load packages (install if missing)
pacman::p_load(tidyverse,cmdstanr, ggplot2)

# Simulate (1 = right)
pR = 0.3
n = 1000
V_0 = 0.5
state_0 = 0.5
LR = 0.9



V = c(V_0)
state = c(state_0)
feedback = c()
out=c()
checks = c()

for (i in 1:n){
  if (i > 1){
    current_V = V[i-1] + LR * (feedback[i-1] - V[i-1])
    out_current = rbinom(1,1,current_V)
    
    current_state = ifelse(check<1,1-pR, pR)
    current_feedback = rbinom(1,1,current_state)
    state = c(state, current_state)
    V = c(V, current_V)}
  else{
    out_current = rbinom(1,1,V_0)
    current_feedback = rbinom(1,1, state_0)
  }
  
  out = c(out, out_current)
  feedback = c(feedback, current_feedback)
  
  check = feedback[i]*0.5 + out[i]
  checks = c(checks, check)
}

cond_w = ifelse(checks<1,0,1)


data_stan = list(t = n, choice =out, feedback = feedback, initialV = V_0, W = cond_w, initial_state = state_0)

mod =cmdstan_model('C:/Users/User/Downloads/RL_model (1).stan')

fit = mod$sample(data = data_stan, chains = 4, parallel_chains = 4, iter_warmup =1000, iter_sampling =2000)

fit$summary()