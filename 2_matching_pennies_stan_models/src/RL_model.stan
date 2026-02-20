data{
# All data is observable 
  int<lower = 1> t; // Trial numbers (t)
  array[t] int choice; // One choice for each trial
  array[t] int feedback; // One piece of feedback for each trial
  real initialV; // Initial belief (V[1])
}

parameters{
# All parameters are inferable
  real<lower = 0, upper = 1> alpha; // We have a learning rate (alpha) that we want to infer
}

model{
  array[t] real<lower = 0, upper = 1> V; // Each trial has a belief/rate (V)
  V[1] = initialV; // Set the first belief to the hardcoded initialV
  
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V[i] ~ V[i-1] + alpha * (feedback[i-1] - V[i-1]);
    choice[i] ~ binomial(1, V[i]);
  }
}

