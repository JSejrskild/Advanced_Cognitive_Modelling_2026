data{
// All data is observable 
  int<lower = 1> t; // Trial numbers (t)
  array[t] int choice; // One choice for each trial
  array[t] int feedback; // One piece of feedback for each trial
  real initialV; // Initial belief (V[1])
  real alpha_prior_mu;
  real<lower = 0> alpha_prior_sd;
}

parameters{
// All parameters are inferable
  real<lower = 0, upper = 1> alpha_logit; // We have a learning rate (alpha) that we want to infer
}

model{
  array[t] real V; // Each trial has a belief/rate (V)
  V[1] = initialV; // Set the first belief to the hardcoded initialV
  
  // Priors
  target += normal_lpdf(alpha_logit | alpha_prior_mu, alpha_prior_sd );
  
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V[i] = V[i-1] + inv_logit(alpha_logit) * (feedback[i-1] - V[i-1]);
    target += binomial_lpmf(choice[i] | 1, V[i]);
  }
}

generated quantities {
  //Prior
  real<lower = 0, upper = 1> alpha_prior;
  array[t] real V_prior; // V (temporary) for prior
  V_prior[1] = initialV; // Set the first belief to the hardcoded initialV
  array[t] int choice_prior_pred;
  //Posterior
  real<lower = 0, upper = 1> alpha;
  array[t] real V_posterior; // V (temporary) for posterior
  V_posterior[1] = initialV; // Set the first belief to the hardcoded initialV
  array[t] int choice_post_pred;
  
  // Draw a prior predictive sample
  alpha_prior = inv_logit(normal_rng(alpha_prior_mu, alpha_prior_sd));
  // Transform posterior
  alpha = inv_logit(alpha_logit);
  
  // Prior Predictive 
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V_prior[i] = V_prior[i-1] + alpha_prior * (feedback[i-1] - V_prior[i-1]);
    choice_prior_pred[i] = bernoulli_rng(V_prior[i]);
  }
  
  // Posterior predictive
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V_posterior[i] = V_posterior[i-1] + alpha * (feedback[i-1] - V_posterior[i-1]);
    choice_post_pred[i] = bernoulli_rng(V_posterior[i]);
  }
  
  // deletet unnecessary parameters
  
} 




