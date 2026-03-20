data{
// All data is observable 
  int<lower = 1> t; // Trial numbers (t)
  array[t] int choice; // One choice for each trial
  array[t] int feedback; // One piece of feedback for each trial
  real initialV; // Initial belief (V[1])
  real alpha_prior_mu; // mean LR in prior distribution specified 
  real<lower = 0> alpha_prior_sd; //standard deviation of LR in prior distribution
}

parameters{
// All parameters are inferable
<<<<<<< HEAD
  real<lower = 0, upper = 1> alpha_logit; // We have a learning rate (alpha) that we want to infer
=======
  real alpha_logit; // We have a learning rate (alpha) that we want to infer
>>>>>>> 248bf4b7f5777c7d056acbca6daf5a694c0acc9d
}

model{
  array[t] real V; // Each trial has a belief/rate (V)
  V[1] = initialV; // Set the first belief to the hardcoded initialV
  
  // Priors
  target += normal_lpdf(alpha_logit | alpha_prior_mu, alpha_prior_sd ); //prior as normal distribution on the logit scale 
  
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V[i] = V[i-1] + inv_logit(alpha_logit) * (feedback[i-1] - V[i-1]); //RW update of value estimate, learning 
    target += binomial_lpmf(choice[i] | 1, V[i]); // probability of choice given the current belief
  }
}

generated quantities {
  //Prior
<<<<<<< HEAD
  real<lower = 0, upper = 1> alpha_prior;
<<<<<<< HEAD
  array[t] real V_prior; // V (temporary) for prior
  V_prior[1] = initialV; // Set the first belief to the hardcoded initialV
  array[t] int choice_prior_pred;
  //Posterior
  real<lower = 0, upper = 1> alpha;
  array[t] real V_posterior; // V (temporary) for posterior
  V_posterior[1] = initialV; // Set the first belief to the hardcoded initialV
  array[t] int choice_post_pred;
  
  // Draw a prior predictive sample
=======
  array[t] int choice_prior_pred;
=======
  real<lower = 0, upper = 1> alpha_prior; // bounds for prior
  array[t] int choice_prior_pred; // one simulated choice for each trial
>>>>>>> 3288e556f1350367954043f7710dd0cf7100a392
  //Posterior
  real<lower = 0, upper = 1> alpha; // bounds for posterior
  array[t] int choice_post_pred; // one simulated choice for each trial
  // temporary rate
  real V_tmp; // temporary value estimate variable
  
  // Draw a prior (predictive) sample
>>>>>>> 248bf4b7f5777c7d056acbca6daf5a694c0acc9d
  alpha_prior = inv_logit(normal_rng(alpha_prior_mu, alpha_prior_sd));
  // Transform posterior
  alpha = inv_logit(alpha_logit);
  
  // Prior Predictive 
<<<<<<< HEAD
<<<<<<< HEAD
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V_prior[i] = V_prior[i-1] + alpha_prior * (feedback[i-1] - V_prior[i-1]);
    choice_prior_pred[i] = bernoulli_rng(V_prior[i]);
  }
  
  // Posterior predictive
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V_posterior[i] = V_posterior[i-1] + alpha * (feedback[i-1] - V_posterior[i-1]);
    choice_post_pred[i] = bernoulli_rng(V_posterior[i]);
  }
  
  // 
=======
  V_tmp = initialV;
  choice_prior_pred[1] = bernoulli_rng(V_tmp);
=======
  V_tmp = initialV; // Set the first temporary belief to the hardcoded initialV
  choice_prior_pred[1] = bernoulli_rng(V_tmp); // random first choice 
>>>>>>> 3288e556f1350367954043f7710dd0cf7100a392
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V_tmp = V_tmp + alpha_prior * (feedback[i-1] - V_tmp); //RW update of value estimate from drawn alpha values from prior
    choice_prior_pred[i] = bernoulli_rng(V_tmp); //choice based on updated belief
  }
>>>>>>> 248bf4b7f5777c7d056acbca6daf5a694c0acc9d
  
  // Posterior predictive
  V_tmp = initialV; // Set the first temporary belief to the hardcoded initialV
  choice_post_pred[1] = bernoulli_rng(V_tmp); // random first choice 
  for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
    V_tmp = V_tmp + alpha * (feedback[i-1] - V_tmp); //RW update of value estimate from posterior alpha value
    choice_post_pred[i] = bernoulli_rng(V_tmp); //choice based on updated belief
  }

} 




