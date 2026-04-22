data{
// All data is observable 
  int<lower = 1> n_subjects;
  int<lower = 1> t; // Trial numbers (t)
  
  array[n_subjects, t] int choice; // One choice for each trial
  array[n_subjects, t] int feedback; // One piece of feedback for each trial
  
  real initialV; // Initial belief (V[1])
  real alpha_prior_mu;
  real<lower = 0> alpha_prior_sd;
}

parameters{
// All parameters are inferable
  real alpha_logit; // We have a learning rate (alpha) that we want to infer
}

model{
  // Priors
  target += normal_lpdf(alpha_logit | alpha_prior_mu, alpha_prior_sd );
  
  real alpha = inv_logit(alpha_logit); // compute the alpha between 0 and 1
  
  // loop over subjects
  for (s in 1:n_subjects) {
    
    // Loop accross trials
    real V = initialV; // Set the first belief to the hardcoded initialV
    for (i in 2:t) { // Ensures that we have first trial to get feedback and belief from
    
      V = V + alpha * (feedback[s, i-1] - V);
      
      target += binomial_lpmf(choice[s, i] | 1, V);
    }
  }
}

generated quantities {
  //Prior
  real<lower = 0, upper = 1> alpha_prior;
  array[n_subjects, t] int choice_prior_pred;
  //Posterior
  real<lower = 0, upper = 1> alpha;
  array[n_subjects, t] int choice_post_pred;
  // temporary rate
  real V_tmp;
  
  // Draw a prior (predictive) sample
  alpha_prior = inv_logit(normal_rng(alpha_prior_mu, alpha_prior_sd));
  // Transform posterior
  alpha = inv_logit(alpha_logit);
  
  
  // Generate predictions for every subject
  for (s in 1:n_subjects) {
    
    // Prior Predictive 
    V_tmp = initialV;
    choice_prior_pred[s, 1] = bernoulli_rng(V_tmp);
    for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
      V_tmp = V_tmp + alpha_prior * (feedback[s, i-1] - V_tmp);
      choice_prior_pred[s, i] = bernoulli_rng(V_tmp);
    }
    
    // Posterior predictive
    V_tmp = initialV;
    choice_post_pred[s, 1] = bernoulli_rng(V_tmp);
    for (i in 2:t){ // Ensures that we have first trial to get feedback and belief from
      V_tmp = V_tmp + alpha * (feedback[s, i-1] - V_tmp);
      choice_post_pred[s, i] = bernoulli_rng(V_tmp);
    }
  }
    
} 




