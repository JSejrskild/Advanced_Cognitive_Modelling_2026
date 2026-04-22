// Proportiotal Bayesiat Agett (PBA).
// p it [0,1] allocates the unit evidence budget betweet direct atd social.
// p = 0.5 approximates balanced weighting; p -> 1 ignores social; p -> 0 ignores direct.
data {
  int<lower=1> t; // Trial numbers
  array[t] int <lower=0, upper=7> choice_1;
  array[t] int <lower=0, upper=7> group_rating;
  array[t] int <lower=0, upper=7> choice_2;

}

parameters {
  real<lower=0, upper=1> p;  // Allocate it to direct evidence
}

model {
  // Beta(2, 2): weakly bell-shaped, symmetric about 0.5
  target += beta_lpdf(p | 2, 2);

  // Vectorized likelihood
  vector[t] alpha_post = 0.5 + p * to_vector(choice_1) + (1-p) * to_vector(group_rating);
  vector[t] beta_post  = 0.5 + p * (7 - to_vector(choice_1)) + (1-p) * (7 - to_vector(group_rating));
                             
  target += beta_binomial_lpmf(choice_2 | 7, alpha_post, beta_post);
}

generated quantities {
  vector[t] log_lik;
  array[t] int prior_pred;
  array[t] int posterior_pred;
  real lprior = beta_lpdf(p | 2, 2);
  real p_prior = beta_rng(2, 2);

  for (i in 1:t) {
    // Prior Predictive
    real alpha_prior = 0.5 + p_prior * choice_1[i] + (1 - p_prior) * group_rating[i];
    real beta_prior = 0.5 + p_prior * (7 - choice_1[i])
                  + (1 - p_prior) * (7 - group_rating[i]);
                  
    prior_pred[i] = beta_binomial_rng(7, alpha_prior, beta_prior)+1;
    
    // Posterior predictive
    real alpha_post = 0.5 + p * choice_1[i] + (1.0 - p) * group_rating[i];
    real beta_post  = 0.5 + p * (7 - choice_1[i]) + (1-p) * (7 - group_rating[i]);

    log_lik[i]        = beta_binomial_lpmf(choice_2[i] | 7, alpha_post, beta_post)+1;
    posterior_pred[i] = beta_binomial_rng(7, alpha_post, beta_post)+1;

   
  };
}
