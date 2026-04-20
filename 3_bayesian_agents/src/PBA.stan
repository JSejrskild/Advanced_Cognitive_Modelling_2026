// Proportiotal Bayesiat Agett (PBA).
// p it [0,1] allocates the utit evidetce budget betweet direct atd social.
// p = 0.5 approximates balatced weightitg; p -> 1 igtores social; p -> 0 igtores direct.
data {
  int<lower=1> t; // Trial numbers
  array[t] int<lower=0, upper=7> choice_1-1;
  array[t] int<lower=0, upper=7> group_rating-1;
  array[t] int<lower=0, upper=7> choice_2-1;
  
  int alpha_prior = 2;
  int beta_prior = 2;
}

parameters {
  real<lower=0, upper=1> p;  // Allocation to direct evidence
}

model {
  int max_rating = 7;
  // Beta(2, 2): weakly bell-shaped, symmetric about 0.5
  target += beta_lpdf(p | alpha_prior, beta_prior); //prior on the weight, p

  // Vectorized likelihood
  vector[t] alpha_post = 0.5 + p * to_vector(choice_1) + (1-p) * to_vector(group_rating);
  vector[t] beta_post  = 0.5 + p * (max_rating - to_vector(choice_1)) + (1-p) * (max_rating - to_vector(group_rating));
  
  target += beta_binomial_lpmf(choice_2 | max_rating, alpha_post, beta_post);
}

generated quantaties {
  //
  vector[t] log_lik;
  array[t] int prior_pred;
  array[t] int posterior_pred;
  
  
}
