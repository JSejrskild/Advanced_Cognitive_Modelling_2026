// WBA model in stan

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
  real<lower=0, upper=1> rho;    // relative weight/ratio: w_d / (w_d + w_s)
  real<lower=0>          kappa;  // total weight: w_d + w_s
}

transformed parameters {
  real<lower=0> wd = rho * kappa; // Self-directed weight
  real<lower=0> ws = (1.0 - rho) * kappa; // Social weight
}


model {
 // rho: weakly centred on equal weighting
  target += beta_lpdf(rho | 2, 2); // Prior
  // kappa: lognormal centered on 2 (SBA equivalent)
  target += lognormal_lpdf(kappa | log(2), 0.5); // Prior

  // Vectorized likelihood
  vector[t] alpha_post = 0.5 + wd * to_vector(choice_1) + ws * to_vector(group_rating);
  vector[t] beta_post  = 0.5 + wd * (7 - to_vector(choice_1)) + ws * (7 - to_vector(group_rating));
                             
  target += beta_binomial_lpmf(choice_2 | 7, alpha_post, beta_post);
}

generated quantities {
  vector[t] log_lik;
  array[t] int prior_pred;
  array[t] int posterior_pred;
  real lprior = beta_lpdf(rho | 2, 2) + lognormal_lpdf(kappa | log(2), 0.5);
  
  // Priors
  real rho_prior   = beta_rng(2, 2);
  real kappa_prior = lognormal_rng(log(2), 0.5);
  real wd_prior    = rho_prior * kappa_prior;
  real ws_prior    = (1.0 - rho_prior) * kappa_prior;

  for (i in 1:t) {
    // Prior Predictive
    real alpha_prior = 0.5 + wd_prior * choice_1[i] + ws_prior * group_rating[i];
    real beta_prior = 0.5 + wd_prior * (7 - choice_1[i])
                  + ws_prior * (7 - group_rating[i]);
                  
    prior_pred[i] = beta_binomial_rng(7, alpha_prior, beta_prior) + 1;
    
    // Posterior predictive
    real alpha_post = 0.5 + wd * choice_1[i] + ws * group_rating[i];
    real beta_post  = 0.5 + wd * (7 - choice_1[i]) + ws * (7 - group_rating[i]);

    log_lik[i]        = beta_binomial_lpmf(choice_2[i] | 7, alpha_post, beta_post)+1;
    posterior_pred[i] = beta_binomial_rng(7, alpha_post, beta_post)+1;

   
  };
}
