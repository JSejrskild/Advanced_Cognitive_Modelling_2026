data {
  int<lower=1> t; // Total trial numbers
  int<lower=1> n_subjects; // Number of unique subjects
  array[t] int<lower=1, upper=n_subjects> subject_id;
  array[t] int<lower=0, upper=7> choice_1;
  array[t] int<lower=0, upper=7> group_rating;
  array[t] int<lower=0, upper=7> choice_2;
}

parameters {
  vector<lower=0, upper=1>[n_subjects] rho;    // Relative weight, per subject
  vector<lower=0>[n_subjects]          kappa;  // Total weight, per subject
}

transformed parameters {
  vector<lower=0>[n_subjects] wd; // Self-directed weight, per subject
  vector<lower=0>[n_subjects] ws; // Social weight, per subject

  for (s in 1:n_subjects) {
    wd[s] = rho[s] * kappa[s];
    ws[s] = (1.0 - rho[s]) * kappa[s];
  }
}

model {
  // Priors for each subject
  for (s in 1:n_subjects) {
    target += beta_lpdf(rho[s] | 2, 2); // Weakly centered on equal weighting
    target += lognormal_lpdf(kappa[s] | log(2), 0.5); // Centered on 2 (SBA equivalent)
  }

  // Likelihood, per trial
  for (i in 1:t) {
    int s = subject_id[i];
    real alpha_post = 0.5 + wd[s] * choice_1[i] + ws[s] * group_rating[i];
    real beta_post  = 0.5 + wd[s] * (7 - choice_1[i]) + ws[s] * (7 - group_rating[i]);
    target += beta_binomial_lpmf(choice_2[i] | 7, alpha_post, beta_post);
  }
}

generated quantities {
  vector[t] log_lik;
  array[t] int prior_pred;
  array[t] int posterior_pred;
  vector[n_subjects] lprior;

  // Priors for each subject
  vector[n_subjects] rho_prior;
  vector[n_subjects] kappa_prior;
  vector[n_subjects] wd_prior;
  vector[n_subjects] ws_prior;

  for (s in 1:n_subjects) {
    rho_prior[s]   = beta_rng(2, 2);
    kappa_prior[s] = lognormal_rng(log(2), 0.5);
    wd_prior[s]    = rho_prior[s] * kappa_prior[s];
    ws_prior[s]    = (1.0 - rho_prior[s]) * kappa_prior[s];
    lprior[s]      = beta_lpdf(rho[s] | 2, 2) + lognormal_lpdf(kappa[s] | log(2), 0.5);
  }

  // Per-trial predictions
  for (i in 1:t) {
    int s = subject_id[i];
    // Prior predictive
    real alpha_prior = 0.5 + wd_prior[s] * choice_1[i] + ws_prior[s] * group_rating[i];
    real beta_prior  = 0.5 + wd_prior[s] * (7 - choice_1[i]) + ws_prior[s] * (7 - group_rating[i]);
    prior_pred[i] = beta_binomial_rng(7, alpha_prior, beta_prior) + 1;

    // Posterior predictive
    real alpha_post = 0.5 + wd[s] * choice_1[i] + ws[s] * group_rating[i];
    real beta_post  = 0.5 + wd[s] * (7 - choice_1[i]) + ws[s] * (7 - group_rating[i]);
    log_lik[i] = beta_binomial_lpmf(choice_2[i] | 7, alpha_post, beta_post);
    posterior_pred[i] = beta_binomial_rng(7, alpha_post, beta_post) + 1;
  }
}
