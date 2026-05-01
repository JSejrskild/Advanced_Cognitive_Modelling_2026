data {
  int<lower=1> t; // Total trial numbers
  int<lower=1> n_subjects; // Number of unique subjects
  array[t] int<lower=1, upper=n_subjects> subject_id;
  array[t] int<lower=0, upper=7> choice_1;
  array[t] int<lower=0, upper=7> group_rating;
  array[t] int<lower=0, upper=7> choice_2;
}

parameters {
  vector<lower=0, upper=1>[n_subjects] p; // Allocate to direct evidence, per subject
}

model {
  // Prior for each subject's p: Beta(2, 2)
  for (s in 1:n_subjects) {
    target += beta_lpdf(p[s] | 2, 2);
  }

  // Likelihood, per trial
  for (i in 1:t) {
    int s = subject_id[i];
    real alpha_post = 0.5 + p[s] * choice_1[i] + (1 - p[s]) * group_rating[i];
    real beta_post  = 0.5 + p[s] * (7 - choice_1[i]) + (1 - p[s]) * (7 - group_rating[i]);
    target += beta_binomial_lpmf(choice_2[i] | 7, alpha_post, beta_post);
  }
}

generated quantities {
  vector[t] log_lik;
  array[t] int prior_pred;
  array[t] int posterior_pred;
  vector[n_subjects] lprior;
  vector[n_subjects] p_prior;

  // Prior and log-prior for each subject
  for (s in 1:n_subjects) {
    p_prior[s] = beta_rng(2, 2);
    lprior[s] = beta_lpdf(p[s] | 2, 2);
  }

  // Per-trial predictions
  for (i in 1:t) {
    int s = subject_id[i];
    // Prior predictive
    real alpha_prior = 0.5 + p_prior[s] * choice_1[i] + (1 - p_prior[s]) * group_rating[i];
    real beta_prior  = 0.5 + p_prior[s] * (7 - choice_1[i]) + (1 - p_prior[s]) * (7 - group_rating[i]);
    prior_pred[i] = beta_binomial_rng(7, alpha_prior, beta_prior) + 1;

    // Posterior predictive
    real alpha_post = 0.5 + p[s] * choice_1[i] + (1 - p[s]) * group_rating[i];
    real beta_post  = 0.5 + p[s] * (7 - choice_1[i]) + (1 - p[s]) * (7 - group_rating[i]);
    log_lik[i] = beta_binomial_lpmf(choice_2[i] | 7, alpha_post, beta_post);
    posterior_pred[i] = beta_binomial_rng(7, alpha_post, beta_post) + 1;
  }
}
