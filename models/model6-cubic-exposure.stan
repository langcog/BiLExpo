data {
  int<lower=1> N;              // number of observations
  int<lower=1> I;              // number of children
  int<lower=1> W;              // number of words (uni_lemma)
  int<lower=1> J;              // number of items
  int<lower=1> L;              // number of languages
  int<lower=0,upper=1> y[N];   // binary outcome: word produced or not
  vector[N] age;               // age of the child
  vector[N] exposure;          // exposure proportion
  int<lower=1,upper=I> child[N];
  int<lower=1,upper=W> word[N];
  int<lower=1,upper=J> item[N];
  int<lower=1,upper=L> lang[N];
}

parameters {
  // Fixed effects
  real alpha;
  real beta_age;
  real beta_exposure;
  real beta_exposure2;
  real beta_exposure3;  
  real beta_age_exposure;

  // Random effects
  vector[I] u_child;
  vector[W] u_word;
  vector[J] u_item;
  vector[L] u_lang;

  // Hyperparameters
  real<lower=0> sigma_child;
  real<lower=0> sigma_word;
  real<lower=0> sigma_item;
  real<lower=0> sigma_lang;
}

transformed parameters {
  vector[N] eta;
  for (n in 1:N) {
    eta[n] = alpha
           + beta_age * age[n]
           + beta_exposure * exposure[n]
           + beta_exposure2 * square(exposure[n])
           + beta_exposure3 * exposure[n] * square(exposure[n]) 
           + beta_age_exposure * age[n] * exposure[n]
           + u_child[child[n]]
           + u_word[word[n]]
           + u_item[item[n]]
           + u_lang[lang[n]];
  }
}

model {
  // Priors
  alpha ~ normal(0, 5);
  beta_age ~ normal(0, 1);
  beta_exposure ~ normal(0, 1);
  beta_exposure2 ~ normal(0, 1);
  beta_exposure3 ~ normal(0, 1);  
  beta_age_exposure ~ normal(0, 1);

  u_child ~ normal(0, sigma_child);
  u_word ~ normal(0, sigma_word);
  u_item ~ normal(0, sigma_item);
  u_lang ~ normal(0, sigma_lang);

  sigma_child ~ cauchy(0, 2.5);
  sigma_word ~ cauchy(0, 2.5);
  sigma_item ~ cauchy(0, 2.5);
  sigma_lang ~ cauchy(0, 2.5);

  y ~ bernoulli_logit(eta);
}

generated quantities {
  int y_rep[N];
  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(eta[n]);
  }
  
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | eta[n]);
  }
}