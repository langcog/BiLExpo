data {
  int<lower=1> N;              // number of observations
  int<lower=1> I;              // number of children
  int<lower=1> W;              // number of words (uni_lemma)
  int<lower=1> J;              // number of items
  int<lower=1> L;              // number of languages
  int<lower=0,upper=1> y[N];   // binary outcome: word produced or not
  vector[N] age;               // age of the child
  vector[N] exposure;          // exposure proportion for each observation
  int<lower=1,upper=I> child[N];    // child_id
  int<lower=1,upper=W> word[N];     // uni_lemma (word random effect)
  int<lower=1,upper=J> item[N];     // item_id
  int<lower=1,upper=L> lang[N];     // language (1 or 2)
}

parameters {
  // Fixed effects
  real alpha;                       // intercept
  real beta_age;                    // fixed effect for age
  real beta_exposure;               // fixed effect for exposure proportion
  real beta_age_exposure;           // interaction: age * exposure

  // Random effects (group-level)
  vector[I] u_child;                // child-level random intercepts
  vector[W] u_word;                 // word-level random intercepts (uni_lemma)
  vector[J] u_item;                 // item-level random intercepts
  vector[L] u_lang;                 // language-level random intercepts

  // Hyperparameters for random effects
  real<lower=0> sigma_child;        // std dev of child random intercepts
  real<lower=0> sigma_word;         // std dev of word random intercepts
  real<lower=0> sigma_item;         // std dev of item random intercepts
  real<lower=0> sigma_lang;         // std dev of language random intercepts
}

transformed parameters {
  vector[N] eta;                   // linear predictor for logit

  // Compute eta here
  for (n in 1:N) {
    eta[n] = alpha 
             + beta_age * age[n] 
             + beta_exposure * exposure[n]
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
  beta_age_exposure ~ normal(0, 1);

  // Random effects priors
  u_child ~ normal(0, sigma_child);
  u_word ~ normal(0, sigma_word);
  u_item ~ normal(0, sigma_item);
  u_lang ~ normal(0, sigma_lang);

  // Hyperpriors for random effects std deviations
  sigma_child ~ cauchy(0, 2.5);
  sigma_word ~ cauchy(0, 2.5);
  sigma_item ~ cauchy(0, 2.5);
  sigma_lang ~ cauchy(0, 2.5);
  
  // Bernoulli likelihood for word production
  y ~ bernoulli_logit(eta);
}
generated quantities {
  // Predictive draws for each observation
  int y_rep[N];
  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(eta[n]);
  }
  
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | eta[n]);
  }
}
