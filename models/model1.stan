data {
  int<lower=1> N;           // number of observations
  int<lower=1> I;           // number of children
  int<lower=1> W;           // number of words
  int<lower=1> C;           // number of concepts
  int<lower=1> L;           // number of languages (e.g., 2)
  int<lower=1,upper=I> child[N];
  int<lower=1,upper=W> word[N];
  int<lower=1,upper=C> concept[C]; // mapping from word to concept
  int<lower=1,upper=L> lang[W];    // language indicator for each word
  vector[N] age;
  matrix[I, L] exposure;    // exposure for each child in each language
  int<lower=0,upper=1> y[N]; // observed responses (word produced or not)
}
parameters {
  // Concept difficulties
  vector[C] theta;
  real mu_theta;
  real<lower=0> sigma_theta;
  
  // Word-level deviation per language
  vector[W] delta;
  real<lower=0> sigma_delta;
  
  // Child abilities (could be centered)
  vector[I] alpha;
  
  // Coefficients for age and exposure effects (language-specific if desired)
  real gamma;           // effect of age
  vector[L] lambda;     // effect of exposure in each language
}
transformed parameters {
  // For each observation, we can compute the linear predictor.
  vector[N] eta;
  for (n in 1:N) {
    int w = word[n];
    int c = concept[w];
    int l = lang[w];
    real beta = theta[c] + delta[w];
    eta[n] = alpha[child[n]] + gamma * age[n] + lambda[l] * exposure[child[n], l] - beta;
  }
}
model {
  // Priors
  mu_theta ~ normal(0, 5);
  sigma_theta ~ cauchy(0, 2.5);
  sigma_delta ~ cauchy(0, 2.5);
  theta ~ normal(mu_theta, sigma_theta);
  delta ~ normal(0, sigma_delta);
  alpha ~ normal(0, 1);
  gamma ~ normal(0, 1);
  lambda ~ normal(0, 1);
  
  // Likelihood
  y ~ bernoulli_logit(eta);
}
generated quantities {
  // Predictive draws for each observation
  int y_rep[N];
  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(eta[n]);
  }
}
