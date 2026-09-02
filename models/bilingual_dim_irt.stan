// Crossed random-effects IRT for the dimensionality of bilingual lexical ability.
//
// One row per (child, item) observation, pooled across BOTH languages of a pair.
// Item difficulty is decomposed into a translation-equivalent "concept" term
// (shared by e.g. dog / perro via uni_lemma) plus a language-specific offset.
// Child ability is decomposed into a GENERAL trait and a LANGUAGE-SPECIFIC
// deviation:
//
//     theta_child[i, L] = lambda_g * theta_g[i] + theta_s[i, L]
//     theta_g[i]        ~ normal(0, 1)                       // scale fixed here
//     theta_s[i, L]     ~ normal(0, sigma_s[L])
//
// Nested hypotheses (set via data flags):
//   estimate_general=0, estimate_specific=1  -> two independent lexicons (rho = 0)
//   estimate_general=1, estimate_specific=0  -> strictly unidimensional (rho = 1)
//   estimate_general=1, estimate_specific=1  -> general + specific; estimate rho, ECV
//
// Parameterisation matches the repo's 2PL: p = inv_logit(alpha_j * (ability + b_j)),
// where b_j is item EASINESS (higher = easier) and alpha_j = exp(logalpha_j).

data {
  int<lower=1> N;                       // observations
  int<lower=1> I;                       // children
  int<lower=1> J;                       // items (language-specific)
  int<lower=1> C;                       // concepts (uni_lemma; singletons allowed)
  int<lower=1,upper=2> L;               // languages in the pair (==2)

  array[N] int<lower=0,upper=1> y;      // produces
  array[N] int<lower=1,upper=I> child;
  array[N] int<lower=1,upper=J> item;
  vector[N] age_sc;                     // centred/scaled age at administration

  array[J] int<lower=1,upper=C> concept;    // item -> concept (translation equiv.)
  array[J] int<lower=1,upper=L> item_lang;  // item -> language

  matrix[I, L] exposure_c;              // centred exposure proportion, per child x language
  array[I] int<lower=1,upper=L> dominant_lang; // unused by likelihood; handy in GQ

  int<lower=0,upper=1> estimate_general;
  int<lower=0,upper=1> estimate_specific;
  int<lower=0,upper=1> compute_loglik;
}

transformed data {
  // language index for each observation
  array[N] int obs_lang;
  for (n in 1:N) obs_lang[n] = item_lang[item[n]];
}

parameters {
  real intercept;                       // grand mean easiness

  // ---- item side ----
  vector[C] z_concept;                  // concept easiness (non-centred)
  real<lower=0> sigma_concept;
  vector[J] z_delta;                    // language-specific item offset (non-centred)
  real<lower=0> sigma_delta;
  vector[J] logalpha;                   // log discrimination
  real<lower=0> sigma_logalpha;

  // ---- person side ----
  vector[I] z_theta_g;                                  // general ability (non-centred)
  real<lower=0> lambda_g_raw;                           // general loading (>=0 for identifiability)
  matrix[I, L] z_theta_s;                               // language-specific ability (non-centred)
  vector<lower=0>[L] sigma_s_raw;

  // ---- exposure / age fixed effects (per language) ----
  vector[L] beta_exp;
  real beta_age;
  vector[L] beta_age_exp;
}

transformed parameters {
  real lambda_g       = estimate_general  ? lambda_g_raw : 0;
  vector[L] sigma_s   = estimate_specific ? sigma_s_raw  : rep_vector(0.0, L);

  vector[C] concept_easiness = sigma_concept * z_concept;
  vector[J] delta            = sigma_delta   * z_delta;
  vector[I] theta_g          = z_theta_g;                       // already N(0,1)
  matrix[I, L] theta_s;
  for (l in 1:L) theta_s[, l] = sigma_s[l] * z_theta_s[, l];

  vector[J] b_item;   // total item easiness
  for (j in 1:J) b_item[j] = intercept + concept_easiness[concept[j]] + delta[j];
}

model {
  // ---- priors ----
  intercept ~ normal(0, 3);

  z_concept ~ std_normal();
  sigma_concept ~ normal(0, 2);
  z_delta ~ std_normal();
  sigma_delta ~ normal(0, 1);
  logalpha ~ normal(0, sigma_logalpha);
  sigma_logalpha ~ normal(0, 1);

  z_theta_g ~ std_normal();
  lambda_g_raw ~ normal(0, 1);
  to_vector(z_theta_s) ~ std_normal();
  sigma_s_raw ~ normal(0, 1);

  beta_exp ~ normal(0, 1);
  beta_age ~ normal(0, 1);
  beta_age_exp ~ normal(0, 1);

  // ---- likelihood ----
  {
    vector[N] eta;
    for (n in 1:N) {
      int i = child[n];
      int j = item[n];
      int l = obs_lang[n];
      real ability = lambda_g * theta_g[i] + theta_s[i, l]
                     + beta_exp[l]     * exposure_c[i, l]
                     + beta_age        * age_sc[n]
                     + beta_age_exp[l] * age_sc[n] * exposure_c[i, l];
      eta[n] = exp(logalpha[j]) * (ability + b_item[j]);
    }
    y ~ bernoulli_logit(eta);
  }
}

generated quantities {
  // Correlation between the two language abilities implied by the decomposition
  real rho;
  real ecv;                     // person-level explained common variance
  {
    real vg = square(lambda_g);
    real v1 = vg + square(sigma_s[1]);
    real v2 = vg + square(sigma_s[2]);
    rho = (v1 > 0 && v2 > 0) ? vg / sqrt(v1 * v2) : 0;
    ecv = (vg + mean(square(sigma_s)) > 0)
            ? vg / (vg + mean(square(sigma_s))) : 1;
  }

  // realised per-child language abilities (for plotting / stage-2 style analyses)
  matrix[I, L] theta_child;
  for (l in 1:L)
    theta_child[, l] = lambda_g * theta_g + theta_s[, l];

  vector[compute_loglik ? N : 0] log_lik;
  if (compute_loglik) {
    for (n in 1:N) {
      int i = child[n];
      int j = item[n];
      int l = obs_lang[n];
      real ability = lambda_g * theta_g[i] + theta_s[i, l]
                     + beta_exp[l]     * exposure_c[i, l]
                     + beta_age        * age_sc[n]
                     + beta_age_exp[l] * age_sc[n] * exposure_c[i, l];
      log_lik[n] = bernoulli_logit_lpmf(
        y[n] | exp(logalpha[j]) * (ability + b_item[j]));
    }
  }
}
