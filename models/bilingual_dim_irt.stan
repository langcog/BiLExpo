// Crossed random-effects IRT for the dimensionality of multilingual lexical
// ability. L (data) is the number of languages and is NOT hardcoded to 2 --
// this fits bilinguals (L=2) and multilinguals (L=3+, e.g. the trilingual
// English/Malay/Mandarin (Malaysian) sample) with the same code.
//
// One row per (child, item) observation, pooled across all L languages.
// Item easiness = a translation-equivalent "concept" term (shared across
// languages via uni_lemma, e.g. dog / perro / anjing) + a language-specific
// offset -- collapsed to one flat item-easiness level when few items link
// (link_concepts).
//
// Child ability across the L languages is multivariate normal with a
// COMPOUND-SYMMETRIC COVARIANCE: every pair of languages shares a common
// covariance c_shared, and each language adds its own specific variance:
//
//     theta_ability[i, ] ~ MVN(0, Sigma)
//     Sigma[l, l]        = c_shared + sigma_s[l]^2
//     Sigma[l1, l2]      = c_shared            (l1 != l2)
//
// This is the same model as the earlier lambda_g * theta_g + theta_s
// decomposition (c_shared == lambda_g^2), but parameterised through the
// Cholesky factor of Sigma rather than a general loading multiplying a shared
// latent -- so when the shared part vanishes (c_shared -> 0, i.e. independent
// lexicons) there is no funnel: the non-centred latent z_ability stays unit
// scale and c_shared is just a smooth shape parameter of the covariance.
//
// Nested hypotheses (set via data flags):
//   estimate_general=0, estimate_specific=1  -> independent lexicons  (rho = 0)
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
  int<lower=2> L;                       // number of languages (2 for a pair, 3+ for multilingual)

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
  // link_concepts=1: item easiness = concept easiness + language-specific offset
  //   (only worth it when many items share a uni_lemma across languages).
  // link_concepts=0: a single flat item-easiness level (better identified /
  //   mixed when translation-equivalent overlap is small).
  int<lower=0,upper=1> link_concepts;
  int<lower=0,upper=1> compute_loglik;
}

transformed data {
  array[N] int obs_lang;
  for (n in 1:N) obs_lang[n] = item_lang[item[n]];
  real jitter = 1e-6;
}

parameters {
  real intercept;                       // grand mean easiness

  // ---- item side ----
  vector[C] z_concept;                  // concept easiness (non-centred)
  real<lower=0> sigma_concept;
  vector[J] z_delta;                    // item easiness / language-specific offset (non-centred)
  real<lower=0> sigma_delta;
  vector[J] logalpha;                   // log discrimination
  real<lower=0> sigma_logalpha;

  // ---- person side ----
  real<lower=0> c_shared_raw;           // shared ability covariance (= old lambda_g^2)
  vector<lower=0>[L] sigma_s_raw;       // per-language specific SD
  matrix[I, L] z_ability;               // non-centred latent, ~ N(0, I)

  // ---- exposure / age fixed effects (per language) ----
  vector[L] beta_exp;
  real beta_age;
  vector[L] beta_age_exp;
}

transformed parameters {
  real c_shared     = estimate_general  ? c_shared_raw : 0;
  vector[L] sigma_s = estimate_specific ? sigma_s_raw  : rep_vector(1e-3, L);

  // compound-symmetric ability covariance and its Cholesky factor
  matrix[L, L] Sigma;
  for (l1 in 1:L)
    for (l2 in 1:L)
      Sigma[l1, l2] = c_shared + (l1 == l2 ? square(sigma_s[l1]) + jitter : 0);
  matrix[L, L] L_Sigma = cholesky_decompose(Sigma);

  // realised per-child abilities: each row ~ MVN(0, Sigma)
  matrix[I, L] theta_ability = z_ability * L_Sigma';

  vector[C] concept_easiness = link_concepts ? sigma_concept * z_concept
                                             : rep_vector(0.0, C);
  vector[J] delta = sigma_delta * z_delta;

  vector[J] b_item;   // total item easiness
  for (j in 1:J)
    b_item[j] = intercept + delta[j]
                + (link_concepts ? concept_easiness[concept[j]] : 0);
}

model {
  // ---- priors ----
  intercept ~ normal(0, 3);

  z_concept ~ std_normal();
  sigma_concept ~ normal(0, 2);
  z_delta ~ std_normal();
  sigma_delta ~ normal(0, 2);
  logalpha ~ normal(0, sigma_logalpha);
  sigma_logalpha ~ normal(0, 0.5);

  to_vector(z_ability) ~ std_normal();
  c_shared_raw ~ normal(0, 4);          // half-normal on the variance scale
  sigma_s_raw ~ normal(0, 3);

  beta_exp ~ normal(0, 3);
  beta_age ~ normal(0, 2);
  beta_age_exp ~ normal(0, 2);

  // ---- likelihood ----
  {
    vector[N] eta;
    for (n in 1:N) {
      int i = child[n];
      int j = item[n];
      int l = obs_lang[n];
      real ability = theta_ability[i, l]
                     + beta_exp[l]     * exposure_c[i, l]
                     + beta_age        * age_sc[n]
                     + beta_age_exp[l] * age_sc[n] * exposure_c[i, l];
      eta[n] = exp(logalpha[j]) * (ability + b_item[j]);
    }
    y ~ bernoulli_logit(eta);
  }
}

generated quantities {
  // Pairwise correlation between every two languages' abilities:
  //   rho[l1,l2] = c_shared / sqrt(v[l1] * v[l2]),  v[l] = c_shared + sigma_s[l]^2
  matrix[L, L] rho_mat;
  real rho;                     // = rho_mat[1,2]; kept for 2-language callers
  real ecv;                     // person-level explained common variance
  real lambda_g = sqrt(c_shared);   // "general loading" name kept for continuity
  {
    vector[L] v_tot;
    for (l in 1:L) v_tot[l] = c_shared + square(sigma_s[l]);
    for (l1 in 1:L)
      for (l2 in 1:L)
        rho_mat[l1, l2] = (v_tot[l1] > 0 && v_tot[l2] > 0)
                            ? c_shared / sqrt(v_tot[l1] * v_tot[l2]) : 0;
    rho = rho_mat[1, 2];
    ecv = (c_shared + mean(square(sigma_s)) > 0)
            ? c_shared / (c_shared + mean(square(sigma_s))) : 1;
  }

  // realised per-child language abilities (for plotting / stage-2 style analyses)
  matrix[I, L] theta_child = theta_ability;

  vector[compute_loglik ? N : 0] log_lik;
  if (compute_loglik) {
    for (n in 1:N) {
      int i = child[n];
      int j = item[n];
      int l = obs_lang[n];
      real ability = theta_ability[i, l]
                     + beta_exp[l]     * exposure_c[i, l]
                     + beta_age        * age_sc[n]
                     + beta_age_exp[l] * age_sc[n] * exposure_c[i, l];
      log_lik[n] = bernoulli_logit_lpmf(
        y[n] | exp(logalpha[j]) * (ability + b_item[j]));
    }
  }
}
