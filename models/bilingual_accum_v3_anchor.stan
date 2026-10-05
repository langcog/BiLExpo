// Bilingual accumulator v3, lambda ANCHORED: lambda is data (e.g. from monolingual
// rulers), so kappa_TE is estimated conditional on it -- the v1/v3 data identify
// only the (lambda, kappa_TE) ridge when CHILDES frequency carries little signal.
//
// v3: as v2, but frequency enters both labels through one
// estimated exponent phi, so translation-equivalent ratios are compressed to match
// how much (CHILDES) frequency actually matters.
//
// v2 used q at full weight inside the transfer term while the data put frequency's
// elasticity at ~0.29; with TE frequency ratios of 10-100x, any kappa > 0 produced
// item-level boosts the data reject, pinning kappa at 0. Here:
//
// Child i at administration a (age t_a), word j (language l, concept c), with
// relative child-directed frequencies q_j (own label) and q'_j (its translation
// equivalent in the other language l'):
//
//   eta = lambda * [ xi_i + u_il + log(e_il q_j^phi + kappa_j e_il' q'_j^phi)
//                    + (1 + delta) log(t_a / a0) - psi_j ]
//   kappa_j = kappa_cog if the pair is an orthographic cognate, else kappa_nc
//   psi_j   = mu_lang[l] + concept_c + eps_j,   concept_c ~ N(m_class, sigma_c)
//
// kappa is identified from item-level variation in the (compressed) ratio
// (q'_j / q_j)^phi within a child and language; lambda from exposure share; phi is
// the own-frequency elasticity relative to exposure's 1 (when kappa is small).
data {
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> A;                        // administrations
  int<lower=1> J;
  int<lower=1> C;
  int<lower=1> K;
  array[N] int<lower=0, upper=1> y;
  array[N] int<lower=1, upper=A> adm;
  array[N] int<lower=1, upper=J> item;
  array[A] int<lower=1, upper=I> adm_kid;
  vector[A] age;
  array[J] int<lower=1, upper=C> concept;
  array[J] int<lower=1, upper=2> item_lang;
  array[J] int<lower=1, upper=K> lex;
  array[J] int<lower=0, upper=1> cognate;
  vector[J] log_q_own;
  vector[J] log_q_te;
  matrix<lower=0, upper=1>[I, 2] E;
  real<lower=0> a0;
  real<lower=0> lambda;                  // anchored scale
}
transformed data {
  vector[A] log_age_rel = log(age / a0);
  array[C] int concept_class;
  for (j in 1:J) concept_class[concept[j]] = lex[j];
  array[N] int kid;
  array[N] int lang;
  vector[N] e_own;
  vector[N] e_oth;
  for (n in 1:N) {
    kid[n] = adm_kid[adm[n]];
    lang[n] = item_lang[item[n]];
    e_own[n] = E[kid[n], lang[n]];
    e_oth[n] = E[kid[n], 3 - lang[n]];
  }
  vector[N] lq_own = log_q_own[item];
  vector[N] lq_te = log_q_te[item];
  vector[N] cog = to_vector(cognate[item]);
}
parameters {
  real<lower=0, upper=1> kappa_nc;
  real<lower=0, upper=1> kappa_cog;
  real<lower=0, upper=2> phi;
  real delta;
  vector[I] xi_raw;
  real<lower=0> sigma_xi;
  matrix[I, 2] u_raw;
  real<lower=0> sigma_u;
  real mu_lang2;
  vector[K] m_class;
  vector[C] c_raw;
  real<lower=0> sigma_c;
  vector[J] eps_raw;
  real<lower=0> sigma_eps;
}
transformed parameters {
  vector[I] xi = sigma_xi * xi_raw;
  matrix[I, 2] u = sigma_u * u_raw;
  vector[C] concept_eff = m_class[concept_class] + sigma_c * c_raw;
  vector[J] psi;
  for (j in 1:J)
    psi[j] = (item_lang[j] == 2 ? mu_lang2 : 0) + concept_eff[concept[j]] + sigma_eps * eps_raw[j];
}
model {
  kappa_nc ~ beta(1, 1);
  kappa_cog ~ beta(1, 1);
  phi ~ normal(0.5, 0.5);
  delta ~ normal(0, 10);
  xi_raw ~ std_normal();
  sigma_xi ~ normal(0, 3);
  to_vector(u_raw) ~ std_normal();
  sigma_u ~ normal(0, 2);
  mu_lang2 ~ normal(0, 5);
  m_class ~ normal(0, 5);
  c_raw ~ std_normal();
  sigma_c ~ normal(0, 3);
  eps_raw ~ std_normal();
  sigma_eps ~ normal(0, 3);
  {
    vector[N] kap = kappa_nc + (kappa_cog - kappa_nc) * cog;
    vector[N] u_n;
    for (n in 1:N) u_n[n] = u[kid[n], lang[n]];
    vector[N] eta = xi[kid] + u_n
                    + log(e_own .* exp(phi * lq_own) + kap .* e_oth .* exp(phi * lq_te))
                    + (1 + delta) * log_age_rel[adm] - psi[item];
    y ~ bernoulli_logit(lambda * eta);
  }
}
generated quantities {
  real age_slope_logit = lambda * (1 + delta);       // Frank 2026's kappa
  real kappa_diff = kappa_cog - kappa_nc;
  real rho_lang = square(sigma_xi) / (square(sigma_xi) + square(sigma_u));
}
