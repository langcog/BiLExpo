// Bilingual accumulator v2: per-administration ages, word-level frequencies in each
// language, and cognate-specific transfer.
//
// Child i at administration a (age t_a), word j (language l, concept c), with
// relative child-directed frequencies q_j (own label) and q'_j (its translation
// equivalent in the other language l'):
//
//   eta = lambda * [ xi_i + u_il + log(e_il q_j + kappa_j e_il' q'_j) + gamma log q_j
//                    + (1 + delta) log(t_a / a0) - psi_j ]
//   kappa_j = kappa_cog if the pair is an orthographic cognate, else kappa_nc
//   psi_j   = mu_lang[l] + concept_c + eps_j,   concept_c ~ N(m_class, sigma_c)
//
// v1's exposure-only term let lambda and kappa trade off along a ridge (both reshape
// the same exposure-response curve). Here the transfer term depends on each word's
// frequency ratio q'_j / q_j, which varies across items within a child and language,
// so kappa is identified from item-level variation and lambda from exposure share.
// gamma lets the own-frequency elasticity differ from exposure's (1 + gamma vs 1).
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
  vector<lower=0>[J] q_own;
  vector<lower=0>[J] q_te;
  array[J] int<lower=0, upper=1> cognate;
  vector[J] log_q_own;
  matrix<lower=0, upper=1>[I, 2] E;
  real<lower=0> a0;
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
  vector[N] own_tok = e_own .* q_own[item];
  vector[N] te_tok = e_oth .* q_te[item];
  vector[N] cog = to_vector(cognate[item]);
}
parameters {
  real<lower=0> lambda;
  real<lower=0, upper=1> kappa_nc;
  real<lower=0, upper=1> kappa_cog;
  real gamma;
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
  lambda ~ lognormal(0, 1);
  kappa_nc ~ beta(1, 1);
  kappa_cog ~ beta(1, 1);
  gamma ~ normal(0, 1);
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
    vector[N] eta = xi[kid] + u_n + log(own_tok + kap .* te_tok) + gamma * log_q_own[item]
                    + (1 + delta) * log_age_rel[adm] - psi[item];
    y ~ bernoulli_logit(lambda * eta);
  }
}
generated quantities {
  real age_slope_logit = lambda * (1 + delta);       // Frank 2026's kappa
  real kappa_diff = kappa_cog - kappa_nc;
  real freq_elasticity = 1 + gamma;                  // relative to exposure's 1
  real rho_lang = square(sigma_xi) / (square(sigma_xi) + square(sigma_u));
}
