// Bilingual accumulator on the log-token scale with cross-language transfer.
//
// Child i hears word j (language l, concept c) at a rate proportional to their
// exposure share e_il; hearing c's translation equivalent in the other language
// l' counts as a fraction kappa_TE of a token:
//
//   eta_ij = lambda * [ xi_i + u_il + log(e_il + kappa_TE * e_il')
//                       + (1 + delta) log(t_i / a0) - psi_j ]
//   psi_j  = mu_lang[l] + concept_c + eps_j,   concept_c ~ N(m_class, sigma_c)
//
//   kappa_TE = 0 : separate registers (only own-label tokens count)
//   kappa_TE = 1 : fully shared registers (exposure share is irrelevant)
//
// The exposure term has coefficient 1 inside the bracket, so lambda (logit per
// log-token) is identified by exposure-share variation -- within child across
// the two languages, with xi_i shared -- and kappa_TE by how the exposure effect
// flattens at low exposure. delta is acceleration beyond the exposure ruler.
data {
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> J;
  int<lower=1> C;
  int<lower=1> K;                        // lexical classes
  array[N] int<lower=0, upper=1> y;
  array[N] int<lower=1, upper=I> child;
  array[N] int<lower=1, upper=J> item;
  array[J] int<lower=1, upper=C> concept;
  array[J] int<lower=1, upper=2> item_lang;
  array[J] int<lower=1, upper=K> lex;
  vector[I] age;                         // months
  matrix<lower=0, upper=1>[I, 2] E;      // exposure shares (floored at 0.01)
  real<lower=0> a0;
}
transformed data {
  vector[I] log_age_rel = log(age / a0);
  array[C] int concept_class;            // class of each concept (from its items)
  for (j in 1:J) concept_class[concept[j]] = lex[j];
}
parameters {
  real<lower=0> lambda;
  real<lower=0, upper=1> kappa_te;
  real delta;
  vector[I] xi_raw;
  real<lower=0> sigma_xi;
  matrix[I, 2] u_raw;
  real<lower=0> sigma_u;
  real mu_lang2;                         // language-2 offset (language 1 = 0)
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
  // child x language log effective exposure
  matrix[I, 2] logx;
  for (i in 1:I) {
    logx[i, 1] = log(E[i, 1] + kappa_te * E[i, 2]);
    logx[i, 2] = log(E[i, 2] + kappa_te * E[i, 1]);
  }
}
model {
  lambda ~ lognormal(0, 1);
  kappa_te ~ beta(1, 1);
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
    vector[N] eta;
    for (n in 1:N) {
      int i = child[n];
      int j = item[n];
      int l = item_lang[j];
      eta[n] = xi[i] + u[i, l] + logx[i, l] + (1 + delta) * log_age_rel[i] - psi[j];
    }
    y ~ bernoulli_logit(lambda * eta);
  }
}
generated quantities {
  real age_slope_logit = lambda * (1 + delta);            // Frank 2026's kappa
  real elasticity_balanced = lambda / (1 + kappa_te);     // d eta / d log e_l at e = 0.5
  real elasticity_minority = lambda * 0.2 / (0.2 + 0.8 * kappa_te);   // at e_l = 0.2
  real rho_lang = square(sigma_xi) / (square(sigma_xi) + square(sigma_u));
}
