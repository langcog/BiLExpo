# Fit models/bilingual_accum_v2.stan to the real EN-SP data, or to data simulated
# on the same design from known parameters (parameter recovery).
# Usage: Rscript scripts/fit_bilingual_accum_v2.R real|sim
suppressPackageStartupMessages({ library(rstan); library(tidyverse); library(here) })
rstan_options(auto_write = TRUE)
mode <- commandArgs(trailingOnly = TRUE)[1]
b <- readRDS(here("models", "bilingual_accum_v2_standata.rds"))
sd <- b$standata
pars <- c("lambda", "kappa_nc", "kappa_cog", "kappa_diff", "gamma", "delta", "age_slope_logit",
          "sigma_xi", "sigma_u", "rho_lang", "mu_lang2", "sigma_c", "sigma_eps")

truth <- NULL
if (mode == "sim") {
  set.seed(17)
  truth <- list(lambda = 1, kappa_nc = 0.2, kappa_cog = 0.5, kappa_diff = 0.3, gamma = 0,
                delta = 6, age_slope_logit = 7, sigma_xi = 1.5, sigma_u = 0.7,
                rho_lang = 1.5^2 / (1.5^2 + 0.7^2), mu_lang2 = 0.3, sigma_c = 1.2, sigma_eps = 0.6)
  cc <- integer(sd$C); cc[sd$concept] <- sd$lex
  ce <- seq(-1, 1, length.out = sd$K)[cc] + rnorm(sd$C, 0, truth$sigma_c)
  psi <- ifelse(sd$item_lang == 2, truth$mu_lang2, 0) + ce[sd$concept] + rnorm(sd$J, 0, truth$sigma_eps)
  xi <- rnorm(sd$I, 0, truth$sigma_xi); u <- matrix(rnorm(sd$I * 2, 0, truth$sigma_u), sd$I, 2)
  kid <- sd$adm_kid[sd$adm]; l <- sd$item_lang[sd$item]
  kap <- ifelse(sd$cognate[sd$item] == 1, truth$kappa_cog, truth$kappa_nc)
  eta <- xi[kid] + u[cbind(kid, l)] +
    log(sd$E[cbind(kid, l)] * sd$q_own[sd$item] + kap * sd$E[cbind(kid, 3 - l)] * sd$q_te[sd$item]) +
    truth$gamma * sd$log_q_own[sd$item] + (1 + truth$delta) * log(sd$age[sd$adm] / sd$a0) - psi[sd$item]
  sd$y <- rbinom(sd$N, 1, plogis(truth$lambda * eta))
  cat(sprintf("simulated mean y = %.2f (real %.2f)\n", mean(sd$y), mean(b$standata$y)))
}

mod <- stan_model(here("models", "bilingual_accum_v2.stan"))
fit <- sampling(mod, data = sd, chains = 4, cores = 4, iter = 1000, warmup = 500,
                seed = if (mode == "sim") 5 else 13, refresh = 100, control = list(adapt_delta = 0.9),
                init = function() list(lambda = 1, kappa_nc = 0.3, kappa_cog = 0.3, gamma = 0, delta = 5,
                                       sigma_xi = 1, sigma_u = 0.5, sigma_c = 1, sigma_eps = 0.5))
saveRDS(fit, here("models", sprintf("bilingual_accum_v2_%s_fit.rds", mode)))
summ <- summary(fit, pars = pars)$summary
out <- round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3)
if (!is.null(truth)) out <- cbind(truth = unlist(truth[pars]), out)
print(out)
write.csv(summ, here("models", sprintf("bilingual_accum_v2_%s_summary.csv", mode)))
check_hmc_diagnostics(fit)
