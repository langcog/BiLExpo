# Fit models/bilingual_accum.stan (run scripts/prep_bilingual_accum.R first).
# Usage: Rscript scripts/fit_bilingual_accum.R [iter] [warmup]
suppressPackageStartupMessages({ library(rstan); library(tidyverse); library(here) })
rstan_options(auto_write = TRUE)
args <- commandArgs(trailingOnly = TRUE)
iter <- if (length(args) >= 1) as.integer(args[1]) else 1000
warmup <- if (length(args) >= 2) as.integer(args[2]) else 500

b <- readRDS(here("models", "bilingual_accum_standata.rds"))
mod <- stan_model(here("models", "bilingual_accum.stan"))
fit <- sampling(mod, data = b$standata, chains = 4, cores = 4, iter = iter, warmup = warmup,
                seed = 11, refresh = 100, control = list(adapt_delta = 0.9),
                init = function() list(lambda = 1, kappa_te = 0.3, delta = 3, sigma_xi = 1,
                                       sigma_u = 0.5, sigma_c = 1, sigma_eps = 0.5))
saveRDS(fit, here("models", "bilingual_accum_fit.rds"))
pars <- c("lambda", "kappa_te", "delta", "age_slope_logit", "elasticity_balanced",
          "elasticity_minority", "sigma_xi", "sigma_u", "rho_lang", "mu_lang2",
          "sigma_c", "sigma_eps", "m_class")
summ <- summary(fit, pars = pars)$summary
print(round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3))
write.csv(summ, here("models", "bilingual_accum_summary.csv"))
check_hmc_diagnostics(fit)
