# Parameter recovery for models/bilingual_accum.stan on the real EN-SP design
# (exposure shares, ages, concept/item structure), with known lambda and kappa_TE.
# Usage: Rscript scripts/sim_bilingual_accum.R
suppressPackageStartupMessages({ library(rstan); library(tidyverse); library(here) })
rstan_options(auto_write = TRUE)
set.seed(7)
b <- readRDS(here("models", "bilingual_accum_standata.rds"))
sd <- b$standata
truth <- list(lambda = 1.0, kappa_te = 0.3, delta = 4, sigma_xi = 1.5, sigma_u = 0.7,
              mu_lang2 = 0.3, sigma_c = 1.2, sigma_eps = 0.6)

concept_class <- integer(sd$C); concept_class[sd$concept] <- sd$lex
m_class <- seq(-1, 1, length.out = sd$K)
ce <- m_class[concept_class] + rnorm(sd$C, 0, truth$sigma_c)
psi <- ifelse(sd$item_lang == 2, truth$mu_lang2, 0) + ce[sd$concept] + rnorm(sd$J, 0, truth$sigma_eps)
xi <- rnorm(sd$I, 0, truth$sigma_xi)
u <- matrix(rnorm(sd$I * 2, 0, truth$sigma_u), sd$I, 2)
logx <- cbind(log(sd$E[, 1] + truth$kappa_te * sd$E[, 2]), log(sd$E[, 2] + truth$kappa_te * sd$E[, 1]))
l <- sd$item_lang[sd$item]
eta <- xi[sd$child] + u[cbind(sd$child, l)] + logx[cbind(sd$child, l)] +
  (1 + truth$delta) * log(sd$age[sd$child] / sd$a0) - psi[sd$item]
sd$y <- rbinom(sd$N, 1, plogis(truth$lambda * eta))
cat(sprintf("simulated mean y = %.2f (real data: %.2f)\n", mean(sd$y), mean(b$standata$y)))

mod <- stan_model(here("models", "bilingual_accum.stan"))
fit <- sampling(mod, data = sd, chains = 4, cores = 4, iter = 1000, warmup = 500, seed = 3,
                refresh = 100, control = list(adapt_delta = 0.9),
                init = function() list(lambda = 1, kappa_te = 0.5, delta = 3, sigma_xi = 1,
                                       sigma_u = 0.5, sigma_c = 1, sigma_eps = 0.5))
pars <- c("lambda", "kappa_te", "delta", "sigma_xi", "sigma_u", "mu_lang2")
summ <- summary(fit, pars = pars)$summary
print(cbind(truth = unlist(truth[pars]), round(summ[, c("mean", "2.5%", "97.5%", "n_eff", "Rhat")], 3)))
write.csv(summ, here("models", "bilingual_accum_recovery.csv"))
