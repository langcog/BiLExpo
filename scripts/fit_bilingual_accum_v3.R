# Fit models/bilingual_accum_v3.stan (shared frequency exponent phi).
# Usage: Rscript scripts/fit_bilingual_accum_v3.R sim|real|real_noimp|anchor <lambda>
#   sim        : parameter recovery on the real design (phi = 0.3, real-like variances)
#   real       : all 118 translation-equivalent concepts
#   real_noimp : drops concepts where either label's CHILDES frequency was imputed
suppressPackageStartupMessages({ library(rstan); library(tidyverse); library(here) })
rstan_options(auto_write = TRUE)
args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]
anchor <- if (mode == "anchor") as.numeric(args[2]) else NA
b <- readRDS(here("models", "bilingual_accum_v2_standata.rds"))
sd <- b$standata
sd$log_q_te <- log(sd$q_te)
pars <- c(if (is.na(anchor)) "lambda", "kappa_nc", "kappa_cog", "kappa_diff", "phi", "delta", "age_slope_logit",
          "sigma_xi", "sigma_u", "rho_lang", "mu_lang2", "sigma_c", "sigma_eps")

if (mode == "real_noimp") {
  bad <- b$pairs |> filter(imp_en | imp_es) |> pull(unilemma)
  keep_j <- which(!b$items$unilemma %in% bad)
  keep_n <- sd$item %in% keep_j
  remap <- match(seq_len(sd$J), keep_j)
  cmap <- match(seq_len(sd$C), sort(unique(sd$concept[keep_j])))
  sd <- within(sd, {
    y <- y[keep_n]; adm <- adm[keep_n]; item <- remap[item[keep_n]]; N <- length(y)
    concept <- cmap[concept[keep_j]]; item_lang <- item_lang[keep_j]; lex <- lex[keep_j]
    cognate <- cognate[keep_j]; log_q_own <- log_q_own[keep_j]; log_q_te <- log_q_te[keep_j]
    q_own <- q_own[keep_j]; q_te <- q_te[keep_j]
    J <- length(keep_j); C <- max(concept)
  })
  cat(sprintf("dropped %d concepts with imputed frequency: %d items, %d responses left\n",
              length(bad), sd$J, sd$N))
}

truth <- NULL
if (mode == "sim") {
  set.seed(23)
  truth <- list(lambda = 1, kappa_nc = 0.2, kappa_cog = 0.5, kappa_diff = 0.3, phi = 0.3,
                delta = 5, age_slope_logit = 6, sigma_xi = 1.0, sigma_u = 1.2,
                rho_lang = 1 / (1 + 1.2^2), mu_lang2 = 0.3, sigma_c = 1.0, sigma_eps = 0.6)
  cc <- integer(sd$C); cc[sd$concept] <- sd$lex
  ce <- seq(-1, 1, length.out = sd$K)[cc] + rnorm(sd$C, 0, truth$sigma_c)
  psi <- ifelse(sd$item_lang == 2, truth$mu_lang2, 0) + ce[sd$concept] + rnorm(sd$J, 0, truth$sigma_eps)
  xi <- rnorm(sd$I, 0, truth$sigma_xi); u <- matrix(rnorm(sd$I * 2, 0, truth$sigma_u), sd$I, 2)
  kid <- sd$adm_kid[sd$adm]; l <- sd$item_lang[sd$item]
  kap <- ifelse(sd$cognate[sd$item] == 1, truth$kappa_cog, truth$kappa_nc)
  eta <- xi[kid] + u[cbind(kid, l)] +
    log(sd$E[cbind(kid, l)] * exp(truth$phi * sd$log_q_own[sd$item]) +
          kap * sd$E[cbind(kid, 3 - l)] * exp(truth$phi * sd$log_q_te[sd$item])) +
    (1 + truth$delta) * log(sd$age[sd$adm] / sd$a0) - psi[sd$item]
  sd$y <- rbinom(sd$N, 1, plogis(truth$lambda * eta))
  cat(sprintf("simulated mean y = %.2f (real %.2f)\n", mean(sd$y), mean(b$standata$y)))
}

if (!is.na(anchor)) sd$lambda <- anchor
mod <- stan_model(here("models", if (is.na(anchor)) "bilingual_accum_v3.stan" else "bilingual_accum_v3_anchor.stan"))
tag <- if (is.na(anchor)) mode else sprintf("anchor%.1f", anchor)
fit <- sampling(mod, data = sd, chains = 4, cores = 4, iter = 1000, warmup = 500,
                seed = 29, refresh = 100, control = list(adapt_delta = 0.9),
                init = function() list(kappa_nc = 0.3, kappa_cog = 0.3, phi = 0.5, delta = 5,
                                       sigma_xi = 1, sigma_u = 0.5, sigma_c = 1, sigma_eps = 0.5))
saveRDS(fit, here("models", sprintf("bilingual_accum_v3_%s_fit.rds", tag)))
summ <- summary(fit, pars = pars)$summary
out <- round(summ[, c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 3)
if (!is.null(truth)) out <- cbind(truth = unlist(truth[pars]), out)
print(out)
write.csv(summ, here("models", sprintf("bilingual_accum_v3_%s_summary.csv", tag)))
check_hmc_diagnostics(fit)
