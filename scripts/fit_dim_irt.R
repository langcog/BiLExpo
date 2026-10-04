# Fit models/bilingual_dim_irt.stan (full variant: general + specific) with
# rstan from the command line -- the same pipeline as 09's data prep, for the
# long overnight runs that are impractical to knit.
#
# Usage:
#   Rscript scripts/fit_dim_irt.R <admins> [thin] [tag] [iter_warmup] [iter_sampling]
#     admins: "all"    -- every administration, child ability shared across
#                         visits + per-administration deviation (estimate_admin)
#             "latest" -- one administration per child x language (cross-sectional)
#     thin:   keep every k-th item per language (default 3, as the 554-item runs)
#
# Writes models/o5_bayes_<tag>_fit.rds, _prep.rds (data + age basis), _summary.csv.
suppressPackageStartupMessages({ library(tidyverse); library(here); library(rstan) })
source(here("scripts", "dim_irt_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
admins <- if (length(args) >= 1) args[1] else "all"
thin   <- if (length(args) >= 2) as.integer(args[2]) else 3L
tag    <- if (length(args) >= 3) args[3] else glue("EN-SP-{admins}-thin{thin}")
iter_warmup   <- if (length(args) >= 4) as.integer(args[4]) else 1000L
iter_sampling <- if (length(args) >= 5) as.integer(args[5]) else 1000L
langs <- c("English (American)", "Spanish (Mexican)")

item_table <- build_item_table(readRDS(here("data", "items.rds")), load_instruments())
df_demogs  <- readRDS(here("data", "demographics.rds"))
print(sample_health(item_table, df_demogs, langs))

long <- build_pair_long(item_table, df_demogs, langs, admins = admins)
if (thin > 1) {
  keep_uid <- long$item_lookup |> group_by(language) |>
    filter(row_number() %% thin == 0) |> pull(uid)
  long$obs <- filter(long$obs, uid %in% keep_uid)
}

prep <- build_dim_irt_stan_data(
  long$obs, admin_language_exposure(df_demogs, langs), langs,
  estimate_general = 1L, estimate_specific = 1L,
  link_concepts = NA, age_spline_df = 3L, compute_loglik = 0L
)
str(prep$data[c("N", "I", "J", "C", "A", "K_age", "link_concepts", "estimate_admin")])
saveRDS(prep, here("models", glue("o5_bayes_{tag}_prep.rds")))

rstan_options(auto_write = TRUE)
mod <- stan_model(here("models", "bilingual_dim_irt.stan"))
t0 <- Sys.time()
fit <- sampling(
  mod, data = prep$data, seed = 123, chains = 4, cores = 4,
  warmup = iter_warmup, iter = iter_warmup + iter_sampling,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  pars = c("z_ability", "z_admin", "theta_ability", "z_delta", "z_concept",
           "concept_easiness", "delta", "Sigma", "L_Sigma", "log_lik"),
  include = FALSE
)
message("sampling time: ", format(Sys.time() - t0))
saveRDS(fit, here("models", glue("o5_bayes_{tag}_fit.rds")))

print(get_num_divergent(fit))
pars <- c("rho", "ecv", "lambda_g", "c_shared", "sigma_s", "sigma_admin",
          "beta_exp", "beta_age", "beta_age_exp", "sigma_delta", "sigma_concept",
          "sigma_logalpha", "intercept")
s <- summary(fit, pars = pars)$summary
out <- as_tibble(s, rownames = "param") |>
  mutate(sample = glue("EN(Am) x ES(Mex), admins={admins}, thin={thin}, ",
                       "{prep$data$I} children, {prep$data$A} admins, {prep$data$J} items"),
         .before = 1)
write_csv(out, here("models", glue("o5_bayes_{tag}_summary.csv")))
print(out |> select(param, mean, `2.5%`, `50%`, `97.5%`, n_eff, Rhat), n = Inf)
