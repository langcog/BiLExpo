# Stan data for models/bilingual_accum_v2.stan. Changes from prep_bilingual_accum.R:
#   1. One row per (child, administration, item) with that administration's own age
#      (build_pair_matrix collapses administrations with max() and takes the age
#      of the first exposure record, which erases the age signal).
#   2. Word-level child-directed frequencies in each language (childes_freq_te.R);
#      zero counts (multiword items, Mexican-specific words) are imputed from the
#      translation equivalent's frequency rather than smoothed to 1.
#   3. An orthographic cognate flag per translation-equivalent pair.
# Same children (exposure records for both languages) and items as v1.
#
# Usage: Rscript scripts/prep_bilingual_accum_v2.R
suppressPackageStartupMessages({ library(tidyverse); library(here); library(stringdist) })
source(here("scripts", "dim_irt_helpers.R"))
v1 <- readRDS(here("models", "bilingual_accum_standata.rds"))
langs <- v1$langs

# --- frequencies, with zero counts imputed from the translation equivalent ---
fq <- read_csv(here("models", "te_childes_freq.csv"), show_col_types = FALSE) |>
  mutate(l = if_else(language == langs[1], "en", "es"),
         logp = if_else(count > 0, log(count / total), NA_real_))
wide <- fq |> select(unilemma, l, logp, forms) |>
  pivot_wider(names_from = l, values_from = c(logp, forms))
fit_es <- lm(logp_es ~ logp_en, wide); fit_en <- lm(logp_en ~ logp_es, wide)
wide <- wide |> mutate(
  imp_es = is.na(logp_es), imp_en = is.na(logp_en),
  logp_es = coalesce(logp_es, predict(fit_es, cur_data())),
  logp_en = coalesce(logp_en, predict(fit_en, cur_data())))
# both labels missing (multiword pairs like good morning / buenos dias): each language's 10th percentile
wide <- wide |> mutate(
  logp_en = coalesce(logp_en, quantile(fq$logp[fq$l == "en"], 0.1, na.rm = TRUE)),
  logp_es = coalesce(logp_es, quantile(fq$logp[fq$l == "es"], 0.1, na.rm = TRUE)))
stopifnot(!anyNA(wide$logp_en), !anyNA(wide$logp_es))
strip <- \(x) iconv(x, to = "ASCII//TRANSLIT", sub = "") |> tolower() |> str_remove_all("[^a-z]")
wide <- wide |> mutate(sim = map2_dbl(forms_en, forms_es, \(a, b) {
  A <- strip(str_split(a, "/")[[1]]); B <- strip(str_split(b, "/")[[1]])
  A <- A[nchar(A) > 0]; B <- B[nchar(B) > 0]
  if (!length(A) || !length(B)) return(0)
  max(outer(A, B, \(x, y) 1 - stringdist(x, y, method = "lv") / pmax(nchar(x), nchar(y))))
}), sim = coalesce(sim, 0), cognate = as.integer(sim >= 0.5))
cat(sprintf("imputed frequencies: %d English, %d Spanish | cognate pairs: %d of %d\n",
            sum(wide$imp_en), sum(wide$imp_es), sum(wide$cognate), nrow(wide)))
cat("cognates:", paste(sprintf("%s/%s", wide$forms_en, wide$forms_es)[wide$cognate == 1], collapse = ", "), "\n")

it <- v1$items |> select(uid, language, unilemma, j, c, l, k) |>
  left_join(wide |> select(unilemma, logp_en, logp_es, cognate), by = "unilemma") |>
  mutate(logp_own = if_else(l == 1, logp_en, logp_es),
         logp_te = if_else(l == 1, logp_es, logp_en))
lp_bar <- mean(c(it$logp_own))
it <- it |> mutate(q_own = exp(logp_own - lp_bar), q_te = exp(logp_te - lp_bar))

# --- per-administration responses ---
obs <- build_item_table(readRDS(here("data", "items.rds")) |> filter(language %in% langs),
                        load_instruments()) |>
  mutate(child_id = as.character(child_id)) |>
  filter(child_id %in% v1$kids, uid %in% it$uid, !is.na(age), !is.na(value)) |>
  distinct(child_id, data_id, uid, .keep_all = TRUE) |>
  mutate(i = match(child_id, v1$kids)) |>
  left_join(it |> select(uid, j), by = "uid")
adm <- obs |> distinct(i, data_id, age) |> arrange(i, age) |> mutate(a = row_number())
obs <- obs |> left_join(adm |> select(data_id, a), by = "data_id")
voc <- obs |> group_by(a) |> summarise(p = mean(value)) |> left_join(adm, by = "a")
cat(sprintf("children: %d | administrations: %d | responses: %d | children with >1 admin per language: %d\n",
            n_distinct(obs$i), nrow(adm), nrow(obs),
            obs |> distinct(i, language, data_id) |> count(i, language) |> filter(n > 1) |> distinct(i) |> nrow()))
cat(sprintf("cor(age, proportion produced) per administration = %.2f (v1 per child: 0.00)\n", cor(voc$age, voc$p)))

standata <- list(
  N = nrow(obs), I = length(v1$kids), A = nrow(adm), J = nrow(it), C = max(it$c), K = max(it$k),
  y = as.integer(obs$value), adm = obs$a, item = obs$j,
  adm_kid = adm$i, age = adm$age,
  concept = it$c, item_lang = it$l, lex = it$k,
  q_own = it$q_own, q_te = it$q_te, cognate = it$cognate, log_q_own = log(it$q_own),
  E = v1$standata$E, a0 = 24)
saveRDS(list(standata = standata, items = it, pairs = wide, kids = v1$kids, langs = langs),
        here("models", "bilingual_accum_v2_standata.rds"))
