# Stan data for models/bilingual_accum.stan: the bilingual accumulator on the
# log-token scale with cross-language transfer (kappa_TE).
#
# Reuses the 09 pipeline (build_pair_matrix -> long obs), then:
#   - keeps translation-equivalent concepts (uni_lemma on both languages' CDIs),
#     stratified-subsampled by lexical class;
#   - keeps children whose exposure record covers both languages (no imputation:
#     exposure share is the ruler for lambda here, so imputed values would bias it);
#   - floors exposure shares at 1% so log(e_l + kappa e_l') stays finite at kappa = 0.
#
# Usage: Rscript scripts/prep_bilingual_accum.R [n_concepts]
suppressPackageStartupMessages({ library(tidyverse); library(here) })
source(here("scripts", "dim_irt_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
n_concepts <- if (length(args)) as.integer(args[1]) else 120L
langs <- c("English (American)", "Spanish (Mexican)")
set.seed(2026)

item_table <- build_item_table(readRDS(here("data", "items.rds")), load_instruments())
df_demogs <- readRDS(here("data", "demographics.rds"))
pair <- build_pair_matrix(item_table, df_demogs, langs, min_items_per_child = 20)
obs <- wide_to_long_obs(pair$resp, pair$item_lookup, pair$covdata)

# translation-equivalent concepts present in both languages, one item per language
te <- pair$item_lookup |>
  filter(!is.na(unilemma)) |>
  group_by(unilemma) |>
  filter(n_distinct(language) == 2) |>
  group_by(unilemma, language) |> slice(1) |> ungroup()
concepts <- te |> distinct(unilemma, lex_class) |>
  group_by(unilemma) |> slice(1) |> ungroup()
cat(sprintf("translation-equivalent concepts available: %d\n", nrow(concepts)))
if (nrow(concepts) > n_concepts) {
  concepts <- concepts |> group_by(lex_class) |>
    slice_sample(prop = n_concepts / nrow(concepts)) |> ungroup()
}
items <- te |> filter(unilemma %in% concepts$unilemma)

# exposure shares, both languages recorded (or a complete profile listing one)
exp_tbl <- child_language_exposure(df_demogs, langs)
expo <- exp_tbl |>
  select(child_id, language, exposure_proportion, total_listed) |>
  pivot_wider(names_from = language, values_from = exposure_proportion) |>
  rename(e1 = !!langs[1], e2 = !!langs[2]) |>
  mutate(complete = !is.na(total_listed) & total_listed >= 0.95,
         e1 = if_else(is.na(e1) & complete, 0, e1),
         e2 = if_else(is.na(e2) & complete, 0, e2)) |>
  filter(!is.na(e1), !is.na(e2))

d <- obs |>
  filter(uid %in% items$uid, child_id %in% expo$child_id) |>
  left_join(items |> select(uid, unilemma), by = c("uid", "unilemma")) |>
  filter(!is.na(age))
kids <- sort(unique(d$child_id))
expo <- expo |> filter(child_id %in% kids) |> arrange(match(child_id, kids))
it <- items |> filter(uid %in% d$uid) |>
  mutate(j = row_number(), c = match(unilemma, sort(unique(unilemma))),
         l = match(language, langs),
         k = as.integer(factor(lex_class)))
d <- d |> mutate(i = match(child_id, kids)) |>
  left_join(it |> select(uid, j), by = "uid")
age_kid <- d |> distinct(i, age) |> group_by(i) |> summarise(age = first(age)) |> arrange(i)

E <- cbind(pmax(expo$e1, 0.01), pmax(expo$e2, 0.01))
cat(sprintf("children: %d | items: %d (%d concepts x 2) | responses: %d\n",
            length(kids), nrow(it), n_distinct(it$c), nrow(d)))
cat(sprintf("age %.0f-%.0f mo | %s exposure: median %.2f, IQR %.2f-%.2f, share < 0.2 or > 0.8: %.0f%%\n",
            min(age_kid$age), max(age_kid$age), langs[1], median(expo$e1),
            quantile(expo$e1, .25), quantile(expo$e1, .75),
            100 * mean(expo$e1 < .2 | expo$e1 > .8)))
cat(sprintf("exposure shares summing to ~1: %.0f%%\n", 100 * mean(abs(expo$e1 + expo$e2 - 1) < 0.05)))

standata <- list(
  N = nrow(d), I = length(kids), J = nrow(it), C = n_distinct(it$c), K = max(it$k),
  y = as.integer(d$y), child = d$i, item = d$j,
  concept = it$c, item_lang = it$l, lex = it$k,
  age = age_kid$age, E = E, a0 = 24)
saveRDS(list(standata = standata, items = it, kids = kids, expo = expo, langs = langs),
        here("models", "bilingual_accum_standata.rds"))
