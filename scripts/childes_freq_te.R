# Per-word child-directed frequencies for the translation-equivalent CDI items in
# models/bilingual_accum_standata.rds, from childes-db (target child excluded).
# Each CDI item's forms (split on "/", parentheticals and "*" stripped) are summed.
# Output: models/te_childes_freq.csv  (uid, language, forms, count, total, p)
suppressPackageStartupMessages({ library(tidyverse); library(here); library(childesr) })

b <- readRDS(here("models", "bilingual_accum_standata.rds"))
source(here("scripts", "dim_irt_helpers.R"))
defs <- build_item_table(readRDS(here("data", "items.rds")) |> filter(language %in% b$langs),
                         load_instruments()) |>
  distinct(uid, item_definition) |> group_by(uid) |> slice(1) |> ungroup()
items <- b$items |> select(uid, language, unilemma) |> left_join(defs |> select(uid, item_definition), by = "uid")
stopifnot(!anyNA(items$item_definition))

forms_of <- \(d) d |> str_remove_all("\\s*\\(.*?\\)|\\*") |> str_split("/") |> pluck(1) |>
  str_trim() |> tolower() |> discard(\(x) x == "")
items <- items |> mutate(forms = map(item_definition, forms_of))

collections <- c("English (American)" = "Eng-NA", "Spanish (Mexican)" = "Spanish")
freq <- map_dfr(names(collections), \(lang) {
  coll <- collections[[lang]]
  it <- items |> filter(language == lang)
  forms <- unique(unlist(it$forms))
  ty <- get_types(collection = coll, role_exclude = "Target_Child", type = forms)
  counts <- ty |> mutate(gloss = tolower(gloss)) |> group_by(gloss) |> summarise(n = sum(count))
  total <- get_speaker_statistics(collection = coll, role_exclude = "Target_Child") |>
    summarise(total = sum(num_tokens, na.rm = TRUE)) |> pull(total)
  cat(sprintf("%s (%s): %d forms queried, %d found, %s total tokens\n",
              lang, coll, length(forms), nrow(counts), format(total, big.mark = ",")))
  it |> mutate(count = map_dbl(forms, \(f) sum(counts$n[counts$gloss %in% f])),
               total = total)
})
freq <- freq |> mutate(p = (count + 1) / total, forms = map_chr(forms, paste, collapse = "/"))
cat(sprintf("items with zero count (smoothed to 1): %d of %d\n", sum(freq$count == 0), nrow(freq)))
write_csv(freq |> select(uid, language, unilemma, item_definition, forms, count, total, p),
          here("models", "te_childes_freq.csv"))
