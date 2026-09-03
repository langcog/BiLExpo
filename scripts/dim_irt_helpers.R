# Shared helpers for the O5 dimensionality analyses
# (08_o5_dimensionality_mirt.qmd, 09_o5_dimensionality_bayes.qmd,
#  10_o5_dimensionality_calibration.qmd)
#
# Keeps the item-table construction, lexical-class derivation, exposure lookup,
# the child x item response matrix, and the long -> Stan data reshape in one
# place so the three files can't drift apart.

suppressPackageStartupMessages({
  library(glue)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
})

# ---------------------------------------------------------------------------
# Instruments + item table
# ---------------------------------------------------------------------------

#' Locate the instrument CSV directory. The committed pipeline (01/04/05) uses
#' `here("instruments")`; this repo's local data drop puts them under
#' `here("data", "instruments")`. Accept either.
instruments_dir <- function() {
  for (p in c(here::here("instruments"), here::here("data", "instruments"))) {
    if (dir.exists(p) && length(list.files(p, pattern = "\\.csv$"))) return(p)
  }
  stop("No instruments/*.csv found in instruments/ or data/instruments/")
}

#' Load every `<language>.csv` in the instruments directory into one long
#' item-metadata table. `instruments_path` defaults to `instruments_dir()`.
load_instruments <- function(instruments_path = instruments_dir()) {
  files <- list.files(instruments_path, pattern = "\\.csv$", full.names = TRUE)
  map(files, \(f) {
    read_csv(f, show_col_types = FALSE) |>
      mutate(uid = glue("item_{row_number()}")) |>
      pivot_longer(
        cols = -c(category, definition, gloss, uni_lemma, uid),
        names_to = "form", values_to = "item_id"
      ) |>
      mutate(
        language = str_remove(basename(f), "\\.csv$"),
        uid = glue("{language}_{uid}")
      )
  }) |>
    list_rbind() |>
    filter(!is.na(item_id)) |>
    mutate(form = recode(form,
      OxfordExtended = "Oxford Extended",
      OxfordShortII  = "Oxford Short II",
      WSExtended     = "WS Extended",
      WGShort        = "WG Short"
    ))
}

#' Join Wordbank item responses to instrument metadata and attach a stable `uid`.
#'
#' `items` is `readRDS(here("data", "items.rds"))`; `instruments` is
#' `load_instruments()`. `category` and `lexical_category` are taken from `items`
#' (Wordbank), matching 01/04; the instruments table supplies only `uid`.
#' Returns one row per (administration, item) with `uid`, `unilemma`, `category`,
#' `lexical_category`, and a numeric `value`.
build_item_table <- function(items, instruments) {
  items |>
    rename(unilemma = uni_lemma) |>
    filter(item_kind == "word", language != "Other Langs") |>
    mutate(item_id = as.character(item_id)) |>
    left_join(
      instruments |> select(language, form, item_id, uid) |>
        mutate(item_id = as.character(item_id)),
      by = join_by(language, form, item_id)
    ) |>
    mutate(
      uid = as.character(coalesce(as.character(uid),
                                  paste0(language, "_", item_id))),
      value = as.numeric(produces)
    ) |>
    filter(!is.na(value))
}

# ---------------------------------------------------------------------------
# Lexical class
# ---------------------------------------------------------------------------

#' Derive a `lex_class` column from Wordbank `lexical_category` plus instrument
#' `category`.
#'
#' `coarse = FALSE` gives the 5-level scheme (nouns / verbs / adjectives /
#' function_words / other), mirroring the `lex_class_o2` rules in
#' 04_o2_lexical_class.qmd. `coarse = TRUE` collapses to 4 levels
#' (nouns / predicates / closed_class / other, the last also absorbing items with
#' no `lexical_category`) — use this for the bifactor-by-lexical-class model in
#' 08, where each extra specific factor adds an EM quadrature dimension.
#'
#' Rows that resolve to NA are kept (so callers can QC them) unless
#' `drop_na = TRUE`.
derive_lex_class <- function(df, coarse = FALSE, drop_na = FALSE) {
  out <- df |>
    mutate(lex_class = case_when(
      category == "action_words"        ~ "verbs",
      category == "descriptive_words"   ~ "adjectives",
      lexical_category == "adverbs"     ~ "other",
      lexical_category == "nouns"       ~ "nouns",
      lexical_category == "function_words" ~ "function_words",
      lexical_category == "verbs"       ~ "verbs",
      lexical_category == "adjectives"  ~ "adjectives",
      lexical_category == "predicates"  ~ "other",
      lexical_category == "other"       ~ "other",
      TRUE ~ NA_character_
    ))
  if (coarse) {
    out <- out |>
      mutate(lex_class = case_when(
        lex_class == "nouns"                    ~ "nouns",
        lex_class %in% c("verbs", "adjectives") ~ "predicates",
        lex_class == "function_words"           ~ "closed_class",
        TRUE                                    ~ "other"   # incl. "other" + NA
      ))
  }
  if (drop_na) out <- filter(out, !is.na(lex_class))
  out
}

# ---------------------------------------------------------------------------
# Exposure lookup
# ---------------------------------------------------------------------------

#' TRUE where a CDI `language` (e.g. "English (American)") corresponds to an
#' exposure record's `exposure_language` (e.g. "English").
#'
#' Same rule used in 01_irt_model.qmd / 05_o3_3_per_language_irt.qmd: prefix
#' match, plus the Chinese/Mandarin special case.
exposure_language_matches <- function(language, exposure_language) {
  base <- str_replace(exposure_language, "^Chinese$", "Mandarin")
  str_detect(language, str_c("^", str_escape(base)))
}

#' One row per (child_id, language) with `exposure_proportion` in [0, 1] for the
#' requested target `languages`. `df_demogs` is `readRDS(here("data",
#' "demographics.rds"))` with its nested `languages` list-column.
child_language_exposure <- function(df_demogs, languages) {
  df_demogs |>
    mutate(child_id = as.character(child_id)) |>
    select(child_id, age, dataset_name, languages) |>
    # Wordbank stores the nested exposure_proportion as integer / double / logical
    # across rows; normalise every nested tibble to (chr, dbl) before unnest so
    # vctrs doesn't choke on the mixed ptypes.
    mutate(languages = map(languages, \(d) {
      if (is.null(d) || !nrow(d)) {
        return(tibble(exposure_language = character(),
                      exposure_proportion = double()))
      }
      tibble(exposure_language   = as.character(d$exposure_language),
             exposure_proportion = suppressWarnings(as.numeric(d$exposure_proportion)))
    })) |>
    unnest(languages) |>
    filter(!is.na(exposure_language), !is.na(exposure_proportion),
           exposure_proportion <= 100) |>
    mutate(exposure_proportion = exposure_proportion / 100) |>
    tidyr::expand_grid(target_language = languages) |>
    filter(exposure_language_matches(target_language, exposure_language)) |>
    group_by(child_id, language = target_language) |>
    summarise(age = first(age),
              dataset_name = first(dataset_name),
              exposure_proportion = first(exposure_proportion),
              .groups = "drop")
}

# ---------------------------------------------------------------------------
# Child x item response matrix for a language pair (used by mIRT + calibration)
# ---------------------------------------------------------------------------

#' Build the wide child x uid response matrix (+ aligned item lookup and person
#' covariates) for one language pair, keeping only children administered in BOTH
#' languages.
#'
#' Returns list(resp, item_lookup, covdata) with rows of `resp` and `covdata`
#' aligned by `child_id`.
build_pair_matrix <- function(item_table, df_demogs, langs,
                              min_items_per_child = 20,
                              min_item_n = 50,
                              item_p_bounds = c(0.02, 0.98),
                              coarse_lex_class = TRUE) {
  stopifnot(length(langs) == 2)

  df_pair <- item_table |> filter(language %in% langs)

  children_both <- df_pair |>
    distinct(child_id, language) |>
    count(child_id, name = "n_langs") |>
    filter(n_langs >= 2) |>
    pull(child_id)

  df_pair <- df_pair |>
    filter(child_id %in% children_both) |>
    mutate(uid = as.character(uid)) |>
    derive_lex_class(coarse = coarse_lex_class)

  # one row per uid: a handful of CDI items carry >1 uni_lemma / lexical_category
  # (e.g. "drink (beverage)" vs "drink (object)", "beach" noun vs predicate) ->
  # take the most common, ties broken by first.
  pick_mode <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_)
    names(sort(table(x), decreasing = TRUE))[1] }
  item_lookup <- df_pair |>
    group_by(uid) |>
    summarise(language  = first(language),
              lex_class = pick_mode(lex_class),
              unilemma  = pick_mode(unilemma),
              .groups = "drop")

  resp_long <- df_pair |>
    group_by(child_id, uid) |>
    summarise(value = max(value), .groups = "drop")

  resp_wide <- resp_long |>
    pivot_wider(names_from = uid, values_from = value) |>
    column_to_rownames("child_id")

  # drop near-constant / thin items (break 2PL)
  keep <- vapply(resp_wide, \(x) {
    p <- mean(x, na.rm = TRUE)
    sum(!is.na(x)) >= min_item_n && p > item_p_bounds[1] && p < item_p_bounds[2]
  }, logical(1))
  resp_wide <- resp_wide[, keep, drop = FALSE]

  # per-child item-count filter
  resp_wide <- resp_wide[rowSums(!is.na(resp_wide)) >= min_items_per_child, ,
                         drop = FALSE]

  # order columns by language then uid, so factor specs can use range syntax
  ord <- item_lookup |>
    filter(uid %in% colnames(resp_wide)) |>
    arrange(match(language, langs), uid) |>
    pull(uid)
  resp_wide <- resp_wide[, ord, drop = FALSE]

  item_lookup <- item_lookup |>
    filter(uid %in% colnames(resp_wide)) |>
    arrange(match(uid, colnames(resp_wide)))

  # person covariates: age + exposure to language A (B implied)
  exp_tbl <- child_language_exposure(df_demogs, langs)
  covdata <- tibble(child_id = rownames(resp_wide)) |>
    left_join(
      exp_tbl |> filter(language == langs[1]) |>
        select(child_id, age, exp_a = exposure_proportion),
      by = "child_id"
    ) |>
    mutate(
      exp_a = coalesce(exp_a, median(exp_a, na.rm = TRUE)),
      age   = coalesce(age, median(age, na.rm = TRUE)),
      exp_a_c = exp_a - mean(exp_a),
      age_c   = age - mean(age)
    )

  resp_wide <- resp_wide[covdata$child_id, , drop = FALSE]
  stopifnot(identical(rownames(resp_wide), covdata$child_id))

  list(resp = as.matrix(resp_wide),
       item_lookup = item_lookup,
       covdata = covdata)
}

# ---------------------------------------------------------------------------
# Long responses -> Stan data for models/bilingual_dim_irt.stan
# ---------------------------------------------------------------------------

#' Assemble the Stan data list for `models/bilingual_dim_irt.stan`.
#'
#' `obs` needs columns: child_id, uid, language, unilemma (may be NA), y, age,
#' plus per-child exposure. `langs` fixes the language index order (1 = langs[1]).
#' `exposure` is one row per (child_id, language) with `exposure_proportion` in
#' [0, 1] (e.g. from `child_language_exposure()`); missing values are imputed to
#' the complement / 0.5.
build_dim_irt_stan_data <- function(obs, exposure, langs,
                                    estimate_general = 1L,
                                    estimate_specific = 1L,
                                    link_concepts = NA,
                                    compute_loglik = 1L) {
  stopifnot(length(langs) == 2)

  obs <- obs |>
    mutate(child_id = as.character(child_id),
           concept_key = coalesce(as.character(unilemma), uid))

  # auto: only link translation equivalents if a decent share of items actually
  # share a uni_lemma across the two languages
  if (is.na(link_concepts)) {
    n_shared <- obs |>
      distinct(uid, concept_key, language) |>
      count(concept_key) |>
      summarise(shared = sum(n > 1)) |>
      pull(shared)
    link_concepts <- as.integer(n_shared >= 0.25 * n_distinct(obs$uid))
    message("build_dim_irt_stan_data: ", n_shared,
            " translation-equivalent concepts -> link_concepts = ", link_concepts)
  }

  child_lvl   <- sort(unique(obs$child_id))
  item_lvl    <- sort(unique(obs$uid))
  concept_lvl <- sort(unique(obs$concept_key))

  obs <- obs |>
    mutate(child_ix = match(child_id, child_lvl),
           item_ix  = match(uid, item_lvl),
           lang_ix  = match(language, langs),
           age_sc   = as.numeric(scale(age)))
  stopifnot(!anyNA(obs$lang_ix))

  item_tbl <- obs |>
    distinct(item_ix, uid, concept_key, lang_ix) |>
    arrange(item_ix) |>
    mutate(concept_ix = match(concept_key, concept_lvl))

  exp_wide <- tibble(child_id = child_lvl) |>
    left_join(exposure |> filter(language == langs[1]) |>
                transmute(child_id = as.character(child_id), e1 = exposure_proportion),
              by = "child_id") |>
    left_join(exposure |> filter(language == langs[2]) |>
                transmute(child_id = as.character(child_id), e2 = exposure_proportion),
              by = "child_id") |>
    mutate(e1 = coalesce(e1, 1 - e2, 0.5),
           e2 = coalesce(e2, 1 - e1, 0.5))

  exposure_c <- cbind(e1 = exp_wide$e1 - mean(exp_wide$e1),
                      e2 = exp_wide$e2 - mean(exp_wide$e2))
  dominant_lang <- max.col(cbind(exp_wide$e1, exp_wide$e2), ties.method = "first")

  list(
    data = list(
      N = nrow(obs), I = length(child_lvl), J = length(item_lvl),
      C = length(concept_lvl), L = 2L,
      y = as.integer(obs$y), child = obs$child_ix, item = obs$item_ix,
      age_sc = obs$age_sc,
      concept = item_tbl$concept_ix, item_lang = item_tbl$lang_ix,
      exposure_c = exposure_c, dominant_lang = as.integer(dominant_lang),
      estimate_general = as.integer(estimate_general),
      estimate_specific = as.integer(estimate_specific),
      link_concepts = as.integer(link_concepts),
      compute_loglik = as.integer(compute_loglik)
    ),
    child_lvl = child_lvl,
    item_lvl = item_lvl,
    concept_lvl = concept_lvl
  )
}

#' Convert a wide 0/1 matrix + covariate frame (as produced by
#' `build_pair_matrix()` or `10_..._calibration.qmd`'s simulator) into the long
#' `obs` frame that `build_dim_irt_stan_data()` expects.
wide_to_long_obs <- function(resp, item_lookup, covdata) {
  as_tibble(resp, rownames = "child_id") |>
    pivot_longer(-child_id, names_to = "uid", values_to = "y") |>
    filter(!is.na(y)) |>
    left_join(item_lookup |> select(uid, language, unilemma), by = "uid") |>
    left_join(covdata |> select(child_id, age), by = "child_id")
}

# ---------------------------------------------------------------------------
# mIRT post-processing
# ---------------------------------------------------------------------------

#' Standardised item loadings from a fitted multidimensional `mirt` model,
#' following Stucky & Edelen (2014): lambda = a / sqrt(1 + sum a^2).
#' Returns an items x factors matrix.
mirt_std_loadings <- function(mod) {
  a <- mirt::coef(mod, simplify = TRUE)$items
  a <- a[, grepl("^a[0-9]+$", colnames(a)), drop = FALSE]
  a / sqrt(1 + rowSums(a^2))
}

#' Standardised-loading ECV, omega_total and omega_h for a fitted bifactor model
#' (general factor + orthogonal specific factors).
#'
#' `general` is the column index of the general-factor slope. When the model is
#' built from an explicit `mirt.model()` spec that names the general factor first
#' (`G = 1-J` then the specifics) this is `1` — the default. mirt's `bfactor()`
#' also orders the general factor first (`a1`).
bifactor_ecv_omega <- function(bf_mod, general = 1) {
  lambda <- mirt_std_loadings(bf_mod)
  gen    <- lambda[, general]
  spec   <- lambda[, setdiff(seq_len(ncol(lambda)), general), drop = FALSE]

  u2 <- pmax(1 - gen^2 - rowSums(spec^2), 0)
  ss_spec <- sum(colSums(spec)^2)
  gg      <- sum(gen)^2
  denom_o <- gg + ss_spec + sum(u2)

  tibble(
    ECV         = sum(gen^2) / sum(gen^2 + rowSums(spec^2)),
    omega_total = (gg + ss_spec) / denom_o,
    omega_h     = gg / denom_o
  )
}

#' Correlation between the two language-ability composites implied by a
#' bifactor-by-language model: r ~ sqrt(ECV_A * ECV_B), where ECV_k is the share
#' of group k's common variance carried by the general factor. `group_items` is a
#' vector (length = n items, in model column order) naming each item's language.
bifactor_cross_corr <- function(bf_mod, group_items, general = 1) {
  lambda <- mirt_std_loadings(bf_mod)
  gen  <- lambda[, general]
  spec <- lambda[, setdiff(seq_len(ncol(lambda)), general), drop = FALSE]
  grp_ecv <- tapply(seq_along(group_items), group_items, function(ix) {
    sum(gen[ix]^2) / sum(gen[ix]^2 + rowSums(spec[ix, , drop = FALSE]^2))
  })
  tibble(group = names(grp_ecv), ecv_group = as.numeric(grp_ecv)) |>
    tidyr::pivot_wider(names_from = group, values_from = ecv_group) |>
    mutate(implied_r = sqrt(prod(grp_ecv)))
}

#' K-fold cross-validated observed-data log-likelihood for a mirt model.
#'
#' `specfun(data, covdata, ...)` must fit and return a mirt object; extra `...`
#' (e.g. `pars=`, `technical=`) are forwarded. Parameters are estimated on the
#' training fold, fixed, and scored on the held-out fold. Requires a `future`
#' plan to be set by the caller for parallelism.
#'
#' NOTE: each fold refits the model from scratch, so for the bifactor models
#' (~15 min per fit on full item sets) a 5-fold CV is ~75 min per model. Thin the
#' item set (08's `thin_items` param) before running this.
cv_loglik_mirt <- function(resp, covdata, specfun, K = 5, seed = 123) {
  set.seed(seed)
  folds <- sample(rep(seq_len(K), length.out = nrow(resp)))
  furrr::future_map_dbl(seq_len(K), function(k) {
    tr <- which(folds != k); te <- which(folds == k)
    fit_tr <- specfun(resp[tr, , drop = FALSE], covdata[tr, , drop = FALSE])
    pars <- mirt::mod2values(fit_tr)
    pars$est <- FALSE
    fit_te <- specfun(resp[te, , drop = FALSE], covdata[te, , drop = FALSE],
                      pars = pars, technical = list(NCYCLES = 1))
    as.numeric(mirt::extract.mirt(fit_te, "logLik"))
  }, .options = furrr::furrr_options(seed = TRUE)) |> sum()
}

#' Build the `mirt.model()` spec strings for the O5 model space, given a
#' `language` vector and a `lex_class` vector in model column order.
#'
#' Returns a list with `M0` (unidimensional), `M_lang` (bifactor: general +
#' one specific per language) and `M_class` (bifactor: general + one specific
#' per lexical class). Columns are assumed grouped by language (as
#' `build_pair_matrix()` returns them), so language specifics use range syntax.
o5_model_specs <- function(language, lex_class) {
  J <- length(language)
  rng <- function(ix) {
    w <- which(ix)
    if (all(diff(w) == 1)) sprintf("%d-%d", min(w), max(w))
    else paste(w, collapse = ",")
  }
  langs <- unique(language)
  classes <- sort(unique(lex_class))

  list(
    M0 = mirt::mirt.model(sprintf("F = 1-%d", J)),
    M_lang = mirt::mirt.model(paste0(
      sprintf("G = 1-%d\n", J),
      paste(sprintf("S_%s = %s", make.names(langs),
                    vapply(langs, \(l) rng(language == l), character(1))),
            collapse = "\n")
    )),
    M_class = mirt::mirt.model(paste0(
      sprintf("G = 1-%d\n", J),
      paste(sprintf("S_%s = %s", make.names(classes),
                    vapply(classes, \(k) rng(lex_class == k), character(1))),
            collapse = "\n")
    ))
  )
}
