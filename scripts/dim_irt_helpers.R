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

#' Check that `child_id` is globally unique across datasets in `df_demogs`.
#' Joins downstream key on `child_id` alone, which is only safe if `child_id` is
#' unique across the WHOLE file, not just within a dataset -- verified true for
#' the current export (every child_id maps to exactly one dataset_name) but
#' re-check this if `data/demographics.rds` is ever regenerated from a different
#' pull.
check_child_id_unique <- function(df_demogs) {
  dup <- df_demogs |> dplyr::distinct(child_id, dataset_name) |>
    dplyr::count(child_id) |> dplyr::filter(n > 1)
  if (nrow(dup)) {
    stop("child_id is not globally unique in demographics.rds (", nrow(dup),
        " child_id(s) span >1 dataset_name) -- joins keyed on child_id alone ",
        "would silently mix different children. Key on (dataset_name, child_id) instead.")
  }
  invisible(TRUE)
}

#' One row per (admin_id, language) with `exposure_proportion` in [0, 1] for the
#' requested target `languages`, where `admin_id` is the Wordbank `data_id` of
#' the administration the exposure record belongs to. `df_demogs` is
#' `readRDS(here("data", "demographics.rds"))` -- one row per administration,
#' with its nested `languages` list-column. Also carries the administration's
#' `age`, plus `total_listed` (sum of that record's exposure proportions across
#' ALL its listed languages, ~1 when the parent gave a complete breakdown) and
#' `n_langs_listed`, so callers can tell an unlisted target language ("0%
#' exposure") from a genuinely missing record.
#'
#' Longitudinal datasets (Hoff, Marchman, Marchman_BTL, Weisleder, ...) have one
#' record per visit, and exposure can change between visits -- use this, not
#' `child_language_exposure()`, whenever responses are kept per administration.
admin_language_exposure <- function(df_demogs, languages) {
  check_child_id_unique(df_demogs)

  long <- df_demogs |>
    mutate(child_id = as.character(child_id)) |>
    select(admin_id = data_id, child_id, age, dataset_name, languages) |>
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
    mutate(exposure_proportion = exposure_proportion / 100)

  # per-record total of ALL listed exposure (not just target languages) -- used
  # downstream to decide whether an unlisted target language means "0% exposure"
  # (the record's listed exposures already sum to ~100) vs. genuinely missing.
  total_listed <- long |>
    group_by(admin_id) |>
    summarise(total_listed = sum(exposure_proportion),
              n_langs_listed = n(), .groups = "drop")

  long |>
    tidyr::expand_grid(target_language = languages) |>
    filter(exposure_language_matches(target_language, exposure_language)) |>
    group_by(admin_id, child_id, language = target_language) |>
    summarise(age = first(age),
              dataset_name = first(dataset_name),
              exposure_proportion = first(exposure_proportion),
              .groups = "drop") |>
    left_join(total_listed, by = "admin_id")
}

#' One row per (child_id, language): the child-level summary of
#' `admin_language_exposure()` -- `exposure_proportion` / `total_listed` are
#' averaged over the child's administrations that have a record for that
#' language. `age` is the child's EARLIEST recorded age; it is kept for
#' backwards compatibility only and must NOT be attached to responses as their
#' age (a child's administrations span many ages in the longitudinal datasets
#' -- see `build_pair_long()`).
child_language_exposure <- function(df_demogs, languages) {
  admin_language_exposure(df_demogs, languages) |>
    group_by(child_id, language) |>
    summarise(age = min(age, na.rm = TRUE),
              dataset_name = first(dataset_name),
              exposure_proportion = mean(exposure_proportion),
              total_listed = mean(total_listed),
              n_langs_listed = max(n_langs_listed),
              .groups = "drop") |>
    mutate(age = if_else(is.finite(age), age, NA_real_))
}

# ---------------------------------------------------------------------------
# Sample health screen -- run BEFORE fitting
# ---------------------------------------------------------------------------

#' Per-language diagnostics for a candidate `langs` sample: what share of
#' administrations have (near-)zero vocabulary in each language (a floor that
#' leaves that language's ability factor unidentified) and how many children
#' have an exposure record.
#'
#' The Malaysian "trilingual" sample looked like independent lexicons only
#' because English (54% of kids <5 words) and Mandarin (86%) were at the floor
#' and only 22% had a Mandarin exposure record -- run this and eyeball
#' `frac_floor` / `exposure_coverage` before trusting any rho.
sample_health <- function(item_table, df_demogs, langs, floor_words = 5) {
  kids <- item_table |>
    filter(language %in% langs, item_kind == "word") |>
    distinct(child_id, language) |>
    count(child_id) |>
    filter(n >= length(langs)) |>
    pull(child_id)
  exp_tbl <- child_language_exposure(df_demogs, langs)
  purrr::map_dfr(langs, \(l) {
    # per ADMINISTRATION: summing over a longitudinal child's visits would
    # inflate their vocabulary and hide a floor
    v <- item_table |>
      filter(child_id %in% kids, language == l, item_kind == "word") |>
      group_by(child_id, data_id) |>
      summarise(n_produced = sum(produces, na.rm = TRUE), .groups = "drop")
    tibble(
      language = l,
      n_children = length(kids),
      n_admins = nrow(v),
      vocab_median = median(v$n_produced),
      frac_floor = mean(v$n_produced < floor_words),
      exposure_coverage = mean(as.character(kids) %in%
        (exp_tbl |> filter(language == l) |> pull(child_id)))
    )
  })
}

# ---------------------------------------------------------------------------
# Responses for a language pair: long (per administration) and wide (per child)
# ---------------------------------------------------------------------------

#' Long-form responses for `langs` (2 or more): one row per (child,
#' administration, item), each carrying its OWN administration's `age` and
#' `admin_id` (Wordbank `data_id`). Keeps only children administered in ALL of
#' `langs` (e.g. the trilingual English/Malay/Mandarin (Malaysian) sample).
#'
#' Longitudinal datasets (EN-American x ES-Mexican: Hoff, Marchman,
#' Marchman_BTL, Weisleder, LewWilliams) give many children several
#' administrations per language, 15-40 months apart. These used to be collapsed
#' to "ever produced" (`max(value)` over visits) and paired with a single
#' child-level age, which destroyed the age signal (cor(age, proportion
#' produced) ~ 0). Here each administration stays its own unit:
#'
#' - `admins = "all"`    -- every administration (for the Stan model, which
#'   shares a child-level ability across visits and adds a per-administration
#'   deviation; see `build_dim_irt_stan_data()`).
#' - `admins = "latest"` -- one administration per (child, language): the
#'   oldest-age one (most words, least floor). Cross-sectional, as the model
#'   originally assumed.
#'
#' Administrations with no recorded age are dropped. Item filters
#' (`min_item_n`, `item_p_bounds`) count administrations; `min_items_per_admin`
#' drops administrations with too few retained items, after which children must
#' still have every language.
#'
#' Returns list(obs, item_lookup, admins), where `obs` has child_id, admin_id,
#' age, uid, language, unilemma, lex_class, y; `admins` is one row per
#' administration (admin_id, child_id, language, age, dataset_name).
build_pair_long <- function(item_table, df_demogs, langs,
                            admins = c("all", "latest"),
                            min_items_per_admin = 20,
                            min_item_n = 50,
                            item_p_bounds = c(0.02, 0.98),
                            coarse_lex_class = TRUE,
                            check_health = TRUE) {
  stopifnot(length(langs) >= 2)   # works for any number of languages, not just pairs
  admins <- match.arg(admins)

  if (check_health) {
    health <- sample_health(item_table, df_demogs, langs)
    bad <- health |> filter(frac_floor > 0.30 | exposure_coverage < 0.60)
    if (nrow(bad)) {
      warning("build_pair_long: shaky sample -- ",
              paste(sprintf("%s (%.0f%% of administrations <5 words, %.0f%% of children have exposure)",
                            bad$language, 100 * bad$frac_floor,
                            100 * bad$exposure_coverage), collapse = "; "),
              ". A near-floor language's ability factor is unidentified; rho will ",
              "be attenuated toward 0. Inspect sample_health() before trusting the fit.",
              call. = FALSE)
    }
  }

  df_pair <- item_table |>
    filter(language %in% langs) |>
    mutate(child_id = as.character(child_id), admin_id = data_id,
           uid = as.character(uid))

  n_no_age <- n_distinct(df_pair$admin_id[is.na(df_pair$age)])
  if (n_no_age) {
    message("build_pair_long: dropping ", n_no_age,
            " administration(s) with no recorded age")
    df_pair <- filter(df_pair, !is.na(age))
  }

  in_all_langs <- function(d) {
    keep <- d |> distinct(child_id, language) |> count(child_id) |>
      filter(n >= length(langs)) |> pull(child_id)
    filter(d, child_id %in% keep)
  }
  df_pair <- in_all_langs(df_pair)

  if (admins == "latest") {
    latest <- df_pair |>
      distinct(child_id, language, admin_id, age) |>
      arrange(child_id, language, desc(age), desc(admin_id)) |>
      distinct(child_id, language, .keep_all = TRUE)
    df_pair <- semi_join(df_pair, latest, by = "admin_id")
  }

  df_pair <- derive_lex_class(df_pair, coarse = coarse_lex_class)

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

  # one response per (administration, item) -- a form never lists an item twice,
  # but guard against duplicated rows in the export
  obs <- df_pair |>
    group_by(child_id, admin_id, age, uid) |>
    summarise(y = max(value), .groups = "drop")

  # drop near-constant / thin items (break 2PL)
  item_ok <- obs |>
    group_by(uid) |>
    summarise(n = n(), p = mean(y), .groups = "drop") |>
    filter(n >= min_item_n, p > item_p_bounds[1], p < item_p_bounds[2]) |>
    pull(uid)
  obs <- filter(obs, uid %in% item_ok)

  # per-administration item-count filter, then children must keep every language
  obs <- obs |>
    group_by(admin_id) |>
    filter(n() >= min_items_per_admin) |>
    ungroup() |>
    left_join(item_lookup |> select(uid, language, unilemma, lex_class), by = "uid") |>
    in_all_langs()

  # order items by language then uid, so factor specs can use range syntax
  item_lookup <- item_lookup |>
    filter(uid %in% obs$uid) |>
    arrange(match(language, langs), uid)

  admin_tbl <- obs |>
    distinct(admin_id, child_id, language, age) |>
    left_join(item_table |> distinct(admin_id = data_id, dataset_name),
              by = "admin_id")

  list(obs = obs |> arrange(child_id, age, admin_id, match(uid, item_lookup$uid)),
       item_lookup = item_lookup,
       admins = admin_tbl)
}

#' Build the wide child x uid response matrix (+ aligned item lookup and person
#' covariates) for `langs` (2 or more), for the mIRT route (08) and the
#' calibration simulator (10), which need one row per child.
#'
#' Each child contributes ONE administration per language -- the latest
#' (`build_pair_long(admins = "latest")`), never a collapse across visits.
#' `covdata$age` is the mean age of the child's selected administrations (the
#' languages are usually administered at the same visit; the median gap is 0),
#' and `covdata$exp_a` is language[1]'s exposure from the selected language[1]
#' administration's own record. For repeated administrations use
#' `build_pair_long()` + `build_dim_irt_stan_data()` instead.
#'
#' `covdata$exp_a` / `exp_a_c` are language[1]'s exposure only (used by the mIRT
#' route in 08, which is pairwise); the Stan pipeline (09/10) gets exposure for
#' every language from `admin_language_exposure()` instead, so it is unaffected
#' by `length(langs) > 2`.
#'
#' Returns list(resp, item_lookup, covdata) with rows of `resp` and `covdata`
#' aligned by `child_id`.
build_pair_matrix <- function(item_table, df_demogs, langs,
                              min_items_per_child = 20,
                              min_item_n = 50,
                              item_p_bounds = c(0.02, 0.98),
                              coarse_lex_class = TRUE) {
  long <- build_pair_long(item_table, df_demogs, langs, admins = "latest",
                          min_items_per_admin = 1, min_item_n = min_item_n,
                          item_p_bounds = item_p_bounds,
                          coarse_lex_class = coarse_lex_class)

  resp_wide <- long$obs |>
    select(child_id, uid, y) |>
    pivot_wider(names_from = uid, values_from = y) |>
    column_to_rownames("child_id")
  resp_wide <- resp_wide[, long$item_lookup$uid, drop = FALSE]

  # per-child item-count filter (across languages, as before)
  resp_wide <- resp_wide[rowSums(!is.na(resp_wide)) >= min_items_per_child, ,
                         drop = FALSE]

  # person covariates: age (of the selected administrations) + exposure to
  # language A (B implied), from the selected language-A administration
  exp_tbl <- admin_language_exposure(df_demogs, langs)
  covdata <- tibble(child_id = rownames(resp_wide)) |>
    left_join(long$admins |> group_by(child_id) |>
                summarise(age = mean(age), .groups = "drop"),
              by = "child_id") |>
    left_join(long$admins |> filter(language == langs[1]) |>
                select(child_id, admin_id) |>
                left_join(exp_tbl |> filter(language == langs[1]) |>
                            select(admin_id, exp_a = exposure_proportion),
                          by = "admin_id") |>
                select(child_id, exp_a),
              by = "child_id") |>
    mutate(
      exp_a = coalesce(exp_a, median(exp_a, na.rm = TRUE)),
      exp_a_c = exp_a - mean(exp_a),
      age_c   = age - mean(age)
    )

  resp_wide <- resp_wide[covdata$child_id, , drop = FALSE]
  stopifnot(identical(rownames(resp_wide), covdata$child_id), !anyNA(covdata$age))

  list(resp = as.matrix(resp_wide),
       item_lookup = long$item_lookup |> filter(uid %in% colnames(resp_wide)),
       covdata = covdata)
}

# ---------------------------------------------------------------------------
# Long responses -> Stan data for models/bilingual_dim_irt.stan
# ---------------------------------------------------------------------------

#' Assemble the Stan data list for `models/bilingual_dim_irt.stan`.
#'
#' `obs` needs columns: child_id, uid, language, unilemma (may be NA), y, age --
#' and, for repeated administrations, `admin_id` (as from `build_pair_long()`),
#' where `age` is that administration's age. `langs` (2 or more) fixes the
#' language index order.
#'
#' `exposure` gives `exposure_proportion` in [0, 1] per language, either per
#' administration (`admin_id` column, from `admin_language_exposure()`; used
#' when `obs` also has `admin_id`) or per child (from
#' `child_language_exposure()`). Missing values are imputed — 0 for a language
#' the record doesn't list when its listed exposures already sum to ~1, else to
#' the complement when `length(langs) == 2`, otherwise to that language's median
#' (a 2-language complement assumption doesn't hold for 3+ languages). Each
#' observation gets the (centred) exposure to its own item's language, from its
#' own administration's record.
#'
#' `estimate_admin` switches on a per-administration ability deviation
#' (`sigma_admin[l] * z_admin`) on top of the child-level ability; `NA` = on iff
#' some child has more than one administration in some language (otherwise it is
#' not separable from the language-specific child ability).
#'
#' Returns list(data, child_lvl, item_lvl, concept_lvl, admin_lvl, age_basis),
#' where `age_basis` holds what is needed to evaluate the fitted age curve at
#' new ages: the age centre / scale and the `splines::ns()` basis object.
build_dim_irt_stan_data <- function(obs, exposure, langs,
                                    estimate_general = 1L,
                                    estimate_specific = 1L,
                                    estimate_admin = NA,
                                    link_concepts = NA,
                                    age_spline_df = 3L,
                                    compute_loglik = 1L) {
  stopifnot(length(langs) >= 2, age_spline_df >= 1L, !anyNA(obs$age))

  obs <- obs |>
    mutate(child_id = as.character(child_id),
           concept_key = coalesce(as.character(unilemma), uid))
  if (!"admin_id" %in% names(obs)) {
    # one-administration-per-child data (wide route): admin = child x language
    obs <- mutate(obs, admin_id = paste(child_id, language, sep = "::"))
  }
  obs <- mutate(obs, admin_id = as.character(admin_id))

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

  if (is.na(estimate_admin)) {
    repeated <- obs |> distinct(child_id, language, admin_id) |>
      count(child_id, language) |> filter(n > 1)
    estimate_admin <- as.integer(nrow(repeated) > 0)
    message("build_dim_irt_stan_data: ", nrow(repeated),
            " child x language cells with repeated administrations -> estimate_admin = ",
            estimate_admin)
  }

  child_lvl   <- sort(unique(obs$child_id))
  item_lvl    <- sort(unique(obs$uid))
  concept_lvl <- sort(unique(obs$concept_key))
  admin_lvl   <- sort(unique(obs$admin_id))

  age_mean <- mean(obs$age); age_sd <- sd(obs$age)
  obs <- obs |>
    mutate(child_ix = match(child_id, child_lvl),
           item_ix  = match(uid, item_lvl),
           admin_ix = match(admin_id, admin_lvl),
           lang_ix  = match(language, langs),
           age_sc   = (age - age_mean) / age_sd)
  stopifnot(!anyNA(obs$lang_ix))

  # age-spline basis (df = 1 -> effectively linear); passed as data so Stan just
  # takes a dot product with the coefficient vector
  ns_basis <- if (age_spline_df > 1L) splines::ns(obs$age_sc, df = age_spline_df)
  age_spline <- if (is.null(ns_basis)) matrix(obs$age_sc, ncol = 1) else unclass(ns_basis)

  item_tbl <- obs |>
    distinct(item_ix, uid, concept_key, lang_ix) |>
    arrange(item_ix) |>
    mutate(concept_ix = match(concept_key, concept_lvl))

  # ---- exposure: one row per key (administration, or child), one col per language
  key <- if ("admin_id" %in% names(exposure) &&
             any(as.character(exposure$admin_id) %in% obs$admin_id)) "admin_id" else "child_id"
  exposure <- exposure |>
    mutate(child_id = as.character(child_id),
           across(any_of("admin_id"), as.character))
  key_lvl <- if (key == "admin_id") admin_lvl else child_lvl
  key_child <- if (key == "admin_id") {
    obs$child_id[match(admin_lvl, obs$admin_id)]
  } else child_lvl

  exp_mat <- sapply(langs, \(l) {
    tibble(k = key_lvl) |>
      left_join(exposure |> filter(language == l) |>
                  select(k = all_of(key), exposure_proportion) |>
                  distinct(k, .keep_all = TRUE),
                by = "k") |>
      pull(exposure_proportion)
  })
  if (is.null(dim(exp_mat))) exp_mat <- matrix(exp_mat, nrow = 1)

  # A parent who lists e.g. "Malay 90 / English 10" and is silent about Mandarin
  # means the child has ~0% Mandarin exposure, not "missing". Impute an unlisted
  # target language to 0 whenever the record's listed exposures already sum to
  # ~1; only genuinely uninformative rows fall back to complement (L=2) / median.
  total_listed <- tibble(k = key_lvl) |>
    left_join(exposure |> select(k = all_of(key), total_listed) |>
                distinct(k, .keep_all = TRUE),
              by = "k") |>
    pull(total_listed)
  complete_profile <- !is.na(total_listed) & total_listed >= 0.95
  for (l in seq_len(ncol(exp_mat))) {
    exp_mat[complete_profile & is.na(exp_mat[, l]), l] <- 0
  }

  if (length(langs) == 2) {
    exp_mat[, 1] <- coalesce(exp_mat[, 1], 1 - exp_mat[, 2], 0.5)
    exp_mat[, 2] <- coalesce(exp_mat[, 2], 1 - exp_mat[, 1], 0.5)
  } else {
    for (l in seq_len(ncol(exp_mat))) {
      exp_mat[, l] <- coalesce(exp_mat[, l], median(exp_mat[, l], na.rm = TRUE), 0.5)
    }
  }
  n_zero_imputed <- sum(complete_profile & rowSums(exp_mat == 0, na.rm = TRUE) > 0)
  if (n_zero_imputed) {
    message("build_dim_irt_stan_data: imputed 0% exposure for an unlisted ",
            "language in ", n_zero_imputed, " ", key, "s with a complete profile")
  }

  # each observation: exposure to its own language from its own key's record,
  # centred per language over the (key, language) cells actually observed
  key_ix <- match(if (key == "admin_id") obs$admin_id else obs$child_id, key_lvl)
  exp_obs <- exp_mat[cbind(key_ix, obs$lang_ix)]
  cells <- distinct(tibble(key_ix, lang_ix = obs$lang_ix, e = exp_obs))
  centre <- tapply(cells$e, cells$lang_ix, mean)
  exposure_obs <- exp_obs - centre[as.character(obs$lang_ix)]

  # child's dominant language = highest mean exposure over their records
  dominant_lang <- rowsum(exp_mat, match(key_child, child_lvl))
  dominant_lang <- max.col(dominant_lang, ties.method = "first")

  list(
    data = list(
      N = nrow(obs), I = length(child_lvl), J = length(item_lvl),
      C = length(concept_lvl), L = length(langs), A = length(admin_lvl),
      y = as.integer(obs$y), child = obs$child_ix, item = obs$item_ix,
      admin = obs$admin_ix,
      age_sc = obs$age_sc,
      K_age = ncol(age_spline), age_spline = age_spline,
      concept = item_tbl$concept_ix, item_lang = item_tbl$lang_ix,
      exposure_obs = as.numeric(exposure_obs),
      dominant_lang = as.integer(dominant_lang),
      estimate_general = as.integer(estimate_general),
      estimate_specific = as.integer(estimate_specific),
      estimate_admin = as.integer(estimate_admin),
      link_concepts = as.integer(link_concepts),
      compute_loglik = as.integer(compute_loglik)
    ),
    child_lvl = child_lvl,
    item_lvl = item_lvl,
    concept_lvl = concept_lvl,
    admin_lvl = admin_lvl,
    age_basis = list(mean = age_mean, sd = age_sd, ns = ns_basis)
  )
}

#' Age-spline contribution to ability at `ages` (months), for posterior draws
#' `beta_age` (draws x K_age matrix), using the basis stored by
#' `build_dim_irt_stan_data()`. Returns a draws x length(ages) matrix.
age_effect_draws <- function(age_basis, beta_age, ages) {
  x <- (ages - age_basis$mean) / age_basis$sd
  loadNamespace("splines")   # registers predict.ns
  B <- if (is.null(age_basis$ns)) matrix(x, ncol = 1) else predict(age_basis$ns, x)
  as.matrix(beta_age) %*% t(B)
}

#' Convert a wide 0/1 matrix + covariate frame (as produced by
#' `build_pair_matrix()` or `10_..._calibration.qmd`'s simulator) into the long
#' `obs` frame that `build_dim_irt_stan_data()` expects. One administration per
#' child, so every response gets the child's `covdata$age`; for repeated
#' administrations use `build_pair_long()$obs` directly.
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
