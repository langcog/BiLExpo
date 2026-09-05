# Is bilingual lexical ability one dimension, or several?

**Status:** working draft, 2026-09. Analyses live in [`08_o5_dimensionality_mirt.qmd`](08_o5_dimensionality_mirt.qmd), [`09_o5_dimensionality_bayes.qmd`](09_o5_dimensionality_bayes.qmd), [`10_o5_dimensionality_calibration.qmd`](10_o5_dimensionality_calibration.qmd), [`models/bilingual_dim_irt.stan`](models/bilingual_dim_irt.stan), [`scripts/dim_irt_helpers.R`](scripts/dim_irt_helpers.R). Underlying data (`data/items.rds`, `data/demographics.rds`, `data/instruments/`) is embargoed and not in the repo; only aggregate posterior summaries are reported here.

## Question

If we look across *all* the words a bilingual child knows — both languages together — does their lexical ability look like one underlying trait, or several? The literature on monolingual CDI vocabulary generally treats this as unidimensional (one factor of maturational/lexical ability). For bilinguals the naive extension — column-bind the two CDIs, run the same exploratory multidimensional IRT — is attractive but biased: differential language exposure alone will force English items and Spanish items onto separate factors regardless of the true structure of ability, so an exploratory fit will report "found 2+ factors" almost no matter what.

We wanted a framing that (a) treats exposure as a covariate to be modeled rather than a factor to be discovered, and (b) reports a continuous answer (how much of the variance is shared vs. language-specific) rather than a binary "unidimensional or not."

## Why not just an exploratory factor model

- **Exposure confound.** A child with 15% Spanish exposure knows few Spanish words regardless of underlying ability; this alone manufactures a language factor.
- **Difficulty-factor artifact.** Binary items spanning CDI's huge difficulty range routinely produce a spurious second factor from nonlinearity alone, independent of any real second dimension.
- **Translation-equivalent structure.** Knowing *dog* and knowing *perro* are correlated beyond general ability (shared concept, `uni_lemma` in Wordbank); ignored, this local dependence distorts a factor solution.
- **Power.** Bilingual CDI samples are modest (hundreds, not thousands); distinguishing 1 vs. 2 vs. bifactor structure needs cross-validated or limited-information fit statistics, not raw in-sample likelihood, which always favors more factors.

## Three complementary approaches

1. **Confirmatory multidimensional IRT** ([`08`](08_o5_dimensionality_mirt.qmd), `mirt`): a small, theoretically motivated model space — unidimensional / bifactor-by-language / bifactor-by-lexical-class — with exposure entered as a latent regressor so a dominance-driven mean shift isn't mistaken for a dimension.
2. **Bayesian crossed random-effects IRT** ([`09`](09_o5_dimensionality_bayes.qmd), Stan): the dimensionality question reframed as a variance-components question — see [Model](#model) below.
3. **Simulation calibration** ([`10`](10_o5_dimensionality_calibration.qmd)): simulate CDI responses from a *known* structure using real item parameters and real age/exposure distributions, then run the same pipeline and see whether it recovers the truth. This is what turns "we estimated X" into "we know how much to trust X."

### What actually worked

The confirmatory mIRT route (1) does not converge on real bilingual samples of this size. On 474–690 children with hundreds of items, unregularized 2PL mIRT (`mirt` 1.46.1) either fails to converge within the EM cycle limit or, when forced, returns degenerate (e.g. negative general-factor) loadings — even after item thinning and hours of compute. `mirt::bfactor()` also does not support latent-regression covariates at all, and its `M2()` limited-information fit statistic rejects any model that has one. `08` is kept for the framing, the model-space logic, and the calibration pipeline, but **it is not the source of any real-data number reported here.**

The Bayesian route (2) is what produced trustworthy estimates, but only once we discovered that `rstan`'s mean-field ADVI diverges completely on this model (oscillating ELBO, posterior collapsing to a degenerate mode with runaway item discriminations) while NUTS on the identical model is well-behaved (zero divergences, sane R-hat/ESS). **Use NUTS, not ADVI/variational inference, for this model.**

## Model

Item easiness is decomposed into a translation-equivalent **concept** term (shared across languages via `uni_lemma`, e.g. *dog* / *perro*) plus a language-specific offset — dropped in favor of a single flat item-easiness level when too few items in a given language pair actually share a concept (an automatic check in `build_dim_irt_stan_data()`; translation-equivalent overlap between full CDI forms in different languages/countries turned out to be modest, 15–22% of items in every sample analyzed so far).

Child ability is decomposed into a **general** trait and **language-specific** deviations, independent across languages:

```
theta_child[i, L] = lambda_g · theta_g[i] + theta_s[i, L]
theta_g[i]        ~ Normal(0, 1)                      # scale-fixing anchor
theta_s[i, L]     ~ Normal(0, sigma_s[L])
```

with exposure and age entered as fixed, per-language effects on ability, and a standard 2PL response model, `p = logit⁻¹(alpha_j · (ability + b_item[j]))`.

This nests the two extreme hypotheses as limiting cases and, critically, generalizes past a single language pair:

- `sigma_s → 0` ⇒ **unidimensional** — one bilingual lexical trait, languages differ only by exposure-driven mean level
- `lambda_g → 0` ⇒ **independent lexicons** — no shared trait at all
- both free ⇒ report the **pairwise cross-language ability correlation** `rho[l1, l2] = lambda_g² / √(v[l1]·v[l2])` (`v[l] = lambda_g² + sigma_s[l]²`) and the **person-level explained common variance** `ECV = lambda_g² / (lambda_g² + mean(sigma_s²))`, both continuous, both defined for any number of languages `L ≥ 2` — this is what lets the same model fit bilingual pairs and the trilingual sample below without a different parameterization.

`rho` and `ECV` are mathematically equivalent for `L = 2`; for `L ≥ 3` `ECV` is the single scalar summary and `rho` becomes a full `L × L` matrix, one pairwise correlation per language pair.

## Data

Wordbank CDI administrations, filtered to children administered the instrument in every language of interest ("both"/"all" languages), pooling all datasets that include that language. Item easiness/discrimination and person ability are estimated jointly from the response data; exposure proportion and age are per-child, per-language covariates from the demographic records.

| sample | children | languages | items (after thinning for tractable NUTS runtime) |
|---|---:|---|---:|
| EN (American) × ES (Mexican) | 474 | 2 | 554 |
| EN (British) × ES (European) | 690 | 2 | ~420 |
| EN (Malaysian) / Malay (Malaysian) / Mandarin (Malaysian) | 569 | 3 | 548 |

Full, unthinned NUTS runs (1,600–1,700 items) were not attempted here; thinning trades estimation precision for tractable runtime (each reported fit is 2–13 hours of 4-chain NUTS depending on machine load) and is not expected to introduce bias in the person-level quantities (`rho`, `ECV`) — a full-item run is the natural robustness check.

## Results

### Bilingual pairs

| pair | children | `rho` / `ECV` (median, 95% CI) | `beta_age` |
|---|---:|---|---|
| English (American) × Spanish (Mexican) | 474 | **0.58** [0.52, 0.64] | 0.15 [−0.05, 0.35] |
| English (British) × Spanish (European) | 690 | **0.71** [0.66, 0.75] | 2.21 [2.03, 2.39] |

Both fits converged cleanly (zero divergences, max R-hat ≤ 1.005, `rho`/`ECV` effective sample sizes in the thousands). Both **decisively reject both extremes**: the posterior for `rho` excludes 1 (a single bilingual lexicon) and excludes 0 (two fully independent lexicons) by a wide margin. Exposure is the dominant driver of *which* language's words a child knows (`beta_exp` ≈ 3–5 on the centered exposure scale in both fits) — modeling it explicitly, rather than letting it manifest as a factor, is what makes the remaining `sigma_s` interpretable as genuine language-specific ability rather than dominance.

The two pairs agree on the qualitative picture but not on the exact split (58% vs. 71% general-dimension share), and disagree sharply on how much residual age explains after exposure and general ability are accounted for. Whether that's a real difference between these bilingual populations or an artifact of item thinning / sample composition is open — see [Limitations](#limitations--open-questions).

### Trilingual sample

English (Malaysian) / Malay (Malaysian) / Mandarin (Malaysian), 569 children administered all three CDIs. The Stan model and its R data-builders were generalized from a hardcoded 2-language parameterization to arbitrary `L` for this (the likelihood was already generic; only the `rho`/`ECV` generated quantities and the exposure-imputation logic were pair-specific). This is the first check of whether the ~55–70%-general picture holds beyond a single language pair, and with three specific factors instead of one — do all three languages share equally in the general dimension, or does one stand apart?

<!-- RESULTS_PLACEHOLDER_TRILINGUAL -->
*(fit in progress — results to follow)*

### Simulation calibration (is any of this trustworthy?)

Before believing the numbers above, we checked whether the Stan model can recover a *known* cross-language correlation. Using the real EN–SP form design (which items each child was and wasn't administered), age, and exposure distribution, with item difficulties from real marginal endorsement rates and discriminations matched to the fitted `sigma_logalpha`, we simulated responses from `rho_true ∈ {0.3, 0.6, 0.9}` and refit the model (one replicate per condition — compute-limited, not a full many-replicate SBC):

| `rho_true` | recovered (median) | 95% CI | covers truth? |
|---:|---:|---|:---:|
| 0.30 | 0.279 | [0.191, 0.364] | ✓ |
| 0.60 | 0.576 | [0.512, 0.636] | ✓ |
| 0.90 | 0.882 | [0.858, 0.902] | ✓ |

Recovery is good across the range with only a small (~0.02) low-side bias and no systematic inflation. This is the opposite failure mode from the confirmatory mIRT route (`08`), which — as an exploratory/near-exploratory multidimensional model on column-bound data — is expected to *overstate* multidimensionality; the Bayesian model does not carry that bias, so the `rho`/`ECV` numbers above can be read at face value rather than as a worst-case bound.

## Interpretation

Across the samples analyzed so far, bilingual lexical ability is **neither a single shared lexicon nor two independent ones**. A substantial general dimension — roughly 55–70% of reliable person-level variance, depending on the pair — coexists with genuine, non-trivial language-specific ability, over and above what is explained by exposure and age. This is closer to a **bifactor** picture (general ability + language-specific residual ability) than to either monolithic alternative, and it is the kind of result the theoretical framing was built to distinguish, not just to detect.

## Limitations & open questions

- **Item thinning.** All fits so far use every 2nd–3rd item to keep NUTS runtime tractable (2–13 hours depending on machine load); a full-item run for at least one pair would confirm thinning isn't introducing bias.
- **Age modeling.** `beta_age` is a linear fixed effect and its estimate differs sharply between the two bilingual pairs (0.15 vs. 2.21) — worth checking with a spline and/or age-banded refits before treating that difference as substantive.
- **Translation-equivalent (concept) linking is off in every fit so far** (`link_concepts` auto-disabled — 15–22% item overlap across every pair/triple checked is below the 25% threshold). The conceptual-vs-lexical-representation question (does knowing *dog* predict knowing *perro* beyond general ability?) hasn't actually been tested yet; it needs either richer translation-equivalent coverage or a targeted design.
- **Semantic (lexical-class) sub-structure** — is any of the language-specific variance actually *semantic* (a noun factor, a predicate factor) rather than linguistic? — was part of the original design (`M_class` in `08`) but hasn't been fit successfully on real data; the mIRT convergence failure applies here too, and would need the Stan model extended with lexical-class-specific factors.
- **Single calibration replicate per condition.** The SBC check above is indicative, not a full multi-replicate simulation-based calibration; more replicates would tighten the false-positive/power picture from `10`, which is currently unrun against this Stan model (it was written against the mirt pipeline).
- **Sample coverage.** Three points (2 pairs + 1 triple) is not enough to say how general the ~55–70% figure is across typologically different language pairs, dominant-vs-heritage-language configurations, or age ranges.

## Reproducing

The pipeline needs `data/items.rds`, `data/demographics.rds`, and `data/instruments/*.csv` (embargoed; not in this repo) and a working Stan toolchain (`cmdstanr` per [`09`](09_o5_dimensionality_bayes.qmd)'s default, or `rstan` as used for the runs reported here). See `params:` blocks in [`08`](08_o5_dimensionality_mirt.qmd)/[`09`](09_o5_dimensionality_bayes.qmd)/[`10`](10_o5_dimensionality_calibration.qmd) for the `language_a`/`language_b`/`thin_items` knobs used to target a specific pair or sample size.
