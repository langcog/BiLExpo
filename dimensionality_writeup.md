# Is bilingual lexical ability one dimension, or several?

**Status:** working draft, 2026-09. Analyses live in [`08_o5_dimensionality_mirt.qmd`](08_o5_dimensionality_mirt.qmd), [`09_o5_dimensionality_bayes.qmd`](09_o5_dimensionality_bayes.qmd), [`10_o5_dimensionality_calibration.qmd`](10_o5_dimensionality_calibration.qmd), [`models/bilingual_dim_irt.stan`](models/bilingual_dim_irt.stan), [`scripts/dim_irt_helpers.R`](scripts/dim_irt_helpers.R). Underlying data (`data/items.rds`, `data/demographics.rds`, `data/instruments/`) is embargoed and not in the repo; only aggregate posterior summaries are reported here.

**In one line:** for two English–Spanish bilingual samples, lexical ability across the two languages is ~58–71% one shared dimension and the rest genuinely language-specific — decisively neither "one bilingual lexicon" nor "two independent ones." A trilingual sample was attempted but is unusable (severe CDI floor effects), and no other multilingual sample exists in the data.

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

The Bayesian route (2) is what produced trustworthy estimates, after two fixes:

- `rstan`'s mean-field ADVI diverges completely on this model (oscillating ELBO, posterior collapsing to a degenerate mode with runaway item discriminations) while NUTS on the identical model is well-behaved. **Use NUTS, not ADVI/variational inference, for this model.**
- The original parameterization — a general loading `lambda_g` multiplying a shared latent — funnels when `lambda_g → 0` (the shared latent becomes unidentified). It sampled fine for the English–Spanish pairs, where `lambda_g` ≈ 1.9, but produced 139 divergences and un-converged estimates on the trilingual sample, where the shared dimension really is near zero. Reparameterizing to sample the ability covariance through its Cholesky factor (see [Model](#model)) removed the funnel; both parameterizations agree where both work.

## Model

Item easiness is decomposed into a translation-equivalent **concept** term (shared across languages via `uni_lemma`, e.g. *dog* / *perro*) plus a language-specific offset — dropped in favor of a single flat item-easiness level when too few items in a given language pair actually share a concept (an automatic check in `build_dim_irt_stan_data()`; translation-equivalent overlap between full CDI forms in different languages/countries turned out to be modest, 15–22% of items in every sample analyzed so far).

Child ability across the `L` languages is multivariate normal with a **compound-symmetric covariance** — a common covariance `c_shared` between every pair of languages, plus a per-language specific variance on the diagonal:

```
theta_child[i, ] ~ MVN(0, Sigma)
Sigma[l, l]      = c_shared + sigma_s[l]²
Sigma[l1, l2]    = c_shared                (l1 ≠ l2)
```

with exposure and age entered as fixed, per-language effects on ability, and a standard 2PL response model, `p = logit⁻¹(alpha_j · (ability + b_item[j]))`.

This is the same model as a general trait plus independent language-specific deviations (`c_shared` = the squared general loading), but it is sampled through the Cholesky factor of `Sigma` applied to a unit-scale latent, rather than as a loading multiplying a shared latent. The difference matters: a general loading funnels badly when it approaches zero (the shared latent becomes unidentified), which is exactly what happened on the trilingual sample. The covariance parameterization has no vanishing latent and samples cleanly whether the shared dimension is large or ≈ 0.

It nests the two extreme hypotheses as limiting cases and generalizes past a single language pair:

- `sigma_s → 0` ⇒ **unidimensional** — one lexical trait, languages differ only by exposure-driven mean level
- `c_shared → 0` ⇒ **independent lexicons** — no shared trait at all
- both free ⇒ report the **pairwise cross-language ability correlation** `rho[l1, l2] = c_shared / √(v[l1]·v[l2])` (`v[l] = c_shared + sigma_s[l]²`) and the **person-level explained common variance** `ECV = c_shared / (c_shared + mean(sigma_s²))`, both continuous, both defined for any `L ≥ 2`.

`rho` and `ECV` coincide for `L = 2` with equal specific variances; for `L ≥ 3` `ECV` is the single scalar summary and `rho` is a full `L × L` matrix, one pairwise correlation per language pair.

## Data

Wordbank CDI administrations, filtered to children administered the instrument in every language of interest ("both"/"all" languages), pooling all datasets that include that language. Item easiness/discrimination and person ability are estimated jointly from the response data; exposure proportion and age are per-child, per-language covariates from the demographic records. Samples are screened for per-language vocabulary floor and exposure coverage (`sample_health()`) before fitting.

| sample | children | languages | items (thinned for NUTS runtime) | usable? |
|---|---:|---|---:|---|
| EN (American) × ES (Mexican) | 474 | 2 | 554 | yes — 2% floor, ~96% exposure coverage |
| EN (British) × ES (European) | 690 | 2 | ~420 | yes — 10–13% floor, 100% coverage |
| EN (Malaysian) / Malay / Mandarin (Malaysian) | 569 | 3 | 548 | **no** — 54/35/86% floor; Mandarin exposure known for only 22% |

Full, unthinned NUTS runs (1,600–1,700 items) were not attempted here; thinning trades estimation precision for tractable runtime (each reported fit is 2–13 hours of 4-chain NUTS depending on machine load) and is not expected to introduce bias in the person-level quantities (`rho`, `ECV`) — a full-item run is the natural robustness check.

## Results

### Bilingual pairs

| pair | children | `rho` / `ECV` (median, 95% CI) | `beta_age` |
|---|---:|---|---|
| English (American) × Spanish (Mexican) | 474 | **0.58** [0.52, 0.64] | 0.15 [−0.05, 0.35] |
| English (British) × Spanish (European) | 690 | **0.71** [0.66, 0.75] | 2.21 [2.03, 2.39] |

Both fits converged cleanly (zero divergences, max R-hat ≤ 1.005, `rho`/`ECV` effective sample sizes in the thousands). Both **decisively reject both extremes**: the posterior for `rho` excludes 1 (a single bilingual lexicon) and excludes 0 (two fully independent lexicons) by a wide margin. Exposure is the dominant driver of *which* language's words a child knows (`beta_exp` ≈ 3–5 on the centered exposure scale in both fits) — modeling it explicitly, rather than letting it manifest as a factor, is what makes the remaining `sigma_s` interpretable as genuine language-specific ability rather than dominance.

The two pairs agree on the qualitative picture but not on the exact split (58% vs. 71% general-dimension share), and disagree sharply on how much residual age explains after exposure and general ability are accounted for. Whether that's a real difference between these bilingual populations or an artifact of item thinning / sample composition is open — see [Limitations](#limitations--open-questions).

### Trilingual sample — attempted, then set aside

English (Malaysian) / Malay (Malaysian) / Mandarin (Malaysian), 569 children administered all three CDIs. The Stan model and its R data-builders were generalized from a hardcoded 2-language parameterization to arbitrary `L` for this, and (after reparameterizing the covariance through its Cholesky factor to remove a boundary funnel — see [What actually worked](#what-actually-worked)) the fit ran cleanly and reported near-zero cross-language correlations: all three pairwise `rho` ≈ 0.01–0.02, `ECV` ≈ 0.015.

**This result is an artifact and is not reported as a finding.** A per-language health check (`sample_health()`, now run automatically by `build_pair_matrix()`) shows why:

| language | median vocabulary | % of children producing <5 words | % with an exposure record |
|---|---:|---:|---:|
| English (Malaysian) | 2 | 54% | 75% |
| Malay (Malaysian) | 47 | 35% | 79% |
| Mandarin (Malaysian) | 0 | **86%** | **22%** |

This is a **Malay-dominant sample** (parent-reported exposure is almost entirely Malay + English, summing to 100%) that was *also administered* the English and Mandarin CDIs, on which most children score at or near zero. When 54–86% of children produce essentially no words in a language, that language's ability factor is measuring "floor vs. not-floor," not a latent trait — and the correlation of a barely-identified factor with anything is attenuated toward zero by construction. Only 22% of children even have a Mandarin exposure record; the original run compounded this by imputing the other 78% to the *median* exposure rather than to 0 (fixed: `build_dim_irt_stan_data()` now imputes 0 for an unlisted language when a child's other exposures already sum to ~100%).

There is **no usable multilingual (3+) sample** in the current Wordbank export — a scan of every language pair and triple with ≥60 children found the Malaysian trilingual to be the only k≥3 set, and every other candidate multilingual sample fails the same floor check. The dimensionality question, with this data, is answerable only for bilingual pairs.

### Simulation calibration (is any of this trustworthy?)

Before believing the numbers above, we checked whether the Stan model can recover a *known* cross-language correlation. Using the real EN–SP form design (which items each child was and wasn't administered), age, and exposure distribution, with item difficulties from real marginal endorsement rates and discriminations matched to the fitted `sigma_logalpha`, we simulated responses from `rho_true ∈ {0.3, 0.6, 0.9}` and refit the model (one replicate per condition — compute-limited, not a full many-replicate SBC):

| `rho_true` | recovered (median) | 95% CI | covers truth? |
|---:|---:|---|:---:|
| 0.30 | 0.279 | [0.191, 0.364] | ✓ |
| 0.60 | 0.576 | [0.512, 0.636] | ✓ |
| 0.90 | 0.882 | [0.858, 0.902] | ✓ |

Recovery is good across the range with only a small (~0.02) low-side bias and no systematic inflation. This is the opposite failure mode from the confirmatory mIRT route (`08`), which — as an exploratory/near-exploratory multidimensional model on column-bound data — is expected to *overstate* multidimensionality; the Bayesian model does not carry that bias, so the `rho`/`ECV` numbers above can be read at face value rather than as a worst-case bound.

## Interpretation

For both **English–Spanish** samples, lexical ability across the two languages is **neither a single shared lexicon nor two independent ones**: a substantial general dimension (~58–71% of reliable person-level variance) coexists with genuine, non-trivial language-specific ability, over and above what exposure and age explain. This is a **bifactor** picture — general ability plus language-specific residual ability — and it is the structure the model space was built to distinguish, not just to detect. The two samples differ on the exact split (58% vs. 71%); whether that reflects a real difference between these bilingual populations, the somewhat higher CDI floor in the British/European sample, or item thinning is not yet resolved.

Two English–Spanish samples is a narrow base. The obvious next question — does the shared fraction depend on the bilingual *situation* (how much the two languages are acquired together, in overlapping contexts, vs. kept functionally separate)? — is exactly what a trilingual or typologically-diverse comparison would speak to, and is exactly what this data can't currently support: the one multilingual sample is unusable, and the remaining bilingual pairs are all English-plus-a-European-language. The honest current claim is limited to English–Spanish.

## Limitations & open questions

- **Sample coverage.** Two English–Spanish samples. The `~58–71%` figure has not been tested on any other language pair, any non-European pair, or any dominant/heritage configuration. Several more low-floor bilingual pairs are available (English + French n=267, English + German n=239, Norwegian + Polish n=112, Afrikaans + English n=95) but all involve English or are still European.
- **No multilingual sample.** The one k≥3 sample (Malaysian) is at the floor; there is nothing to extend the analysis past two languages with this data.
- **Item thinning.** All fits use every 2nd–3rd item for tractable NUTS runtime (2–13 h depending on machine load); a full-item run for at least one pair would confirm thinning isn't introducing bias.
- **Age modeling.** `beta_age` is a linear fixed effect and its estimate differs sharply between the two pairs (0.15 EN-SP American vs. 2.21 EN-GB/ES-EU) — worth a spline and/or age-banded refits before treating that difference as substantive.
- **Translation-equivalent (concept) linking is off in every fit** (`link_concepts` auto-disabled — 15–22% item overlap is below the 25% threshold). The conceptual-vs-lexical-representation question (does knowing *dog* predict knowing *perro* beyond general ability?) is untested; it needs richer translation-equivalent coverage or a targeted design.
- **Semantic (lexical-class) sub-structure** — is any of the language-specific variance actually *semantic* (a noun factor, a predicate factor)? — was part of the original design (`M_class` in `08`) but hasn't been fit on real data; the mIRT convergence failure applies, and it would need the Stan model extended with lexical-class-specific factors.
- **Single calibration replicate per condition.** The SBC check is indicative, not a full multi-replicate calibration; `10` (written against the now-abandoned mirt pipeline) has not been re-pointed at the Stan model.

## Reproducing

The pipeline needs `data/items.rds`, `data/demographics.rds`, and `data/instruments/*.csv` (embargoed; not in this repo) and a working Stan toolchain (`cmdstanr` per [`09`](09_o5_dimensionality_bayes.qmd)'s default, or `rstan` as used for the runs reported here). See `params:` blocks in [`08`](08_o5_dimensionality_mirt.qmd)/[`09`](09_o5_dimensionality_bayes.qmd)/[`10`](10_o5_dimensionality_calibration.qmd) for the `language_a`/`language_b`/`thin_items` knobs used to target a specific pair or sample size.
