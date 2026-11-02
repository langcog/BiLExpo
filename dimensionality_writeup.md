# Is bilingual lexical ability one dimension, or several?

**Status:** working draft, 2026-09; English (American) × Spanish (Mexican) corrected 2026-10 (see [Correction](#correction-longitudinal-administrations-2026-10)). Analyses live in [`08_o5_dimensionality_mirt.qmd`](08_o5_dimensionality_mirt.qmd), [`09_o5_dimensionality_bayes.qmd`](09_o5_dimensionality_bayes.qmd), [`10_o5_dimensionality_calibration.qmd`](10_o5_dimensionality_calibration.qmd), [`models/bilingual_dim_irt.stan`](models/bilingual_dim_irt.stan), [`scripts/dim_irt_helpers.R`](scripts/dim_irt_helpers.R). Underlying data (`data/items.rds`, `data/demographics.rds`, `data/instruments/`) is embargoed and not in the repo; only aggregate posterior summaries are reported here. The cross-language-transfer (accumulator) analysis has its own section under [Results](#cross-language-transfer-a-bilingual-accumulator-model-english--spanish-translation-equivalents) and lives in `models/bilingual_accum*.stan` and `scripts/*bilingual_accum*.R`.

**In one line:** across three bilingual samples — English–Spanish (American and British/European) and Norwegian–Polish — lexical ability across a child's two languages is ~40–70% one shared dimension and the rest genuinely language-specific, decisively neither "one bilingual lexicon" nor "two independent ones." A trilingual sample was attempted but is unusable (severe CDI floor effects), and no other multilingual sample exists in the data.

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

Wordbank CDI administrations, filtered to children administered the instrument in every language of interest ("both"/"all" languages), pooling all datasets that include that language. Item easiness/discrimination and person ability are estimated jointly from the response data; exposure proportion and age are covariates from each **administration's** own demographic record. Children with several (longitudinal) administrations keep each one as its own unit with its own age — see [Correction](#correction-longitudinal-administrations-2026-10). Samples are screened for per-language vocabulary floor and exposure coverage (`sample_health()`) before fitting.

| sample | children | languages | items | usable? |
|---|---:|---|---:|---|
| EN (American) × ES (Mexican) | 474 (1749 administrations) | 2 | 543 thinned | yes — 3–5% of administrations at floor, ~96% exposure coverage |
| EN (British) × ES (European) | 690 | 2 | 417 thinned | yes — 10–13% floor, 100% coverage |
| Norwegian × Polish | 112 | 2 | 697 thinned | yes — 5% / 1% floor, 94% coverage |
| EN (Malaysian) / Malay / Mandarin (Malaysian) | 569 | 3 | 548 thinned | **no** — 54/35/86% floor; Mandarin exposure known for only 22% |

Most fits use every 2nd–3rd item to keep 4-chain NUTS runtime tractable (2–15 h). One full-item run was done as the check, on the pre-correction EN (American) × ES (Mexican) data: **all 1665 items and translation-equivalent concept linking enabled** (46 h, 0 divergences) gave `rho` = 0.587 [0.528, 0.644] — indistinguishable from the thinned 0.585 [0.519, 0.642] on the same data. So item thinning and the concept-linking choice do not move `rho` / `ECV`. (That check has not been repeated on the corrected data.)

## Results

### Bilingual pairs

| pair | children | `rho` / `ECV` (median, 95% CI) | divergences |
|---|---:|---|---:|
| English (American) × Spanish (Mexican) | 474 | **0.40** [0.32, 0.47] (was 0.58 before the [correction](#correction-longitudinal-administrations-2026-10)) | 0 |
| English (British) × Spanish (European) | 690 | **0.70** [0.65, 0.74] (0.71 before the correction) | 0 |
| Norwegian × Polish | 112 | **0.56** [0.41, 0.69] (0.59 before the correction) | 0 |

All three fits converged cleanly (zero divergences, max R-hat ≤ 1.07). All three **decisively reject both extremes** — the posterior for `rho` excludes 1 (a single bilingual lexicon) and excludes 0 (two fully independent lexicons). Exposure is the dominant driver of *which* language's words a child knows (`beta_exp` ≈ 2–5 on the centered exposure scale) — modeling it explicitly, rather than letting it manifest as a factor, is what makes the remaining `sigma_s` interpretable as genuine language-specific ability rather than dominance.

The estimate ranges from 0.40 to 0.70 across the three, with **Norwegian × Polish** — a Germanic + Slavic pair with no English, a different bilingual population, different countries — at 0.56, between the two English–Spanish samples. The wide interval reflects n = 112. The spread is now wider than chance: the American/Mexican (0.40) and British/European (0.70) English–Spanish intervals do not overlap. The British sample has a higher CDI floor (10–13% vs. 3–5%), and the American sample spans a wider age range (15–40 months, mostly longitudinal); which, if either, drives the gap is not resolved.

### Correction: longitudinal administrations (2026-10)

The English (American) × Spanish (Mexican) sample is mostly longitudinal (Hoff, Marchman, Marchman_BTL, Weisleder): 474 children, **1749 administrations**, 15–40 months, up to 11 visits per child. The original pipeline (`build_pair_matrix()`) collapsed each child's visits to "ever produced" (`max` over administrations) and attached a single child-level age — the first exposure record's, median-imputed where missing. That erased the age signal: the correlation between age and proportion of items produced was **0.01** (−0.17 within Hoff), against **0.47** per administration. The fitted age spline came out U-shaped — 18-month-olds 0.6 logits *above* 24-month-olds — and the earlier "`beta_age` 0.15 vs. 2.21 across the two English–Spanish pairs" discrepancy, attributed at the time to linear-age misspecification, was this bug. The British/European and Norwegian–Polish samples are effectively cross-sectional (2 of 690 and 0 of 112 children have a repeat visit). Refit with the new pipeline (latest administration, same item thinning, concept linking off), their estimates barely move: British/European `rho` 0.71 → **0.70 [0.65, 0.74]** (this refit also replaces that sample's old linear age term with the spline), Norwegian–Polish 0.59 → **0.56 [0.41, 0.69]** (7 children without an exposure record now get their own age rather than the median).

The pipeline now keeps one row per (child, administration, item) with that administration's own age and exposure record (`build_pair_long()`). Refit with **one administration per child × language (the latest)** — the cross-sectional design the model assumes — 543 items, 948 administrations, 14.7 h NUTS, 0 divergences:

| quantity | before (visits collapsed, 1665 items) | after (latest administration, own age) |
|---|---|---|
| `rho` / `ECV` | 0.59 [0.53, 0.64] | **0.40 [0.32, 0.47]** |
| `c_shared` | 3.42 | 1.65 |
| `sigma_s` EN / ES | 1.74 / 1.35 | 1.64 / 1.52 |
| age effect, 18 vs. 24 months | +0.64 [0.18, 1.10] | −1.12 [−1.83, −0.46] |
| age effect, 30 vs. 24 months | +0.94 [0.48, 1.42] | +1.91 [1.54, 2.33] |
| `beta_exp` EN / ES | 5.2 / 3.5 | 5.3 / 4.0 |
| `beta_age_exp` EN / ES (per month) | 0.00 / 0.14 | 0.18 / 0.16 |

The age curve is now monotone, and the exposure effects barely move. The shared covariance halves while the language-specific variances don't: with no usable age covariate, age-driven growth, which raises both languages together, had been absorbed into the shared factor. A descriptive check gives the same picture. The correlation between children's English and Spanish vocabulary (logit proportion produced), after regressing out exposure, is 0.60 on the old collapsed data, 0.52 on the latest administrations, and **0.36** once age is also regressed out. Most of the drop is the age adjustment; a smaller part is using one administration instead of "ever produced".

A second variant uses **all administrations** (1749 administrations, 554 items, 16.6 h NUTS, 0 divergences, max R-hat 1.02). It shares each child's ability across visits and adds a per-visit deviation `sigma_admin`, independent across the two languages. Its `rho` is the correlation of children's *stable* abilities: **0.63 [0.53, 0.72]**. The visit deviations are large (`sigma_admin` 1.29 / 1.20, against `sigma_s` 0.93 / 1.00 and `c_shared` 1.59). The stable part is ~60% (English) and ~64% (Spanish) of a single visit's adjusted ability variance. The correlation the model implies for a *single* visit is **0.39 [0.32, 0.45]**, which matches the latest-administration fit, so the two designs agree. The single-visit value is the like-for-like comparison with the two cross-sectional samples, which cannot separate stable from visit-specific variance. The stable-ability value says that, averaging over visit-to-visit fluctuation, about two-thirds of what distinguishes a child's English and Spanish ability (beyond age and exposure) is shared. Caveat: a visit-level deviation *shared* by both CDIs (same-day reporting, a growth spurt) and child-specific growth rates are not modelled; both are absorbed into `sigma_admin`.

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

### Cross-language transfer: a bilingual accumulator model (English × Spanish translation equivalents)

*Added 2026-11. Code: `models/bilingual_accum*.stan`, `scripts/*bilingual_accum*.R`, `scripts/childes_freq_te.R`. Preliminary; see the caveats below.*

The IRT model says **how much** of a child's lexical ability is shared across languages, not **why**. One mechanism is cross-language transfer: hearing a word's translation equivalent (*dog*) may count, in part, toward learning the word itself (*perro*). The accumulator models this directly, on a log-token scale: the chance a child produces a word rises with the log of the tokens heard, and a token of the translation equivalent counts as a fraction κ of a token of the word itself. <!-- TODO(George): cite the source of this log-token framework. The Stan header calls the age-slope parameter "Frank 2026's kappa" and the v1 commit message says "SM2"; neither is defined anywhere in the repo. -->

For child *i* at administration *a* (age *t_a*), word *j* in language *l* with translation equivalent *j′* in the other language *l′*:

```
eta = λ · [ ξ_i + u_il + log( e_il · q_j^φ + κ_j · e_il′ · q_j′^φ ) + (1 + δ) · log(t_a / a0) − ψ_j ]
κ_j = κ_cog if the translation-equivalent pair is an orthographic cognate, else κ_nc
```

`e_il` is the child's exposure share to language *l*; `q_j` is the word's relative child-directed frequency (CHILDES, target child excluded); `φ` is one estimated frequency exponent shared by both labels; `ξ_i` is shared child ability and `u_il` language-specific ability; `ψ_j` is item difficulty (language offset + concept effect + item residual). κ = 0 means separate registers (only own-label tokens count); κ = 1 means fully shared (exposure share is irrelevant). The age effect is reported as `age_slope_logit = λ(1 + δ)`. `rho_lang = σ_ξ² / (σ_ξ² + σ_u²)` is the same quantity as the IRT `rho` when the language-specific variances are equal, as they are here (one `σ_u`), though the two models differ in other ways (see caveats).

**Data.** The same source as the IRT's EN (American) × ES (Mexican) sample, restricted to the 118 translation-equivalent concepts (236 items, of which only **15 pairs are orthographic cognates**) and to children with an exposure record for both languages (no imputation): 462 children, 1,713 administrations, 166,153 responses, 11–41 months. Zero CHILDES counts (3 English and 16 Spanish labels: multiword items, Mexican-specific words) are imputed from the translation equivalent's frequency.

**Why four versions.**

- **v1** (exposure share only) cannot separate λ from κ. In a simulation with λ = 1 and κ = 0.3 it recovered λ = 0.50 [0.34, 0.77] and κ = 0.09 [0.02, 0.24], with the truth outside both intervals, while the scale-free products came out right (posterior-mean λ·σ_ξ = 1.49 against a truth of 1.5). It also used the collapsed-visit age later corrected in the IRT pipeline.
- **v2** added per-administration ages, word-level frequency (to give κ something item-level to be identified from) and a cognate-specific κ. On real data it pinned κ at exactly 0 with λ = 1.07 [0.95, 1.19]: the model applied CHILDES frequency at full weight, while the data put its elasticity at 0.29 (γ = −0.71), so any κ > 0 produced item-level boosts the data reject.
- **v3** estimates one exponent φ for both labels. A simulation generated with φ = 0.3 recovers all 13 monitored parameters (every 95% interval covers the truth). On the real data φ turns out to be ≈ 0.03.
- **v3-anchor** takes λ as data, so κ can be estimated conditional on a chosen λ.

**Results** (EN × ES, posterior median [95% CI]):

| fit | λ | κ non-cognate | κ cognate | κ cognate − κ non-cognate | φ | `rho_lang` | `age_slope_logit` |
|---|---|---|---|---|---|---|---|
| free λ | 2.30 [1.61, 3.73] | 0.178 [0.071, 0.353] | 0.251 [0.119, 0.432] | 0.070 [0.042, 0.100] | 0.027 [0.007, 0.072] | 0.41 [0.33, 0.49] | 6.00 [5.89, 6.11] |
| free λ, concepts with imputed frequency dropped | 2.33 [1.62, 3.72] | 0.167 [0.057, 0.349] | 0.232 [0.095, 0.428] | 0.061 [0.032, 0.092] | 0.029 [0.006, 0.083] | 0.41 [0.33, 0.49] | 6.21 [6.09, 6.33] |
| λ fixed at 0.6 | fixed | 0.001 [0.000, 0.004] | 0.031 [0.012, 0.064] | 0.030 [0.011, 0.062] | 0.432 [0.322, 0.552] | 0.29 [0.21, 0.38] | 5.99 [5.88, 6.10] |
| λ fixed at 1.0 | fixed | 0.002 [0.000, 0.010] | 0.020 [0.009, 0.039] | 0.017 [0.008, 0.032] | 0.275 [0.191, 0.349] | 0.38 [0.29, 0.46] | 5.99 [5.88, 6.11] |
| λ fixed at 1.2 | fixed | 0.011 [0.002, 0.033] | 0.034 [0.014, 0.079] | 0.023 [0.010, 0.048] | 0.184 [0.099, 0.266] | 0.40 [0.32, 0.48] | 6.00 [5.88, 6.11] |

1. **Frequency carries no item-level signal, so only a λ–κ ridge is identified.** With λ free, transfer looks sizeable (κ ≈ 0.18–0.25) at λ ≈ 2.3, but φ ≈ 0.03, i.e. CHILDES frequency barely predicts which words a child produces. That removes the lever meant to separate λ from κ. Dropping the 16 concepts with imputed frequency changes nothing.
2. **The amount of transfer depends on the λ you impose.** Fixing λ at 0.6–1.2 collapses transfer to roughly 0–3% of a token, and it moves with the anchor: non-cognate κ goes 0.001 → 0.011 and φ goes 0.43 → 0.18 as λ rises from 0.6 to 1.2.
3. **Three things are stable across all five fits.** (a) Cognate transfer exceeds non-cognate transfer: the interval for κ_cog − κ_nc excludes 0 in every fit (smallest lower bound 0.008). (b) The age slope λ(1 + δ) ≈ 6.0 logit per log-age unit, although λ and δ individually are not identified (δ = 1.6 free, against 9.0 / 5.0 / 4.0 when anchored). (c) `rho_lang` is 0.29–0.41, bracketing the IRT `rho` of 0.40 [0.32, 0.47] for the same pair (latest-visit fit).

**What this does and doesn't say.** The data cannot tell "λ ≈ 1.1 with essentially no transfer" (v2, the anchored fits) from "λ ≈ 2–4 with κ ≈ 0.2" (v3, free), so **this analysis makes no claim about the amount of cross-language transfer.** The only qualitative result that survives the ridge is a cognate advantage, and it rests on 15 concepts. The conceptual-vs-lexical question in the Limitations therefore stays open. What would identify κ is item-level variation in the translation-equivalent frequency ratio that actually predicts production, which is the lever the v2/v3 design relies on; CHILDES frequency does not supply it. The agreement of `rho_lang` with the IRT `rho` is suggestive but not a replication: the accumulator uses only the 118 translation-equivalent concepts, puts exposure on a log-token ruler rather than as a linear fixed effect, uses all administrations with no per-visit deviation, and `rho_lang` itself shifts with the anchor (0.29 → 0.40).

**Caveats.**

- *Exposure provenance.* Exposure shares are each child's first-listed record, fixed across visits: the behaviour of `child_language_exposure()` before the longitudinal-age fix. The IRT now uses per-administration exposure, and the current helper averages a child's records, so re-running `scripts/prep_bilingual_accum*.R` today will not reproduce these numbers exactly. The fits above were run on 2026-10-04 to 2026-10-10.
- *Cognate flag.* An orthographic similarity (normalised Levenshtein) ≥ 0.5 between any pair of forms; crude, and only 15 of 118 pairs qualify.
- *Anchor values.* The λ values 0.6, 1.0 and 1.2 are described in the code as the monolingual "rulers"; where they come from is not recorded in the repo. <!-- TODO(George): record the source of the anchor values 0.6 / 1.0 / 1.2. -->
- *Sample.* English (American) × Spanish (Mexican) only (462 children, a subset of the IRT's 474).
- *Simulation realism.* Simulated data are easier than the real data (mean produced 0.44 against 0.26), and the φ = 0.3 used to generate them is ten times the real-data estimate, so the v3 recovery check does not speak to the real-data regime.
- *Sampler health.* 0 divergences in every fit but one (1 in the no-imputation fit), none at maximum treedepth, E-BFMI ≥ 0.67, max R-hat ≈ 1.01, minimum n_eff 352 (λ = 0.6 fit).

## Interpretation

Across all three usable samples, lexical ability across a child's two languages is **neither a single shared lexicon nor two independent ones**: a substantial general dimension (~40–70% of reliable person-level variance) coexists with genuine, non-trivial language-specific ability, over and above what exposure and age explain. This is a **bifactor** picture — general ability plus language-specific residual ability — and it is the structure the model space was built to distinguish, not just to detect. That Norwegian–Polish (no shared script beyond Latin, no English, Germanic + Slavic, a different country and dataset) lands in the same range as English–Spanish is the main reason to think the result is a property of early bilingual lexical development rather than of one language pair or population.

Open: the exact split (0.40 / 0.56 / 0.70), and in particular why the two English–Spanish samples differ. The British/European sample has the higher CDI floor; the American/Mexican sample has the wider age range, and the correction above shows how much shared variance age alone can contribute when it isn't modeled. And the strong version of the question — does the shared fraction depend on how *together* vs. *separate* the two languages are acquired? — still needs a sample where the languages are genuinely functionally segregated (which the one available such sample, the Malaysian trilingual, is too floor-bound to provide).

## Limitations & open questions

- **Sample coverage.** Three samples: two English–Spanish and one Norwegian–Polish. All European, all alphabetic scripts, all Wordbank age ranges. No dominant/heritage configuration, no non-Latin script, no functionally-segregated pair. More low-floor pairs are available (English + French n=267, English + German n=239, Afrikaans + English n=95) but stay close to this profile.
- **No multilingual sample.** The one k≥3 sample (Malaysian) is at the floor; there is nothing to extend the analysis past two languages with this data.
- **Item thinning** — checked once, on the pre-correction EN-American × ES-Mexican data (full 1665-item fit gave the same `rho` as the thinned one); assumed to carry over.
- **Age modeling.** The main age effect on ability is a natural spline (`age_spline_df = 3`); the age × exposure interaction is still linear in age. Age and exposure are taken per administration. Ability is a child-level random effect, with no child-specific growth rates: in the all-administrations variant, growth-rate differences land in the per-visit deviation `sigma_admin`.
- **Repeated administrations.** The headline EN-American × ES-Mexican estimate (0.40) is per visit (latest visit only), comparable to the cross-sectional samples. Across all visits, stable abilities correlate at 0.63, but that model treats per-visit deviations as independent across languages and has no child-specific growth rates.
- **Translation-equivalent (concept) linking** is off in the three thinned fits (`link_concepts` auto-disabled — item overlap below the 25% threshold) but *on* in the full 1665-item EN-American × ES-Mexican fit (~636 shared `uni_lemma`s), where it left `rho` unchanged. So the shared-concept structure isn't inflating `rho` — but the sharper conceptual-vs-lexical-representation question (does knowing *dog* predict knowing *perro* over and above general ability, and how much?) is still open. The accumulator analysis above attacks it directly but could not identify the amount of transfer: only a λ–κ ridge is identified, because CHILDES frequency (φ ≈ 0.03) supplies no item-level signal. It does find a cognate advantage, on 15 concepts.
- **Semantic (lexical-class) sub-structure** — is any of the language-specific variance actually *semantic* (a noun factor, a predicate factor)? — was part of the original design (`M_class` in `08`) but hasn't been fit on real data; the mIRT convergence failure applies, and it would need the Stan model extended with lexical-class-specific factors.
- **Single calibration replicate per condition.** The SBC check is indicative, not a full multi-replicate calibration, and it used the pre-correction EN–SP design (child-level ages); `10` (written against the now-abandoned mirt pipeline) has not been re-pointed at the Stan model.

## Reproducing

The pipeline needs `data/items.rds`, `data/demographics.rds`, and `data/instruments/*.csv` (embargoed; not in this repo) and a working Stan toolchain (`cmdstanr` per [`09`](09_o5_dimensionality_bayes.qmd)'s default, or `rstan` as used for the runs reported here). See `params:` blocks in [`08`](08_o5_dimensionality_mirt.qmd)/[`09`](09_o5_dimensionality_bayes.qmd)/[`10`](10_o5_dimensionality_calibration.qmd) for the `language_a`/`language_b`/`thin_items` knobs used to target a specific pair or sample size.
