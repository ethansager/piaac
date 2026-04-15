# Plan: APC Regression Analysis
**Status:** DRAFT
**Date:** 2026-04-14
**Script to create:** `01_scripts/07_apc_regression.r`
**Data in:** `02_output/piaac_clean.rds`, `02_output/cohort_pisa_piaac.rds`
**Outputs:** `02_output/apc_*.rds`, `Figures/apc_*.pdf`

---

## The Identification Problem

APC models have a fundamental linear dependency: **Birth Year = Survey Year − Age**.
With two survey years (2014, 2023) we cannot separately identify all three effects without
imposing a constraint. We can identify any two:

| Model | Identified | Constraint imposed |
|-------|------------|-------------------|
| Age + Period | age & period effects | cohorts identical in quality |
| Age + Cohort | age & cohort effects | no period effect (2023 ≠ worse) |
| Cohort + Period | cohort & period effects | age profile flat |
| **PISA-instrumented** | all three (approximately) | PISA score proxies cohort quality |

The last row is our key advantage — we have PISA data.

---

## Proposed Specifications (in order of complexity)

### Spec 1 — Age × Period (baseline)

```
literacy_score ~ age_group + I(round == "cy2") + controls
```

- **Identifies:** age profile + period shift
- **Assumes:** cohorts in 2014 and 2023 are equally "able" at each age → all change is period
- **Interpretation of period dummy:** the pure 2023 effect (smartphones, etc.) net of age
- **Problem:** if younger cohorts in 2023 are genuinely less able (worse schools), we
  overattribute decline to the period

---

### Spec 2 — Age × Cohort (no period)

```
literacy_score ~ age_group + cohort_decade + controls
```

- **Identifies:** age profile + cohort quality gradient
- **Assumes:** period effect = 0 (2023 is not categorically different from 2014)
- **Interpretation:** positive cohort coefficients = later-born cohorts more literate
- **Problem:** if smartphones genuinely hurt 2023 independently of cohort composition,
  we miss it

---

### Spec 3 — Within-cohort aging (most credible for aging)

Track cohorts observed in BOTH rounds:

| Cohort (birth decade) | 2014 age | 2023 age |
|---|---|---|
| 1980–89 | 25–34 | 35–44 |
| 1970–79 | 35–44 | 45–54 |
| 1960–69 | 45–54 | 55–64 |

```
literacy_score ~ cohort_FE + age_group + controls
```

Estimated within cohort, across rounds. The age coefficients here are the cleanest
estimate of the aging effect because cohort composition is held constant by the FE.
The residual variation across rounds (after age + cohort FE) = period effect.

**Caveat:** With only 2 periods, period FE is perfectly collinear with cohort FE for
cohorts observed in one round only. Restrict to the 3 cohorts above (observed in both).

---

### Spec 4 — PISA-instrumented cohort quality (preferred)

This breaks the APC collinearity using **pre-determined cohort quality**:

```
PIAAC_score ~ PISA_cohort_score + age_midpoint + I(round == "cy2") + controls
```

Where `PISA_cohort_score` = the average PISA reading score of that birth cohort when
they were 15 years old (from scripts 05/06).

- **β_PISA:** how much of your age-15 skill you retain into adulthood (persistence)
- **β_age:** aging gradient net of cohort quality
- **β_period:** 2023 effect holding cohort quality AND age constant → closest we get
  to isolating "smartphones/screens made adults worse"
- **Identification:** PISA scores are measured ~9 years before PIAAC, so they are
  predetermined relative to the 2023 period effect. This breaks the linear dependency.

**Limitation:** Only countries/cohorts with matched PISA–PIAAC data (18 countries from
script 06). Also, PISA measures 15-year-olds only, so the instrument is noisy for
cohorts where we're interpolating.

---

### Spec 5 — Country FE + period (robustness)

```
literacy_score ~ age_group + country_FE + I(round == "cy2") + age × round interaction
```

- Tests whether the period effect is uniform across countries or concentrated in
  specific ones (e.g., English-speaking countries with higher smartphone penetration)
- Age × round interaction: did the period effect hit young people harder? (phones
  disproportionately used by young)

---

## What We Can Say About Each Driver

| Claim | Best spec | Caveat |
|-------|-----------|--------|
| Cohorts born later are less literate | Spec 2, 4 | Could be period not cohort |
| Ageing reduces literacy by X pts/decade | Spec 3 | Within observed cohorts only |
| 2023 is categorically worse (phones etc.) | Spec 1, 4 | Assumes cohort quality fixed |
| Young adults hit harder by 2023 | Spec 5 | Ecological; not causal |

**Key honest caveat for the blog post:** With 2 time points we cannot fully separate
cohort from period. Spec 4 (PISA instrument) is the most defensible but still relies on
the assumption that PISA scores capture cohort quality and not early exposure to the same
period trend (e.g., if 2006 PISA cohorts already had smartphones).

---

## Implementation Plan

### Step 1 — Build the analysis dataset

- Stack `piaac_clean.rds` (both rounds)
- Construct:
  - `birth_decade`: floor((survey_year - age_midpoint) / 10) × 10
  - `age_midpoint`: midpoint of AGEG10LFS band (e.g., 16–24 → 20)
  - `cohort_in_both`: flag cohorts observed in both rounds
- Merge in PISA cohort scores from `cohort_pisa_piaac.rds`

### Step 2 — Run specs 1–3 using PV+BRR

- Use `pv_group_mean()` framework from `00_helpers.r`
- For regression: run each spec 10 times (once per PV), combine with Rubin's rules
- Standard errors via BRR (80 replicate weights)

### Step 3 — Run spec 4 (PISA-instrumented)

- Collapse to country × cohort × round cell means (weighted)
- OLS on cell means with PISA score as regressor
- Cluster SEs at country level

### Step 4 — Figures

- Coefficient plot: age gradient from spec 3
- Cohort quality plot: spec 2 cohort FE by birth decade
- Period effect forest plot: spec 1 and 4 period estimates side by side
- Age × period interaction: spec 5 — did young fare worse in 2023?

---

## Files

| File | Role |
|------|------|
| `01_scripts/07_apc_regression.r` | Main script |
| `02_output/apc_analysis_data.rds` | Stacked dataset with constructed vars |
| `02_output/apc_spec1.rds` | Age + period results |
| `02_output/apc_spec2.rds` | Age + cohort results |
| `02_output/apc_spec3.rds` | Within-cohort results |
| `02_output/apc_spec4.rds` | PISA-instrumented results |
| `Figures/apc_age_gradient.pdf` | Age coefficients |
| `Figures/apc_cohort_fe.pdf` | Cohort FE plot |
| `Figures/apc_period_effect.pdf` | Period estimates comparison |
| `Figures/apc_age_period_interaction.pdf` | Young vs old in 2023 |

---

## Verification

- [ ] Script runs without errors
- [ ] Rubin's rules applied correctly across 10 PVs
- [ ] Cohort cell counts reported (thin cells flagged)
- [ ] Period estimates from spec 1 and 4 directionally consistent
- [ ] Blog-post-ready summary paragraph drafted
