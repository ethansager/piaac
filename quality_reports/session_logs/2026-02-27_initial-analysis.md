# Session Log: Initial PIAAC Analysis

**Date:** 2026-02-27
**Goal:** Build first-pass analysis pipeline for PIAAC score decline research

---

## What Was Done

Implemented four R scripts based on approved plan:

| Script                  | Output                                                           | Status |
| ----------------------- | ---------------------------------------------------------------- | ------ |
| `01_load_clean.r`       | `02_output/piaac_clean.rds`                                      | ✅     |
| `02_cohort_analysis.r`  | `02_output/cohort_scores.rds`, `Figures/cohort_trends.pdf`       | ✅     |
| `03_illiteracy_rates.r` | `02_output/illiteracy_rates.rds`, `Figures/illiteracy_rates.pdf` | ✅     |
| `04_college_returns.r`  | `02_output/college_returns.rds`, `Figures/college_premium.pdf`   | ✅     |

---

## Data Notes

- **58 SAV files**: 34 round-1 countries (lowercase filenames), 24 round-2 countries (uppercase)
- **USA is round 2 only** (no `prgusap1.sav` exists)
- **Round 2 education variable** is `EDCAT7_TC1`, not `EDCAT7` — handled by rename in load script
- **AGE_R is 35.4% missing** — birth cohort calculation has many NAs; cohort analysis falls back to using `AGEG10LFS` (0% missing) implicitly via the `cohort_decade` bins
- **GBR round 1**: college_share = 0% — `EDCAT7` appears to be coded differently or all-zero; needs investigation
- Total: 351,096 rows, 35 countries, 2 rounds

---

## Key Findings

### Cohort Decline (Script 02)

- 12 countries appear in both rounds: BEL, CHL, CZE, ESP, FIN, GBR, IRL, ISR, ITA, LTU, POL, SVK
- **11/12 show score declines** for same birth cohort across rounds
- Largest declines: POL (−33 pts), LTU (−29), SVK (−20), CZE (−16), ISR (−15)
- Only Finland improved (+6 pts)
- Interpretation caveat: "same birth decade" in cy1 vs cy2 means slightly different ages (4 yrs older), so some of this is aging; but the magnitudes (10–33 pts) are large

### Illiteracy Rates (Script 03, Round 1 only)

- Worst: ECU 71%, PER 70%, CHL 53%, MEX 51%, TUR 45%
- Best: JPN 4.6%, FIN 10%, SVK/CZE/NZL/NOR ~12%
- Sanity checks vs email benchmarks:
  - CHL: 52.6% (benchmark 53.4%) ✅
  - ESP: 27.1% (benchmark 31%) — 3.9 pp off ✅
  - FIN: 10.2% (benchmark 12%) ✅
  - SWE: 13.2% (benchmark 12%) ✅
  - DEU: 17.4% (benchmark 22.5%) — **5.1 pp off** ⚠️ (PV1-only vs all 10 PVs likely explains some; may also reflect different threshold interpretation)

### College Premium (Script 04)

- College premium is declining in most countries with both rounds
- Biggest declines: POL (−15 pts), SGP (−13), IRL (−9), ESP (−8), CAN (−6)
- Some countries increased: EST (+16), NZL (+10), FRA (+8), HUN (+8), JPN (+9)
- Email back-of-envelope (pooled across all countries, not USA-only):
  - cy1 birth cohort 1968–73: mean = 258, college share = 22% → `0.22X + 0.78Y = 258`
  - cy2 birth cohort 1982–87: mean = 259, college share = 38% → `0.38X + 0.62Y = 259`
  - Similar overall means despite higher college share in cy2 is consistent with the compression story

---

## Open Questions / Next Steps

1. **GBR round 1 EDCAT7 = 0** — investigate whether the variable is differently coded or missing for UK cy1
2. **DEU illiteracy gap** — 5.1 pp vs benchmark; rerun with all 10 PVs to check if it closes
3. **AGE_R missingness** — 35.4% missing; consider using `AGEG10LFS` midpoints for birth cohort instead of `AGE_R`
4. **USA cohort trend** — USA only in cy2; need cy1 to show the 10–20 pt decline described in emails
5. **Upgrade to all 10 PVs + BRR weights** for publication-quality SEs
6. **Blog post draft** — findings are strong enough to start outlining

---

---

## 2026-04-12 Session: PISA Data Acquisition + Chile Comparison

### Goal
Extend analysis to PISA (2006–2022) to compare 15-year-old literacy/numeracy trends with PIAAC adult trends, starting with Chile.

### Download script fixes (`01_scripts/00_download_data.r`)
- Fixed broken `walk2` call (missing `.y` destfile arg) in PIAAC section
- Added `options(timeout = 3600)` for large PISA files
- Added `.done` marker system — re-runs skip already-downloaded files
- Added PISA section: 6 years (2006–2022), SPSS format for 2015+, TXT for 2006–2012
- Seeded `.done` markers for all 60 existing PIAAC `.sav` files

### PISA data acquired (`00_data/pisa/`)
| Year | File | Format | Status |
|------|------|--------|--------|
| 2006 | `INT_Stu06_Dec07.txt` | TXT/FWF | ✅ |
| 2009 | `INT_STQ09_DEC11.txt` | TXT/FWF | ✅ |
| 2012 | `INT_STU12_DEC03.txt` | TXT/FWF | ✅ |
| 2015 | `CY6_MS_CMB_STU_QQQ.sav` | SPSS | ✅ |
| 2018 | `STU/CY07_MSU_STU_QQQ.sav` | SPSS | ✅ |
| 2022 | — | — | ❌ not yet downloaded |

SPSS control files for 2006/2009/2012 in `docs/` — needed to parse fixed-width TXT files.

### Key data notes
- 2006–2012: 5 PVs (`PV1READ`–`PV5READ`, `PV1MATH`–`PV5MATH`), BRR weights `W_FSTR1`–`W_FSTR80`
- 2015–2018: 10 PVs, BRR weights named `W_FSTURWT1`–`W_FSTURWT80` (standardised to `W_FSTR*` in script)
- Main weight all years: `W_FSTUWT`
- Country identifier: `CNT` (3-letter ISO)
- Chile PIAAC: 9,938 rows, survey years 2014 + 2023

### Script in progress: `01_scripts/05_chile_pisa_piaac.r`
- Parses SPSS DATA LIST syntax to get FWF column positions (`parse_spss_layout`)
- Reads TXT files efficiently with `read_fwf` (only needed columns)
- Reads SPSS files with `haven::read_sav` + column rename
- PV+BRR variance estimation (Rubin's rules) for both PISA and PIAAC
- Produces: `02_output/chile_pisa.rds`, `02_output/chile_piaac.rds`
- Figures: raw PISA trends, raw PIAAC trends, normalised index comparison
- Status: running — all countries at once, cores=1 (no cluster overhead)

### Key decisions / lessons
- **Rrepest overhead**: spins up `makeCluster(detectCores()-1)` per call → slow on 2-core WSL machine. Fix: `cores=1` + run all countries at once with `by="cnt"` so cluster cost is paid once per year not per country
- **System**: Intel Core Ultra 5 125U, 2 physical cores (4 logical), 5.8GB RAM, Intel Arc iGPU (not CUDA). No NVIDIA GPU.
- **PISA BRR note**: PISA uses Fay's BRR k=0.5 (not standard BRR). Rrepest handles this automatically. Our custom helper would need `sum(delta^2)/(R*(1-0.5)^2)` = 4x correction.
- **Cache built**: `00_data/pisa/cache/pisa_{year}.rds` — all countries, needed columns only. 23-62 MB per year vs 700MB-1.5GB raw. Sub-second loads.
- **Script structure**: `05a_cache_pisa.r` (one-time), `05_chile_pisa_piaac.r` (analysis, loads from cache)

---

## Methodological Simplifications (First Pass)

- PV1 only (not all 10 plausible values)
- SPFWT0 only (no BRR/JK replicate weights → no SEs)
- Birth cohort = `survey_year - AGE_R` (many NAs due to AGE_R missingness)
- College = `EDCAT7 ≥ 6` (bachelor's or higher)
- Illiteracy = `PVLIT1 ≤ 225` (Level 1 ceiling)

---
**Context compaction (auto) at 22:34**
Check git log and quality_reports/plans/ for current state.

---

## 2026-04-12 Session (continued): PISA pipeline + cohort linkage

### Completed scripts

| Script | Output | Status |
|--------|--------|--------|
| `05a_cache_pisa.r` | `00_data/pisa/cache/pisa_{year}.rds` (23–62 MB each) | ✅ |
| `05_chile_pisa_piaac.r` | `chile_pisa.rds`, `chile_piaac.rds`, 3 figures | ✅ |
| `06_cohort_pisa_piaac.r` | `cohort_pisa_piaac.rds`, 6 figures | ✅ |

### Key fixes applied this session
- **Rrepest dropped**: WSL2 socket serialization failed on full dataset. Replaced with custom `pv_group_mean(fay_k=0.5)` for PISA Fay BRR. Faster and transparent.
- **PISA 9997 missing codes**: USA 2006 Reading PVs coded 9997 (not NA) — inflated means to ~9997. Fix: `ifelse(x > 900, NA, x)` applied on cache load in `05_chile_pisa_piaac.r`. Requires `pv_group_mean` to handle NA in PV cols (updated helpers).
- **Helpers updated**: `pv_group_mean()` now accepts `fay_k=0` param (backward compatible); NA in PV columns handled per-observation with adjusted weights.
- **Z-score reference**: fixed to use only the 34-country PISA-PIAAC overlap (not all 98 PISA countries) for fair cross-survey comparison.

### Age-matched cohort analysis (script 06, Fig 4)
Compares young adults (~age 20-21) across two generations to detect peak literacy:
- **Gen 1**: PISA 2006 cohort (born 1989–1993) × PIAAC 2012, age ~21
- **Gen 2**: PISA 2018 cohort (born 2001–2005) × PIAAC 2023, age ~20
- 18 countries in both: BEL, CAN, CZE, DEU, DNK, ESP, EST, FIN, FRA, GBR, IRL, ITA, JPN, KOR, LTU, POL, SVK, USA

**Literacy change (Gen2 - Gen1, same age):**
- Largest declines: POL −33.9, LTU −25.3, SVK −23.5, KOR −18.0, USA −10.3
- Improvers: GBR +18.2, ITA +11.3, IRL +11.1, EST +3.0, CAN +2.0
- Mixed: DEU +0.3, FIN +1.4 (near-flat)

**Numeracy change:**
- Largest declines: LTU −27.6, POL −20.2, SVK −16.0, KOR −9.8
- Improvers: GBR +24.9, ITA +17.3, IRL +14.9, JPN +12.5

Eastern European countries (POL, LTU, SVK) show sharp skill declines in young adults; several Western European countries (GBR, ITA, IRL) improved.

### Open questions
- Why are GBR/ITA/IRL improving? Sample composition or genuine gains?
- ΔPISA vs ΔPIAAC scatter (Fig 4b): does the PISA trend predict the PIAAC change direction?
- PISA 2022 data still not downloaded — would extend the analysis
- NLD, NOR, SWE in PIAAC 2023 but missing from age-matched comparison (not in PIAAC 2012)

---
**Context compaction (auto) at 23:48**
Check git log and quality_reports/plans/ for current state.

---
**Context compaction (auto) at 11:14**
Check git log and quality_reports/plans/ for current state.

---
**Context compaction (auto) at 15:57**
Check git log and quality_reports/plans/ for current state.

---

## 2026-04-14 Session: FT Article Comment Scraping + Sentiment Analysis

### Goal
Scrape and run sentiment analysis on comments from FT article on literacy/PIAAC
(https://www.ft.com/content/e2ddd496-4f07-4dc8-a47c-314354da8d46).

### Script
`explorations/ft_comments/scrape_sentiment.r` — outputs to `explorations/ft_comments/output/`

### Key findings (technical)
- FT uses Coral comment platform; comments loaded via GET GraphQL API (not in HTML)
- Endpoint: `https://ft.coral.coralproject.net/api/graphql`
- Persisted query ID: `fe6db435838489b39afeb993214af799`
- No auth cookie needed — but unauthenticated access is capped at **164 comments** (445 total; remainder requires FT subscription)
- AFINN lexicon downloaded directly from GitHub (textdata package wouldn't install)

### Sentiment results (164 comments)
- Mean AFINN score: **0.09** (near-neutral)
- Breakdown: 65% Neutral, 18% Positive, 17% Negative
- Tone is measured/analytical — typical for FT opinion readership

---
**Context compaction (auto) at 23:43**
Check git log and quality_reports/plans/ for current state.

---
**Context compaction (auto) at 23:44**
Check git log and quality_reports/plans/ for current state.
