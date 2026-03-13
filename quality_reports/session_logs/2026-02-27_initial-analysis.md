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

## Methodological Simplifications (First Pass)

- PV1 only (not all 10 plausible values)
- SPFWT0 only (no BRR/JK replicate weights → no SEs)
- Birth cohort = `survey_year - AGE_R` (many NAs due to AGE_R missingness)
- College = `EDCAT7 ≥ 6` (bachelor's or higher)
- Illiteracy = `PVLIT1 ≤ 225` (Level 1 ceiling)
