# CLAUDE.md -- PIAAC Literacy Project

**Project:** PIAAC Adult Literacy Analysis
**Institution:** Center for Global Development (CGD)
**Primary Stack:** R (tidyverse)
**Branch:** main

---

## Research Questions

1. **Did OECD adult literacy peak around 2015?** Track cohort-level PIAAC scores across rounds (2012/14, 2017, 2023).
2. **Cohort analysis:** Follow 10-year age bands across survey waves to separate aging effects from cohort effects.
3. **College returns:** Has the literacy gain from attending college declined? (Back-of-envelope: 0.15X + 0.85Y ≈ 220 in 2013 vs. 0.5X + 0.5Y ≈ 220 in 2023 suggests massive compression.)
4. **Cross-country comparisons:** Functional illiteracy rates (below Level 1, ≤225) across 35 PIAAC countries.
5. **Possible output:** CGD blog post on PIAAC trends.

---

## Core Principles

- **Plan first** -- for non-trivial work: plan, save, approve, then execute
- **Verify after** -- run scripts/tests/checks before reporting completion
- **Single source of truth** -- analysis code and outputs must be reproducible from versioned source
- **Quality gates** -- nothing ships below 80/100
- **[LEARN] tags** -- when corrected, append `[LEARN:category] wrong -> right` to `MEMORY.md`

---

## Data

**Source:** OECD PIAAC Public Use Files (SPSS `.sav` format)
**Location:** `00_data/` → symlink to `/mnt/box/das-human-kapital/piaac/00_data`
**Download script:** `01_scripts/00_download_data.r`
**URL pattern:** `https://webfs.oecd.org/piaac/cy{round}-puf-data/SPSS/prg{iso3}p{round}_sav.zip`

**Rounds:** cy1 (2012/14), cy2 (2017), cy3 (2023)

**Countries (35):** AUT, BEL, CAN, CHL, CZE, DNK, ECU, EST, FIN, FRA, DEU, GRC, HUN, IRL, ISR, ITA, JPN, KAZ, KOR, LTU, MEX, NLD, NZL, NOR, PER, POL, RUS, SGP, SVK, SVN, ESP, SWE, TUR, GBR, USA

**Key variables:**
- Literacy score (plausible values, ~200–350 range; Level 1 ≤ 225, Level 2 ≤ 276)
- Age bands: 10-year cohorts (16–24, 25–34, 35–44, 45–54, 55–65)
- Education: college/non-college
- Survey weights (must be used for population estimates)

---

## Folder Structure

```text
piaac/
├── CLAUDE.md
├── .claude/                     # Rules, skills, agents, hooks
├── 00_data -> /mnt/box/...      # Raw PIAAC PUF data (symlink, not committed)
├── 01_scripts/                  # R analysis scripts
├── 02_output/                   # Figures, tables, derived data
├── 03_docs/                     # Reference docs, notes, emails
├── Figures/                     # Publication-ready figures
├── Quarto/                      # Manuscripts, blog posts, appendices
├── master_supporting_docs/      # Papers, external references
├── explorations/                # Sandbox / fast-track experiments
├── quality_reports/             # Plans, session logs, merge reports
├── templates/                   # Reusable report/log templates
└── docs/                        # Rendered site artifacts (optional)
```

---

## Commands

```bash
# Run a script
Rscript 01_scripts/00_download_data.r

# Save a plan before non-trivial tasks
# quality_reports/plans/YYYY-MM-DD_short-description.md

# Paper/blog build
make paper
```

---

## R Conventions

| Item | Convention |
|------|-----------|
| Paths | Project-relative via `here::here()` |
| Seed | `set.seed(42)` at top of each script |
| Weights | Always use PIAAC survey weights for population stats |
| Plausible values | Average across all 10 PVs; use BRR or JK replicate weights for SEs |
| Figures | `ggplot2`; vector output (PDF/SVG); consistent palette |
| Reporting style | Finding first, then evidence |

---

## Quality Thresholds

| Score | Gate | Meaning |
|-------|------|---------|
| 80 | Commit | Good enough to save |
| 90 | PR | Ready for merge/sharing |
| 95 | Excellence | Aspirational |

---

## Skills Quick Reference

| Command | What It Does |
|---------|-------------|
| `/data-analysis [dataset]` | End-to-end empirical analysis workflow |
| `/review-r [file]` | R code quality and reproducibility review |
| `/lit-review [topic]` | Literature search + synthesis |
| `/research-ideation [topic]` | Research questions, hypotheses, empirical strategy |
| `/interview-me [topic]` | Interactive idea formalization |
| `/review-paper [file]` | Manuscript-level argument/specification review |
| `/qa-quarto [file]` | Adversarial QA for Quarto manuscripts |
| `/proofread [file]` | Language/clarity/consistency QA |
| `/validate-bib` | Citation/bibliography consistency checks |
| `/commit [msg]` | Stage, commit, PR, merge workflow |

---

## Current Project State

| Workstream | Status | Canonical Paths | Notes |
|------------|--------|-----------------|-------|
| Data download | Done | `01_scripts/00_download_data.r`, `00_data/` | cy1 + cy2; cy3 (2023) TBD |
| Cohort analysis | TODO | `01_scripts/`, `02_output/` | Track age bands across rounds |
| Cross-country illiteracy rates | TODO | `01_scripts/`, `Figures/` | Below Level 1 (≤225) by country |
| College returns analysis | TODO | `01_scripts/`, `02_output/` | College vs. non-college score gaps over time |
| CGD blog post | TODO | `Quarto/` | Summarize findings; possible joint authorship |
