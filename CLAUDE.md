# CLAUDE.md -- Research Project Workflow Template

<!-- HOW TO USE: Replace [BRACKETED PLACEHOLDERS] with your project info.
     Keep this file concise (~150 lines). This is loaded every session. -->

**Project:** [PROJECT NAME]
**Institution:** [INSTITUTION OR TEAM]
**Primary Stack:** [R / Python / Stata / mixed]
**Branch:** main

---

## Core Principles

- **Plan first** -- for non-trivial work: plan, save, approve, then execute
- **Verify after** -- run scripts/tests/checks before reporting completion
- **Single source of truth** -- analysis code and outputs must be reproducible from versioned source
- **Quality gates** -- nothing ships below 80/100
- **[LEARN] tags** -- when corrected, append `[LEARN:category] wrong -> right` to `MEMORY.md`

---

## Folder Structure

```text
[PROJECT-ROOT]/
├── CLAUDE.md
├── .claude/                     # Rules, skills, agents, hooks
├── scripts/                     # Analysis scripts and utilities
├── Figures/                     # Generated figures
├── data/                        # Raw/processed datasets (optional)
├── master_supporting_docs/      # Papers, notes, external references
├── explorations/                # Sandbox work (fast-track rules)
├── quality_reports/             # Plans, session logs, merge reports
├── templates/                   # Reusable report/log templates
├── Quarto/                      # Quarto manuscripts, notes, appendices
└── docs/                        # Optional: rendered site artifacts
```

---

## Commands

```bash
# Save a plan before non-trivial tasks
# quality_reports/plans/YYYY-MM-DD_short-description.md

# Paper build (Hikmah)
make paper

# Run analysis scripts (example)
Rscript scripts/analysis.R

# Optional: quality score utility for rendered artifacts
python scripts/quality_score.py <target-file>
```

---

## Quality Thresholds

| Score | Gate | Meaning |
|-------|------|---------|
| 80 | Commit | Good enough to save |
| 90 | PR | Ready for merge/deployment |
| 95 | Excellence | Aspirational |

---

## Skills Quick Reference (Research-First)

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

## Research Defaults To Fill Once

| Item | Default | Replace With |
|------|---------|--------------|
| Path convention | `here::here()` or project-relative paths | [YOUR CHOICE] |
| Seed convention | one global `set.seed()` per script | [YOUR CHOICE] |
| Tolerance thresholds | estimates `1e-6`, SEs `1e-4`, coverage `+/-0.01` | [YOUR CHOICE] |
| Figure standards | vector output preferred, readable labels, consistent palette | [YOUR CHOICE] |
| Reporting style | concise findings first, then evidence | [YOUR CHOICE] |

---

## Current Project State

| Workstream | Status | Canonical Paths | Notes |
|------------|--------|-----------------|-------|
| Core workflow configuration | Active | `.claude/`, `CLAUDE.md` | Claude Code research template |
| Empirical analysis pipeline | [TODO] | `scripts/`, `Figures/` | Add dataset- and project-specific conventions |
| Quarto manuscript pipeline | [TODO] | `Quarto/`, `docs/`, `master_supporting_docs/` | Draft, QA, and render papers/notes |
| Other research tasks | Active | `explorations/`, `quality_reports/` | Ideation, lit review, replication, scoped experiments |
