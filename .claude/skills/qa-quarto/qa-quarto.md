---
description: Adversarial QA workflow for Quarto manuscripts/notes. Iterates between critic (finds issues) and fixer (applies fixes) until APPROVED or max iterations reached.
---

**Arguments:** "[Quarto file path, e.g., Quarto/paper.qmd]"


# Adversarial Quarto Manuscript QA Workflow

Run an iterative critic/fixer loop on a Quarto paper or research note.
Target outcomes: factual consistency, citation integrity, argument clarity, reproducible rendering, and clean output.


## Workflow

```
Phase 0: Pre-flight → Phase 1: Critic audit → Phase 2: Fixes → Phase 3: Re-audit → Loop until APPROVED (max 5 rounds)
```

## Hard Gates (Non-Negotiable)

| Gate | Condition |
|------|-----------|
| **Render Success** | Quarto renders without errors |
| **Citation Integrity** | Every cite key resolves in bibliography |
| **Reproducibility** | Code chunks run or are intentionally frozen with rationale |
| **Argument Coherence** | Claims supported by results and references |
| **Output Integrity** | Figures/tables exist, labels and references resolve |
| **Writing Quality** | No high-severity grammar/clarity issues |

## Phase 0: Pre-flight

1. Locate target `.qmd` and bibliography sources
2. Render Quarto output (HTML/PDF/docx as configured)
3. Confirm expected output files exist and are non-empty

## Phase 1: Initial Audit

Audit the rendered document and source for:
- section flow and argument logic
- unresolved cross-refs/citations
- table/figure caption consistency
- code chunk reproducibility and deterministic settings
- style, grammar, notation consistency

Save report to `quality_reports/[name]_qa_round1.md`.

## Phase 2: Fix Cycle

Apply fixes in priority order (Critical → Major → Minor), re-render, and verify gates.

## Phase 3: Re-Audit

Re-audit after fixes. Loop back to Phase 2 if needed.

## Iteration Limits

Max 5 fix rounds. After that, escalate to user with remaining issues.

## Final Report

Save to `quality_reports/[name]_qa_final.md` with hard gate status, iteration summary, and unresolved issues.
