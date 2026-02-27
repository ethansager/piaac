# Workflow Quick Reference

**Model:** Contractor (you direct, Claude orchestrates)

---

## The Loop

```
Your instruction
    ↓
[PLAN] (if multi-file or unclear) → Show plan → Your approval
    ↓
[EXECUTE] Implement, verify, done
    ↓
[REPORT] Summary + what's ready
    ↓
Repeat
```

---

## I Ask You When

- **Design forks:** "Option A (fast) vs. Option B (robust). Which?"
- **Code ambiguity:** "Spec unclear on X. Assume Y?"
- **Replication edge case:** "Just missed tolerance. Investigate?"
- **Scope question:** "Also refactor Y while here, or focus on X?"

---

## I Just Execute When

- Code fix is obvious (bug, pattern application)
- Verification (tolerance checks, tests, compilation)
- Documentation (logs, commits)
- Plotting (per established standards)
- Quarto manuscript QA/rendering (after you approve, I ship automatically)

---

## Quality Gates (No Exceptions)

| Score | Action |
|-------|--------|
| >= 80 | Ready to commit |
| < 80  | Fix blocking issues |

---

## Non-Negotiables (Template Defaults)

- Paths: project-relative only (`here::here()` in R; no absolute paths)
- Seeds: one explicit `set.seed()` at top of each stochastic script
- Figures: reproducible export, readable labels, consistent fonts/palette
- Palette: define once (e.g., in theme/scss) and reuse across outputs
- Tolerances: point estimates `1e-6`, standard errors `1e-4`, coverage `+/-0.01`

---

## Preferences

**Visual:** publication-ready, clean typography, no clipping/overlap
**Reporting:** findings first, then evidence and verification notes
**Session logs:** Always (post-plan, incremental, end-of-session)
**Replication:** strict by default, flag near-misses explicitly

---

## Exploration Mode

For experimental work, use the **Fast-Track** workflow:
- Work in `explorations/` folder
- 60/100 quality threshold (vs. 80/100 for production)
- No plan needed — just a research value check (2 min)
- See `.claude/rules/exploration-fast-track.md`

---

## Next Step

You provide task → I plan (if needed) → Your approval → Execute → Done.
