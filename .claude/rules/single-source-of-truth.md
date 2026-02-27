---
globs:
  - "Figures/**/*"
  - "Quarto/**/*.qmd"
  - "scripts/**/*.R"
---

# Single Source of Truth: Enforcement Protocol

**Each project must define one canonical source per artifact type and keep derived outputs in sync.**

## The SSOT Chain

```
Canonical analysis scripts / notebooks (SOURCE OF TRUTH)
  ├── Figures/* (derived)
  ├── Tables/* (derived)
  ├── Quarto outputs (derived, if used)
  ├── docs/* rendered artifacts (derived, if used)
  └── Bibliography_base.bib (shared reference source)

NEVER edit derived artifacts independently.
ALWAYS propagate changes from source → derived.
```

---

## Content Fidelity Checklist

```
[ ] Source scripts reproduce all claimed figures/tables/results
[ ] Derived outputs regenerated after source edits
[ ] Citation keys in text exist in bibliography
[ ] No manual edits to rendered artifacts in docs/
```
