---
globs:
  - "Quarto/**/*.qmd"
  - "master_supporting_docs/**/*"
  - "scripts/**/*.R"
  - "docs/**"
---

# Task Completion Verification Protocol

**At the end of EVERY task, Claude MUST verify the output works correctly.** This is non-negotiable.

## For Quarto Manuscripts / Notes (`.qmd`):
1. Render with Quarto (`quarto render path/to/file.qmd`)
2. Verify output exists (HTML/PDF/docx as configured in front matter)
3. Check citations and cross-references resolve
4. Confirm figures/tables referenced in text are present
5. Report verification results

## For R Scripts:
1. Run `Rscript path/to/file.R`
2. Verify output files (figures/tables/RDS) were created with non-zero size
3. Spot-check estimates for reasonable magnitude

## Common Pitfalls:
- **Unrendered citations**: missing bibliography linkage or wrong cite keys
- **Stale outputs**: rendered docs not regenerated after code/data changes
- **Hidden chunk failures**: code chunks fail silently when warnings/messages are suppressed
- **Assuming success**: always verify output files exist AND contain expected content

## Verification Checklist:
```
[ ] Output file created successfully
[ ] No compilation/render errors
[ ] Images/figures display correctly
[ ] Paths resolve in deployment location (docs/)
[ ] Opened in browser/viewer to confirm visual appearance
[ ] Reported results to user
```
