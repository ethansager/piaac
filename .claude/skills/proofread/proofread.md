---
description: Run the proofreading protocol on research writing files. Checks grammar, typos, consistency, and academic writing quality. Produces a report without editing files.
---

**Arguments:** "[filename or 'all']"


# Proofread Research Writing Files

Run the mandatory proofreading protocol on research writing files. This produces a report of all issues found WITHOUT editing any source files.

## Steps

1. **Identify files to review:**
   - If `$ARGUMENTS` is a specific filename: review that file only
   - If `$ARGUMENTS` is "all": review all writing files in `Quarto/` and manuscript sources in `master_supporting_docs/`

2. **For each file, launch the proofreader agent** that checks for:

   **GRAMMAR:** Subject-verb agreement, articles (a/an/the), prepositions, tense consistency
   **TYPOS:** Misspellings, search-and-replace artifacts, duplicated words
   **STRUCTURE:** Section flow, paragraph coherence, heading consistency
   **CONSISTENCY:** Citation format, notation, terminology
   **ACADEMIC QUALITY:** Informal language, missing words, awkward constructions

3. **Produce a detailed report** for each file listing every finding with:
   - Location (line number or section heading)
   - Current text (what's wrong)
   - Proposed fix (what it should be)
   - Category and severity

4. **Save each report** to `quality_reports/`:
   - For `.qmd` files: `quality_reports/FILENAME_qmd_report.md`
   - For manuscript files: `quality_reports/FILENAME_proofread_report.md`

5. **IMPORTANT: Do NOT edit any source files.**
   Only produce the report. Fixes are applied separately after user review.

6. **Present summary** to the user:
   - Total issues found per file
   - Breakdown by category
   - Most critical issues highlighted
