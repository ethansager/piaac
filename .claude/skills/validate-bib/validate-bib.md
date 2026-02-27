---
description: Validate bibliography entries against citations in Quarto manuscripts and research documents. Find missing entries and unused references.
---


# Validate Bibliography

Cross-reference all citations in research writing files against bibliography entries.

## Steps

1. **Read the bibliography file** and extract all citation keys

2. **Scan all research writing files for citation keys:**
   - `.tex` files: look for `\cite{`, `\citet{`, `\citep{`, `\citeauthor{`, `\citeyear{`
   - `.qmd` files: look for `@key`, `[@key]`, `[@key1; @key2]`
   - Extract all unique citation keys used

3. **Cross-reference:**
   - **Missing entries:** Citations used in manuscripts/notes but NOT in bibliography
   - **Unused entries:** Entries in bibliography not cited anywhere
   - **Potential typos:** Similar-but-not-matching keys

4. **Check entry quality** for each bib entry:
   - Required fields present (author, title, year, journal/booktitle)
   - Author field properly formatted
   - Year is reasonable
   - No malformed characters or encoding issues

5. **Report findings:**
   - List of missing bibliography entries (CRITICAL)
   - List of unused entries (informational)
   - List of potential typos in citation keys
   - List of quality issues

## Files to scan:
```
Quarto/*.qmd
master_supporting_docs/**/*
```

## Bibliography location:
```
Bibliography_base.bib  (repo root)
```
