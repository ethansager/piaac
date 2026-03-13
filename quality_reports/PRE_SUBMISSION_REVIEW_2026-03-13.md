# PRE-SUBMISSION REVIEW — 2026-03-13
## Paper: "Did Adult Literacy Peak Around 2015?"
## Type: CGD Blog Post
## File: Quarto/blog_did-literacy-peak.qmd

---

## TRIAGE — Critical issues (must fix before publication)

### C1. BRR variance formula may be missing Fay correction factor
**Agent: Math/Methodology**
The `pv_group_mean()` helper computes BRR sampling variance as `mean((theta_r - theta_k)^2)` = `(1/R) * sum(...)`. PIAAC uses Fay's BRR with perturbation factor k=0.5, which requires scaling by `1 / (R * (1-k)^2)` = `1 / (80 * 0.25)` = `1/20`, not `1/80`. If uncorrected, **all standard errors in the project are understated by a factor of 2** (variance understated by 4×). Must verify against OECD PIAAC Technical Standards Section 15 before any published claims.

**Fix:** Verify the perturbation factor. If k=0.5, change line 58 of `00_helpers.r`:
```r
# current (possibly wrong):
v_brr <- mean((theta_r - theta_k[k])^2)
# corrected for Fay k=0.5:
v_brr <- sum((theta_r - theta_k[k])^2) / (length(theta_r) * (1 - 0.5)^2)
```

---

### C2. "All 24 show score declines" is likely wrong
**Agents: Consistency, Claims, Math/Methodology, Copy Editor**
The blog states (line 66): *"Among the 24 countries that participated in both Cycle 1 and Cycle 2, all 24 show score declines."* But:
- The project MEMORY.md records Finland at +5 points (improvement)
- Line 81 says "Finland shows the smallest decline at under 1 point" — contradicts both the data and the "all 24 decline" claim
- MEMORY.md says 12 countries in both cy1 and cy2 for cohort analysis; the blog claims 24

**Fix:** Run `readRDS("02_output/cohort_change.rds")` and verify the exact country count and direction for each. Correct the "all 24" claim to match actual output. If Finland improved, say "23 of 24" (or whatever the correct count is).

---

### C3. Back-of-envelope cohort arithmetic is backwards
**Agents: Consistency, Claims, Math/Methodology**
Line 95: *"in the early 2010s, roughly 15 percent of 40–44 year-olds held college degrees... A decade later, the same birth cohort (now 30–34) has a 50 percent college-going rate."*

This is mathematically impossible. A cohort aged 40–44 in 2012 would be 50–54 in 2023, not 30–34. The blog is comparing two *different* birth cohorts (born ~1968–72 vs. ~1988–93) and incorrectly calling them the same cohort. The algebra is sound but the setup description is wrong.

**Fix:** Rewrite to compare two different cohorts: "Adults aged 40–44 in 2012 (born ~1970, when ~15% attended college) had average literacy of ~220. In 2023, a *younger* cohort aged 30–34 (born ~1990, when ~50% attended college) scores similarly after age adjustment. For the averages to be similar despite the tripling of college shares, the college premium must have compressed."

---

### C4. US illiteracy rate (18%) is inconsistent with the figure
**Agents: Consistency, Claims, Referee**
Line 24 opens with the US at 18% illiteracy. But `03_illiteracy_rates.r` filters to round 1 only (`filter(piaac, round == 1, ...)`), and per MEMORY.md *"USA only has p2 file (no p1 released as PUF); survey_year = 2023."* The US therefore cannot be in the Cycle 1 illiteracy figure, yet the blog leads with the US number and the email benchmark for the US is 28% (not 18%). Either the US 18% comes from a different round/method than the figure shows, or the figure does include Cycle 2 data for the US but is mislabeled.

**Fix:** Clarify which round the US 18% comes from. If it is from Cycle 2, note this explicitly and don't imply it comes from the same Cycle 1 analysis as the cross-country figure. Also reconcile the 18% (per-PV method) vs. 28% (email benchmark) discrepancy somewhere.

---

### C5. "25 points separates Level 1 from Level 2" — arithmetic error
**Agents: Math/Methodology, Claims, Copy Editor**
Line 85: *"a difference of 25 points separates Level 1 (functional illiteracy, ≤225) from Level 2 (≤276)."* The gap between 225 and 276 is **51 points**, not 25. This is a simple factual error that will undermine credibility.

**Fix:** Correct to: *"Level 2 spans 226 to 275 — a roughly 50-point range above the functional illiteracy threshold."*

---

## MAJOR issues (fix before publication)

### M1. Aging confound dismissed without evidence
**Agents: Claims, Referee, Math/Methodology**
Line 91: *"the magnitudes... are too large to be explained by aging alone."* This is asserted without quantification. The PIAAC cross-sectional age gradient is roughly 1–2 points per year (10–22 points over an 11-year gap). Countries showing 10–15 point declines (Austria, Chile, Ireland, Italy, Spain, Japan, USA) could have declines **fully explained by aging**. Poland and Lithuania (30+ points) likely show genuine skill change, but the blog treats all countries the same.

**Fix:** Add a sentence quantifying the expected aging effect: "Cross-sectional PIAAC data suggest an age gradient of roughly 1–2 points per year, implying 10–20 points of expected decline from aging alone over the ~11-year gap. Countries with declines well beyond this range — Poland (−37), Lithuania (−34), Korea (−34) — likely reflect genuine cohort deterioration. For countries showing 10–15 point declines, aging could account for much or most of the change."

---

### M2. "The data confirm this" for college premium compression — but 12 of 21 countries show *increases*
**Agent: Claims**
Line 99: *"The data confirm this, across many countries"* — referring to the back-of-envelope prediction of compression. But the very next sentence reports that 12 of 21 countries show *increases* in the college premium. The word "confirm" is directly contradicted by the data.

**Fix:** Replace "The data confirm this" with "The data show a mixed picture." Lead with the 9-vs-12 split rather than burying it.

---

### M3. "NA" country label in college premium figures
**Agent: Figures**
Both `college_premium.png` and `college_premium_gender.png` display a country labeled "NA" as one of the bars. This is a data/code bug in `04_college_returns.r` — likely a row where the country code was not matched or a missing value slipped through the filter.

**Fix:** Investigate and fix the NA-country row in the college returns output. Add a filter `!is.na(country)` in the plot data preparation.

---

### M4. Gender and native-speaker breakdown figures show only one subgroup
**Agent: Figures**
- `illiteracy_rates_gender.png` shows only Female panel
- `illiteracy_rates_nativespeaker.png` shows only Non-native speakers
- `cohort_trends_gender.png` and `college_premium_gender.png` similarly show Female only

The text discusses "gaps" (e.g., "gaps are narrower than expected") but no figure actually shows a gap — readers see only one subgroup. A reader cannot assess the gap from a single panel.

**Fix:** Either (a) create side-by-side or stacked comparison figures, or (b) create a "gap" figure showing the male–female difference directly, or (c) at minimum update captions to note that the overall chart (Figure 1) serves as the comparison baseline.

---

### M5. Missing ranked bar chart of cohort score changes
**Agent: Figures**
The text (lines 68–81) lists country-specific declines in a bulleted list. A ranked bar chart of mean cohort score changes (cy2 − cy1) ranked from largest to smallest decline would be more impactful and memorable than the current small-multiples figure. The `cohort_change.rds` file already contains this data.

**Fix:** Add a horizontal ranked bar chart of country-level mean cohort changes to Finding 2.

---

### M6. "Finland and several Nordic countries cluster around 10–12 percent" — Sweden and Denmark don't fit
**Agent: Consistency**
Line 46: claims Nordic countries cluster around 10–12%. Data shows: Finland 10.3%, Norway 12.2%, Sweden 12.9%, Denmark 15.4%. Sweden is above 12% and Denmark is well above it.

**Fix:** Expand the range or exclude Denmark: *"Finland (10%), Norway (12%), and Sweden (13%) cluster near the top of the distribution."*

---

### M7. Israel classified as "wealthy European"
**Agent: Copy Editor**
Line 50: *"Even wealthy European countries like Israel, Italy, and Spain (each around 27 percent)."* Israel is not a European country — it is a Middle Eastern country and OECD member.

**Fix:** *"Even some wealthy OECD countries — Israel, Italy, and Spain (each around 27 percent)..."*

---

### M8. Repeated cross-sections vs. panel — "same group of people" is misleading
**Agent: Claims**
Line 91: *"genuine deterioration in the skills of the same group of people over time."* PIAAC is a repeated cross-section, not a panel. The "same cohort" across rounds is composed of different individuals. Differential migration, mortality, and survey non-response can shift cohort composition.

**Fix:** Replace "same group of people" with "same birth cohorts (different individuals sampled at each round)" and add one sentence acknowledging compositional change.

---

### M9. Test comparability across PIAAC rounds not mentioned
**Agent: Claims, Referee**
PIAAC Cycle 2 (2023) used a redesigned computer-adaptive assessment format. Equating between Cycle 1 and Cycle 2 rests on assumptions. If equating is imperfect, a systematic downward shift could be artifactual. This is a standard caveat for any cross-round comparison and its absence will be noticed by readers familiar with PIAAC.

**Fix:** Add one sentence in "The Data" section: *"PIAAC Cycle 2 used a redesigned computer-adaptive format; the OECD conducted equating studies to maintain comparability across cycles, but any cross-round comparison rests on the assumption that this equating is successful."*

---

### M10. Empty bibliography file
**Agent: Referee**
`Quarto/references.bib` is effectively empty. The blog cites no references. At minimum, it should cite the OECD PIAAC technical standards and the OECD Skills Outlook.

**Fix:** Add at minimum: OECD (2023) PIAAC Technical Standards, OECD (2013/2019) Skills Outlook reports, and any prior cohort-level PIAAC analysis.

---

## MINOR issues (fix if time permits)

### m1. "10–33 points" cohort decline range is wrong in both directions
**Agent: Consistency**
Line 85: *"Cohort declines of 10–33 points."* The maximum decline is −36.5 (Poland), not −33. And several countries show declines under 10 points. The range should be the actual observed range.

---

### m2. Poland cohort decline: −36.5 in data, text says −37
The data shows Poland at −36.5 mean cohort change; text says −37. Minor rounding but slightly overstated.

---

### m3. Singapore college premium: −13.4 in data, text says −14
Data shows −13.4; text says −14. Consider rounding to −13.

---

### m4. Figure captions are too sparse
**Agent: Figures**
Most figure captions are 1–2 sentences with no data source, no threshold definition, and no abbreviation explanations. Each caption should include: what the figure shows, data source, key definition (e.g., "illiteracy = ≤225"), and note what color/gold highlighting means.

---

### m5. Gold bar highlighting not explained in captions
**Agent: Figures**
Figures 1–3 use gold bars for "highlighted countries" but the blog text never explains the gold highlighting. Add an explanation in each caption or remove the highlighting.

---

### m6. Figures use raw markdown image syntax, not Quarto cross-references
**Agent: Figures**
All figures use `![caption](path)` instead of Quarto labeled blocks (`{#fig-label}` / `@fig-label`). No automatic figure numbering. For a multi-figure post this makes text references ("the figure above") ambiguous.

**Fix:** Convert to Quarto figure blocks with `{#fig-X}` labels.

---

### m7. Cycle 1 round structure misstated
**Agent: Math/Methodology**
Lines 34–38 list two Cycle 1 sub-rounds "(2012/14)" and "(2014/17)," but there are actually three: Round 1 (2012, 21 countries), Round 2 (2014, 10 countries), Round 3 (2017, 3 countries). Simplify to "Cycle 1 (2012–2017): 34 countries across three rounds."

---

### m8. AUT's round 2 is Cycle 1 Round 2 (2014), not Cycle 2 (2023)
**Agent: Consistency**
The blog implies all 24 cohort-analysis countries participated in Cycle 1 vs. Cycle 2. Austria's second round is 2014 (both rounds are within Cycle 1). Note this or exclude AUT from the "Cycle 1 vs Cycle 2" framing.

---

### m9. The Chile/PISA section needs specific numbers
**Agent: Referee**
The Chile section (lines 117–122) is the most novel and compelling part of the post, but it presents no numbers — no specific PISA gain figures, no specific PIAAC cohort scores. Citing the PISA improvement (e.g., reading score improvement from PISA 2000 to 2009) and the expected vs. actual 2023 PIAAC score for that cohort would make the puzzle concrete and quotable.

---

### m10. The subtitle overstates the evidence
**Agent: Referee**
*"a quiet crisis hiding inside the education statistics"* — given the unresolved aging confound and the mixed college premium results, "crisis" is too strong for what is currently a descriptive puzzle with several alternative explanations.

**Suggestion:** *"New PIAAC data raises uncomfortable questions about adult literacy trends"*

---

### m11. Opening illiteracy paragraph implies three-round comparison
**Agent: Consistency**
Line 28: *"all three of these surveys—2012/14, 2017, and 2023—took place after decades of rising school enrollment."* This implies the illiteracy numbers come from all three rounds, but the figure caption says "Cycle 1" only. Align or clarify.

---

## AGENT REPORTS (full detail)

### Agent 1 — Copy Editor
Key issues (summarized from full output):
- **HIGH:** 24-country count (12 per project data), Poland −37 (data: −36.5), Finland claimed as decline (data: gain), Israel "European", Level 1-to-Level 2 gap stated as 25 pts (actual: 51), cohort age arithmetic reversed
- **MEDIUM:** "10–33 points" range understates Poland, Nordic cluster description, "data confirm" before mixed results, figure captions sparse
- **LOW:** Style/placeholder items (authors TBD, repo TBD), no figure numbering

### Agent 2 — Consistency Checker
Key issues (data-verified):
- All specific illiteracy percentages verified ✓ (US 17.8→18%, DEU 17.2→17%, ESP 27.0→27%, CHL 52.8→53%, PER 70.1→70%, JPN 4.70→4.7%, MEX 50.0→50%, TUR 45.7→46%, ISR/ITA/ESP each ~27% ✓)
- All cohort decline magnitudes verified ✓ (within rounding), except: Poland −36.5 (text: −37), Singapore premium −13.4 (text: −14)
- "9 show declines, 12 show increases" for college premium ✓
- **SIGNIFICANT:** Back-of-envelope cohort aging direction is backwards (40-44 → 50-54, not 30-34)
- **SIGNIFICANT:** "10–33 points" range wrong; actual max is −36.5, and many countries are <10 pts
- Sweden 12.9% and Denmark 15.4% don't fit "10–12 percent" Nordic cluster description

### Agent 3 — Claims & Identification Auditor
Key issues:
1. "All 24 decline" likely wrong — Finland improvement, count unverified
2. Aging confound dismissed without quantification
3. "Same group of people" misleading for repeated cross-sections
4. Chile/PISA "not a data artifact" overstated — PISA/PIAAC methodological differences not addressed
5. Missing caveat: test comparability across rounds
6. "Schools are producing credentials faster than skills" stated as finding, not hypothesis
7. US three-round participation understated
8. BOE cohort age confusion — same issue as consistency agent
9. Migration/compositional change only noted in passing
10. US 18% inconsistency with email benchmark (28%) unexplained
11. Subtitle editorializes beyond evidence
12. "Data confirm this" contradicted by 12/21 countries showing increases
13. US illiteracy in Cycle 1 figure but US not in round 1
14. Illiteracy definition (≤225 = at or below Level 1) differs from OECD "below Level 1" — worth noting
15. "25 points separates Level 1 from Level 2" — arithmetic error (actual: 51 pts)

### Agent 4 — Math & Methodology Reviewer
Key issues:
1. BOE cohort aging direction backwards (MAJOR)
2. BOE "220" for younger cohort depends on unspecified age adjustment; actual data from `04_college_returns.r` BOE check not reported
3. 15% and 50% college-going rates may not match actual data (US bachelor's ~30–35% for 40–44 year-olds circa 2012)
4. "25 points separates Level 1 from Level 2" — wrong, gap is ~51 points (MAJOR)
5. "All 24 show declines" contradicted by MEMORY.md showing Finland at +5 (MAJOR)
6. Aging confound: 1–2 pts/year typical = 10–22 pts over 11 years; overlaps with many reported declines
7. **Potentially CRITICAL:** BRR Fay correction factor possibly missing — SEs may be understated 2× if k=0.5
8. Cycle 1 round structure misstated (2 rounds shown; actually 3)
9. USA data availability contradicts MEMORY.md — reconcile

### Agent 5 — Tables & Figures Auditor
Key issues:
1. **CRITICAL:** "NA" country label in college premium figures (Figures 6, 7) — data bug in script 04
2. Gender figures show Female panel only — no comparison (Figures 2, 5, 7)
3. Native speaker figure shows non-native only — no comparison (Figure 3)
4. Missing ranked bar chart of cohort score changes (most impactful missing figure)
5. All captions too sparse — missing data source, threshold definition, abbreviation key
6. Gold bar highlighting unexplained
7. No Quarto cross-reference syntax — no figure numbering
8. Cohort trends panels too dense for blog readability
9. No figure for Chile PISA-vs-PIAAC puzzle section
10. `illiteracy_rates_imgen.png` exists but unused

### Agent 6 — Referee Report

**Summary:** This blog uses three rounds of PIAAC data to argue adult literacy may have peaked around 2015, presenting findings on cross-country illiteracy rates, cohort score declines, and college premium trends. The technical implementation (PV+BRR variance estimation) is a genuine strength.

**Verdict: Needs revision.** The core question is important and the data work is technically competent, but the post has factual inconsistencies between text and analysis (country counts, US data availability, BOE numbers), an underplayed aging confound that could explain a substantial share of the headline finding, and a framing more alarmist than the mixed evidence warrants.

**Required revisions (pre-publication):**
1. Verify and correct the "all 24 countries decline" claim against actual output
2. Quantify the expected aging effect and show it against observed declines
3. Fix or remove the back-of-envelope calculation (cohort age error)
4. Resolve the US illiteracy rate sourcing issue
5. Remove "data confirm this" when 12 of 21 countries show premium increases
6. Add PIAAC round comparability caveat

**Pointed questions from referee:**
1. Where does the "24 countries" figure come from? MEMORY.md says 12.
2. Can you quantify the expected aging effect using the cross-sectional age gradient?
3. The email says US scored ~220 for 40–45 year-olds in 2013, but MEMORY.md says USA has no round 1 PUF. Where does the US cohort-tracking data come from?
4. Why does line 81 say "Finland shows the smallest decline" when the data shows Finland improved?
5. Have you checked whether Chile's PISA gains were concentrated in math/science rather than reading?
6. Have you checked sensitivity of the college premium to the EDCAT7 ≥ 6 cutoff, especially in Germany and Austria with strong vocational tracks?
7. What is the basis for "within-country sorting shapes measured literacy as much as language background does"?
8. The bibliography is empty — what prior PIAAC work should readers know about?
