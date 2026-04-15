# 07_apc_regression.r
# Age-Period-Cohort regression analysis of PIAAC literacy decline
#
# Estimation strategy: collapse to country x age_band x period cells using
# proper PV+BRR means (pv_group_mean), then run WLS on cells weighted by
# inverse variance (1/se^2). Equivalent to GLS with known cell variances.
#
# NOTE ON STANDARD ERRORS: the WLS SEs from lm() treat cell means as observed
# data. They capture model-fit uncertainty but do not propagate PV imputation
# variance into regression SEs. In practice the bias is small when V_B/V_total
# is small (verified in pv_group_mean output), but this should be noted in any
# write-up. Full Rubin's-rules regression would require running each spec 10x
# (once per PV) and combining coefficient distributions.
#
# Five specifications (plan: quality_reports/plans/2026-04-14_apc-regression.md):
#   Spec 1 — Age + Period:           period dummy captures 2023 shift conditional
#                                     on age composition; upper bound on period effect
#                                     if cohort quality has also declined
#   Spec 2 — Age + Cohort:           cohort gradient assuming zero period effect;
#                                     WARNING: 1950s and 2000s cohort estimates are
#                                     period-confounded (single round only)
#   Spec 3 — Cohort-tracked age profile: age gradient with cohort + country controls,
#                                     restricted to 4 cohorts in both rounds;
#                                     NOTE: with only 2 time points, within-cohort age
#                                     change is algebraically collinear with the period
#                                     change — aging and period are NOT separately
#                                     identified here. Spec 3 reports the compound
#                                     age+period gradient for tracked cohorts.
#   Spec 4 — PISA-instrumented:      uses pre-determined cohort PISA score to proxy
#                                     cohort quality; allows age, cohort quality and
#                                     period to enter simultaneously without collinearity
#                                     in this dataset; check PISA instrument strength
#   Spec 5 — Age x Period interaction: does the period effect differ by age group?
#
# Inputs:  02_output/piaac_clean.rds, 02_output/cohort_pisa_piaac.rds
# Outputs: 02_output/apc_*.rds, Figures/apc_*.pdf/png

set.seed(20260414)

library(tidyverse)
library(here)

source(here("01_scripts/00_helpers.r"))

out_dir <- here("02_output")
fig_dir <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Palette & theme --------------------------------------------------------

primary_blue   <- "#012169"
accent_gray    <- "#525252"
positive_green <- "#15803d"
negative_red   <- "#b91c1c"
primary_gold   <- "#f2a900"

SPEC_COLOURS <- c(
  "Spec 1: Age+Period"        = primary_blue,
  "Spec 2: Age+Cohort"        = primary_gold,
  "Spec 3: Cohort-tracked"    = positive_green,
  "Spec 4: PISA-instrumented" = negative_red
)

theme_custom <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", color = primary_blue),
      plot.subtitle = element_text(color = accent_gray),
      legend.position = "bottom"
    )
}

AGE_LABELS <- c("1" = "16-24", "2" = "25-34", "3" = "35-44",
                "4" = "45-54", "5" = "55-65")
AGE_MID    <- c("1" = 20, "2" = 30, "3" = 40, "4" = 50, "5" = 60)

# ---- Load & prepare ---------------------------------------------------------

cat("Loading data...\n")
piaac <- readRDS(file.path(out_dir, "piaac_clean.rds")) |>
  filter(!is.na(AGEG10LFS), !is.na(SPFWT0), SPFWT0 > 0, !is.na(PVLIT1)) |>
  mutate(
    age_band    = as.character(AGEG10LFS),
    age_mid     = AGE_MID[age_band],
    period_2023 = as.integer(survey_year == 2023),
    # Birth decade approximated from age-band midpoint + survey year.
    # Midpoint approximation is consistent across cy1 years (2012-2017) given
    # 10-year bands — verified: all (age_band, cy1_year) combinations land in
    # the same birth decade regardless of whether cy1 = 2012, 2014, or 2017.
    birth_yr  = survey_year - age_mid,
    birth_dec = floor(birth_yr / 10) * 10
  )

# Countries with BOTH a pre-2023 round and a 2023 round
countries_both <- piaac |>
  summarise(
    has_pre  = any(survey_year != 2023),
    has_2023 = any(survey_year == 2023),
    .by = country
  ) |>
  filter(has_pre, has_2023) |>
  pull(country)

cat(sprintf("Countries with pre-2023 + 2023 rounds: %d\n", length(countries_both)))

piaac_both <- filter(piaac, country %in% countries_both)

# ---- Step 1: Collapse to cell means (PV+BRR) --------------------------------

cat("Computing cell means (PV+BRR)...\n")

cells <- pv_group_mean(
  piaac_both, LIT_PVS,
  c("country", "age_band", "period_2023", "survey_year", "birth_dec"),
  "SPFWT0", BRR_WTS
) |>
  rename(lit_mean = mean, lit_se = se) |>
  mutate(
    age_band      = factor(age_band, levels = as.character(1:5), labels = AGE_LABELS),
    age_mid       = c("16-24"=20,"25-34"=30,"35-44"=40,"45-54"=50,"55-65"=60)[as.character(age_band)],
    wt_iv         = 1 / lit_se^2,   # correct inverse-variance weight for WLS
    birth_dec_fct = factor(birth_dec),
    # Flag for V_B share diagnostic (imputation variance fraction)
    pv_share      = V_B / (V_W + (1 + 1/10) * V_B)
  )

saveRDS(cells, file.path(out_dir, "apc_cells.rds"))
cat(sprintf("Cell dataset: %d rows\n", nrow(cells)))

# PV imputation variance diagnostic — if median pv_share is low, treating cell
# means as observed data in WLS understates SE by only a small amount
cat(sprintf("Median V_B share of total cell variance: %.1f%%\n",
            100 * median(cells$pv_share, na.rm = TRUE)))

# Cohort-round coverage table — essential for interpreting Spec 2 and Spec 3
cat("\nCohort x period coverage (n cells):\n")
cells |>
  count(birth_dec, period_2023) |>
  pivot_wider(names_from = period_2023, values_from = n,
              names_prefix = "period_") |>
  arrange(birth_dec) |>
  print()

# ---- Helper: run WLS and extract clean coefficient table --------------------

run_wls <- function(formula, data, weight_col = "wt_iv") {
  fit   <- do.call(lm, list(formula = formula, data = data,
                            weights = data[[weight_col]]))
  coefs <- summary(fit)$coefficients
  tibble(
    term     = rownames(coefs),
    estimate = coefs[, "Estimate"],
    se       = coefs[, "Std. Error"],
    t_stat   = coefs[, "t value"],
    p_value  = coefs[, "Pr(>|t|)"],
    ci_lo    = estimate - 1.96 * se,
    ci_hi    = estimate + 1.96 * se
  )
}

# ---- Spec 1: Age + Period (country FE) --------------------------------------
# Identifies: age profile + period shift conditional on stable cohort composition
# Period coef is an upper bound on the true period effect if cohort quality has
# declined (in that case some of the -8 pt drop is cohort, not period)

cat("\n--- Spec 1: Age + Period ---\n")

spec1 <- run_wls(
  lit_mean ~ age_band + period_2023 + country,
  cells,
  weight_col = "wt_iv"
)

period_est_s1 <- spec1 |> filter(term == "period_2023")
cat(sprintf("Period (2023) effect: %.1f pts (SE %.1f, p=%.3f)\n",
            period_est_s1$estimate, period_est_s1$se, period_est_s1$p_value))
cat("  [Interpretation: 2023 scores lower by this amount holding age composition constant]\n")
cat("  [Upper bound on true period effect if cohort quality has also declined]\n")

saveRDS(spec1, file.path(out_dir, "apc_spec1.rds"))

# ---- Spec 2: Age + Cohort (country FE) --------------------------------------
# Identifies: cohort quality gradient assuming zero period effect
# WARNING: cohorts observed in only ONE round have period-confounded estimates:
#   - 1950s cohort: only in cy1 (too old for 2023 sample) — effect conflates cohort + cy1
#   - 2000s cohort: only in cy2 (too young for cy1 sample) — effect conflates cohort + 2023
# Restrict cohort-quality narrative to 1960s–1990s (observed in both rounds)

cat("\n--- Spec 2: Age + Cohort ---\n")

cells_s2 <- mutate(cells, birth_dec_fct = relevel(factor(birth_dec), ref = "1970"))

spec2 <- run_wls(
  lit_mean ~ age_band + birth_dec_fct + country,
  cells_s2,
  weight_col = "wt_iv"
)

cohort_coefs <- spec2 |>
  filter(str_starts(term, "birth_dec_fct")) |>
  mutate(
    birth_dec      = as.integer(str_remove(term, "birth_dec_fct")),
    period_confounded = birth_dec %in% c(1950, 2000)
  )

cat("Cohort coefficients (ref = 1970s):\n")
print(select(cohort_coefs, birth_dec, estimate, se, p_value, period_confounded))
cat("  [Rows with period_confounded=TRUE are observed in only one round;\n")
cat("   their estimates absorb the period effect and cannot be interpreted as pure cohort quality]\n")

saveRDS(spec2, file.path(out_dir, "apc_spec2.rds"))

# ---- Spec 3: Cohort-tracked age profile (cohort FE + country FE) -----------
# Restricted to 4 cohorts observed in BOTH rounds:
#   1960s: 45-54 in cy1 -> 55-65 in cy2
#   1970s: 35-44 in cy1 -> 45-54 in cy2
#   1980s: 25-34 in cy1 -> 35-44 in cy2
#   1990s: 16-24 in cy1 -> 25-34 in cy2
#
# IDENTIFICATION CAVEAT: With exactly 2 time points, the change in age band
# within a cohort is algebraically determined by the period change (every cohort
# moves exactly one age band from cy1 to cy2). Age dummies and cohort FEs
# therefore span the same space as age dummies + a period dummy. The age
# coefficients below capture aging + period jointly — they are NOT a clean
# aging estimate. The coefficient difference between Spec 1 and Spec 3 age
# gradients reflects the interaction between cohort composition and period, not
# a separate aging channel. Use Spec 3 as descriptive, not causal.

cat("\n--- Spec 3: Cohort-tracked age profile ---\n")
cat("  [CAVEAT: age and period are collinear within cohorts with 2 time points]\n")
cat("  [Age coefficients reflect aging + period compound, not pure aging]\n")

within_cohorts <- c(1960, 1970, 1980, 1990)

cells_s3 <- cells |>
  filter(birth_dec %in% within_cohorts) |>
  mutate(birth_dec_fct = relevel(factor(birth_dec), ref = "1970"))

# Verify cell counts per cohort x period
cat("\nSpec 3 cell counts (cohort x period):\n")
cells_s3 |> count(birth_dec, period_2023) |>
  pivot_wider(names_from = period_2023, values_from = n,
              names_prefix = "period_") |>
  print()

spec3 <- run_wls(
  lit_mean ~ age_band + birth_dec_fct + country,
  cells_s3,
  weight_col = "wt_iv"
)

age_coefs_s3 <- spec3 |>
  filter(str_starts(term, "age_band")) |>
  mutate(age_band = str_remove(term, "age_band"))

cat("\nAge gradient from Spec 3 (compound aging+period, ref = 16-24):\n")
print(select(age_coefs_s3, age_band, estimate, se, p_value))

# Sanity check: Spec 3 gradient should be steeper than Spec 1 if period=-8 pts
# is absorbed into the age coefficients
age_coefs_s1 <- spec1 |>
  filter(str_starts(term, "age_band")) |>
  mutate(age_band = str_remove(term, "age_band"))
cat("\nSpec 1 vs Spec 3 age gradient comparison:\n")
left_join(
  select(age_coefs_s1, age_band, s1_est = estimate),
  select(age_coefs_s3, age_band, s3_est = estimate),
  by = "age_band"
) |>
  mutate(diff = s3_est - s1_est) |>
  print()
cat("  [If diff ~ period effect (-8 pts), confirms aging+period are confounded in Spec 3]\n")

saveRDS(spec3, file.path(out_dir, "apc_spec3.rds"))

# ---- Spec 4: PISA-instrumented ----------------------------------------------
# Uses pre-determined cohort PISA score (measured at age 15, before 2023) to
# proxy cohort quality. With country FE and age, PISA score provides additional
# within-country across-cohort variation that breaks practical collinearity.
#
# INSTRUMENT STRENGTH CHECK: if PISA persistence coefficient is small / not
# significant after partialling out country FE and age, the instrument is weak
# and Spec 4 offers no credibility advantage over Spec 1.
# Rule of thumb: t-stat on pisa_mean > 3.2 (F > 10) for a strong instrument.

cat("\n--- Spec 4: PISA-instrumented ---\n")

pisa_piaac <- readRDS(file.path(out_dir, "cohort_pisa_piaac.rds")) |>
  filter(subject == "Literacy") |>
  mutate(
    period_2023 = as.integer(piaac_year == 2023),
    wt_iv       = 1 / piaac_se^2   # correct inverse-variance weight
  ) |>
  filter(!is.na(pisa_mean), !is.na(piaac_mean), n_piaac >= 50)

cat(sprintf("Spec 4 cells: %d (countries: %d)\n",
            nrow(pisa_piaac), n_distinct(pisa_piaac$country)))

spec4 <- run_wls(
  piaac_mean ~ pisa_mean + age_at_piaac + period_2023 + country,
  pisa_piaac,
  weight_col = "wt_iv"
)

period_est_s4 <- spec4 |> filter(term == "period_2023")
pisa_coef     <- spec4 |> filter(term == "pisa_mean")

cat(sprintf("PISA persistence: %.3f (SE %.3f, t=%.2f)\n",
            pisa_coef$estimate, pisa_coef$se, pisa_coef$t_stat))

# Instrument strength check
if (abs(pisa_coef$t_stat) < 3.2) {
  cat("  [WARNING: PISA instrument is WEAK (|t| < 3.2 after country FE + age)]\n")
  cat("  [Country FE likely absorbs most cross-country PISA-PIAAC correlation]\n")
  cat("  [Spec 4 period estimate offers limited credibility advantage over Spec 1]\n")
} else {
  cat("  [Instrument strength OK: |t| >= 3.2]\n")
}

cat(sprintf("Period (2023) effect: %.1f pts (SE %.1f, p=%.3f)\n",
            period_est_s4$estimate, period_est_s4$se, period_est_s4$p_value))

saveRDS(spec4, file.path(out_dir, "apc_spec4.rds"))

# ---- Spec 5: Age x Period interaction ---------------------------------------
# Does the 2023 period effect differ by age group?
# Baseline period effect = coefficient on period_2023 alone (for 16-24 group)
# Interactions = differential effect for other age groups relative to 16-24

cat("\n--- Spec 5: Age x Period interaction ---\n")

spec5 <- run_wls(
  lit_mean ~ age_band * period_2023 + country,
  cells,
  weight_col = "wt_iv"
)

# Main period effect (for 16-24 reference group)
period_main_s5 <- spec5 |> filter(term == "period_2023")

# Interaction terms = differential period effect vs 16-24
interaction_coefs <- spec5 |>
  filter(str_detect(term, "age_band.*period_2023|period_2023.*age_band")) |>
  mutate(age_band = str_extract(term, "[0-9]+-[0-9]+"))

# Absolute period effect for each age group = main + interaction
period_by_age <- bind_rows(
  tibble(age_band = "16-24",
         period_effect = period_main_s5$estimate,
         se = period_main_s5$se,
         interaction = 0),
  interaction_coefs |>
    mutate(period_effect = estimate + period_main_s5$estimate,
           interaction   = estimate) |>
    select(age_band, period_effect, se, interaction)
) |>
  mutate(
    ci_lo = period_effect - 1.96 * se,
    ci_hi = period_effect + 1.96 * se,
    age_band = factor(age_band, levels = AGE_LABELS)
  )

cat("Period effect by age group (absolute, ref age group = 16-24 for interaction):\n")
print(select(period_by_age, age_band, period_effect, interaction, se))

saveRDS(spec5, file.path(out_dir, "apc_spec5.rds"))

# ---- Summary: period estimates across specs ---------------------------------

cat("\n=== PERIOD EFFECT SUMMARY ===\n")

baseline_score <- cells |>
  filter(period_2023 == 0) |>
  summarise(mean_lit = weighted.mean(lit_mean, wt_iv)) |>
  pull(mean_lit)

period_summary <- bind_rows(
  period_est_s1 |> mutate(spec = "Spec 1: Age+Period"),
  period_est_s4 |> mutate(spec = "Spec 4: PISA-instrumented")
) |>
  select(spec, estimate, se, ci_lo, ci_hi, p_value)

cat(sprintf("Baseline mean literacy (cy1): %.1f pts\n\n", baseline_score))
print(period_summary)

if (abs(pisa_coef$t_stat) < 3.2) {
  cat("\n[NOTE: Spec 4 estimate not more credible than Spec 1 due to weak PISA instrument]\n")
}

saveRDS(period_summary, file.path(out_dir, "apc_period_summary.rds"))

# ---- Figure 1: Age gradient (Spec 1 vs Spec 3) ------------------------------

age_plot_data <- bind_rows(
  spec1 |>
    filter(str_starts(term, "age_band")) |>
    mutate(age_band = str_remove(term, "age_band"),
           spec     = "Spec 1: Age+Period"),
  spec3 |>
    filter(str_starts(term, "age_band")) |>
    mutate(age_band = str_remove(term, "age_band"),
           spec     = "Spec 3: Cohort-tracked")
) |>
  bind_rows(
    tibble(age_band = "16-24", estimate = 0, ci_lo = 0, ci_hi = 0,
           spec = c("Spec 1: Age+Period", "Spec 3: Cohort-tracked"))
  ) |>
  mutate(age_band = factor(age_band, levels = AGE_LABELS))

p_age <- ggplot(age_plot_data, aes(x = age_band, y = estimate,
                                    colour = spec, group = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = accent_gray) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = spec),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = SPEC_COLOURS, name = NULL) +
  scale_fill_manual(values   = SPEC_COLOURS, name = NULL) +
  labs(
    title    = "Age gradient in literacy (ref: 16-24 age group)",
    subtitle = "Spec 3 steeper than Spec 1 because it compounds aging + period effect",
    x        = "Age group",
    y        = "Points relative to 16-24 group"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "apc_age_gradient.pdf"), p_age, width = 10, height = 6)
ggsave(file.path(fig_dir, "apc_age_gradient.png"), p_age, width = 10, height = 6, dpi = 150)

# ---- Figure 2: Cohort quality gradient (Spec 2) ----------------------------

cohort_plot_data <- cohort_coefs |>
  bind_rows(tibble(birth_dec = 1970L, estimate = 0, ci_lo = 0, ci_hi = 0,
                   period_confounded = FALSE)) |>
  arrange(birth_dec) |>
  mutate(birth_dec_label = paste0(birth_dec, "s"))

p_cohort <- ggplot(cohort_plot_data,
                   aes(x = factor(birth_dec_label,
                                  levels = paste0(sort(unique(birth_dec)), "s")),
                       y = estimate,
                       colour = period_confounded,
                       shape  = period_confounded)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = accent_gray) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), size = 0.6) +
  geom_line(aes(group = 1), colour = accent_gray, linewidth = 0.6, linetype = "dashed") +
  scale_colour_manual(values = c("FALSE" = primary_blue, "TRUE" = negative_red),
                      labels = c("FALSE" = "Both rounds", "TRUE" = "Period-confounded (1 round)"),
                      name = NULL) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
                     labels = c("FALSE" = "Both rounds", "TRUE" = "Period-confounded (1 round)"),
                     name = NULL) +
  labs(
    title    = "Cohort quality gradient (Spec 2: Age+Cohort, no period effect assumed)",
    subtitle = "Red X = cohort observed in only one round; estimates are period-confounded",
    x        = "Birth decade",
    y        = "Literacy points relative to 1970s cohort"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "apc_cohort_gradient.pdf"), p_cohort, width = 10, height = 6)
ggsave(file.path(fig_dir, "apc_cohort_gradient.png"), p_cohort, width = 10, height = 6, dpi = 150)

# ---- Figure 3: Period effect comparison (Specs 1 and 4) --------------------

p_period <- ggplot(period_summary,
                   aes(x = spec, y = estimate, colour = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = accent_gray) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), size = 0.8, linewidth = 1.2) +
  scale_colour_manual(values = SPEC_COLOURS, guide = "none") +
  labs(
    title    = "Estimated 2023 period effect on literacy",
    subtitle = "Negative = 2023 worse than cy1 | Spec 4 credibility depends on instrument strength",
    x        = NULL,
    y        = "Points (PIAAC literacy scale)"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "apc_period_effect.pdf"), p_period, width = 8, height = 5)
ggsave(file.path(fig_dir, "apc_period_effect.png"), p_period, width = 8, height = 5, dpi = 150)

# ---- Figure 4: Absolute period effect by age group (Spec 5) ----------------

p_interaction <- ggplot(period_by_age, aes(x = age_band, y = period_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = accent_gray) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), colour = primary_blue, size = 0.7) +
  geom_line(aes(group = 1), colour = primary_blue, linewidth = 0.8) +
  labs(
    title    = "2023 period effect by age group (Spec 5)",
    subtitle = "Absolute decline in 2023 relative to cy1, by age group | country FE",
    x        = "Age group",
    y        = "Period effect (pts)"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "apc_period_by_age.pdf"), p_interaction, width = 10, height = 6)
ggsave(file.path(fig_dir, "apc_period_by_age.png"), p_interaction, width = 10, height = 6, dpi = 150)

# ---- Figure 5: cy1 vs cy2 raw age profiles (descriptive) -------------------

age_period_means <- cells |>
  group_by(age_band, period_2023) |>
  summarise(
    mean_lit = weighted.mean(lit_mean, wt_iv),
    # SE of weighted mean: propagates cell-level uncertainty correctly
    se_wmean = sqrt(sum(wt_iv^2 * lit_se^2) / sum(wt_iv)^2),
    .groups  = "drop"
  ) |>
  mutate(
    period_label = if_else(period_2023 == 1, "2023 (cy2)", "Pre-2023 (cy1)"),
    ci_lo = mean_lit - 1.96 * se_wmean,
    ci_hi = mean_lit + 1.96 * se_wmean
  )

p_raw <- ggplot(age_period_means,
                aes(x = age_band, y = mean_lit,
                    colour = period_label, group = period_label)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = period_label),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Pre-2023 (cy1)" = primary_blue,
                                 "2023 (cy2)"     = negative_red), name = NULL) +
  scale_fill_manual(values   = c("Pre-2023 (cy1)" = primary_blue,
                                 "2023 (cy2)"     = negative_red), name = NULL) +
  labs(
    title    = "Raw literacy scores by age group: cy1 vs 2023",
    subtitle = "Weighted means across countries | 95% CI propagates cell-level SEs",
    x        = "Age group",
    y        = "Mean literacy score"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "apc_age_period.pdf"), p_raw, width = 10, height = 6)
ggsave(file.path(fig_dir, "apc_age_period.png"), p_raw, width = 10, height = 6, dpi = 150)

cat("\n=== DONE ===\n")
cat("Outputs saved to:\n")
cat("  02_output/apc_cells.rds\n")
cat("  02_output/apc_spec1.rds through apc_spec5.rds\n")
cat("  02_output/apc_period_summary.rds\n")
cat("  Figures/apc_age_gradient, apc_cohort_gradient, apc_period_effect\n")
cat("  Figures/apc_period_by_age, apc_age_period\n")
