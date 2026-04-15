# 06_cohort_pisa_piaac.r
# Lagged cohort linkage: follow PISA cohorts (age 15) into PIAAC (adults).
#
# Linkage: PISA students born in year B are ~15 at PISA time (B+15).
# The same birth cohort (±2 years) appears in PIAAC at adult ages.
# Country-level means are matched across surveys to measure skill persistence.
#
# Key design:
#   - Birth window: PISA_year - 15 ± 2 (5-year band)
#   - Age validity: drop cohort-PIAAC pairs where age < 16
#   - Subject mapping: PISA Reading <-> PIAAC Literacy, Math <-> Numeracy
#   - Variance: pv_group_mean() from helpers (standard BRR for PIAAC)
#   - PISA scores already in pisa_all_countries.rds (Fay BRR applied in 05)

set.seed(20260412)

library(tidyverse)
library(cowplot)
library(here)

source(here("01_scripts/00_helpers.r"))

out_dir <- here("02_output")
fig_dir <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

primary_blue   <- "#012169"
primary_gold   <- "#f2a900"
accent_gray    <- "#525252"
negative_red   <- "#b91c1c"
positive_green <- "#15803d"

COHORT_PALETTE <- c(
  "PISA 2000" = "#6b7280",  # slate gray — earliest cohort
  "PISA 2003" = "#0369a1",  # steel blue
  "PISA 2006" = "#012169",  # primary blue
  "PISA 2009" = "#f2a900",  # gold
  "PISA 2012" = "#15803d",  # green
  "PISA 2015" = "#b91c1c",  # red
  "PISA 2018" = "#7c3aed"   # purple
)

theme_custom <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", color = primary_blue),
      plot.subtitle = element_text(color = accent_gray),
      legend.position = "bottom"
    )
}

save_plot_dual <- function(stem, plot, width, height, dpi = 300) {
  ggsave(file.path(fig_dir, paste0(stem, ".pdf")), plot,
         width = width, height = height)
  ggsave(file.path(fig_dir, paste0(stem, ".png")), plot,
         width = width, height = height, dpi = dpi)
}

# =============================================================================
# 1. Cohort linkage definition
# =============================================================================

# Birth window = pisa_year - 17 to pisa_year - 13 (±2 years around the 15-yr-old target).
# PISA 2000 cohort (born 1983-1987): age 25-29 in PIAAC 2012, age 36-40 in PIAAC 2023.
# PISA 2003 cohort (born 1986-1990): age 22-26 in PIAAC 2012, age 33-37 in PIAAC 2023.
# These extend the within-country transmission analysis back to the early-gains period.
cohort_links <- tribble(
  ~pisa_year, ~birth_lo, ~birth_hi, ~cohort_label,
  2000L, 1983L, 1987L, "PISA 2000",
  2003L, 1986L, 1990L, "PISA 2003",
  2006L, 1989L, 1993L, "PISA 2006",
  2009L, 1992L, 1996L, "PISA 2009",
  2012L, 1995L, 1999L, "PISA 2012",
  2015L, 1998L, 2002L, "PISA 2015",
  2018L, 2001L, 2005L, "PISA 2018"
)

piaac_years_all <- c(2012L, 2014L, 2017L, 2023L)

# =============================================================================
# 2. PIAAC: cohort means by country for all (cohort, piaac_year) pairs
# =============================================================================

message("Loading PIAAC data...")
piaac_raw <- readRDS(file.path(out_dir, "piaac_clean.rds")) |>
  filter(if_all(all_of(c(LIT_PVS, NUM_PVS)), \(x) !is.na(x)))

message("Computing PIAAC cohort means (all countries, all cohorts)...")

# Drop any cohort × PIAAC-year pair where the cohort's average age at PIAAC
# is <= 15 (the PISA testing age). Comparing a 14-year-old PIAAC observation
# to a 15-year-old PISA result is not apples-to-apples.
cohort_grid <- crossing(cohort_links, piaac_year = piaac_years_all) |>
  mutate(age_at_piaac = piaac_year - (birth_lo + birth_hi) / 2) |>
  filter(age_at_piaac > 15)

piaac_cohort_means <- pmap_dfr(cohort_grid, function(pisa_year, birth_lo, birth_hi,
                                                       cohort_label, piaac_year,
                                                       age_at_piaac) {
  df_sub <- piaac_raw |>
    filter(
      survey_year == piaac_year,
      birth_cohort >= birth_lo,
      birth_cohort <= birth_hi,
      piaac_year - birth_cohort >= 16L   # individual-level: drop anyone under 16 at PIAAC
    )

  if (nrow(df_sub) == 0) return(tibble())

  # Drop countries with < 30 observations in this cohort window
  df_sub <- df_sub |>
    group_by(country) |>
    filter(n() >= 30) |>
    ungroup()

  if (nrow(df_sub) == 0) return(tibble())

  bind_rows(
    pv_group_mean(df_sub, LIT_PVS, "country", "SPFWT0", BRR_WTS) |>
      mutate(subject = "Literacy"),
    pv_group_mean(df_sub, NUM_PVS, "country", "SPFWT0", BRR_WTS) |>
      mutate(subject = "Numeracy")
  ) |>
    mutate(
      cohort_label = cohort_label,
      pisa_year    = pisa_year,
      piaac_year   = piaac_year,
      age_at_piaac = piaac_year - (birth_lo + birth_hi) / 2
    )
})

message("  Done. Rows: ", nrow(piaac_cohort_means),
        "  Countries: ", n_distinct(piaac_cohort_means$country))

# =============================================================================
# 3. Join with PISA country means
# =============================================================================

message("Joining with PISA means...")

pisa_all <- readRDS(file.path(out_dir, "pisa_all_countries.rds")) |>
  rename(country = cnt, pisa_year = year) |>
  mutate(subject = recode(subject,
                          "Reading"     = "Literacy",
                          "Mathematics" = "Numeracy")) |>
  rename(pisa_mean = mean, pisa_se = se,
         pisa_V_W = V_W, pisa_V_B = V_B, n_pisa = n)

cohort_pisa_piaac <- pisa_all |>
  inner_join(cohort_links |> select(pisa_year, cohort_label), by = "pisa_year") |>
  inner_join(
    piaac_cohort_means |>
      select(country, cohort_label, piaac_year, subject,
             piaac_mean = mean, piaac_se = se,
             piaac_V_W = V_W, piaac_V_B = V_B,
             n_piaac = n, age_at_piaac),
    by = c("country", "cohort_label", "subject")
  ) |>
  filter(!is.na(pisa_mean), !is.na(piaac_mean))

message("Matched cohort dataset: ", nrow(cohort_pisa_piaac), " rows  ",
        n_distinct(cohort_pisa_piaac$country), " countries")

saveRDS(cohort_pisa_piaac, file.path(out_dir, "cohort_pisa_piaac.rds"))

chile_cohort_mapping <- cohort_pisa_piaac |>
  filter(country == "CHL") |>
  left_join(cohort_links, by = c("pisa_year", "cohort_label")) |>
  mutate(
    birth_window = sprintf("%d-%d", birth_lo, birth_hi),
    piaac_age_band = case_when(
      age_at_piaac >= 16 & age_at_piaac < 25 ~ "16-24",
      age_at_piaac >= 25 & age_at_piaac < 35 ~ "25-34",
      TRUE ~ "other"
    )
  ) |>
  select(
    country, subject, cohort_label, pisa_year, birth_window,
    piaac_year, piaac_age_band, age_at_piaac, pisa_mean, piaac_mean,
    pisa_se, piaac_se, n_pisa, n_piaac
  ) |>
  arrange(subject, pisa_year, piaac_year)

saveRDS(chile_cohort_mapping, file.path(out_dir, "chile_cohort_mapping.rds"))
readr::write_csv(chile_cohort_mapping,
                 file.path(out_dir, "chile_cohort_mapping.csv"))

age_support_2023 <- piaac_raw |>
  filter(survey_year == 2023) |>
  group_by(country) |>
  summarise(
    n_ages = n_distinct(AGE_R),
    age_support = if_else(n_ages <= 5, "band-midpoints only", "exact ages"),
    .groups = "drop"
  ) |>
  arrange(age_support, country)

saveRDS(age_support_2023, file.path(out_dir, "piaac_2023_age_support.rds"))
readr::write_csv(age_support_2023,
                 file.path(out_dir, "piaac_2023_age_support.csv"))

# Quick summary
cohort_pisa_piaac |>
  count(cohort_label, piaac_year, subject) |>
  pivot_wider(names_from = subject, values_from = n) |>
  print(n = 40)

# =============================================================================
# 3b. Design ladder: naive cross-section vs age restriction vs cohort lag
# =============================================================================

message("Estimating PIAAC design ladder summaries for 2023...")

estimate_piaac_slice <- function(df, design_label) {
  df |>
    group_by(country) |>
    filter(n() >= 30) |>
    ungroup() |>
    {\(d) bind_rows(
      pv_group_mean(d, LIT_PVS, "country", "SPFWT0", BRR_WTS) |>
        mutate(subject = "Literacy"),
      pv_group_mean(d, NUM_PVS, "country", "SPFWT0", BRR_WTS) |>
        mutate(subject = "Numeracy")
    )}() |>
    transmute(country, subject, piaac_mean = mean, piaac_se = se,
              n_piaac = n, design = design_label)
}

piaac_2023_all <- estimate_piaac_slice(
  piaac_raw |> filter(survey_year == 2023),
  "Naive cross-section"
)

piaac_2023_1624 <- estimate_piaac_slice(
  piaac_raw |> filter(survey_year == 2023, AGEG10LFS == 1),
  "Age-restricted (16-24)"
)

piaac_2023_20 <- estimate_piaac_slice(
  piaac_raw |> filter(survey_year == 2023, AGE_R == 20),
  "Short-run (~20)"
)

piaac_2023_2534 <- estimate_piaac_slice(
  piaac_raw |> filter(survey_year == 2023, AGEG10LFS == 2),
  "Cohort-lagged (25-34)"
)

design_compare <- bind_rows(
  pisa_all |>
    filter(pisa_year == 2018) |>
    select(country, subject, pisa_year, pisa_mean, pisa_se, n_pisa) |>
    inner_join(piaac_2023_all, by = c("country", "subject")),
  pisa_all |>
    filter(pisa_year == 2018) |>
    select(country, subject, pisa_year, pisa_mean, pisa_se, n_pisa) |>
    inner_join(piaac_2023_1624, by = c("country", "subject")),
  pisa_all |>
    filter(pisa_year == 2018) |>
    select(country, subject, pisa_year, pisa_mean, pisa_se, n_pisa) |>
    inner_join(piaac_2023_20, by = c("country", "subject"))
) |>
  mutate(
    design = factor(
      design,
      levels = c(
        "Naive cross-section",
        "Age-restricted (16-24)",
        "Short-run (~20)"
      )
    ),
    country_group = if_else(country == "CHL", "Chile", "Other countries")
  )

design_stats <- design_compare |>
  group_by(design, subject) |>
  summarise(
    pearson_r = cor(pisa_mean, piaac_mean, use = "complete.obs"),
    n_countries = n(),
    .groups = "drop"
  ) |>
  mutate(label = sprintf("r = %.2f\nn = %d", pearson_r, n_countries))

saveRDS(design_compare, file.path(out_dir, "pisa_piaac_design_comparison.rds"))
readr::write_csv(design_stats, file.path(out_dir, "pisa_piaac_design_stats.csv"))

# =============================================================================
# 3c. Main specification and exact-age sensitivity
# =============================================================================

exact_age_countries <- age_support_2023 |>
  filter(age_support == "exact ages") |>
  pull(country)

main_spec <- cohort_pisa_piaac |>
  filter(pisa_year == 2018, piaac_year == 2023, age_at_piaac == 20)

main_spec_stats <- main_spec |>
  group_by(subject) |>
  summarise(
    pearson_r   = cor(pisa_mean, piaac_mean, use = "complete.obs"),
    spearman_r  = cor(pisa_mean, piaac_mean, method = "spearman", use = "complete.obs"),
    slope       = unname(coef(lm(piaac_mean ~ pisa_mean))[2]),
    n_countries = n(),
    .groups = "drop"
  )

saveRDS(main_spec, file.path(out_dir, "pisa_piaac_main_spec.rds"))
readr::write_csv(main_spec, file.path(out_dir, "pisa_piaac_main_spec.csv"))
readr::write_csv(main_spec_stats, file.path(out_dir, "pisa_piaac_main_spec_stats.csv"))

persistence_spec <- cohort_pisa_piaac |>
  filter(pisa_year == 2012, piaac_year == 2023, age_at_piaac >= 25, age_at_piaac < 35)

persistence_spec_stats <- persistence_spec |>
  group_by(subject) |>
  summarise(
    pearson_r   = cor(pisa_mean, piaac_mean, use = "complete.obs"),
    spearman_r  = cor(pisa_mean, piaac_mean, method = "spearman", use = "complete.obs"),
    slope       = unname(coef(lm(piaac_mean ~ pisa_mean))[2]),
    n_countries = n(),
    .groups = "drop"
  )

saveRDS(persistence_spec, file.path(out_dir, "pisa_piaac_persistence_spec.rds"))
readr::write_csv(persistence_spec, file.path(out_dir, "pisa_piaac_persistence_spec.csv"))
readr::write_csv(persistence_spec_stats,
                 file.path(out_dir, "pisa_piaac_persistence_spec_stats.csv"))

exact_age_20 <- estimate_piaac_slice(
  piaac_raw |>
    filter(survey_year == 2023, country %in% exact_age_countries, AGE_R == 20),
  "Exact-age 20"
)

exact_age_sensitivity <- (
  pisa_all |>
    filter(pisa_year == 2018) |>
    select(country, subject, pisa_year, pisa_mean, pisa_se, n_pisa) |>
    inner_join(exact_age_20, by = c("country", "subject"))
) |>
  mutate(
    design = factor(design, levels = c("Exact-age 20")),
    country_group = if_else(country == "CHL", "Chile", "Other countries")
  )

exact_age_stats <- exact_age_sensitivity |>
  group_by(design, subject) |>
  summarise(
    pearson_r  = cor(pisa_mean, piaac_mean, use = "complete.obs"),
    spearman_r = cor(pisa_mean, piaac_mean, method = "spearman", use = "complete.obs"),
    n_countries = n(),
    label = sprintf("r = %.2f\nn = %d", pearson_r, n_countries),
    .groups = "drop"
  )

saveRDS(exact_age_sensitivity,
        file.path(out_dir, "pisa_piaac_exact_age_sensitivity.rds"))
readr::write_csv(exact_age_sensitivity,
                 file.path(out_dir, "pisa_piaac_exact_age_sensitivity.csv"))
readr::write_csv(exact_age_stats,
                 file.path(out_dir, "pisa_piaac_exact_age_sensitivity_stats.csv"))

# =============================================================================
# 4. Figures
# =============================================================================

# --- Fig 1: Chile cohort-first view ------------------------------------------

message("Figure 1: Chile cohort trajectories...")

chile_pisa_plot <- cohort_pisa_piaac |>
  filter(country == "CHL", pisa_year %in% c(2003, 2018), age_at_piaac == 20) |>
  distinct(subject, cohort_label, pisa_year, pisa_mean, pisa_se) |>
  transmute(
    subject,
    cohort_label,
    survey = "PISA (age 15)",
    survey_year = pisa_year,
    score = pisa_mean,
    se = pisa_se
  )

chile_piaac_plot <- cohort_pisa_piaac |>
  filter(country == "CHL", pisa_year %in% c(2003, 2018), age_at_piaac == 20) |>
  distinct(subject, cohort_label, piaac_year, piaac_mean, piaac_se) |>
  transmute(
    subject,
    cohort_label,
    survey = "PIAAC (age ~20)",
    survey_year = piaac_year,
    score = piaac_mean,
    se = piaac_se
  )

chile_raw <- bind_rows(chile_pisa_plot, chile_piaac_plot) |>
  mutate(
    survey = factor(survey, levels = c("PISA (age 15)", "PIAAC (age ~20)")),
    cohort_label = factor(cohort_label, levels = c("PISA 2003", "PISA 2018"))
  )

p_chile <- chile_raw |>
  ggplot(aes(cohort_label, score, color = subject, group = subject)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_point(size = 2.8) +
  geom_errorbar(aes(ymin = score - 1.96 * se, ymax = score + 1.96 * se),
                width = 0.12, alpha = 0.55) +
  scale_color_manual(values = c(Literacy = primary_blue, Numeracy = primary_gold),
                     name = NULL) +
  facet_grid(survey ~ subject, scales = "free_y") +
  labs(
    title    = "Chile: higher PISA cohorts do not cleanly translate into higher PIAAC cohorts 5 years later",
    subtitle = "Top row shows PISA at age 15; bottom row shows the same cohorts later in PIAAC at age ~20",
    x = "Linked PISA cohort year",
    y = "Raw score"
  ) +
  theme_custom()

save_plot_dual("cohort_pisa_piaac_chile", p_chile, width = 10, height = 5)

# --- Fig 2: Short-run transmission scatter -----------------------------------

message("Figure 2: Cross-country persistence scatter...")

scatter_data <- main_spec |>
  mutate(country_group = if_else(country == "CHL", "Chile", "Other countries"))

scatter_stats <- scatter_data |>
  group_by(subject) |>
  summarise(
    pearson_r = cor(pisa_mean, piaac_mean, use = "complete.obs"),
    n_countries = n(),
    label = sprintf("r = %.2f\nn = %d", pearson_r, n_countries),
    .groups = "drop"
  )

p_scatter <- ggplot(scatter_data, aes(pisa_mean, piaac_mean)) +
  geom_smooth(method = "lm", se = TRUE, color = accent_gray,
              linewidth = 0.7, alpha = 0.15) +
  geom_point(
    data = scatter_data |> filter(country != "CHL"),
    color = "#bdbdbd", size = 2, alpha = 0.8
  ) +
  geom_point(
    data = scatter_data |> filter(country == "CHL"),
    color = primary_gold, size = 2.8
  ) +
  geom_text(
    data = scatter_data |> filter(country == "CHL"),
    aes(label = country), color = primary_gold,
    size = 3, vjust = -0.8, fontface = "bold"
  ) +
  geom_text(
    data = scatter_stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1.2, size = 3.2, color = accent_gray
  ) +
  facet_wrap(~subject, scales = "free") +
  labs(
    title    = "Short-run transmission: PISA 2018 to PIAAC 2023 at age ~20",
    subtitle = "Each point = one country; Chile highlighted; age ~20 is exact for some countries and the 16-24 midpoint for others",
    x = "PISA mean score (age 15)",
    y = "PIAAC mean score (age ~20, 2023)"
  ) +
  theme_custom()

save_plot_dual("cohort_pisa_piaac_scatter", p_scatter, width = 10, height = 5)

# --- Fig 3: Design ladder ----------------------------------------------------

message("Figure 3: Design ladder (naive vs age-restricted vs cohort-lagged)...")

p_design <- ggplot(design_compare, aes(pisa_mean, piaac_mean)) +
  geom_smooth(method = "lm", se = TRUE, color = accent_gray,
              linewidth = 0.6, alpha = 0.15) +
  geom_point(
    data = design_compare |> filter(country != "CHL"),
    color = "#c0c0c0", size = 1.8, alpha = 0.8
  ) +
  geom_point(
    data = design_compare |> filter(country == "CHL"),
    color = primary_gold, size = 2.8
  ) +
  geom_text(
    data = design_compare |> filter(country == "CHL"),
    aes(label = country), color = primary_gold,
    size = 2.7, vjust = -0.8, fontface = "bold"
  ) +
  geom_text(
    data = design_stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1.2, size = 3.0, color = accent_gray
  ) +
  facet_grid(subject ~ design, scales = "free") +
  labs(
    title    = "Design choice changes the apparent PISA-PIAAC relationship",
    subtitle = "From left to right: naive cross-section, younger adults only, then a short-run age ~20 comparison",
    x = "PISA mean score",
    y = "PIAAC mean score"
  ) +
  theme_custom(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

save_plot_dual("pisa_piaac_design_comparison", p_design, width = 13, height = 7)

# --- Fig 3b: Exact-age sensitivity -------------------------------------------

message("Figure 3b: Exact-age sensitivity (age 20 only)...")

p_exact <- ggplot(exact_age_sensitivity, aes(pisa_mean, piaac_mean)) +
  geom_smooth(method = "lm", se = TRUE, color = accent_gray,
              linewidth = 0.6, alpha = 0.15) +
  geom_point(
    data = exact_age_sensitivity |> filter(country != "CHL"),
    color = "#c0c0c0", size = 1.8, alpha = 0.8
  ) +
  geom_point(
    data = exact_age_sensitivity |> filter(country == "CHL"),
    color = primary_gold, size = 2.8
  ) +
  geom_text(
    data = exact_age_sensitivity |> filter(country == "CHL"),
    aes(label = country), color = primary_gold,
    size = 2.7, vjust = -0.8, fontface = "bold"
  ) +
  geom_text(
    data = exact_age_stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1.2, size = 3.0, color = accent_gray
  ) +
  facet_grid(subject ~ design, scales = "free") +
  labs(
    title    = "Exact-age sensitivity for short-run transmission",
    subtitle = "Restricted to countries with exact ages in PIAAC 2023 and evaluated at age 20",
    x = "PISA 2018 mean score",
    y = "PIAAC 2023 mean score at age 20"
  ) +
  theme_custom(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

save_plot_dual("pisa_piaac_exact_age_sensitivity", p_exact, width = 10, height = 7)

overlap_countries <- intersect(
  unique(pisa_all$country),
  unique(piaac_cohort_means$country)
)

# --- Fig 4: Persistence correlation by PIAAC survey year ---------------------
# Spearman rank correlation between PISA and PIAAC means, computed
# across countries for each (piaac_year, subject) — shows how well
# PISA age-15 rankings predict adult PIAAC rankings over time.

message("Figure 4: Persistence correlation by PIAAC survey year...")

persistence <- cohort_pisa_piaac |>
  group_by(piaac_year, subject) |>
  summarise(
    spearman_r = cor(pisa_mean, piaac_mean, method = "spearman", use = "complete.obs"),
    n_countries = n(),
    .groups = "drop"
  )

p_persist <- persistence |>
  ggplot(aes(factor(piaac_year), spearman_r, fill = spearman_r > 0)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f\n(n=%d)", spearman_r, n_countries)),
            vjust = -0.3, size = 3.2, color = accent_gray) +
  scale_fill_manual(values = c("TRUE" = primary_blue, "FALSE" = negative_red),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 1.05)) +
  facet_wrap(~subject) +
  labs(
    title    = "Persistence of PISA rankings into adult PIAAC outcomes",
    subtitle = "Spearman rank correlation between country PISA means and PIAAC means (pooled cohorts)",
    x = "PIAAC survey year", y = "Spearman r"
  ) +
  theme_custom()

save_plot_dual("cohort_pisa_piaac_persistence", p_persist, width = 8, height = 5)

# --- Fig 4: Age-matched comparison -------------------------------------------
# Compare young adults (~age 20-21) across two generations:
#   Gen 1: PISA 2006 cohort (born 1989-1993) tested in PIAAC 2012 at age ~21
#   Gen 2: PISA 2018 cohort (born 2001-2005) tested in PIAAC 2023 at age ~20
# Same age, 11 years apart. Controls for life-stage; shows whether
# young-adult skills have declined across generations.

message("Figure 5: Age-matched comparison (PIAAC 2012 vs PIAAC 2023 young adults)...")

gen1 <- piaac_cohort_means |>
  filter(pisa_year == 2006, piaac_year == 2012) |>
  rename(piaac_mean = mean, piaac_se = se) |>
  mutate(generation = "Gen 1: born ~1991\n(age ~21 in PIAAC 2012)")

gen2 <- piaac_cohort_means |>
  filter(pisa_year == 2018, piaac_year == 2023) |>
  rename(piaac_mean = mean, piaac_se = se) |>
  mutate(generation = "Gen 2: born ~2003\n(age ~20 in PIAAC 2023)")

countries_both_gens <- intersect(gen1$country, gen2$country)
message("  Countries in both generations: ", length(countries_both_gens),
        " (", paste(sort(countries_both_gens), collapse = ", "), ")")

age_matched <- bind_rows(gen1, gen2) |>
  filter(country %in% countries_both_gens)

# --- Fig 4a: Dumbbell chart — PIAAC score at age ~20-21, two generations ----

# Compute change and sort by it.
# Rename with starts_with() to avoid position-based fragility.
dumbbell_data <- age_matched |>
  select(country, subject, generation, piaac_mean) |>
  pivot_wider(names_from = generation, values_from = piaac_mean) |>
  rename_with(\(x) "g1", starts_with("Gen 1")) |>
  rename_with(\(x) "g2", starts_with("Gen 2")) |>
  mutate(delta      = g2 - g1,
         direction  = if_else(delta >= 0, "Improved", "Declined"))

country_order_lit <- dumbbell_data |>
  filter(subject == "Literacy") |>
  arrange(delta) |>
  pull(country)

p_dumbbell <- dumbbell_data |>
  mutate(country = factor(country, levels = country_order_lit)) |>
  ggplot() +
  geom_segment(aes(x = g1, xend = g2, y = country, yend = country,
                   color = direction), linewidth = 1) +
  geom_point(aes(x = g1, y = country), color = accent_gray, size = 3) +
  geom_point(aes(x = g2, y = country, color = direction), size = 3) +
  scale_color_manual(values = c(Improved = positive_green, Declined = negative_red),
                     name = NULL) +
  facet_wrap(~subject, scales = "free_x") +
  labs(
    title    = "Young-adult skills: same age, two generations",
    subtitle = "Grey dot = PISA 2006 cohort in PIAAC 2012 (age ~21)  |  Colour dot = PISA 2018 cohort in PIAAC 2023 (age ~20)",
    x = "PIAAC mean score", y = NULL
  ) +
  theme_custom() +
  theme(legend.position = "top")

ggsave(file.path(fig_dir, "cohort_age_matched_dumbbell.pdf"), p_dumbbell,
       width = 10, height = 7)

# --- Fig 4b: DELTA-PISA vs DELTA-PIAAC scatter --------------------------------
# Does improvement in PISA translate to improvement in PIAAC young-adult scores?
# If points cluster below the 45-degree line, skills decay more than PISA would predict.

# Use PISA 2000 as the Gen 1 baseline to capture the full early-gains arc.
# Countries without 2000 data are dropped (shown in subtitle).
pisa_2000_data <- pisa_all |>
  filter(pisa_year == 2000, country %in% countries_both_gens) |>
  select(country, subject, pisa_2000 = pisa_mean)

pisa_2018_data <- pisa_all |>
  filter(pisa_year == 2018, country %in% countries_both_gens) |>
  select(country, subject, pisa_2018 = pisa_mean)

delta_data <- dumbbell_data |>
  left_join(pisa_2000_data, by = c("country", "subject")) |>
  left_join(pisa_2018_data, by = c("country", "subject")) |>
  mutate(
    delta_pisa  = pisa_2018 - pisa_2000,   # Gen 2 minus Gen 1 at age 15 (PISA)
    delta_piaac = delta                     # Gen 2 minus Gen 1 at age ~20-21 (PIAAC)
  ) |>
  filter(!is.na(delta_pisa), !is.na(delta_piaac)) |>
  # Keep only countries with complete data for BOTH subjects so panels are symmetric
  group_by(country) |>
  filter(n_distinct(subject) == 2) |>
  ungroup()

dropped_from_scatter <- setdiff(countries_both_gens, unique(delta_data$country))
if (length(dropped_from_scatter) > 0) {
  message("  Countries dropped from delta scatter (incomplete PISA data): ",
          paste(dropped_from_scatter, collapse = ", "))
}

scatter_subtitle <- paste0(
  "Dotted line = 1:1 (perfect transmission). Points below = PIAAC lost more than PISA gained.\n",
  "Gen 1 (PISA 2006 cohort): PIAAC 2012 age ~21  vs  Gen 2 (PISA 2018 cohort): PIAAC 2023 age ~20",
  if (length(dropped_from_scatter) > 0)
    paste0("  |  Excluded (no PISA 2000 data): ", paste(dropped_from_scatter, collapse = ", "))
  else ""
)

p_delta <- delta_data |>
  ggplot(aes(delta_pisa, delta_piaac, color = direction)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = accent_gray) +
  geom_vline(xintercept = 0, linetype = "dashed", color = accent_gray) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted",
              color = accent_gray, linewidth = 0.5) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(aes(label = country), size = 2.8, vjust = -0.7, alpha = 0.8) +
  scale_color_manual(values = c(Improved = positive_green, Declined = negative_red),
                     name = NULL) +
  facet_wrap(~subject) +
  labs(
    title    = "Did PISA gains translate to young-adult PIAAC gains?",
    subtitle = scatter_subtitle,
    x = "Change in PISA score: Gen 2 minus Gen 1 at age 15 (2018 vs 2006)",
    y = "Change in PIAAC score: Gen 2 (age ~20) minus Gen 1 (age ~21)"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "cohort_age_matched_delta.pdf"), p_delta,
       width = 10, height = 5)

# Print summary
message("\n  Age-matched change summary (PIAAC 2023 vs 2012, same age ~20-21):")
dumbbell_data |>
  arrange(subject, delta) |>
  select(subject, country, delta) |>
  print(n = 60)

# =============================================================================
# 5. Illiteracy cohort analysis: % PVLIT <= 225 by cohort x PIAAC round
#    Binary outcome estimated with PV+BRR (treat indicator as a mean).
# =============================================================================

message("Computing illiteracy rates by cohort...")

PVLIT_THRESHOLD <- 225L
IL_COLS <- paste0("il_pvlit", 1:10)   # binary indicator column names

# Pre-compute binary indicators on piaac_raw (all LIT PVs already non-NA)
piaac_il <- piaac_raw
for (k in 1:10) {
  piaac_il[[IL_COLS[k]]] <- as.numeric(piaac_il[[LIT_PVS[k]]] <= PVLIT_THRESHOLD)
}

piaac_illiteracy_cohort <- pmap_dfr(cohort_grid, function(pisa_year, birth_lo, birth_hi,
                                                            cohort_label, piaac_year,
                                                            age_at_piaac) {
  df_sub <- piaac_il |>
    filter(
      survey_year == piaac_year,
      birth_cohort >= birth_lo,
      birth_cohort <= birth_hi,
      piaac_year - birth_cohort >= 16L
    ) |>
    group_by(country) |>
    filter(n() >= 30) |>
    ungroup()

  if (nrow(df_sub) == 0) return(tibble())

  pv_group_mean(df_sub, IL_COLS, "country", "SPFWT0", BRR_WTS) |>
    mutate(
      pct_il       = mean * 100,
      se_pct       = se   * 100,
      cohort_label = cohort_label,
      pisa_year    = pisa_year,
      piaac_year   = piaac_year,
      age_at_piaac = piaac_year - (birth_lo + birth_hi) / 2
    ) |>
    select(country, cohort_label, pisa_year, piaac_year,
           age_at_piaac, pct_il, se_pct, V_W, V_B, n)
})

message("  Illiteracy cohort rows: ", nrow(piaac_illiteracy_cohort),
        "  Countries: ", n_distinct(piaac_illiteracy_cohort$country))

saveRDS(piaac_illiteracy_cohort, file.path(out_dir, "cohort_illiteracy_piaac.rds"))

# --- Fig 5: All-countries illiteracy trajectory ------------------------------
# x = age at PIAAC (shows cohort ageing); color = PISA cohort (who they are);
# each panel = one country.  Dots at different ages for each cohort reveal
# both age gradients and whether newer cohorts carry higher illiteracy.

message("Figure 6: Illiteracy trajectories, all countries...")

# Filter: countries with >= 2 distinct PIAAC rounds AND >= 2 distinct PISA cohorts observed
il_multi <- piaac_illiteracy_cohort |>
  filter(country %in% overlap_countries) |>
  group_by(country) |>
  filter(n_distinct(piaac_year) >= 2, n_distinct(pisa_year) >= 2) |>
  ungroup()

message("  Countries in filtered all-countries figure: ", n_distinct(il_multi$country))

p_il_all <- il_multi |>
  ggplot(aes(piaac_year, pct_il,
             color = cohort_label, group = cohort_label)) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  geom_point(size = 1.8) +
  scale_color_manual(values = COHORT_PALETTE, name = "PISA cohort") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2023),
                     labels = c("2012", "2014", "2017", "2023")) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  facet_wrap(~country, ncol = 6) +
  labs(
    title    = "Functional illiteracy by cohort across PIAAC rounds (PVLIT <= 225)",
    subtitle = "Countries with >= 2 PIAAC rounds and >= 2 PISA cohorts; each line = one birth cohort",
    x = "PIAAC survey year", y = "% functionally illiterate"
  ) +
  theme_custom(base_size = 9) +
  theme(strip.text = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "cohort_illiteracy_all_countries.pdf"), p_il_all,
       width = 14, height = 14)

# --- Fig 6: Age-matched illiteracy dumbbell ----------------------------------
# Same generation comparison as Fig 4 but with illiteracy rate.

message("Figure 7: Age-matched illiteracy dumbbell...")

il_gen1 <- piaac_illiteracy_cohort |>
  filter(pisa_year == 2006, piaac_year == 2012,
         country %in% countries_both_gens) |>
  select(country, g1_pct = pct_il, g1_se = se_pct)

il_gen2 <- piaac_illiteracy_cohort |>
  filter(pisa_year == 2018, piaac_year == 2023,
         country %in% countries_both_gens) |>
  select(country, g2_pct = pct_il, g2_se = se_pct)

il_dumbbell <- il_gen1 |>
  inner_join(il_gen2, by = "country") |>
  mutate(delta     = g2_pct - g1_pct,
         direction = if_else(delta >= 0, "Worsened", "Improved"))

country_order_il <- il_dumbbell |> arrange(delta) |> pull(country)

p_il_dumbbell <- il_dumbbell |>
  mutate(country = factor(country, levels = country_order_il)) |>
  ggplot() +
  geom_segment(aes(x = g1_pct, xend = g2_pct, y = country, yend = country,
                   color = direction), linewidth = 1) +
  geom_point(aes(x = g1_pct, y = country), color = accent_gray, size = 3) +
  geom_point(aes(x = g2_pct, y = country, color = direction), size = 3) +
  scale_color_manual(values = c(Improved = positive_green, Worsened = negative_red),
                     name = NULL) +
  scale_x_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title    = "Functional illiteracy at age ~20: two generations compared",
    subtitle = "Grey = PISA 2006 cohort in PIAAC 2012 (age ~21)  |  Colour = PISA 2018 cohort in PIAAC 2023 (age ~20)",
    x = "% scoring at or below PIAAC Level 1 (<= 225)", y = NULL
  ) +
  theme_custom() +
  theme(legend.position = "top")

ggsave(file.path(fig_dir, "cohort_illiteracy_dumbbell.pdf"), p_il_dumbbell,
       width = 9, height = 7)

message("\n  Illiteracy change (Gen 2 vs Gen 1, same age ~20-21):")
il_dumbbell |> arrange(delta) |>
  select(country, g1_pct, g2_pct, delta) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  print(n = 30)

# --- Fig 7: Within-cohort illiteracy trajectories ----------------------------
# Follow the SAME birth cohort across PIAAC rounds as they age.
# E.g., PISA 2006 cohort (born 1989-1993): age ~21 in PIAAC 2012 -> age ~32 in PIAAC 2023.
# This answers: does illiteracy rise as cohorts age? Are newer cohorts worse at every age?
# One panel per cohort; countries are colored lines; only cohorts with >= 2 PIAAC rounds shown.

message("Figure 8: Within-cohort illiteracy trajectories (same cohort, aging over time)...")

# Find which cohorts have >= 2 distinct piaac_years for at least some countries
multi_round_cohorts <- piaac_illiteracy_cohort |>
  group_by(cohort_label, country) |>
  filter(n_distinct(piaac_year) >= 2) |>
  ungroup() |>
  pull(cohort_label) |>
  unique()

# Focus on PISA 2006 and 2009 — richest multi-round coverage (18+ countries each)
focus_cohorts <- intersect(multi_round_cohorts, c("PISA 2006", "PISA 2009"))

traj_data <- piaac_illiteracy_cohort |>
  filter(cohort_label %in% focus_cohorts) |>
  group_by(cohort_label, country) |>
  filter(n_distinct(piaac_year) >= 2) |>   # only countries with actual multi-round data
  ungroup()

message("  Countries with >= 2 PIAAC rounds for focus cohorts: ", n_distinct(traj_data$country))

# Compute cross-country mean trajectory for each (cohort_label, piaac_year)
traj_mean <- traj_data |>
  group_by(cohort_label, piaac_year, age_at_piaac) |>
  summarise(pct_il = mean(pct_il, na.rm = TRUE), .groups = "drop")

p_traj <- ggplot() +
  # Country lines: grey background
  geom_line(
    data = traj_data,
    aes(age_at_piaac, pct_il, group = country),
    color = "#aaaaaa", linewidth = 0.55, alpha = 0.75
  ) +
  geom_point(
    data = traj_data,
    aes(age_at_piaac, pct_il, group = country),
    color = "#aaaaaa", size = 1.6, alpha = 0.75
  ) +
  # Mean trend: red foreground
  geom_line(
    data = traj_mean,
    aes(age_at_piaac, pct_il, group = cohort_label),
    color = negative_red, linewidth = 1.4
  ) +
  geom_point(
    data = traj_mean,
    aes(age_at_piaac, pct_il),
    color = negative_red, size = 3.5
  ) +
  scale_x_continuous(breaks = sort(unique(traj_data$age_at_piaac))) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  facet_wrap(~cohort_label, ncol = 2, scales = "free_x") +
  labs(
    title    = "Within-cohort illiteracy trajectories: same people, older",
    subtitle = paste0(
      "Grey = individual countries; red = cross-country mean  |  ",
      "PVLIT <= 225  |  ",
      "PISA 2006 cohort born ~1989-1993, PISA 2009 born ~1992-1996"
    ),
    x = "Age at PIAAC survey",
    y = "% functionally illiterate (Level 1 or below)"
  ) +
  theme_custom() +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave(file.path(fig_dir, "cohort_illiteracy_trajectories.pdf"), p_traj,
       width = 12, height = 6)

# Print trajectory summary: change in illiteracy rate as cohort ages
message("\n  Within-cohort illiteracy change (youngest -> oldest PIAAC round):")
traj_data |>
  group_by(cohort_label, country) |>
  summarise(
    age_young  = min(age_at_piaac),
    age_old    = max(age_at_piaac),
    pct_young  = pct_il[which.min(age_at_piaac)],
    pct_old    = pct_il[which.max(age_at_piaac)],
    delta      = pct_old - pct_young,
    .groups    = "drop"
  ) |>
  arrange(cohort_label, delta) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  print(n = 60)

# =============================================================================
# 6. PISA -> PIAAC transmission: pooled vs within-country
#    Young adults (age 18-25) only, so PISA and PIAAC are comparable life-stages.
#
#    Key question: do gains in PISA (within a country, across cohorts) translate
#    to gains in adult literacy (PIAAC)?
#
#    Pooled OLS: mostly captures stable country differences (Finland vs Chile).
#    Within-country OLS (demean by country x subject): isolates cohort-to-cohort
#    changes within a country -- the actual "gain" question.
# =============================================================================

message("Computing PISA -> PIAAC transmission (pooled vs within-country)...")

# Fixed within-survey reference used only to put PISA and PIAAC onto a common
# relative scale for the pooled-vs-within-country transmission diagnostic.
# Anchor z-scores to PISA 2000 (earliest available) so within-country deviations
# are relative to the pre-gains baseline, not a midpoint in the improvement period.
pisa_ref <- pisa_all |>
  filter(country %in% overlap_countries, pisa_year == 2000, !is.na(pisa_mean)) |>
  group_by(subject) |>
  summarise(ref_mean = mean(pisa_mean), ref_sd = sd(pisa_mean), .groups = "drop")

piaac_ref <- piaac_cohort_means |>
  rename(piaac_mean = mean) |>
  filter(country %in% overlap_countries, piaac_year == 2012) |>
  group_by(subject) |>
  summarise(ref_mean = mean(piaac_mean, na.rm = TRUE),
            ref_sd   = sd(piaac_mean,   na.rm = TRUE),
            .groups  = "drop")

ya <- cohort_pisa_piaac |>
  filter(age_at_piaac >= 18, age_at_piaac <= 25) |>
  # Convert to within-survey z-scores using the fixed reference anchors above
  left_join(pisa_ref  |> rename(pisa_ref_mean  = ref_mean, pisa_ref_sd  = ref_sd),
            by = "subject") |>
  left_join(piaac_ref |> rename(piaac_ref_mean = ref_mean, piaac_ref_sd = ref_sd),
            by = "subject") |>
  mutate(
    pisa_z  = (pisa_mean  - pisa_ref_mean)  / pisa_ref_sd,
    piaac_z = (piaac_mean - piaac_ref_mean) / piaac_ref_sd
  )

# Demean by country x subject (within-country deviations, in z-score units)
ya_dm <- ya |>
  group_by(country, subject) |>
  mutate(
    pisa_dm  = pisa_z  - mean(pisa_z,  na.rm = TRUE),
    piaac_dm = piaac_z - mean(piaac_z, na.rm = TRUE),
    n_obs    = n()
  ) |>
  ungroup()

# OLS in z-score units: slope = partial correlation
get_beta <- function(df, x_col, y_col) {
  m  <- lm(reformulate(x_col, y_col), data = df)
  cf <- coef(summary(m))
  list(
    beta = round(cf[x_col, "Estimate"],  3),
    se   = round(cf[x_col, "Std. Error"], 3),
    r2   = round(summary(m)$r.squared,   3),
    n    = nrow(df)
  )
}

transmission <- map_dfr(c("Literacy", "Numeracy"), function(s) {
  pooled <- get_beta(ya |> filter(subject == s), "pisa_z", "piaac_z")
  within <- get_beta(ya_dm |> filter(subject == s, n_obs >= 2), "pisa_dm", "piaac_dm")
  tibble(
    subject   = s,
    type      = c("Pooled (cross-country)", "Within-country (cohort gains)"),
    beta      = c(pooled$beta, within$beta),
    se        = c(pooled$se,   within$se),
    r2        = c(pooled$r2,   within$r2),
    rho       = c(sqrt(pooled$r2), sqrt(within$r2)),   # correlation = sqrt(R2) in bivariate OLS
    n         = c(pooled$n,    within$n)
  )
})

message("  Transmission coefficients (z-score units):")
print(transmission)

# --- Fig 8: Two-panel scatter — pooled (left) vs within-country (right) ------

message("Figure 9: PISA->PIAAC transmission scatter...")

# Annotation: show correlation (rho) and SE of beta (beta SE is still informative)
annot <- transmission |>
  mutate(label = sprintf("rho == %.2f~~(beta == %.2f*','~SE~%.2f~~R^2 == %.3f)",
                         rho, beta, se, r2))

# Panel A: pooled cross-country scatter (z-score units)
panel_a <- ya |>
  left_join(annot |> filter(type == "Pooled (cross-country)") |>
              select(subject, label), by = "subject") |>
  ggplot(aes(pisa_z, piaac_z)) +
  geom_smooth(method = "lm", se = TRUE, color = primary_blue,
              fill = primary_blue, alpha = 0.15, linewidth = 0.8) +
  geom_point(aes(color = cohort_label), size = 2, alpha = 0.75) +
  geom_text(aes(label = country, color = cohort_label),
            size = 1.9, vjust = -0.5, alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(x = -Inf, y = Inf, label = label), parse = TRUE,
            hjust = -0.05, vjust = 1.4, size = 3.2, color = primary_blue,
            inherit.aes = FALSE,
            data = annot |> filter(type == "Pooled (cross-country)")) +
  scale_color_manual(values = COHORT_PALETTE, name = "PISA cohort") +
  facet_wrap(~subject, scales = "free") +
  labs(title = "A. Cross-country (pooled)",
       subtitle = "Each point = one country-cohort; age 18-25 at PIAAC; z=0 anchored to 2006/2012 OECD mean",
       x = "PISA score (z, anchored to 2006 reference)", y = "PIAAC score (z, anchored to 2012 reference)") +
  theme_custom() +
  theme(legend.position = "bottom")

# Panel B: within-country demeaned scatter (z-score units)
panel_b <- ya_dm |>
  filter(n_obs >= 2) |>
  left_join(annot |> filter(type == "Within-country (cohort gains)") |>
              select(subject, label), by = "subject") |>
  ggplot(aes(pisa_dm, piaac_dm)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = accent_gray, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = accent_gray, linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = negative_red,
              fill = negative_red, alpha = 0.15, linewidth = 0.8) +
  geom_line(aes(group = country), color = accent_gray,
            linewidth = 0.35, alpha = 0.5) +
  geom_point(aes(color = cohort_label), size = 2, alpha = 0.8) +
  geom_text(aes(label = country, color = cohort_label),
            size = 1.9, vjust = -0.5, alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(x = -Inf, y = Inf, label = label), parse = TRUE,
            hjust = -0.05, vjust = 1.4, size = 3.2, color = negative_red,
            inherit.aes = FALSE,
            data = annot |> filter(type == "Within-country (cohort gains)")) +
  scale_color_manual(values = COHORT_PALETTE, name = "PISA cohort") +
  facet_wrap(~subject, scales = "free") +
  labs(title = "B. Within-country (cohort gains)",
       subtitle = "Country means removed; lines connect cohorts within a country; slope = within-country correlation",
       x = "PISA z-score deviation from country mean",
       y = "PIAAC z-score deviation from country mean") +
  theme_custom() +
  theme(legend.position = "bottom")

p_transmission <- cowplot::plot_grid(panel_a, panel_b, nrow = 2, align = "v")

ggsave(file.path(fig_dir, "pisa_piaac_transmission.pdf"), p_transmission,
       width = 12, height = 11)

message("  => Pooled beta (Literacy): ", transmission$beta[1],
        "  Within-country beta: ", transmission$beta[2])
message("  Interpretation: cross-country correlation is real but within-country",
        " cohort gains do NOT translate to PIAAC gains.")

# =============================================================================
# 7. Chile cohort lifecycle: adult PIAAC trajectories only
#    Keep the lifecycle view in a single metric space by plotting only adult
#    PIAAC scores. Cohorts are still identified by the PISA year when they
#    were age 15, but the figure itself stays entirely in raw PIAAC points.
# =============================================================================

message("Figure 10: Chile cohort lifecycle (PIAAC only)...")

chile_lifecycle <- cohort_pisa_piaac |>
  filter(country == "CHL") |>
  transmute(
    cohort_label = factor(cohort_label, levels = names(COHORT_PALETTE)),
    pisa_year,
    subject,
    age = age_at_piaac,
    score = piaac_mean,
    se = piaac_se,
    survey_type = factor(
      paste0("PIAAC ", piaac_year),
      levels = c("PIAAC 2014", "PIAAC 2023")
    )
  ) |>
  arrange(cohort_label, subject, age)

p_chile_lc <- chile_lifecycle |>
  ggplot(aes(age, score, color = cohort_label, group = cohort_label)) +
  geom_hline(yintercept = 225, linetype = "dotted", color = "gray60", linewidth = 0.4) +
  geom_line(linewidth = 0.75, alpha = 0.85) +
  geom_errorbar(aes(ymin = score - 1.96 * se, ymax = score + 1.96 * se),
                width = 0.7, alpha = 0.45) +
  geom_point(aes(shape = survey_type), size = 2.8) +
  scale_color_manual(values = COHORT_PALETTE, name = "Cohort (linked PISA year)") +
  scale_shape_manual(
    values = c("PIAAC 2014" = 16, "PIAAC 2023" = 17),
    name   = "Survey"
  ) +
  scale_x_continuous(
    breaks = c(17, 20, 23, 26, 29, 32, 35, 38),
    limits = c(16, 39)
  ) +
  facet_wrap(~subject) +
  labs(
    title    = "Chile: adult cohort differences are small in raw PIAAC space",
    subtitle = paste0(
      "Each line = one Chile cohort, identified by the PISA year when that cohort was age 15.\n",
      "Only adult PIAAC scores are plotted. Dotted line = Level 1 threshold (225)."
    ),
    x = "Age at PIAAC survey",
    y = "PIAAC mean score"
  ) +
  theme_custom() +
  theme(legend.position = "right", legend.box = "vertical")

save_plot_dual("cohort_pisa_piaac_chile_lifecycle", p_chile_lc, width = 13, height = 6)

message("  => Saved cohort_pisa_piaac_chile_lifecycle.pdf/.png")

message("\nAll outputs saved:")
message("  02_output/cohort_pisa_piaac.rds")
message("  02_output/chile_cohort_mapping.csv")
message("  02_output/piaac_2023_age_support.csv")
message("  02_output/pisa_piaac_main_spec.csv")
message("  02_output/pisa_piaac_persistence_spec.csv")
message("  02_output/pisa_piaac_exact_age_sensitivity.csv")
message("  02_output/cohort_illiteracy_piaac.rds")
message("  Figures/cohort_pisa_piaac_chile_lifecycle.pdf")
message("  Figures/cohort_pisa_piaac_chile.pdf")
message("  Figures/cohort_pisa_piaac_chile.png")
message("  Figures/cohort_pisa_piaac_scatter.pdf")
message("  Figures/cohort_pisa_piaac_scatter.png")
message("  Figures/pisa_piaac_design_comparison.pdf")
message("  Figures/pisa_piaac_design_comparison.png")
message("  Figures/pisa_piaac_exact_age_sensitivity.pdf")
message("  Figures/pisa_piaac_exact_age_sensitivity.png")
message("  Figures/cohort_pisa_piaac_persistence.pdf")
message("  Figures/cohort_pisa_piaac_persistence.png")
message("  Figures/cohort_age_matched_dumbbell.pdf")
message("  Figures/cohort_age_matched_delta.pdf")
message("  Figures/cohort_illiteracy_all_countries.pdf")
message("  Figures/cohort_illiteracy_dumbbell.pdf")
message("  Figures/cohort_illiteracy_trajectories.pdf")
