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
  "PISA 2006" = "#012169",
  "PISA 2009" = "#f2a900",
  "PISA 2012" = "#15803d",
  "PISA 2015" = "#b91c1c",
  "PISA 2018" = "#7c3aed"
)

theme_custom <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", color = primary_blue),
      plot.subtitle = element_text(color = accent_gray),
      legend.position = "bottom"
    )
}

# =============================================================================
# 1. Cohort linkage definition
# =============================================================================

cohort_links <- tribble(
  ~pisa_year, ~birth_lo, ~birth_hi, ~cohort_label,
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

cohort_grid <- crossing(cohort_links, piaac_year = piaac_years_all)

piaac_cohort_means <- pmap_dfr(cohort_grid, function(pisa_year, birth_lo, birth_hi,
                                                       cohort_label, piaac_year) {
  df_sub <- piaac_raw |>
    filter(
      survey_year == piaac_year,
      birth_cohort >= birth_lo,
      birth_cohort <= birth_hi,
      piaac_year - birth_cohort >= 16L   # age validity: must be adult at PIAAC time
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

# Quick summary
cohort_pisa_piaac |>
  count(cohort_label, piaac_year, subject) |>
  pivot_wider(names_from = subject, values_from = n) |>
  print(n = 40)

# =============================================================================
# 4. Figures
# =============================================================================

# --- Shared z-score reference ------------------------------------------------
# Use only the 34-country overlap (in both PISA and PIAAC) so both surveys
# are standardised against the same peer group.

overlap_countries <- intersect(
  unique(pisa_all$country),
  unique(piaac_cohort_means$country)
)

pisa_ref <- pisa_all |>
  filter(country %in% overlap_countries) |>
  group_by(pisa_year, subject) |>
  summarise(ref_mean = mean(pisa_mean, na.rm = TRUE),
            ref_sd   = sd(pisa_mean,   na.rm = TRUE),
            .groups  = "drop")

piaac_ref <- piaac_cohort_means |>
  rename(piaac_mean = mean) |>
  filter(country %in% overlap_countries) |>
  group_by(piaac_year, subject) |>
  summarise(ref_mean = mean(piaac_mean, na.rm = TRUE),
            ref_sd   = sd(piaac_mean,   na.rm = TRUE),
            .groups  = "drop")

# Build z-scored long table for all countries
all_pisa_pts <- pisa_all |>
  filter(country %in% overlap_countries) |>
  left_join(pisa_ref, by = c("pisa_year", "subject")) |>
  mutate(z      = (pisa_mean - ref_mean) / ref_sd,
         age    = 15,
         survey = "PISA") |>
  left_join(cohort_links |> select(pisa_year, cohort_label), by = "pisa_year") |>
  select(country, cohort_label, subject, age, z, survey)

all_piaac_pts <- piaac_cohort_means |>
  rename(piaac_mean = mean) |>
  filter(country %in% overlap_countries) |>
  left_join(piaac_ref, by = c("piaac_year", "subject")) |>
  mutate(z      = (piaac_mean - ref_mean) / ref_sd,
         age    = age_at_piaac,
         survey = "PIAAC") |>
  select(country, cohort_label, subject, age, z, survey)

all_combined <- bind_rows(all_pisa_pts, all_piaac_pts) |>
  filter(!is.na(cohort_label), !is.na(z))

# --- Fig 1: Chile cohort trajectories ----------------------------------------

message("Figure 1: Chile cohort trajectories...")

p_chile <- all_combined |>
  filter(country == "CHL") |>
  ggplot(aes(age, z, color = cohort_label, group = cohort_label)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = accent_gray, linewidth = 0.4) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_point(aes(shape = survey), size = 3) +
  scale_color_manual(values = COHORT_PALETTE, name = "Cohort") +
  scale_shape_manual(values = c(PISA = 16, PIAAC = 17), name = "Survey") +
  scale_x_continuous(breaks = c(15, 20, 25, 30, 35)) +
  facet_wrap(~subject) +
  labs(
    title    = "Chile: skill trajectory from PISA (age 15) to PIAAC (adults)",
    subtitle = "Z-score vs 34-country overlap mean; circle = PISA, triangle = PIAAC",
    x = "Age at survey", y = "Z-score (SD above/below peer-country mean)"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "cohort_pisa_piaac_chile.pdf"), p_chile,
       width = 10, height = 5)

# --- Fig 1b: All countries (facet by country, both subjects as linetypes) ----

message("Figure 1b: All-country cohort trajectories...")

p_all <- all_combined |>
  ggplot(aes(age, z, color = cohort_label, group = interaction(cohort_label, subject),
             linetype = subject)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = accent_gray, linewidth = 0.3) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_point(aes(shape = survey), size = 1.8) +
  scale_color_manual(values = COHORT_PALETTE, name = "Cohort") +
  scale_shape_manual(values = c(PISA = 16, PIAAC = 17), name = "Survey") +
  scale_linetype_manual(values = c(Literacy = "solid", Numeracy = "dashed"),
                        name = "Subject") +
  scale_x_continuous(breaks = c(15, 25, 35)) +
  facet_wrap(~country, ncol = 6) +
  labs(
    title    = "Skill trajectories from PISA (age 15) to PIAAC (adults) — all countries",
    subtitle = "Z-score vs 34-country mean; solid = Literacy/Reading, dashed = Numeracy/Math",
    x = "Age at survey", y = "Z-score"
  ) +
  theme_custom(base_size = 9) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 7, face = "bold"))

ggsave(file.path(fig_dir, "cohort_pisa_piaac_all_countries.pdf"), p_all,
       width = 14, height = 16)

# --- Fig 2: All-country persistence scatter (PIAAC 2023) ---------------------

message("Figure 2: Cross-country persistence scatter...")

scatter_data <- cohort_pisa_piaac |>
  filter(piaac_year == 2023)

p_scatter <- scatter_data |>
  ggplot(aes(pisa_mean, piaac_mean, color = cohort_label)) +
  geom_smooth(method = "lm", se = TRUE, color = accent_gray,
              linewidth = 0.7, alpha = 0.15) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(aes(label = country), size = 2.2, vjust = -0.6, alpha = 0.7) +
  scale_color_manual(values = COHORT_PALETTE, name = "PISA cohort") +
  facet_wrap(~subject, scales = "free") +
  labs(
    title    = "PISA scores at age 15 predict adult literacy in PIAAC (2023)",
    subtitle = "Each point = one country; OLS fit pooled across cohorts",
    x = "PISA mean score (age 15)",
    y = "PIAAC mean score (adults, 2023)"
  ) +
  theme_custom()

ggsave(file.path(fig_dir, "cohort_pisa_piaac_scatter.pdf"), p_scatter,
       width = 10, height = 5)

# --- Fig 3: Persistence correlation by PIAAC survey year ---------------------
# Spearman rank correlation between PISA and PIAAC means, computed
# across countries for each (piaac_year, subject) — shows how well
# PISA age-15 rankings predict adult PIAAC rankings over time.

message("Figure 3: Persistence correlation by PIAAC survey year...")

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

ggsave(file.path(fig_dir, "cohort_pisa_piaac_persistence.pdf"), p_persist,
       width = 8, height = 5)

# --- Fig 4: Age-matched comparison -------------------------------------------
# Compare young adults (~age 20-21) across two generations:
#   Gen 1: PISA 2006 cohort (born 1989-1993) tested in PIAAC 2012 at age ~21
#   Gen 2: PISA 2018 cohort (born 2001-2005) tested in PIAAC 2023 at age ~20
# Same age, 11 years apart. Controls for life-stage; shows whether
# young-adult skills have declined across generations.

message("Figure 4: Age-matched comparison (PIAAC 2012 vs PIAAC 2023 young adults)...")

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

# Compute change and sort by it
dumbbell_data <- age_matched |>
  select(country, subject, generation, piaac_mean) |>
  pivot_wider(names_from = generation, values_from = piaac_mean) |>
  rename(g1 = 3, g2 = 4) |>
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

pisa_2006_data <- pisa_all |>
  filter(pisa_year == 2006, country %in% countries_both_gens) |>
  select(country, subject, pisa_2006 = pisa_mean)

pisa_2018_data <- pisa_all |>
  filter(pisa_year == 2018, country %in% countries_both_gens) |>
  select(country, subject, pisa_2018 = pisa_mean)

delta_data <- dumbbell_data |>
  left_join(pisa_2006_data, by = c("country", "subject")) |>
  left_join(pisa_2018_data, by = c("country", "subject")) |>
  mutate(
    delta_pisa  = pisa_2018 - pisa_2006,    # change in PISA score (age-15)
    delta_piaac = delta                      # change in young-adult PIAAC score
  ) |>
  filter(!is.na(delta_pisa), !is.na(delta_piaac))

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
    subtitle = "Dotted line = 1:1 (perfect transmission). Below = PIAAC declined more than PISA.\nGen 1: PISA 2006 -> PIAAC 2012 (age ~21)  vs  Gen 2: PISA 2018 -> PIAAC 2023 (age ~20)",
    x = "Change in PISA score (2018 vs 2006, age 15)",
    y = "Change in young-adult PIAAC score (2023 vs 2012, age ~20)"
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
                                                            cohort_label, piaac_year) {
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

message("Figure 5: Illiteracy trajectories, all countries...")

p_il_all <- piaac_illiteracy_cohort |>
  filter(country %in% overlap_countries) |>
  ggplot(aes(age_at_piaac, pct_il,
             color = cohort_label, group = cohort_label)) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  geom_point(size = 1.8) +
  scale_color_manual(values = COHORT_PALETTE, name = "PISA cohort") +
  scale_x_continuous(breaks = c(20, 25, 30, 35)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  facet_wrap(~country, ncol = 6) +
  labs(
    title    = "Functional illiteracy by cohort across PIAAC rounds (PVLIT <= 225)",
    subtitle = "Each line = one PISA birth cohort; x-axis = age at PIAAC survey",
    x = "Age at PIAAC survey", y = "% functionally illiterate"
  ) +
  theme_custom(base_size = 9) +
  theme(strip.text = element_text(size = 7, face = "bold"))

ggsave(file.path(fig_dir, "cohort_illiteracy_all_countries.pdf"), p_il_all,
       width = 14, height = 16)

# --- Fig 6: Age-matched illiteracy dumbbell ----------------------------------
# Same generation comparison as Fig 4 but with illiteracy rate.

message("Figure 6: Age-matched illiteracy dumbbell...")

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

message("Figure 7: Within-cohort illiteracy trajectories (same cohort, aging over time)...")

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

n_countries_traj <- n_distinct(traj_data$country)
message("  Countries with >= 2 PIAAC rounds for focus cohorts: ", n_countries_traj)

# Country color palette (34 countries max — use a readable set)
country_colors <- scales::hue_pal()(n_countries_traj)
names(country_colors) <- sort(unique(traj_data$country))

p_traj <- traj_data |>
  mutate(ci_lo = pct_il - 1.96 * se_pct,
         ci_hi = pct_il + 1.96 * se_pct) |>
  ggplot(aes(age_at_piaac, pct_il, color = country, group = country)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_point(size = 2.2) +
  geom_text(
    data = \(d) d |> group_by(cohort_label, country) |> slice_max(age_at_piaac, n = 1),
    aes(label = country), hjust = -0.15, size = 2.6, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(values = country_colors, guide = "none") +
  scale_x_continuous(breaks = c(18, 21, 24, 27, 30, 33),
                     limits = c(NA, 36)) +   # right-pad for labels
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  facet_wrap(~cohort_label, ncol = 2) +
  labs(
    title    = "Within-cohort illiteracy trajectories: same people, older",
    subtitle = paste0(
      "Each line = one country's birth cohort followed across PIAAC rounds ",
      "(PVLIT \u2264 225)\n",
      "PISA 2006 cohort born ~1989\u20131993 | PISA 2009 cohort born ~1992\u20131996"
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

message("\nAll outputs saved:")
message("  02_output/cohort_pisa_piaac.rds")
message("  02_output/cohort_illiteracy_piaac.rds")
message("  Figures/cohort_pisa_piaac_chile.pdf")
message("  Figures/cohort_pisa_piaac_all_countries.pdf")
message("  Figures/cohort_pisa_piaac_scatter.pdf")
message("  Figures/cohort_pisa_piaac_persistence.pdf")
message("  Figures/cohort_age_matched_dumbbell.pdf")
message("  Figures/cohort_age_matched_delta.pdf")
message("  Figures/cohort_illiteracy_all_countries.pdf")
message("  Figures/cohort_illiteracy_dumbbell.pdf")
message("  Figures/cohort_illiteracy_trajectories.pdf")
