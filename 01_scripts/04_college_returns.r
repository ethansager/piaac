# 04_college_returns.r
# College literacy premium decomposition across PIAAC rounds.
# Uses proper PV+BRR variance estimation (Rubin's rules) via pv_group_premium().
#
# Inputs:  02_output/piaac_clean.rds
# Outputs: 02_output/college_returns.rds
#          02_output/college_premium_by_gender.rds
#          Figures/college_premium.pdf
#          Figures/college_premium_gender.pdf

set.seed(20260227)

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("01_scripts/00_helpers.r"))

# ---- Setup ----
out_dir <- here("02_output")
fig_dir <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load ----
piaac <- readRDS(file.path(out_dir, "piaac_clean.rds"))
cat(sprintf("Loaded: %s rows\n", format(nrow(piaac), big.mark = ",")))

# ---- Define college: EDCAT7 ≥ 6 (bachelor's or higher; ISCED 5A/6) ----
piaac_edu <- piaac |>
  filter(
    !is.na(PVLIT1), !is.na(EDCAT7), !is.na(SPFWT0),
    SPFWT0 > 0,
    EDCAT7 >= 1, EDCAT7 <= 7      # valid codes only
  ) |>
  mutate(college = as.integer(EDCAT7 >= 6))

cat(sprintf("Valid education obs: %s\n", format(nrow(piaac_edu), big.mark = ",")))

# ---- College premium: PV+BRR estimation ----
cat("Computing college premium (PV+BRR)...\n")
college_premium <- pv_group_premium(
  piaac_edu, LIT_PVS,
  c("country", "round", "survey_year"),
  "college", "SPFWT0", BRR_WTS
)

saveRDS(college_premium, file.path(out_dir, "college_returns.rds"))

cat("\nCollege premium by country × round (sorted by cy1 premium):\n")
prem_wide <- college_premium |>
  select(country, round, premium) |>
  pivot_wider(names_from = round, values_from = premium, names_prefix = "r") |>
  mutate(change = r2 - r1) |>
  arrange(desc(r1))
print(prem_wide, n = Inf)

# ---- Overall mean literacy: PV+BRR ----
mean_overall <- pv_group_mean(
  piaac_edu, LIT_PVS, c("country", "round", "survey_year"), "SPFWT0", BRR_WTS
) |>
  rename(mean_overall = mean, se_overall = se) |>
  select(country, round, survey_year, mean_overall, se_overall, n)

# ---- College share by country × round ----
# College share comes from EDCAT7, not PVs — single weighted.mean is correct here.
college_share <- piaac_edu |>
  group_by(country, round, survey_year) |>
  summarise(
    college_share = weighted.mean(college, SPFWT0, na.rm = TRUE) * 100,
    n_obs         = n(),
    .groups       = "drop"
  ) |>
  left_join(mean_overall |> select(country, round, mean_overall),
            by = c("country", "round"))

cat("\nCollege share and overall mean by country × round:\n")
print(college_share, n = Inf)

# ---- Email back-of-envelope check (pooled across countries) ----
# The email's BOE used US-centric figures: "40-45 yr olds in 2013" for cy1
# and "30-35 yr olds in 2017" for what was actually comparing cy1 to cy3 (2023).
# Here we use birth cohorts that correspond to 40-45 yr olds in cy1 (~2012)
# and 30-35 yr olds in cy2 (2023).
# cy1 reference: birth cohort ≈ 1967-1972 (40-45 yr olds in 2012)
# cy2 reference: birth cohort ≈ 1988-1993 (30-35 yr olds in 2023)
cat("\n--- Email back-of-envelope check ---\n")
cat("cy1 (round 1): birth cohort 1967-1972 (~40-45 yr olds in cy1 ~2012)\n")
boe_cy1_data <- piaac_edu |>
  filter(round == 1, birth_cohort >= 1967, birth_cohort <= 1972)
boe_cy1_mean <- pv_group_mean(boe_cy1_data, LIT_PVS, character(0), "SPFWT0", BRR_WTS)
boe_cy1_share <- boe_cy1_data |>
  summarise(
    college_share = weighted.mean(college, SPFWT0, na.rm = TRUE) * 100,
    n_obs         = n()
  )
cat(sprintf("  N = %s; college share = %.1f%%\n",
            format(boe_cy1_share$n_obs, big.mark = ","), boe_cy1_share$college_share))
cat(sprintf("  Mean lit (PV+BRR) = %.1f (SE = %.2f)\n",
            boe_cy1_mean$mean, boe_cy1_mean$se))
cat(sprintf("  Implied: %.0f%%*X + %.0f%%*Y = %.1f\n",
            boe_cy1_share$college_share, 100 - boe_cy1_share$college_share,
            boe_cy1_mean$mean))

cat("\ncy2 (round 2): birth cohort 1988-1993 (~30-35 yr olds in cy2 ~2023)\n")
boe_cy2_data <- piaac_edu |>
  filter(round == 2, birth_cohort >= 1988, birth_cohort <= 1993)
boe_cy2_mean <- pv_group_mean(boe_cy2_data, LIT_PVS, character(0), "SPFWT0", BRR_WTS)
boe_cy2_share <- boe_cy2_data |>
  summarise(
    college_share = weighted.mean(college, SPFWT0, na.rm = TRUE) * 100,
    n_obs         = n()
  )
cat(sprintf("  N = %s; college share = %.1f%%\n",
            format(boe_cy2_share$n_obs, big.mark = ","), boe_cy2_share$college_share))
cat(sprintf("  Mean lit (PV+BRR) = %.1f (SE = %.2f)\n",
            boe_cy2_mean$mean, boe_cy2_mean$se))
cat(sprintf("  Implied: %.0f%%*X1 + %.0f%%*Y1 = %.1f\n",
            boe_cy2_share$college_share, 100 - boe_cy2_share$college_share,
            boe_cy2_mean$mean))

# ---- Countries in both rounds ----
# Require non-NA premium in both rounds (GBR excluded: EDCAT7 data issue in cy1)
both_rounds <- college_premium |>
  filter(!is.na(premium)) |>
  distinct(country, round) |>
  group_by(country) |>
  filter(n() == 2) |>
  pull(country) |>
  unique()

cat(sprintf("\n%d countries in both rounds: %s\n",
            length(both_rounds), paste(sort(both_rounds), collapse = ", ")))

# ---- Helper: build college premium plot ----
make_premium_plot <- function(plot_df, title_extra = "") {
  # Sort countries by cy1 premium descending
  country_order <- plot_df |>
    filter(round == 1) |>
    arrange(desc(premium)) |>
    pull(country)
  plot_df <- mutate(plot_df,
    country      = factor(country, levels = rev(country_order)),
    round_label  = if_else(round == 1, "cy1 (2012/14)", "cy2 (2023)")
  )

  ggplot(plot_df, aes(x = country, y = premium, fill = round_label)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_errorbar(
      aes(ymin = premium - 1.96 * se,
          ymax = premium + 1.96 * se),
      position = position_dodge(width = 0.75),
      width = 0.35, color = "#222222", linewidth = 0.5
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c("cy1 (2012/14)" = "#012169", "cy2 (2023)" = "#f2a900"),
      name   = "Round"
    ) +
    labs(
      title    = paste0("College Literacy Premium: cy1 vs cy2", title_extra),
      subtitle = paste0("Premium = mean(bachelor\u2019s+) \u2013 mean(non-college). ",
                        "Countries in both rounds only.\nError bars = ±1.96 SE (PV+BRR)."),
      x        = NULL,
      y        = "Score premium (PV+BRR)",
      caption  = paste0("Source: PIAAC cy1 & cy2 PUF. PV+BRR SEs (Rubin\u2019s rules), SPFWT0 weights. ",
                        "cy1 field dates: 2012 (most OECD), 2014 (CHL, ISR, SGP, etc.). cy2 = Cycle 2 (2023).\n",
                        "College = EDCAT7 \u2265 6 (bachelor\u2019s or higher).")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", color = "#012169", size = 13),
      plot.subtitle   = element_text(size = 10),
      legend.position = "bottom",
      axis.text.y     = element_text(size = 9)
    )
}

# ---- Main college premium plot ----
plot_data <- filter(college_premium, country %in% both_rounds, !is.na(premium))
p_premium <- make_premium_plot(plot_data)

fig_path <- file.path(fig_dir, "college_premium.pdf")
ggsave(fig_path, p_premium, width = 10, height = 8)
ggsave(file.path(fig_dir, "college_premium.png"), p_premium, width = 10, height = 8, dpi = 150)
cat(sprintf("\nSaved figure: %s\n", fig_path))

# ---- Gender stratification ----
cat("\nComputing college premium by gender (PV+BRR)...\n")
piaac_edu_gender <- filter(piaac_edu, !is.na(GENDER_R))

college_premium_gender <- pv_group_premium(
  piaac_edu_gender, LIT_PVS,
  c("country", "round", "survey_year", "GENDER_R"),
  "college", "SPFWT0", BRR_WTS
) |>
  mutate(gender_label = if_else(GENDER_R == 1, "Male", "Female"))

saveRDS(college_premium_gender, file.path(out_dir, "college_premium_by_gender.rds"))

# Plot female college premium
plot_gender <- college_premium_gender |>
  filter(country %in% both_rounds, !is.na(premium), gender_label == "Female")
p_premium_gender <- make_premium_plot(plot_gender, title_extra = " — Female")

fig_path_gender <- file.path(fig_dir, "college_premium_gender.pdf")
ggsave(fig_path_gender, p_premium_gender, width = 10, height = 8)
ggsave(file.path(fig_dir, "college_premium_gender.png"), p_premium_gender, width = 10, height = 8, dpi = 150)
cat(sprintf("Saved figure: %s\n", fig_path_gender))

cat("Done: college returns complete.\n")
