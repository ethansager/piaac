# 02_cohort_analysis.r
# Track birth cohorts across PIAAC rounds (cy1 ≈ 2012/15, cy2 ≈ 2017).
# Uses proper PV+BRR variance estimation (Rubin's rules) via pv_group_mean().
#
# Inputs:  02_output/piaac_clean.rds
# Outputs: 02_output/cohort_scores.rds, 02_output/cohort_change.rds
#          02_output/cohort_scores_by_gender.rds
#          Figures/cohort_trends.pdf, Figures/cohort_trends_gender.pdf

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

# ---- Birth cohort decades ----
# Floor to nearest decade: 1968 → 1960s, 1975 → 1970s
piaac <- mutate(piaac,
  cohort_decade = floor(birth_cohort / 10) * 10
)

# Keep plausible cohorts (born 1930–2000) and valid observations
piaac_coh <- filter(
  piaac,
  cohort_decade >= 1930, cohort_decade <= 2000,
  !is.na(PVLIT1), !is.na(SPFWT0), SPFWT0 > 0
)

# ---- Cohort means: PV+BRR estimation ----
cat("Computing cohort scores (PV+BRR)...\n")
cohort_scores <- pv_group_mean(
  piaac_coh, LIT_PVS,
  c("country", "cohort_decade", "round", "survey_year"),
  "SPFWT0", BRR_WTS
) |>
  rename(mean_lit = mean, n_obs = n)

saveRDS(cohort_scores, file.path(out_dir, "cohort_scores.rds"))
cat("Cohort scores sample (first 12 rows):\n")
print(head(cohort_scores, 12))

# ---- Countries present in both rounds ----
# Use >= 2 to include countries with 3 rounds (e.g. USA: 2012/2017/2023)
both_rounds <- cohort_scores |>
  distinct(country, round) |>
  group_by(country) |>
  filter(n() >= 2) |>
  pull(country) |>
  unique()

cat(sprintf("\n%d countries in both rounds: %s\n",
            length(both_rounds), paste(sort(both_rounds), collapse = ", ")))

# ---- Helper: build cohort trend plot ----
make_cohort_plot <- function(plot_data, subtitle_extra = "") {
  round_palette <- c("1" = "#012169", "2" = "#f2a900")

  ggplot(plot_data,
         aes(x = cohort_decade, y = mean_lit,
             color = factor(round), fill = factor(round),
             group = factor(round))) +
    geom_ribbon(aes(ymin = mean_lit - 1.96 * se, ymax = mean_lit + 1.96 * se),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 225, linetype = "dashed", color = "red", alpha = 0.6) +
    facet_wrap(~country, ncol = 5) +
    scale_color_manual(
      values = round_palette,
      labels = c("1" = "cy1 (2012/14)", "2" = "cy2 (2023)"),
      name   = "Round"
    ) +
    scale_fill_manual(
      values = round_palette,
      labels = c("1" = "cy1 (2012/14)", "2" = "cy2 (2023)"),
      name   = "Round"
    ) +
    scale_x_continuous(
      breaks = seq(1940, 2000, 20),
      labels = c("1940s", "1960s", "1980s", "2000s")
    ) +
    labs(
      title    = "PIAAC Literacy by Birth Cohort: cy1 vs cy2",
      subtitle = paste0("Dashed red = Level 1 ceiling (225). ",
                        "Same birth decade is ~9-11 yrs older in cy2.",
                        if (nchar(subtitle_extra) > 0) paste0("\n", subtitle_extra) else ""),
      x        = "Birth decade",
      y        = "Mean literacy score (PV+BRR)",
      caption  = paste0("Source: PIAAC cy1 & cy2 PUF. Mean of 10 PVs, BRR SEs (Rubin's rules), ",
                        "SPFWT0 weights.\nShading = \u00b11.96 SE. Cells with n<50 suppressed. ",
                        "cy1 field dates: 2012 (most OECD), 2014 (CHL, ISR, SGP, etc.), 2017 (GRC, KAZ, SVN). cy2 = Cycle 2 (2023).")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title      = element_text(face = "bold", color = "#012169", size = 13),
      plot.subtitle   = element_text(size = 10),
      legend.position = "bottom",
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      strip.text      = element_text(size = 8, face = "bold")
    )
}

# ---- Plot: all-country cohort trends ----
# Restrict to rounds 1 and 2 for the main cy1 vs cy2 comparison
# (round 3 = USA 2017 intermediate data, available in cohort_scores.rds)
plot_data <- cohort_scores |>
  filter(country %in% both_rounds, round %in% c(1, 2), cohort_decade >= 1940, n_obs >= 50)

p_cohort <- make_cohort_plot(plot_data)

fig_path <- file.path(fig_dir, "cohort_trends.pdf")
ggsave(fig_path, p_cohort, width = 14, height = 10)
ggsave(file.path(fig_dir, "cohort_trends.png"), p_cohort, width = 14, height = 10, dpi = 150)
cat(sprintf("Saved figure: %s\n", fig_path))

# ---- Cohort change: cy2 - cy1 for matched birth decades ----
cohort_change <- cohort_scores |>
  filter(country %in% both_rounds, n_obs >= 50) |>
  select(country, cohort_decade, round, mean_lit, se) |>
  pivot_wider(names_from = round, values_from = c(mean_lit, se),
              names_sep = "_r") |>
  filter(!is.na(mean_lit_r1), !is.na(mean_lit_r2)) |>
  mutate(
    change    = mean_lit_r2 - mean_lit_r1,
    # cy1 and cy2 are independent samples → SEs add in quadrature
    se_change = sqrt(se_r1^2 + se_r2^2),
    direction = if_else(change < 0, "decline", "improvement")
  )

saveRDS(cohort_change, file.path(out_dir, "cohort_change.rds"))

cat("\nAverage cohort change by country (cy2 - cy1, same birth decade):\n")
country_summary <- cohort_change |>
  group_by(country) |>
  summarise(
    mean_change = mean(change, na.rm = TRUE),
    n_cohorts   = n(),
    pct_decline = mean(direction == "decline") * 100,
    .groups     = "drop"
  ) |>
  arrange(mean_change)
print(country_summary, n = Inf)

# ---- Ranked bar chart of mean cohort changes ----
country_bar <- country_summary |>
  mutate(
    country_fac = factor(country, levels = country),
    bar_color   = if_else(mean_change < 0, "#b91c1c", "#15803d")
  )

p_change_bar <- ggplot(country_bar,
                       aes(x = country_fac, y = mean_change, fill = bar_color)) +
  geom_col(width = 0.75) +
  geom_text(
    aes(label  = sprintf("%.0f", mean_change),
        hjust  = if_else(mean_change < 0, 1.15, -0.15)),
    size = 3.0, color = "#222222"
  ) +
  geom_hline(yintercept = 0, color = "#222222", linewidth = 0.5) +
  coord_flip(clip = "off") +
  scale_fill_identity() +
  scale_y_continuous(
    limits = c(min(country_bar$mean_change) * 1.18, 5),
    labels = scales::label_number(suffix = " pts")
  ) +
  labs(
    title    = "Mean Cohort Score Change: Cycle 2 vs. Cycle 1",
    subtitle = paste0("Average change across matched birth-decade cohorts within each country.\n",
                      "Negative = cohort scored lower in Cycle 2 (2023) than in Cycle 1 (2012/14)."),
    x        = NULL,
    y        = "Mean cohort score change (cy2 \u2212 cy1)",
    caption  = paste0("Source: PIAAC cy1 & cy2 PUF. Average across all matched birth decades per country.\n",
                      "PV+BRR estimation, SPFWT0 weights. Same birth decade is ~9\u201311 years older in cy2.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#012169", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "none",
    plot.margin   = margin(5, 50, 5, 5)
  )

ggsave(file.path(fig_dir, "cohort_change_bar.pdf"), p_change_bar, width = 10, height = 8)
ggsave(file.path(fig_dir, "cohort_change_bar.png"), p_change_bar, width = 10, height = 8, dpi = 150)
cat(sprintf("Saved figure: %s\n", file.path(fig_dir, "cohort_change_bar.pdf")))

# ---- Gender stratification ----
cat("\nComputing cohort scores by gender (PV+BRR)...\n")
cohort_scores_gender <- pv_group_mean(
  piaac_coh |> filter(!is.na(GENDER_R)),
  LIT_PVS,
  c("country", "cohort_decade", "round", "survey_year", "GENDER_R"),
  "SPFWT0", BRR_WTS
) |>
  rename(mean_lit = mean, n_obs = n) |>
  mutate(gender_label = if_else(GENDER_R == 1, "Male", "Female"))

saveRDS(cohort_scores_gender, file.path(out_dir, "cohort_scores_by_gender.rds"))

# ---- Plot: gender cohort trends (first 10 countries for readability) ----
plot_gender <- cohort_scores_gender |>
  filter(country %in% both_rounds, round %in% c(1, 2), cohort_decade >= 1940, n_obs >= 50) |>
  mutate(facet_label = paste0(country, " (", gender_label, ")"))

# Show up to 10 countries to keep figure readable
top_countries <- sort(unique(plot_gender$country))[1:min(10, length(unique(plot_gender$country)))]
plot_gender_top <- filter(plot_gender, country %in% top_countries)

p_gender <- make_cohort_plot(plot_gender_top, subtitle_extra = "By gender")

fig_path_gender <- file.path(fig_dir, "cohort_trends_gender.pdf")
ggsave(fig_path_gender, p_gender, width = 14, height = 10)
ggsave(file.path(fig_dir, "cohort_trends_gender.png"), p_gender, width = 14, height = 10, dpi = 150)
cat(sprintf("Saved figure: %s\n", fig_path_gender))

cat("\nDone: cohort analysis complete.\n")
