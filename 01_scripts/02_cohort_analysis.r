# 02_cohort_analysis.r
# Track birth cohorts across PIAAC rounds (cy1 ≈ 2013, cy2 ≈ 2017).
# Simplified first pass: PV1 only, weighted means, no BRR.
#
# Inputs:  02_output/piaac_clean.rds
# Outputs: 02_output/cohort_scores.rds, 02_output/cohort_change.rds
#          Figures/cohort_trends.pdf

set.seed(20260227)

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

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

# ---- Weighted means by country × cohort decade × round ----
cohort_scores <- piaac_coh |>
  group_by(country, cohort_decade, round, survey_year) |>
  summarise(
    mean_lit = weighted.mean(PVLIT1, SPFWT0, na.rm = TRUE),
    n_obs    = n(),
    .groups  = "drop"
  )

saveRDS(cohort_scores, file.path(out_dir, "cohort_scores.rds"))
cat("Cohort scores sample (first 12 rows):\n")
print(head(cohort_scores, 12))

# ---- Countries present in both rounds ----
both_rounds <- cohort_scores |>
  distinct(country, round) |>
  group_by(country) |>
  filter(n() == 2) |>
  pull(country) |>
  unique()

cat(sprintf("\n%d countries in both rounds: %s\n",
            length(both_rounds), paste(sort(both_rounds), collapse = ", ")))

# ---- Plot: cohort trends cy1 vs cy2 ----
# Birth decade on x-axis, score on y-axis; two lines per country (cy1 vs cy2)
# Same birth cohort should shift right by ~4 years in cy2; lower score = decline
round_palette <- c("1" = "#012169", "2" = "#f2a900")

plot_data <- cohort_scores |>
  filter(country %in% both_rounds, cohort_decade >= 1940, n_obs >= 50)

p_cohort <- ggplot(plot_data,
                   aes(x = cohort_decade, y = mean_lit,
                       color = factor(round), group = factor(round))) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 225, linetype = "dashed", color = "red", alpha = 0.6) +
  facet_wrap(~country, ncol = 5) +
  scale_color_manual(
    values = round_palette,
    labels = c("1" = "cy1 (~2013)", "2" = "cy2 (~2017)"),
    name   = "Round"
  ) +
  scale_x_continuous(
    breaks = seq(1940, 2000, 20),
    labels = c("1940s", "1960s", "1980s", "2000s")
  ) +
  labs(
    title    = "PIAAC Literacy by Birth Cohort: cy1 (~2013) vs cy2 (~2017)",
    subtitle = paste0("Dashed red = Level 1 ceiling (225). ",
                      "Same birth decade is ~4 yrs older in cy2."),
    x        = "Birth decade",
    y        = "Mean literacy score (PV1, weighted)",
    caption  = "Source: PIAAC cy1 & cy2 PUF. PV1 only, SPFWT0 weights. Cells with n<50 suppressed."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", color = "#012169", size = 13),
    plot.subtitle   = element_text(size = 10),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
    strip.text      = element_text(size = 8, face = "bold")
  )

fig_path <- file.path(fig_dir, "cohort_trends.pdf")
ggsave(fig_path, p_cohort, width = 14, height = 10)
cat(sprintf("Saved figure: %s\n", fig_path))

# ---- Cohort change: cy2 - cy1 for matched birth decades ----
cohort_change <- cohort_scores |>
  filter(country %in% both_rounds, n_obs >= 50) |>
  select(country, cohort_decade, round, mean_lit) |>
  pivot_wider(names_from = round, values_from = mean_lit, names_prefix = "r") |>
  filter(!is.na(r1), !is.na(r2)) |>
  mutate(
    change    = r2 - r1,
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

cat("\nDone: cohort analysis complete.\n")
