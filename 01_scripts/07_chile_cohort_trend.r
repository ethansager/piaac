#!/usr/bin/env Rscript

# 07_chile_cohort_trend.r
# Build a Chile-only cohort trend figure for slide 12.
#
# Outputs:
#   02_output/chile_cohort_scores.csv
#   02_output/chile_cohort_change.csv
#   02_output/chile_cohort_pl1_shares.csv
#   02_output/chile_cohort_pl1_change.csv
#   Figures/chile_cohort_trend.png
#   Figures/chile_cohort_trend.pdf
#   Figures/chile_cohort_pl1_trend.png
#   Figures/chile_cohort_pl1_trend.pdf
#   Figures/chile_cohort_arrows.png
#   Figures/chile_cohort_arrows.pdf

set.seed(20260411)

library(haven)
library(here)
library(scales)
library(tidyverse)

# ---- Setup ----
root_candidates <- c(here(), here("piaac"))
piaac_root <- root_candidates[dir.exists(file.path(root_candidates, "01_scripts"))][1]
if (is.na(piaac_root)) {
  stop("Could not locate the PIAAC analysis root. Checked: . and ./piaac")
}

out_dir <- file.path(piaac_root, "02_output")
fig_dir <- file.path(piaac_root, "Figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

data_dir_candidates <- c(
  file.path(piaac_root, "0_data"),
  file.path(piaac_root, "00_data")
)
data_dir <- data_dir_candidates[file.exists(data_dir_candidates)][1]
if (is.na(data_dir)) {
  stop("Could not find raw PIAAC data directory. Checked: 0_data and 00_data")
}

source(file.path(piaac_root, "01_scripts", "00_helpers.r"))

# ---- Load Chile files ----
chile_files <- tibble(
  file = c("prgchlp1.sav", "PRGCHLP2.sav"),
  round = c(1L, 2L),
  survey_year = c(2014L, 2023L)
)

required_paths <- file.path(data_dir, chile_files$file)
if (!all(file.exists(required_paths))) {
  missing_paths <- required_paths[!file.exists(required_paths)]
  stop(
    "Missing one or more required Chile files:\n",
    paste(missing_paths, collapse = "\n")
  )
}

load_chile_file <- function(file, round, survey_year) {
  d <- read_sav(
    file.path(data_dir, file),
    col_select = any_of(c(
      "SPFWT0", BRR_WTS,
      "AGE_R", "AGEG10LFS", "AGEG10LFS_T",
      LIT_PVS
    ))
  )

  if (!"AGEG10LFS" %in% names(d) && "AGEG10LFS_T" %in% names(d)) {
    d <- rename(d, AGEG10LFS = AGEG10LFS_T)
  }

  age_r_suppressed <- !"AGE_R" %in% names(d) ||
    (all(is.na(d$AGE_R)) && "AGEG10LFS" %in% names(d))
  if (age_r_suppressed && "AGEG10LFS" %in% names(d)) {
    age_midpoints <- c(20L, 30L, 40L, 50L, 60L)
    d$AGE_R <- age_midpoints[as.integer(d$AGEG10LFS)]
  }

  d |>
    mutate(
      round = round,
      survey_year = survey_year,
      birth_cohort = survey_year - as.numeric(AGE_R),
      cohort_decade = floor(birth_cohort / 10) * 10
    )
}

chile <- pmap_dfr(chile_files, load_chile_file) |>
  mutate(across(where(haven::is.labelled), as.numeric))

chile_coh <- chile |>
  filter(
    !is.na(PVLIT1), !is.na(SPFWT0), SPFWT0 > 0,
    cohort_decade >= 1930, cohort_decade <= 2000
  )

# ---- Cohort scores ----
cohort_scores <- pv_group_mean(
  chile_coh,
  LIT_PVS,
  c("cohort_decade", "round", "survey_year"),
  "SPFWT0",
  BRR_WTS
) |>
  rename(mean_lit = mean, n_obs = n) |>
  arrange(cohort_decade, round)

cohort_change <- cohort_scores |>
  select(cohort_decade, round, mean_lit, se, n_obs) |>
  pivot_wider(
    names_from = round,
    values_from = c(mean_lit, se, n_obs),
    names_sep = "_r"
  ) |>
  filter(!is.na(mean_lit_r1), !is.na(mean_lit_r2)) |>
  mutate(
    change = mean_lit_r2 - mean_lit_r1,
    se_change = sqrt(se_r1^2 + se_r2^2),
    direction = if_else(change < 0, "decline", "improvement")
  ) |>
  arrange(cohort_decade)

write_csv(cohort_scores, file.path(out_dir, "chile_cohort_scores.csv"))
write_csv(cohort_change, file.path(out_dir, "chile_cohort_change.csv"))

cat("\nChile matched birth-decade cohort changes:\n")
print(cohort_change |>
  transmute(
    cohort = paste0(cohort_decade, "s"),
    score_2014 = round(mean_lit_r1, 1),
    score_2023 = round(mean_lit_r2, 1),
    change = round(change, 1),
    direction
  ))

# ---- Plot ----
plot_data <- cohort_scores |>
  filter(cohort_decade %in% cohort_change$cohort_decade, n_obs >= 50) |>
  mutate(round_label = if_else(round == 1, "2014", "2023"))

p <- ggplot(
  plot_data,
  aes(
    x = cohort_decade,
    y = mean_lit,
    color = round_label,
    fill = round_label,
    group = round_label
  )
) +
  geom_ribbon(
    aes(ymin = mean_lit - 1.96 * se, ymax = mean_lit + 1.96 * se),
    alpha = 0.14,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  geom_hline(yintercept = 225, linetype = "dashed", color = "#b91c1c", linewidth = 0.8) +
  scale_color_manual(values = c("2014" = "#1f4e79", "2023" = "#d97706"), name = NULL) +
  scale_fill_manual(values = c("2014" = "#1f4e79", "2023" = "#d97706"), name = NULL) +
  scale_x_continuous(
    breaks = sort(unique(plot_data$cohort_decade)),
    labels = function(x) paste0(x, "s")
  ) +
  labs(
    title = "Chile: The Same Birth Cohorts Score Lower in 2023 Than in 2014",
    x = "Birth decade",
    y = "Mean literacy score",
    caption = "Dashed line = Level 1 ceiling (225)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#1f4e79"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

p

ggsave(file.path(fig_dir, "chile_cohort_trend.pdf"), p, width = 9, height = 6)
ggsave(file.path(fig_dir, "chile_cohort_trend.png"), p, width = 9, height = 6, dpi = 160)

# ---- Alternative y-axis: PL1 share by birth cohort ----
pl1_pvs <- paste0("PL1_PV", 1:10)
for (i in seq_along(LIT_PVS)) {
  chile_coh[[pl1_pvs[i]]] <- as.integer(chile_coh[[LIT_PVS[i]]] <= 225)
}

cohort_pl1 <- pv_group_mean(
  chile_coh,
  pl1_pvs,
  c("cohort_decade", "round", "survey_year"),
  "SPFWT0",
  BRR_WTS
) |>
  rename(pl1_share = mean, n_obs = n) |>
  arrange(cohort_decade, round)

cohort_pl1_change <- cohort_pl1 |>
  select(cohort_decade, round, pl1_share, se, n_obs) |>
  pivot_wider(
    names_from = round,
    values_from = c(pl1_share, se, n_obs),
    names_sep = "_r"
  ) |>
  filter(!is.na(pl1_share_r1), !is.na(pl1_share_r2)) |>
  mutate(
    change = pl1_share_r2 - pl1_share_r1,
    se_change = sqrt(se_r1^2 + se_r2^2),
    direction = if_else(change > 0, "increase", "decrease")
  ) |>
  arrange(cohort_decade)

write_csv(cohort_pl1, file.path(out_dir, "chile_cohort_pl1_shares.csv"))
write_csv(cohort_pl1_change, file.path(out_dir, "chile_cohort_pl1_change.csv"))

cat("\nChile matched birth-decade PL1 share changes:\n")
print(cohort_pl1_change |>
  transmute(
    cohort = paste0(cohort_decade, "s"),
    share_2014 = scales::percent(pl1_share_r1, accuracy = 0.1),
    share_2023 = scales::percent(pl1_share_r2, accuracy = 0.1),
    change_pp = round(change * 100, 1),
    direction
  ))

plot_pl1_data <- cohort_pl1 |>
  filter(cohort_decade %in% cohort_pl1_change$cohort_decade, n_obs >= 50) |>
  mutate(
    round_label = if_else(round == 1, "2014", "2023"),
    ymin = pmax(0, pl1_share - 1.96 * se),
    ymax = pmin(1, pl1_share + 1.96 * se)
  )

p_pl1 <- ggplot(
  plot_pl1_data,
  aes(
    x = cohort_decade,
    y = pl1_share,
    color = round_label,
    fill = round_label,
    group = round_label
  )
) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax),
    alpha = 0.14,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  scale_color_manual(values = c("2014" = "#1f4e79", "2023" = "#d97706"), name = NULL) +
  scale_fill_manual(values = c("2014" = "#1f4e79", "2023" = "#d97706"), name = NULL) +
  scale_x_continuous(
    breaks = sort(unique(plot_pl1_data$cohort_decade)),
    labels = function(x) paste0(x, "s")
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Chile: PL1-VOTS Likelihood Rises Across Birth Cohorts Over Time",
    x = "(\u2190 Older)       Birth decade      (Younger \u2192)",
    y = "Share at Level 1 or below",
    caption = "PL1-VOTS = scoring at or below Level 1 in literacy (<= 225)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#1f4e79"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

p_pl1

ggsave(file.path(fig_dir, "chile_cohort_pl1_trend.pdf"), p_pl1, width = 9, height = 6)
ggsave(file.path(fig_dir, "chile_cohort_pl1_trend.png"), p_pl1, width = 9, height = 6, dpi = 160)


# Just 2014 Values --------------------------------------------------------

p_pl1 <- 
  plot_pl1_data %>% 
  filter(survey_year==2014) %>% 
  ggplot(
  aes(
    x = cohort_decade,
    y = pl1_share,
    color = round_label,
    fill = round_label,
    group = round_label
  )
) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax),
    alpha = 0.14,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  scale_color_manual(values = c("2014" = "#1f4e79", "2023" = "#d97706"), name = NULL) +
  scale_fill_manual(values = c("2014" = "#1f4e79", "2023" = "#d97706"), name = NULL) +
  scale_x_continuous(
    breaks = sort(unique(plot_pl1_data$cohort_decade)),
    labels = function(x) paste0(x, "s")
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Chile: PL1-VOTS Likelihood Rises Across Birth Cohorts Over Time",
    x = "(\u2190 Older)       Birth decade      (Younger \u2192)",
    y = "Share at Level 1 or below",
    caption = "PL1-VOTS = scoring at or below Level 1 in literacy (<= 225)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#1f4e79"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

p_pl1

ggsave(file.path(fig_dir, "chile_cohort_pl1_trend_r1only.pdf"), p_pl1, width = 9, height = 6)
ggsave(file.path(fig_dir, "chile_cohort_pl1_trend_r1only.png"), p_pl1, width = 9, height = 6, dpi = 160)

# ---- Alternative plot: cohort-to-cohort arrows ----
# arrow_df <- cohort_change |>
#   transmute(
#     cohort_decade = cohort_decade,
#     cohort_label = paste0(cohort_decade, "s"),
#     score_2014 = mean_lit_r1,
#     score_2023 = mean_lit_r2,
#     change = change,
#     direction = direction
#   ) |>
#   mutate(
#     cohort_label = factor(
#       cohort_label,
#       levels = paste0(sort(unique(cohort_decade)), "s")
#     ),
#     label_2014 = sprintf("%.0f", score_2014),
#     label_2023 = sprintf("%.0f", score_2023)
#   )
# 
# p_arrow <- ggplot(arrow_df, aes(y = cohort_label)) +
#   geom_vline(xintercept = 225, linetype = "dashed", color = "#b91c1c", linewidth = 0.8) +
#   geom_segment(
#     aes(
#       x = score_2014,
#       xend = score_2023,
#       yend = cohort_label,
#       color = direction
#     ),
#     linewidth = 1.2,
#     arrow = arrow(length = grid::unit(0.16, "inches"), type = "closed")
#   ) +
#   geom_point(aes(x = score_2014), color = "#1f4e79", size = 3) +
#   geom_point(aes(x = score_2023), color = "#d97706", size = 3) +
#   geom_text(
#     aes(x = score_2014, label = label_2014),
#     nudge_y = 0.22,
#     hjust = 1.15,
#     size = 3.6,
#     color = "#1f4e79"
#   ) +
#   geom_text(
#     aes(x = score_2023, label = label_2023),
#     nudge_y = -0.22,
#     hjust = -0.15,
#     size = 3.6,
#     color = "#d97706"
#   ) +
#   scale_color_manual(
#     values = c("decline" = "#7f1d1d", "improvement" = "#166534"),
#     name = NULL
#   ) +
#   labs(
#     title = "Chile: Most Birth Cohorts Move Downward from 2014 to 2023",
#     subtitle = "Each arrow follows the same birth decade from its 2014 score to its 2023 score.",
#     x = "Mean literacy score",
#     y = "Birth decade",
#     caption = paste0(
#       "Source: PIAAC Chile public-use files (2014 and 2023). ",
#       "Means use all 10 plausible values with BRR standard errors. ",
#       "Dashed line = Level 1 ceiling (225)."
#     )
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     plot.title = element_text(face = "bold", color = "#1f4e79"),
#     plot.subtitle = element_text(size = 11),
#     legend.position = "none"
#   )
# 
# p_arrow
# 
# ggsave(file.path(fig_dir, "chile_cohort_arrows.pdf"), p_arrow, width = 9, height = 6)
# ggsave(file.path(fig_dir, "chile_cohort_arrows.png"), p_arrow, width = 9, height = 6, dpi = 160)
# 
# cat("\nSaved:\n")
# cat("  ", file.path(out_dir, "chile_cohort_scores.csv"), "\n")
# cat("  ", file.path(out_dir, "chile_cohort_change.csv"), "\n")
# cat("  ", file.path(fig_dir, "chile_cohort_trend.pdf"), "\n")
# cat("  ", file.path(fig_dir, "chile_cohort_trend.png"), "\n")
# cat("  ", file.path(fig_dir, "chile_cohort_arrows.pdf"), "\n")
# cat("  ", file.path(fig_dir, "chile_cohort_arrows.png"), "\n")
