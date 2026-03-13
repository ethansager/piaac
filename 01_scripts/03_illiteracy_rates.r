# 03_illiteracy_rates.r
# Cross-country functional illiteracy rates (PVLIT_k ≤ 225 = at or below Level 1).
# Uses proper PV+BRR variance estimation (Rubin's rules) via pv_group_mean().
# Illiteracy indicator is derived per PV before averaging (methodologically correct).
#
# Inputs:  02_output/piaac_clean.rds
# Outputs: 02_output/illiteracy_rates.rds
#          02_output/illiteracy_by_gender.rds
#          02_output/illiteracy_by_nativespeaker.rds
#          02_output/illiteracy_by_imgen.rds
#          Figures/illiteracy_rates.pdf
#          Figures/illiteracy_rates_gender.pdf
#          Figures/illiteracy_rates_nativespeaker.pdf
#          Figures/illiteracy_rates_imgen.pdf

set.seed(20260227)

library(dplyr)
library(ggplot2)
library(here)

source(here("01_scripts/00_helpers.r"))

# ---- Setup ----
out_dir <- here("02_output")
fig_dir <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load ----
piaac <- readRDS(file.path(out_dir, "piaac_clean.rds"))

# ---- Filter: round 1 (more countries), valid observations ----
r1 <- filter(piaac, round == 1, !is.na(PVLIT1), !is.na(SPFWT0), SPFWT0 > 0)

cat(sprintf("Round 1: %s obs across %d countries\n",
            format(nrow(r1), big.mark = ","),
            n_distinct(r1$country)))

# ---- Per-PV binary indicators: illit_pv_k = 1 if PVLIT_k ≤ 225 ----
# Must derive the indicator per PV before averaging — do NOT average scores first.
illit_pv_cols <- paste0("illit_pv", 1:10)
for (k in 1:10) {
  r1[[illit_pv_cols[k]]] <- as.integer(r1[[paste0("PVLIT", k)]] <= 225)
}

# ---- Illiteracy rates: PV+BRR estimation ----
cat("Computing illiteracy rates (PV+BRR)...\n")
illiteracy_rates <- pv_group_mean(r1, illit_pv_cols, "country", "SPFWT0", BRR_WTS) |>
  mutate(
    illiteracy_rate = mean * 100,
    se_rate         = se   * 100
  ) |>
  arrange(illiteracy_rate)

# Also compute mean literacy score per country (for cross-check)
mean_lit_r1 <- pv_group_mean(r1, LIT_PVS, "country", "SPFWT0", BRR_WTS) |>
  rename(mean_lit = mean, se_lit = se) |>
  select(country, mean_lit, se_lit)

illiteracy_rates <- left_join(illiteracy_rates, mean_lit_r1, by = "country")

saveRDS(illiteracy_rates, file.path(out_dir, "illiteracy_rates.rds"))

cat("\nFunctional illiteracy rates (%, PVLIT_k ≤ 225, PV+BRR):\n")
print(illiteracy_rates |> arrange(desc(illiteracy_rate)) |>
      select(country, illiteracy_rate, se_rate, mean_lit, n), n = Inf)

# ---- Sanity check vs email benchmarks ----
# From emails.txt: US ~28%, DEU ~22.5%, ESP ~31%, CHL ~53.4%, FIN/SWE ~12%
# Note: USA not in round 1; DEU/ESP/CHL/FIN/SWE should match
benchmarks <- tibble::tibble(
  country   = c("DEU", "ESP", "CHL", "FIN", "SWE"),
  benchmark = c( 22.5,  31.0,  53.4,  12.0,  12.0)
)

check <- left_join(benchmarks, illiteracy_rates, by = "country") |>
  mutate(diff = illiteracy_rate - benchmark)

cat("\n--- Sanity checks vs email benchmarks ---\n")
print(check |> select(country, benchmark, illiteracy_rate, se_rate, diff, n))

max_diff <- max(abs(check$diff), na.rm = TRUE)
if (max_diff > 5) {
  cat(sprintf("\nWARNING: Largest benchmark deviation = %.1f pp (expected < 5 pp)\n", max_diff))
} else {
  cat(sprintf("\nOK: All benchmark deviations ≤ %.1f pp\n", max_diff))
}

# ---- Helper: build illiteracy bar chart ----
make_illiteracy_plot <- function(plot_df, title_extra = "", country_order = NULL) {
  highlight_countries <- c("USA", "DEU", "ESP", "CHL", "NOR", "FIN", "SWE")

  if (is.null(country_order)) country_order <- plot_df$country[order(plot_df$illiteracy_rate)]

  plot_df <- plot_df |>
    mutate(
      is_highlight = country %in% highlight_countries,
      bar_color    = if_else(is_highlight, "#f2a900", "#525252"),
      country_fac  = factor(country, levels = country_order)
    )

  y_max <- max(plot_df$illiteracy_rate + 1.96 * plot_df$se_rate, na.rm = TRUE)

  ggplot(plot_df, aes(x = country_fac, y = illiteracy_rate, fill = bar_color)) +
    geom_col(width = 0.75) +
    geom_errorbar(
      aes(ymin = illiteracy_rate - 1.96 * se_rate,
          ymax = illiteracy_rate + 1.96 * se_rate),
      width = 0.35, color = "#222222", linewidth = 0.5
    ) +
    geom_text(aes(label = sprintf("%.0f%%", illiteracy_rate)),
              hjust = -0.15, size = 3.0, color = "#222222") +
    coord_flip(clip = "off") +
    scale_fill_identity() +
    scale_y_continuous(limits = c(0, y_max * 1.15),
                       labels = scales::label_number(suffix = "%")) +
    labs(
      title    = paste0("Functional Illiteracy Across PIAAC Countries: cy1", title_extra),
      subtitle = paste0("Share of adults scoring \u2264225 (Level 1 ceiling). ",
                        "Error bars = ±1.96 SE (PV+BRR).\n",
                        "Gold = highlighted countries (USA not in round 1)."),
      x        = NULL,
      y        = "Functional illiteracy rate",
      caption  = paste0("Source: PIAAC cy1 PUF. Per-PV binary indicators averaged with BRR SEs ",
                        "(Rubin's rules), SPFWT0 weights.\n",
                        "\u2264225 = at or below Level 1 (functionally illiterate).")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", color = "#012169", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.text.y   = element_text(size = 9),
      plot.margin   = margin(5, 40, 5, 5)
    )
}

# ---- Main bar chart ----
country_order <- illiteracy_rates$country[order(illiteracy_rates$illiteracy_rate)]
p_illiteracy <- make_illiteracy_plot(illiteracy_rates, country_order = country_order)

fig_path <- file.path(fig_dir, "illiteracy_rates.pdf")
ggsave(fig_path, p_illiteracy, width = 10, height = 8)
ggsave(file.path(fig_dir, "illiteracy_rates.png"), p_illiteracy, width = 10, height = 8, dpi = 150)
cat(sprintf("\nSaved figure: %s\n", fig_path))

# ============================================================
# Demographic breakdowns
# ============================================================

# ---- Gender breakdown ----
cat("\nComputing illiteracy by gender (PV+BRR)...\n")
r1_gender <- filter(r1, !is.na(GENDER_R))
for (k in 1:10) {
  r1_gender[[illit_pv_cols[k]]] <- as.integer(r1_gender[[paste0("PVLIT", k)]] <= 225)
}

illiteracy_gender <- pv_group_mean(
  r1_gender, illit_pv_cols, c("country", "GENDER_R"), "SPFWT0", BRR_WTS
) |>
  mutate(
    illiteracy_rate = mean * 100,
    se_rate         = se   * 100,
    gender_label    = if_else(GENDER_R == 1, "Male", "Female")
  ) |>
  arrange(country, GENDER_R)

saveRDS(illiteracy_gender, file.path(out_dir, "illiteracy_by_gender.rds"))

# Plot: side-by-side male vs female comparison
p_gender <- illiteracy_gender |>
  mutate(country_fac = factor(country, levels = country_order)) |>
  ggplot(aes(x = country_fac, y = illiteracy_rate, fill = gender_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(
    aes(ymin = illiteracy_rate - 1.96 * se_rate,
        ymax = illiteracy_rate + 1.96 * se_rate),
    position = position_dodge(width = 0.75),
    width = 0.35, color = "#222222", linewidth = 0.4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("Male" = "#012169", "Female" = "#f2a900"),
    name   = "Gender"
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    title    = "Functional Illiteracy by Gender: cy1",
    subtitle = paste0("Share of adults scoring \u2264225 (Level 1 ceiling). ",
                      "Error bars = \u00b11.96 SE (PV+BRR)."),
    x        = NULL,
    y        = "Functional illiteracy rate",
    caption  = paste0("Source: PIAAC cy1 PUF. Per-PV binary indicators averaged with BRR SEs ",
                      "(Rubin\u2019s rules), SPFWT0 weights.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", color = "#012169", size = 13),
    plot.subtitle   = element_text(size = 10),
    legend.position = "bottom",
    axis.text.y     = element_text(size = 8)
  )
ggsave(file.path(fig_dir, "illiteracy_rates_gender.pdf"), p_gender, width = 10, height = 10)
ggsave(file.path(fig_dir, "illiteracy_rates_gender.png"), p_gender, width = 10, height = 10, dpi = 150)
cat(sprintf("Saved figure: %s\n", file.path(fig_dir, "illiteracy_rates_gender.pdf")))

# ---- Native speaker breakdown ----
if ("NATIVESPEAKER" %in% names(r1)) {
  cat("\nComputing illiteracy by native speaker status (PV+BRR)...\n")
  r1_ns <- filter(r1, !is.na(NATIVESPEAKER), NATIVESPEAKER %in% c(1, 2))
  for (k in 1:10) {
    r1_ns[[illit_pv_cols[k]]] <- as.integer(r1_ns[[paste0("PVLIT", k)]] <= 225)
  }

  illiteracy_ns <- pv_group_mean(
    r1_ns, illit_pv_cols, c("country", "NATIVESPEAKER"), "SPFWT0", BRR_WTS
  ) |>
    mutate(
      illiteracy_rate  = mean * 100,
      se_rate          = se   * 100,
      nativespeaker_label = if_else(NATIVESPEAKER == 1, "Native speaker", "Non-native speaker")
    ) |>
    arrange(country, NATIVESPEAKER)

  saveRDS(illiteracy_ns, file.path(out_dir, "illiteracy_by_nativespeaker.rds"))

  p_ns <- illiteracy_ns |>
    filter(nativespeaker_label == "Non-native speaker") |>
    make_illiteracy_plot(title_extra = " — Non-native speakers", country_order = country_order)
  ggsave(file.path(fig_dir, "illiteracy_rates_nativespeaker.pdf"), p_ns, width = 10, height = 8)
  ggsave(file.path(fig_dir, "illiteracy_rates_nativespeaker.png"), p_ns, width = 10, height = 8, dpi = 150)
  cat(sprintf("Saved figure: %s\n", file.path(fig_dir, "illiteracy_rates_nativespeaker.pdf")))
} else {
  cat("NATIVESPEAKER not found in data; skipping native speaker breakdown.\n")
}

# ---- Immigration generation breakdown ----
if ("IMGEN" %in% names(r1)) {
  cat("\nComputing illiteracy by immigration generation (PV+BRR)...\n")
  r1_imgen <- filter(r1, !is.na(IMGEN))
  for (k in 1:10) {
    r1_imgen[[illit_pv_cols[k]]] <- as.integer(r1_imgen[[paste0("PVLIT", k)]] <= 225)
  }

  illiteracy_imgen <- pv_group_mean(
    r1_imgen, illit_pv_cols, c("country", "IMGEN"), "SPFWT0", BRR_WTS
  ) |>
    mutate(
      illiteracy_rate = mean * 100,
      se_rate         = se   * 100
    ) |>
    arrange(country, IMGEN)

  saveRDS(illiteracy_imgen, file.path(out_dir, "illiteracy_by_imgen.rds"))

  p_imgen <- illiteracy_imgen |>
    filter(IMGEN == 1) |>
    make_illiteracy_plot(title_extra = " — 1st generation immigrants", country_order = country_order)
  ggsave(file.path(fig_dir, "illiteracy_rates_imgen.pdf"), p_imgen, width = 10, height = 8)
  ggsave(file.path(fig_dir, "illiteracy_rates_imgen.png"), p_imgen, width = 10, height = 8, dpi = 150)
  cat(sprintf("Saved figure: %s\n", file.path(fig_dir, "illiteracy_rates_imgen.pdf")))
} else {
  cat("IMGEN not found in data; skipping immigration generation breakdown.\n")
}

cat("Done: illiteracy rates complete.\n")
