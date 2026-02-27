# 03_illiteracy_rates.r
# Cross-country functional illiteracy rates (PVLIT1 ≤ 225 = at or below Level 1).
# Simplified first pass: PV1 only, weighted means, no BRR.
#
# Inputs:  02_output/piaac_clean.rds
# Outputs: 02_output/illiteracy_rates.rds
#          Figures/illiteracy_rates.pdf

set.seed(20260227)

library(dplyr)
library(ggplot2)
library(here)

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

# ---- Functional illiteracy: PVLIT1 ≤ 225 (Level 1 ceiling) ----
r1 <- mutate(r1, illiterate = as.integer(PVLIT1 <= 225))

illiteracy_rates <- r1 |>
  group_by(country) |>
  summarise(
    illiteracy_rate = weighted.mean(illiterate, SPFWT0, na.rm = TRUE) * 100,
    mean_lit        = weighted.mean(PVLIT1, SPFWT0, na.rm = TRUE),
    n_obs           = n(),
    .groups         = "drop"
  ) |>
  arrange(illiteracy_rate)

saveRDS(illiteracy_rates, file.path(out_dir, "illiteracy_rates.rds"))

cat("\nFunctional illiteracy rates (%, PVLIT1 ≤ 225):\n")
print(illiteracy_rates |> arrange(desc(illiteracy_rate)), n = Inf)

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
print(check |> select(country, benchmark, illiteracy_rate, diff, n_obs))

max_diff <- max(abs(check$diff), na.rm = TRUE)
if (max_diff > 5) {
  cat(sprintf("\nWARNING: Largest benchmark deviation = %.1f pp (expected < 5 pp)\n", max_diff))
} else {
  cat(sprintf("\nOK: All benchmark deviations ≤ %.1f pp\n", max_diff))
}

# ---- Bar chart ----
highlight_countries <- c("USA", "DEU", "ESP", "CHL", "NOR", "FIN", "SWE")

plot_data <- illiteracy_rates |>
  mutate(
    is_highlight = country %in% highlight_countries,
    bar_color    = if_else(is_highlight, "#f2a900", "#525252"),
    country_fac  = factor(country, levels = illiteracy_rates$country)
  )

p_illiteracy <- ggplot(plot_data,
                       aes(x = country_fac, y = illiteracy_rate, fill = bar_color)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.0f%%", illiteracy_rate)),
            hjust = -0.15, size = 3.0, color = "#222222") +
  coord_flip(clip = "off") +
  scale_fill_identity() +
  scale_y_continuous(limits = c(0, max(plot_data$illiteracy_rate) * 1.12),
                     labels = scales::label_number(suffix = "%")) +
  labs(
    title    = "Functional Illiteracy Across PIAAC Countries: cy1 (~2013)",
    subtitle = "Share of adults scoring \u2264225 (Level 1 ceiling).\nGold = highlighted countries (USA not in round 1).",
    x        = NULL,
    y        = "Functional illiteracy rate",
    caption  = paste0("Source: PIAAC cy1 PUF. PV1 only, SPFWT0 weights.\n",
                      "\u2264225 = at or below Level 1 (functionally illiterate).")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#012169", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.y   = element_text(size = 9),
    plot.margin   = margin(5, 40, 5, 5)
  )

fig_path <- file.path(fig_dir, "illiteracy_rates.pdf")
ggsave(fig_path, p_illiteracy, width = 10, height = 8)
cat(sprintf("\nSaved figure: %s\n", fig_path))
cat("Done: illiteracy rates complete.\n")
