#!/usr/bin/env Rscript

# 08b_education_budget_trends.r
# Companion figure for Article 2: annual per-pupil spending used in the article.
#
# This script intentionally does not edit 08_education_cost.r. It reads the
# curated spending file produced by that script so the graph and the text use
# the same unit: annual primary/secondary spending per pupil, USD PPP / roughly
# comparable USD.
#
# Outputs:
#   02_output/education_spending_per_pupil_article.csv
#   Figures/education_spending_per_pupil_article.png
#   Figures/education_spending_per_pupil_article.pdf

set.seed(20260521)

library(tidyverse)
library(here)

root_candidates <- c(here(), here("piaac"))
piaac_root <- root_candidates[dir.exists(file.path(root_candidates, "01_scripts"))][1]
if (is.na(piaac_root)) stop("Could not locate PIAAC analysis root")

out_dir <- file.path(piaac_root, "02_output")
fig_dir <- file.path(piaac_root, "Figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Same curated article inputs as 08_education_cost.r. That script writes this
# file as 02_output/oecd_spending.csv before computing the cost-per-literate
# figures. If the file is missing, run:
#   Rscript piaac/01_scripts/08_education_cost.r
spending_path <- file.path(out_dir, "oecd_spending.csv")
if (!file.exists(spending_path)) {
  stop(
    "Missing 02_output/oecd_spending.csv. Run 01_scripts/08_education_cost.r first ",
    "so this figure uses the exact spending inputs from the cost script."
  )
}

country_lookup <- tribble(
  ~country, ~country_name,
  "USA", "United States",
  "GBR", "UK",
  "FRA", "France"
)

plot_data <- readr::read_csv(spending_path, show_col_types = FALSE) |>
  left_join(country_lookup, by = "country") |>
  mutate(country_name = if_else(is.na(country_name), country, country_name)) |>
  mutate(
    country_name = factor(country_name, levels = c("United States", "UK", "France")),
    label_value = if_else(year == max(year), scales::dollar(spend_per_pupil_usd_ppp, accuracy = 100), NA_character_)
  )

readr::write_csv(plot_data, file.path(out_dir, "education_spending_per_pupil_article.csv"))

usa_first <- plot_data |>
  filter(country == "USA", year == min(year[country == "USA"])) |>
  pull(spend_per_pupil_usd_ppp)

usa_last <- plot_data |>
  filter(country == "USA", year == max(year[country == "USA"])) |>
  pull(spend_per_pupil_usd_ppp)

message(sprintf(
  "U.S. annual per-pupil spending: $%s in 1970 -> $%s in 2021 (%.1fx)",
  scales::comma(usa_first),
  scales::comma(usa_last),
  usa_last / usa_first
))

primary_blue <- "#012169"
accent_gold <- "#b7791f"
accent_green <- "#15803d"
line_grey <- "#b8b8b8"

pal <- c(
  "United States" = primary_blue,
  "UK" = accent_green,
  "France" = accent_gold
)

label_data <- plot_data |>
  group_by(country_name) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(
    label_y = case_when(
      country_name == "UK" ~ spend_per_pupil_usd_ppp + 650,
      country_name == "France" ~ spend_per_pupil_usd_ppp - 650,
      TRUE ~ spend_per_pupil_usd_ppp
    )
  )

p <- ggplot(plot_data, aes(year, spend_per_pupil_usd_ppp, group = country_name)) +
  geom_line(color = line_grey, linewidth = 1.1) +
  geom_point(color = line_grey, size = 2.2) +
  geom_line(aes(color = country_name), linewidth = 1.25) +
  geom_point(aes(color = country_name), size = 2.5) +
  geom_text(
    data = label_data,
    aes(
      y = label_y,
      label = paste0(country_name, "\n", scales::dollar(spend_per_pupil_usd_ppp, accuracy = 100))
    ),
    hjust = -0.05,
    vjust = 0.5,
    lineheight = 0.95,
    size = 3.6,
    show.legend = FALSE
  ) +
  annotate(
    "label",
    x = 1972,
    y = usa_first,
    label = "U.S. 1970\n$7,200",
    hjust = 0,
    vjust = 1.25,
    color = primary_blue,
    fill = "white",
    linewidth = 0.2,
    size = 3.5
  ) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_continuous(
    breaks = c(1970, 1980, 1990, 2000, 2010, 2020),
    limits = c(1969, 2028),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(accuracy = 1000),
    limits = c(0, 20000),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = NULL,
    caption = stringr::str_wrap(
      paste(
        "Sources: Article spending inputs read from 02_output/oecd_spending.csv, produced by 01_scripts/08_education_cost.r.",
        "U.S. historical values are deflated to roughly 2021 dollars using NCES/BLS; recent values are OECD Education at a Glance USD PPP figures.",
        "The series is a back-of-envelope comparability device, not a complete production-function estimate."
      ),
      width = 130
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = primary_blue, size = 17),
    plot.subtitle = element_text(color = "#4f4f4f"),
    plot.caption = element_text(color = "#666666", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p

ggsave(file.path(fig_dir, "education_spending_per_pupil_article.png"),
       p, width = 10, height = 6.2, dpi = 180)
ggsave(file.path(fig_dir, "education_spending_per_pupil_article.pdf"),
       p, width = 10, height = 6.2)

message("Saved: 02_output/education_spending_per_pupil_article.csv")
message("Saved: Figures/education_spending_per_pupil_article.png/.pdf")
