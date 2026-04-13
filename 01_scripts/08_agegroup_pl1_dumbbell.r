#!/usr/bin/env Rscript

# 08_agegroup_pl1_dumbbell.r
# Build a presentation-friendly pooled age-group PL1 chart for the
# 23 participants observed in both Cycle 1 and Cycle 2 (2023).
#
# Outputs:
#   02_output/agegroup_pl1_overlap_pooled.csv
#   Figures/agegroup_pl1_dumbbell.png
#   Figures/agegroup_pl1_dumbbell.pdf

set.seed(20260412)

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

# ---- Survey-year lookups ----
cy1_year_lookup <- c(
  AUT = 2012L, BEL = 2012L, CAN = 2012L, CZE = 2012L, DNK = 2012L, EST = 2012L,
  FIN = 2012L, FRA = 2012L, DEU = 2012L, IRL = 2012L, ITA = 2012L, JPN = 2012L,
  KOR = 2012L, LTU = 2012L, NLD = 2012L, NOR = 2012L, POL = 2012L, SVK = 2012L,
  ESP = 2012L, SWE = 2012L, GBR = 2012L,
  CHL = 2014L, ECU = 2014L, HUN = 2014L, ISR = 2014L, MEX = 2014L, NZL = 2014L,
  PER = 2014L, RUS = 2014L, SGP = 2014L, TUR = 2014L,
  GRC = 2017L, KAZ = 2017L, SVN = 2017L
)

cy2_year_lookup <- c(
  AUT = 2014L,
  BEL = 2023L, CAN = 2023L, CHL = 2023L, CZE = 2023L, DEU = 2023L, DNK = 2023L,
  ESP = 2023L, EST = 2023L, FIN = 2023L, FRA = 2023L, GBR = 2023L, HUN = 2023L,
  IRL = 2023L, ISR = 2023L, ITA = 2023L, JPN = 2023L, KOR = 2023L, LTU = 2023L,
  NZL = 2023L, POL = 2023L, SGP = 2023L, SVK = 2023L, USA = 2023L
)

round_override <- list(
  USA = c(`2012` = 1L, `2023` = 2L, `2017` = 3L)
)

age_labels <- c(
  `1` = "16-24",
  `2` = "25-34",
  `3` = "35-44",
  `4` = "45-54",
  `5` = "55-65"
)

parse_filename <- function(filepath) {
  fname <- basename(filepath)
  country <- toupper(substr(fname, 4, 6))
  raw_round <- as.integer(substr(fname, 8, 8))

  year_suffix <- regmatches(
    fname,
    regexpr("_(\\d{4})\\.sav$", fname, ignore.case = TRUE)
  )

  if (length(year_suffix) > 0 && nzchar(year_suffix)) {
    survey_year <- as.integer(substr(year_suffix, 2, 5))
  } else {
    survey_year <- if (raw_round == 1L) {
      unname(cy1_year_lookup[country])
    } else {
      unname(cy2_year_lookup[country])
    }
  }

  round_num <- if (country %in% names(round_override)) {
    unname(round_override[[country]][as.character(survey_year)])
  } else {
    raw_round
  }

  tibble(
    path = filepath,
    country = country,
    round = round_num,
    survey_year = survey_year
  )
}

all_files <- list.files(
  data_dir,
  pattern = "\\.sav$",
  ignore.case = TRUE,
  full.names = TRUE
)

meta <- bind_rows(lapply(all_files, parse_filename))

overlap_countries <- meta |>
  distinct(country, round, survey_year) |>
  group_by(country) |>
  summarise(
    has_round_1 = any(round == 1),
    has_cycle2_2023 = any(round == 2 & survey_year == 2023),
    .groups = "drop"
  ) |>
  filter(has_round_1, has_cycle2_2023) |>
  pull(country)

load_one <- function(path, country, round, survey_year) {
  d <- read_sav(
    path,
    col_select = any_of(c(
      "SPFWT0", "AGEG10LFS", "AGEG10LFS_T",
      paste0("PVLIT", 1:10),
      paste0("PVNUM", 1:10)
    ))
  )

  if (!"AGEG10LFS" %in% names(d) && "AGEG10LFS_T" %in% names(d)) {
    d <- rename(d, AGEG10LFS = AGEG10LFS_T)
  }

  d |>
    mutate(
      country = country,
      round = round,
      survey_year = survey_year
    )
}

pooled_data <- pmap_dfr(
  meta |>
    filter(country %in% overlap_countries, round == 1 | (round == 2 & survey_year == 2023)),
  load_one
)

pooled_agegroup <- bind_rows(lapply(1:10, function(k) {
  lit <- paste0("PVLIT", k)
  num <- paste0("PVNUM", k)

  pooled_data |>
    filter(!is.na(AGEG10LFS), !is.na(SPFWT0), SPFWT0 > 0) |>
    group_by(round, AGEG10LFS) |>
    summarise(
      pop = sum(SPFWT0, na.rm = TRUE),
      literacy = weighted.mean(as.integer(.data[[lit]] <= 225), SPFWT0, na.rm = TRUE),
      numeracy = weighted.mean(as.integer(.data[[num]] <= 225), SPFWT0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(k = k)
})) |>
  group_by(round, AGEG10LFS) |>
  summarise(
    pop = max(pop),
    literacy = mean(literacy),
    numeracy = mean(numeracy),
    .groups = "drop"
  ) |>
  mutate(
    cycle = if_else(round == 1, "Cycle 1", "Cycle 2"),
    age_band = age_labels[as.character(AGEG10LFS)]
  )

write_csv(
  pooled_agegroup,
  file.path(out_dir, "agegroup_pl1_overlap_pooled.csv")
)

cat("\nPooled overlap-sample age-group PL1 rates:\n")
print(
  pooled_agegroup |>
    mutate(
      pop_m = round(pop / 1e6, 1),
      literacy_pct = round(literacy * 100, 1),
      numeracy_pct = round(numeracy * 100, 1)
    ) |>
    select(cycle, age_band, pop_m, literacy_pct, numeracy_pct),
  n = Inf
)

# ---- Dumbbell / arrow plot ----
plot_df <- pooled_agegroup |>
  select(cycle, age_band, literacy, numeracy) |>
  pivot_longer(
    cols = c(literacy, numeracy),
    names_to = "domain",
    values_to = "share"
  ) |>
  mutate(
    domain = recode(domain, literacy = "Literacy", numeracy = "Numeracy"),
    share_pct = share * 100,
    age_band = factor(age_band, levels = rev(unname(age_labels)))
  )

segments_df <- plot_df |>
  select(domain, age_band, cycle, share_pct) |>
  pivot_wider(names_from = cycle, values_from = share_pct)

p <- ggplot() +
  geom_segment(
    data = segments_df,
    aes(
      x = `Cycle 1`,
      xend = `Cycle 2`,
      y = age_band,
      yend = age_band
    ),
    linewidth = 1.2,
    color = "#6b7280",
  ) +
  geom_point(
    data = plot_df |> filter(cycle == "Cycle 1"),
    aes(x = share_pct, y = age_band),
    size = 3.4,
    color = "#1f4e79"
  ) +
  geom_point(
    data = plot_df |> filter(cycle == "Cycle 2"),
    aes(x = share_pct, y = age_band),
    size = 3.8,
    color = "#d97706"
  ) +
  geom_text(
    data = plot_df |> filter(cycle == "Cycle 1"),
    aes(x = share_pct, y = age_band, label = sprintf("%.0f%%", share_pct)),
    color = "#1f4e79",
    hjust = 1.15,
    nudge_y = 0.22,
    size = 3.5
  ) +
  geom_text(
    data = plot_df |> filter(cycle == "Cycle 2"),
    aes(x = share_pct, y = age_band, label = sprintf("%.0f%%", share_pct)),
    color = "#d97706",
    hjust = -0.15,
    nudge_y = 0.22,
    size = 3.5
  ) +
  geom_point(
    data = plot_df |> filter(cycle %in% c("Cycle 1", "Cycle 2")),
    aes(x = share_pct, y = age_band, color = cycle),
    size = 3.6
  ) +
  scale_color_manual(
    values = c("Cycle 1" = "#1f4e79", "Cycle 2" = "#d97706"),
    name = NULL
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(~domain, ncol = 2) +
  scale_x_continuous(
    labels = label_number(suffix = "%"),
    limits = c(10, 38)
  ) +
  labs(
    title = "Even Within Age Groups, PL1-VOTS Rise from Cycle 1 to Cycle 2",
    x = "Share at Level 1 or below",
    y = NULL,
    caption = "Point estimates use SPFWT0 weights and average across 10 plausible values."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#1f4e79"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

p

ggsave(file.path(fig_dir, "agegroup_pl1_dumbbell.pdf"), p, width = 10, height = 6.5)
ggsave(file.path(fig_dir, "agegroup_pl1_dumbbell.png"), p, width = 10, height = 6.5, dpi = 170)

cat("\nSaved:\n")
cat("  ", file.path(out_dir, "agegroup_pl1_overlap_pooled.csv"), "\n")
cat("  ", file.path(fig_dir, "agegroup_pl1_dumbbell.pdf"), "\n")
cat("  ", file.path(fig_dir, "agegroup_pl1_dumbbell.png"), "\n")
