#!/usr/bin/env Rscript

# 06_pl1_overlap_change.r
# Compare Level 1-or-below shares and counts across overlapping PIAAC countries.
#
# Main slide-11 use case:
#   Compare countries with an earlier PIAAC participation and a 2023 participation,
#   then quantify how many adults are at or below Level 1 in literacy/numeracy
#   in the overlapping country set.
#
# Outputs:
#   02_output/pl1_overlap_country_changes.csv
#   02_output/pl1_overlap_summary_all_both_rounds.csv
#   02_output/pl1_overlap_summary_2023_only.csv
#   Figures/pl1_overlap_change_2023.png
#   Figures/pl1_overlap_change_2023.pdf

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

country_labels <- c(
  AUT = "Austria", BEL = "Belgium", CAN = "Canada", CHL = "Chile",
  CZE = "Czechia", DEU = "Germany", DNK = "Denmark", ESP = "Spain",
  EST = "Estonia", FIN = "Finland", FRA = "France", GBR = "United Kingdom",
  HUN = "Hungary", IRL = "Ireland", ISR = "Israel", ITA = "Italy",
  JPN = "Japan", KOR = "Korea", LTU = "Lithuania", NZL = "New Zealand",
  POL = "Poland", SGP = "Singapore", SVK = "Slovakia", USA = "United States"
)

# ---- Parse file metadata ----
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

pl1_rate <- function(data, pv_prefix, wt) {
  pv_names <- paste0(pv_prefix, 1:10)
  mean(vapply(
    pv_names,
    function(v) weighted.mean(as.integer(data[[v]] <= 225), wt, na.rm = TRUE),
    numeric(1)
  ))
}

calc_country_round <- function(path, country, round, survey_year) {
  d <- read_sav(
    path,
    col_select = any_of(c("SPFWT0", paste0("PVLIT", 1:10), paste0("PVNUM", 1:10)))
  )

  wt <- d$SPFWT0
  pop <- sum(wt, na.rm = TRUE)
  lit_rate <- pl1_rate(d, "PVLIT", wt)
  num_rate <- pl1_rate(d, "PVNUM", wt)

  tibble(
    country = country,
    country_name = unname(country_labels[country]),
    round = round,
    survey_year = survey_year,
    pop = pop,
    lit_rate = lit_rate,
    num_rate = num_rate,
    lit_pop = pop * lit_rate,
    num_pop = pop * num_rate
  )
}

# ---- Load overlapping countries ----
all_files <- list.files(
  data_dir,
  pattern = "\\.sav$",
  ignore.case = TRUE,
  full.names = TRUE
)

meta <- bind_rows(lapply(all_files, parse_filename))

both_rounds <- meta |>
  distinct(country, round) |>
  group_by(country) |>
  summarise(
    has_round_1 = any(round == 1),
    has_round_2 = any(round == 2),
    .groups = "drop"
  ) |>
  filter(has_round_1, has_round_2) |>
  pull(country)

country_rounds <- bind_rows(lapply(
  seq_len(nrow(meta |> filter(country %in% both_rounds, round %in% c(1, 2)))),
  function(i) {
    row <- meta |> filter(country %in% both_rounds, round %in% c(1, 2)) |> slice(i)
    calc_country_round(row$path, row$country, row$round, row$survey_year)
  }
))

country_changes <- country_rounds |>
  select(country, country_name, round, survey_year, pop, lit_rate, num_rate, lit_pop, num_pop) |>
  pivot_wider(
    names_from = round,
    values_from = c(survey_year, pop, lit_rate, num_rate, lit_pop, num_pop),
    names_sep = "_r"
  ) |>
  mutate(
    lit_change_pp = (lit_rate_r2 - lit_rate_r1) * 100,
    num_change_pp = (num_rate_r2 - num_rate_r1) * 100,
    lit_change_millions = (lit_pop_r2 - lit_pop_r1) / 1e6,
    num_change_millions = (num_pop_r2 - num_pop_r1) / 1e6
  ) |>
  arrange(desc(lit_change_pp))

summary_all_both_rounds <- country_changes |>
  summarise(
    countries = n(),
    round1_min_year = min(survey_year_r1),
    round1_max_year = max(survey_year_r1),
    round2_min_year = min(survey_year_r2),
    round2_max_year = max(survey_year_r2),
    pop_r1 = sum(pop_r1),
    lit_pop_r1 = sum(lit_pop_r1),
    num_pop_r1 = sum(num_pop_r1),
    lit_rate_r1 = lit_pop_r1 / pop_r1,
    num_rate_r1 = num_pop_r1 / pop_r1,
    pop_r2 = sum(pop_r2),
    lit_pop_r2 = sum(lit_pop_r2),
    num_pop_r2 = sum(num_pop_r2),
    lit_rate_r2 = lit_pop_r2 / pop_r2,
    num_rate_r2 = num_pop_r2 / pop_r2,
    lit_increase_countries = sum(lit_change_pp > 0, na.rm = TRUE),
    lit_decrease_countries = sum(lit_change_pp < 0, na.rm = TRUE),
    num_increase_countries = sum(num_change_pp > 0, na.rm = TRUE),
    num_decrease_countries = sum(num_change_pp < 0, na.rm = TRUE),
    sample = "All countries with both round 1 and round 2 data"
  )

# Main slide-11 frame: countries with a 2023 round 2 participation.
summary_2023_only <- country_changes |>
  filter(survey_year_r2 == 2023) |>
  summarise(
    countries = n(),
    round1_min_year = min(survey_year_r1),
    round1_max_year = max(survey_year_r1),
    round2_year = 2023,
    pop_r1 = sum(pop_r1),
    lit_pop_r1 = sum(lit_pop_r1),
    num_pop_r1 = sum(num_pop_r1),
    lit_rate_r1 = lit_pop_r1 / pop_r1,
    num_rate_r1 = num_pop_r1 / pop_r1,
    pop_r2 = sum(pop_r2),
    lit_pop_r2 = sum(lit_pop_r2),
    num_pop_r2 = sum(num_pop_r2),
    lit_rate_r2 = lit_pop_r2 / pop_r2,
    num_rate_r2 = num_pop_r2 / pop_r2,
    lit_increase_countries = sum(lit_change_pp > 0, na.rm = TRUE),
    lit_decrease_countries = sum(lit_change_pp < 0, na.rm = TRUE),
    num_increase_countries = sum(num_change_pp > 0, na.rm = TRUE),
    num_decrease_countries = sum(num_change_pp < 0, na.rm = TRUE),
    sample = "Countries with an earlier PIAAC round and a 2023 round"
  )

write_csv(country_changes, file.path(out_dir, "pl1_overlap_country_changes.csv"))
write_csv(summary_all_both_rounds, file.path(out_dir, "pl1_overlap_summary_all_both_rounds.csv"))
write_csv(summary_2023_only, file.path(out_dir, "pl1_overlap_summary_2023_only.csv"))

cat("\nSlide-11 summary: 2023 overlap countries only\n")
print(summary_2023_only |>
  transmute(
    sample,
    countries,
    earlier_round_years = paste0(round1_min_year, "-", round1_max_year),
    pop_millions_earlier = round(pop_r1 / 1e6, 1),
    lit_pl1_millions_earlier = round(lit_pop_r1 / 1e6, 1),
    num_pl1_millions_earlier = round(num_pop_r1 / 1e6, 1),
    lit_share_pct_earlier = round(lit_rate_r1 * 100, 1),
    num_share_pct_earlier = round(num_rate_r1 * 100, 1),
    pop_millions_2023 = round(pop_r2 / 1e6, 1),
    lit_pl1_millions_2023 = round(lit_pop_r2 / 1e6, 1),
    num_pl1_millions_2023 = round(num_pop_r2 / 1e6, 1),
    lit_share_pct_2023 = round(lit_rate_r2 * 100, 1),
    num_share_pct_2023 = round(num_rate_r2 * 100, 1),
    lit_increase_countries,
    lit_decrease_countries,
    num_increase_countries,
    num_decrease_countries
  ))

cat("\nLargest country changes in literacy PL1 share (percentage points):\n")
print(country_changes |>
  filter(survey_year_r2 == 2023) |>
  transmute(
    country_name,
    earlier_year = survey_year_r1,
    lit_change_pp = round(lit_change_pp, 1),
    num_change_pp = round(num_change_pp, 1)
  ) |>
  arrange(desc(lit_change_pp)))

# ---- Plot: pp change by country for the 2023 overlap sample ----
plot_df <- country_changes |>
  filter(survey_year_r2 == 2023) |>
  select(country_name, lit_change_pp, num_change_pp) |>
  pivot_longer(
    cols = c(lit_change_pp, num_change_pp),
    names_to = "domain",
    values_to = "change_pp"
  ) |>
  mutate(
    domain = recode(
      domain,
      lit_change_pp = "Literacy",
      num_change_pp = "Numeracy"
    )
  )

country_order <- country_changes |>
  filter(survey_year_r2 == 2023) |>
  arrange(lit_change_pp) |>
  pull(country_name)

plot_df <- mutate(plot_df, country_name = factor(country_name, levels = country_order))

p <- ggplot(plot_df, aes(x = country_name, y = change_pp, fill = domain)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(
    values = c("Literacy" = "#1f4e79", "Numeracy" = "#7a9a01"),
    name = NULL
  ) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = "Change in Adults at or Below Level 1: Cycle 1 vs Cycle 2",
    subtitle = "Countries with an earlier PIAAC participation and a 2023 participation.",
    x = NULL,
    y = "Increase in share at or below Level 1 \u2192",
    caption = paste0(
      "Point estimates use SPFWT0 weights and average across 10 plausible values. ",
      "Earlier-round years range from 2012 to 2014."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#1f4e79"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

p

ggsave(file.path(fig_dir, "pl1_overlap_change_2023.pdf"), p, width = 10, height = 8)
ggsave(file.path(fig_dir, "pl1_overlap_change_2023.png"), p, width = 10, height = 8, dpi = 150)

cat("\nSaved:\n")
cat("  ", file.path(out_dir, "pl1_overlap_country_changes.csv"), "\n")
cat("  ", file.path(out_dir, "pl1_overlap_summary_all_both_rounds.csv"), "\n")
cat("  ", file.path(out_dir, "pl1_overlap_summary_2023_only.csv"), "\n")
cat("  ", file.path(fig_dir, "pl1_overlap_change_2023.pdf"), "\n")
cat("  ", file.path(fig_dir, "pl1_overlap_change_2023.png"), "\n")
