#!/usr/bin/env Rscript

# 06b_pl1_overlap_compare_oecd.r
# Compare Figure 1 country changes from 06_pl1_overlap_change.r with
# OECD Annex A.3.2 published changes.
#
# Inputs:
#   02_output/pl1_overlap_country_changes.csv
#   docs/owiypl.xlsx
# Outputs:
#   02_output/pl1_overlap_compare_oecd.csv
#   02_output/pl1_overlap_compare_oecd_summary.csv

set.seed(20260411)

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(here)

root_candidates <- c(here(), here("piaac"))
piaac_root <- root_candidates[dir.exists(file.path(root_candidates, "01_scripts"))][1]
if (is.na(piaac_root)) {
  stop("Could not locate the PIAAC analysis root. Checked: . and ./piaac")
}

out_dir <- file.path(piaac_root, "02_output")
docs_dir <- file.path(piaac_root, "docs")

figure_path <- file.path(out_dir, "pl1_overlap_country_changes.csv")
oecd_path <- file.path(docs_dir, "owiypl.xlsx")

if (!file.exists(figure_path)) {
  stop("Missing Figure 1 data: ", figure_path)
}
if (!file.exists(oecd_path)) {
  stop("Missing OECD workbook: ", oecd_path)
}

read_oecd_a32 <- function(sheet, domain) {
  raw <- read_excel(oecd_path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")

  tibble(
    oecd_name = as.character(raw[[1]]),
    oecd_year_r1 = as.character(raw[[2]]),
    oecd_rate_r1 = suppressWarnings(as.numeric(raw[[3]])),
    oecd_se_r1 = suppressWarnings(as.numeric(raw[[4]])),
    oecd_year_r2 = as.character(raw[[11]]),
    oecd_rate_r2 = suppressWarnings(as.numeric(raw[[12]])),
    oecd_se_r2 = suppressWarnings(as.numeric(raw[[13]])),
    oecd_change_pp = suppressWarnings(as.numeric(raw[[20]])),
    oecd_change_se = suppressWarnings(as.numeric(raw[[21]])),
    domain = domain
  ) |>
    filter(
      !is.na(oecd_name),
      !is.na(oecd_change_pp),
      !is.na(oecd_rate_r1),
      !is.na(oecd_rate_r2)
    )
}

oecd <- bind_rows(
  read_oecd_a32("A.3.2 (L)", "literacy"),
  read_oecd_a32("A.3.2 (N)", "numeracy")
)

figure_changes <- read_csv(figure_path, show_col_types = FALSE) |>
  mutate(
    oecd_name = recode(
      country_name,
      "Belgium" = "Flemish Region (Belgium)",
      "United Kingdom" = "England (UK)",
      "Slovakia" = "Slovak Republic",
      "Poland" = "Poland*",
      .default = country_name
    )
  )

local_long <- figure_changes |>
  transmute(
    country,
    country_name,
    oecd_name,
    local_year_r1 = as.character(survey_year_r1),
    local_year_r2 = as.character(survey_year_r2),
    literacy_rate_r1 = lit_rate_r1 * 100,
    literacy_rate_r2 = lit_rate_r2 * 100,
    literacy_change_pp = lit_change_pp,
    numeracy_rate_r1 = num_rate_r1 * 100,
    numeracy_rate_r2 = num_rate_r2 * 100,
    numeracy_change_pp = num_change_pp
  ) |>
  pivot_longer(
    cols = c(
      literacy_rate_r1, literacy_rate_r2, literacy_change_pp,
      numeracy_rate_r1, numeracy_rate_r2, numeracy_change_pp
    ),
    names_to = c("domain", ".value"),
    names_pattern = "(literacy|numeracy)_(rate_r1|rate_r2|change_pp)"
  ) |>
  rename(
    local_rate_r1 = rate_r1,
    local_rate_r2 = rate_r2,
    local_change_pp = change_pp
  )

comparison <- local_long |>
  left_join(oecd, by = c("oecd_name", "domain")) |>
  mutate(
    rate_r1_diff_pp = local_rate_r1 - oecd_rate_r1,
    rate_r2_diff_pp = local_rate_r2 - oecd_rate_r2,
    change_diff_pp = local_change_pp - oecd_change_pp,
    sign_mismatch = sign(local_change_pp) != sign(oecd_change_pp),
    magnitude_flag = abs(change_diff_pp) > 1,
    year_note = case_when(
      is.na(oecd_year_r1) ~ NA_character_,
      local_year_r1 == oecd_year_r1 ~ "",
      TRUE ~ paste0("local r1 year ", local_year_r1, "; OECD r1 year ", oecd_year_r1)
    )
  ) |>
  arrange(domain, desc(abs(change_diff_pp)))

summary <- comparison |>
  group_by(domain) |>
  summarise(
    figure_countries = n_distinct(country),
    matched_rows = sum(!is.na(oecd_change_pp)),
    sign_mismatches = sum(sign_mismatch, na.rm = TRUE),
    magnitude_flags_abs_gt_1pp = sum(magnitude_flag, na.rm = TRUE),
    mean_abs_change_diff_pp = mean(abs(change_diff_pp), na.rm = TRUE),
    max_abs_change_diff_pp = max(abs(change_diff_pp), na.rm = TRUE),
    .groups = "drop"
  )

write_csv(comparison, file.path(out_dir, "pl1_overlap_compare_oecd.csv"))
write_csv(summary, file.path(out_dir, "pl1_overlap_compare_oecd_summary.csv"))

oecd_lit_names <- oecd |>
  filter(domain == "literacy") |>
  pull(oecd_name)
figure_oecd_names <- figure_changes |>
  distinct(oecd_name) |>
  pull(oecd_name)
oecd_only <- setdiff(oecd_lit_names, figure_oecd_names)

cat("\nFigure 1 vs OECD Annex A.3.2 comparison\n")
cat("Figure countries:", n_distinct(figure_changes$country), "\n")
cat("Matched rows:", sum(!is.na(comparison$oecd_change_pp)), "of", nrow(comparison), "\n")
cat("OECD trend countries not in Figure 1:", paste(oecd_only, collapse = ", "), "\n\n")

cat("Summary by domain:\n")
print(summary, width = Inf)

cat("\nRows flagged by sign or >1 pp change-difference:\n")
flags <- comparison |>
  filter(sign_mismatch | magnitude_flag) |>
  transmute(
    country_name,
    domain,
    local_change_pp = round(local_change_pp, 2),
    oecd_change_pp = round(oecd_change_pp, 2),
    change_diff_pp = round(change_diff_pp, 2),
    sign_mismatch,
    magnitude_flag
  )

if (nrow(flags) == 0) {
  cat("None.\n")
} else {
  print(flags, n = Inf)
}

cat("\nLargest absolute change differences:\n")
print(
  comparison |>
    transmute(
      country_name,
      domain,
      local_change_pp = round(local_change_pp, 2),
      oecd_change_pp = round(oecd_change_pp, 2),
      change_diff_pp = round(change_diff_pp, 2),
      year_note
    ) |>
    arrange(desc(abs(change_diff_pp))) |>
    head(10),
  n = 10
)

cat("\nSaved:\n")
cat("  ", file.path(out_dir, "pl1_overlap_compare_oecd.csv"), "\n")
cat("  ", file.path(out_dir, "pl1_overlap_compare_oecd_summary.csv"), "\n")
