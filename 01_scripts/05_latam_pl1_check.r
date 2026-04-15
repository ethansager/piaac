#!/usr/bin/env Rscript

# 05_latam_pl1_check.r
# Check slide-level Latin America PL1 counts using the 2014 PIAAC files
# for Chile, Mexico, Ecuador, and Peru, and create a country bar chart of
# shares at or below Level 1 in literacy and numeracy.
#
# Outputs:
#   02_output/latam_pl1_country_summary.csv
#   02_output/latam_pl1_pooled_summary.csv
#   Figures/latam_pl1_shares_2014.pdf
#   Figures/latam_pl1_shares_2014.png

set.seed(20260411)

library(haven)
library(scales)
library(here)
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

country_files <- c(
  CHL = "prgchlp1.sav",
  MEX = "prgmexp1.sav",
  ECU = "prgecup1.sav",
  PER = "prgperp1.sav"
)

country_labels <- c(
  CHL = "Chile",
  MEX = "Mexico",
  ECU = "Ecuador",
  PER = "Peru"
)

required_paths <- file.path(data_dir, country_files)
if (!all(file.exists(required_paths))) {
  missing_paths <- required_paths[!file.exists(required_paths)]
  stop(
    "Missing one or more required LATAM files:\n",
    paste(missing_paths, collapse = "\n")
  )
}

# ---- Helpers ----
pl1_rate <- function(data, pv_prefix, wt) {
  pv_names <- paste0(pv_prefix, 1:10)
  mean(vapply(
    pv_names,
    function(v) weighted.mean(as.integer(data[[v]] <= 225), wt, na.rm = TRUE),
    numeric(1)
  ))
}

pl1_rate_adjusted <- function(data, pv_prefix, wt, adj) {
  pv_names <- paste0(pv_prefix, 1:10)
  mean(vapply(
    pv_names,
    function(v) {
      sum(as.integer(data[[v]] <= 225) * wt * adj, na.rm = TRUE) /
        sum(wt * adj, na.rm = TRUE)
    },
    numeric(1)
  ))
}

load_country_summary <- function(iso, filename) {
  d <- read_sav(
    file.path(data_dir, filename),
    col_select = any_of(c(
      "SPFWT0", "AGEG10LFS", "AGEG10LFS_T",
      paste0("PVLIT", 1:10),
      paste0("PVNUM", 1:10)
    ))
  )

  if (!"AGEG10LFS" %in% names(d) && "AGEG10LFS_T" %in% names(d)) {
    d <- rename(d, AGEG10LFS = AGEG10LFS_T)
  }

  wt <- d$SPFWT0

  # Official PIAAC target population is ages 16-65.
  pop_16_65 <- sum(wt, na.rm = TRUE)
  lit_rate_16_65 <- pl1_rate(d, "PVLIT", wt)
  num_rate_16_65 <- pl1_rate(d, "PVNUM", wt)

  # Approximate 16-64 by removing 1/11 of the 55-65 age-band weight.
  # AGEG10LFS codes: 1=16-24, 2=25-34, 3=35-44, 4=45-54, 5=55-65.
  if ("AGEG10LFS" %in% names(d)) {
    age_band <- as.numeric(d$AGEG10LFS)
    adj_16_64 <- ifelse(age_band == 5, 10 / 11, 1)

    pop_16_64_est <- sum(wt * adj_16_64, na.rm = TRUE)
    lit_rate_16_64_est <- pl1_rate_adjusted(d, "PVLIT", wt, adj_16_64)
    num_rate_16_64_est <- pl1_rate_adjusted(d, "PVNUM", wt, adj_16_64)
  } else {
    pop_16_64_est <- NA_real_
    lit_rate_16_64_est <- NA_real_
    num_rate_16_64_est <- NA_real_
  }

  tibble(
    country = iso,
    country_name = unname(country_labels[iso]),
    pop_16_65 = pop_16_65,
    lit_rate_16_65 = lit_rate_16_65,
    num_rate_16_65 = num_rate_16_65,
    lit_pop_16_65 = pop_16_65 * lit_rate_16_65,
    num_pop_16_65 = pop_16_65 * num_rate_16_65,
    pop_16_64_est = pop_16_64_est,
    lit_rate_16_64_est = lit_rate_16_64_est,
    num_rate_16_64_est = num_rate_16_64_est,
    lit_pop_16_64_est = pop_16_64_est * lit_rate_16_64_est,
    num_pop_16_64_est = pop_16_64_est * num_rate_16_64_est
  )
}

# ---- Country summaries ----
country_summary <- bind_rows(lapply(
  names(country_files),
  function(iso) load_country_summary(iso, country_files[[iso]])
))

pooled_summary <- country_summary |>
  summarise(
    countries = n(),
    pop_16_65 = sum(pop_16_65),
    lit_pop_16_65 = sum(lit_pop_16_65),
    num_pop_16_65 = sum(num_pop_16_65),
    lit_rate_16_65 = lit_pop_16_65 / pop_16_65,
    num_rate_16_65 = num_pop_16_65 / pop_16_65,
    pop_16_64_est = sum(pop_16_64_est, na.rm = TRUE),
    lit_pop_16_64_est = sum(lit_pop_16_64_est, na.rm = TRUE),
    num_pop_16_64_est = sum(num_pop_16_64_est, na.rm = TRUE),
    lit_rate_16_64_est = lit_pop_16_64_est / pop_16_64_est,
    num_rate_16_64_est = num_pop_16_64_est / pop_16_64_est
  ) |>
  mutate(sample = "CHL + MEX + ECU + PER (2014 PIAAC)")

write_csv(country_summary, file.path(out_dir, "latam_pl1_country_summary.csv"))
write_csv(pooled_summary, file.path(out_dir, "latam_pl1_pooled_summary.csv"))

# ---- Participation note for 2023 ----
p2_present <- c(
  CHL = file.exists(file.path(data_dir, "PRGCHLP2.sav")),
  MEX = file.exists(file.path(data_dir, "PRGMEXP2.sav")),
  ECU = file.exists(file.path(data_dir, "PRGECUP2.sav")),
  PER = file.exists(file.path(data_dir, "PRGPERP2.sav"))
)

cat("\n2023 participation files present among these four countries:\n")
print(p2_present)

cat("\nCountry summary (ages 16-65 official PIAAC population):\n")
print(country_summary |>
  transmute(
    country_name,
    pop_millions_16_65 = round(pop_16_65 / 1e6, 1),
    lit_share_pct = round(lit_rate_16_65 * 100, 1),
    num_share_pct = round(num_rate_16_65 * 100, 1),
    lit_pl1_millions = round(lit_pop_16_65 / 1e6, 1),
    num_pl1_millions = round(num_pop_16_65 / 1e6, 1)
  ))

cat("\nPooled four-country totals:\n")
print(pooled_summary |>
  transmute(
    sample,
    pop_millions_16_65 = round(pop_16_65 / 1e6, 1),
    lit_pl1_millions_16_65 = round(lit_pop_16_65 / 1e6, 1),
    num_pl1_millions_16_65 = round(num_pop_16_65 / 1e6, 1),
    lit_share_pct_16_65 = round(lit_rate_16_65 * 100, 1),
    num_share_pct_16_65 = round(num_rate_16_65 * 100, 1),
    pop_millions_16_64_est = round(pop_16_64_est / 1e6, 1),
    lit_pl1_millions_16_64_est = round(lit_pop_16_64_est / 1e6, 1),
    num_pl1_millions_16_64_est = round(num_pop_16_64_est / 1e6, 1)
  ))

# ---- Bar chart: country shares at or below Level 1 ----
plot_order <- country_summary |>
  arrange(desc(lit_rate_16_65)) |>
  pull(country_name)

plot_df <- country_summary |>
  select(country_name, lit_rate_16_65, num_rate_16_65) |>
  pivot_longer(
    cols = c(lit_rate_16_65, num_rate_16_65),
    names_to = "domain",
    values_to = "share"
  ) |>
  mutate(
    domain = recode(
      domain,
      lit_rate_16_65 = "Literacy",
      num_rate_16_65 = "Numeracy"
    ),
    share_pct = share * 100,
    country_name = factor(country_name, levels = plot_order)
  )

p <- ggplot(plot_df, aes(x = country_name, y = share_pct, fill = domain)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(
    aes(label = sprintf("%.0f%%", share_pct)),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.4,
    color = "#222222"
  ) +
  scale_fill_manual(
    values = c("Literacy" = "#1f4e79", "Numeracy" = "#7a9a01"),
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_df$share_pct) * 1.15),
    labels = label_number(suffix = "%")
  ) +
  labs(
    title = "Adults at or Below Level 1 in PIAAC Cycle 1",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Point estimates use SPFWT0 weights and average across 10 plausible values. ",
      "Level 1 threshold = score <= 225."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#1f4e79"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

p

ggsave(file.path(fig_dir, "latam_pl1_shares_2014.pdf"), p, width = 9, height = 6)
ggsave(file.path(fig_dir, "latam_pl1_shares_2014.png"), p, width = 9, height = 6, dpi = 150)

cat("\nSaved:\n")
cat("  ", file.path(out_dir, "latam_pl1_country_summary.csv"), "\n")
cat("  ", file.path(out_dir, "latam_pl1_pooled_summary.csv"), "\n")
cat("  ", file.path(fig_dir, "latam_pl1_shares_2014.pdf"), "\n")
cat("  ", file.path(fig_dir, "latam_pl1_shares_2014.png"), "\n")
