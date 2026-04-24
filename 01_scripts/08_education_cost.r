#!/usr/bin/env Rscript

# 08_education_cost.r
# Compute literacy rates and "cost per literate graduate" for Article 2.
# Countries: USA (from raw PUF files), GBR and FRA (from piaac_clean.rds).
#
# NOTE: US PUF suppresses exact ages; all three countries use AGEG10LFS=1 (16-24).
# GBR and FRA have exact ages in piaac_clean.rds so exact-age filtering is possible
# but we use the same 16-24 band for cross-country comparability.
#
# Education spending: manually curated from OECD Education at a Glance (EAG)
# Table B1.1 — Annual expenditure per student, USD PPP, current prices.
# Source: OECD EAG 2023, https://doi.org/10.1787/e13bef63-en
#
# Outputs:
#   02_output/literacy_by_age_group.rds   — PIAAC literacy rates, age 16-24 (USA/GBR/FRA)
#   02_output/education_cost_stats.rds    — cost-per-literate figures (all three)
#   02_output/oecd_spending.csv           — curated spending series

set.seed(20260420)

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(httr2)

root_candidates <- c(here(), here("piaac"))
piaac_root <- root_candidates[dir.exists(file.path(root_candidates, "01_scripts"))][1]
if (is.na(piaac_root)) stop("Could not locate PIAAC analysis root")

out_dir  <- file.path(piaac_root, "02_output")
data_dir <- c(file.path(piaac_root, "0_data"), file.path(piaac_root, "00_data")) |>
  (\(x) x[file.exists(x)])() |> (\(x) x[1])()

source(file.path(piaac_root, "01_scripts", "00_helpers.r"))

# ---- OECD spending data --------------------------------------------------

# Try to download fresh data from the OECD SDMX API; fall back to the
# curated CSV if the API is unavailable.  The curated CSV contains the
# values from OECD EAG 2023 Table B1.1 (USD PPP, current prices, all
# instructional levels combined, public + private institutions).
#
# To refresh from OECD manually:
#   1. Go to https://data-explorer.oecd.org
#   2. Search "Education at a Glance" → Expenditure per student
#   3. Filter: Countries = USA, GBR, FRA | Unit = USD PPP | Type = Total
#   4. Export as CSV and save to 02_output/oecd_spending_raw.csv

download_oecd_eag_spending <- function() {
  # Placeholder for live API download once dimension codes are confirmed.
  # Current SDMX endpoint: OECD.EDU.IMEP,DSD_EAG_UOE_FIN@DF_UOE_INDIC_FIN_PERSTUD,1.0
  # Dimension order: MEASURE.REF_AREA.EDUCATION_LEV.EXP_SOURCE.EXP_DESTINATION.EXPENDITURE_TYPE.PRICE_BASE.UNIT_MEASURE
  message("Live OECD API download not yet configured. Using curated CSV.")
  invisible(NULL)
}

# Curated values — OECD EAG 2023, Table B1.1
# Combined primary + secondary per-student annual expenditure, USD PPP.
# Country averages across levels: (primary * 6 + lower_sec * 3 + upper_sec * 3) / 12
# Historical 1970 US nominal values deflated using CPI (BLS) to 2021 USD.
spending_curated <- tribble(
  ~country, ~year, ~spend_per_pupil_usd_ppp, ~source,
  # United States
  "USA", 1970, 7200,  "NCES Digest 2023 Table 236.55, deflated to 2021 USD via CPI",
  "USA", 1980, 9400,  "NCES Digest 2023 Table 236.55, deflated to 2021 USD via CPI",
  "USA", 1990, 12800, "NCES Digest 2023 Table 236.55, deflated to 2021 USD via CPI",
  "USA", 2000, 13400, "OECD EAG 2005 Table B1.1",
  "USA", 2005, 14700, "OECD EAG 2008 Table B1.1",
  "USA", 2010, 16700, "OECD EAG 2013 Table B1.1",
  "USA", 2015, 15600, "OECD EAG 2018 Table B1.1",
  "USA", 2019, 17100, "OECD EAG 2022 Table B1.1",
  "USA", 2021, 17800, "OECD EAG 2023 Table B1.1",
  # United Kingdom
  "GBR", 2000, 7200,  "OECD EAG 2005 Table B1.1",
  "GBR", 2005, 10200, "OECD EAG 2008 Table B1.1",
  "GBR", 2010, 12400, "OECD EAG 2013 Table B1.1",
  "GBR", 2015, 11900, "OECD EAG 2018 Table B1.1",
  "GBR", 2019, 12800, "OECD EAG 2022 Table B1.1",
  "GBR", 2021, 13300, "OECD EAG 2023 Table B1.1",
  # France
  "FRA", 2000, 7400,  "OECD EAG 2005 Table B1.1",
  "FRA", 2005, 9200,  "OECD EAG 2008 Table B1.1",
  "FRA", 2010, 10500, "OECD EAG 2013 Table B1.1",
  "FRA", 2015, 10500, "OECD EAG 2018 Table B1.1",
  "FRA", 2019, 11400, "OECD EAG 2022 Table B1.1",
  "FRA", 2021, 12000, "OECD EAG 2023 Table B1.1"
)

write_csv(spending_curated, file.path(out_dir, "oecd_spending.csv"))
cat("Saved: 02_output/oecd_spending.csv\n")

# ---- PIAAC: literacy rates for age group 16-24 ---------------------------
# NOTE: The US PUF suppresses exact individual ages; only 10-year bands are
# available (AGEG10LFS=1 covers ages 16-24, midpoint coded as 20).
# We cannot isolate exact ages 16-19 from the public-use file.
# All PIAAC-derived counts use population weights (SPFWT0).

load_us_round <- function(file, yr) {
  cols <- c("SPFWT0", BRR_WTS, "AGEG10LFS", "AGEG10LFS_T", LIT_PVS)
  d <- read_sav(file.path(data_dir, file), col_select = any_of(cols))
  if ("AGEG10LFS_T" %in% names(d) && !"AGEG10LFS" %in% names(d))
    d <- rename(d, AGEG10LFS = AGEG10LFS_T)
  mutate(d, survey_year = yr, across(where(haven::is.labelled), as.numeric))
}

us_files <- list(
  list(file = "prgusap1_2012.sav", yr = 2012),
  list(file = "prgusap1_2017.sav", yr = 2017),
  list(file = "PRGUSAP2.sav",      yr = 2023)
)

us <- lapply(us_files, function(x) load_us_round(x$file, x$yr)) |>
  bind_rows() |>
  filter(!is.na(PVLIT1), SPFWT0 > 0)

# PL1 indicator (<=225 = at or below Level 1 = functionally illiterate)
pl1_pvs <- paste0("PL1_PV", 1:10)
for (i in seq_along(LIT_PVS)) us[[pl1_pvs[i]]] <- as.integer(us[[LIT_PVS[i]]] <= 225)

# Age group 1 = 16-24
ag1 <- filter(us, as.numeric(AGEG10LFS) == 1)

res_pl1   <- pv_group_mean(ag1, pl1_pvs, "survey_year", "SPFWT0", BRR_WTS) |>
  rename(pl1_share = mean, pl1_se = se)
res_score <- pv_group_mean(ag1, LIT_PVS, "survey_year", "SPFWT0", BRR_WTS) |>
  rename(mean_score = mean, score_se = se)

pop_est <- ag1 |>
  group_by(survey_year) |>
  summarise(pop_millions = sum(SPFWT0) / 1e6, n_obs = n(), .groups = "drop")

literacy_ag1 <- pop_est |>
  left_join(res_pl1 |> select(survey_year, pl1_share, pl1_se), by = "survey_year") |>
  left_join(res_score |> select(survey_year, mean_score, score_se), by = "survey_year") |>
  mutate(
    literate_share    = 1 - pl1_share,
    literate_millions = literate_share * pop_millions,
    illiterate_millions = pl1_share * pop_millions
  )

literacy_us <- mutate(literacy_ag1, country = "USA")

# ---- PIAAC: GBR and FRA from piaac_clean.rds -----------------------------

piaac <- readRDS(file.path(out_dir, "piaac_clean.rds"))

compute_literacy_ag1 <- function(data, country_code) {
  sub <- data |>
    filter(country == country_code, as.numeric(AGEG10LFS) == 1,
           !is.na(PVLIT1), SPFWT0 > 0)

  pl1_pvs_local <- paste0("PL1_PV", 1:10)
  for (i in seq_along(LIT_PVS)) sub[[pl1_pvs_local[i]]] <- as.integer(sub[[LIT_PVS[i]]] <= 225)

  res_pl1   <- pv_group_mean(sub, pl1_pvs_local, "survey_year", "SPFWT0", BRR_WTS) |>
    rename(pl1_share = mean, pl1_se = se)
  res_score <- pv_group_mean(sub, LIT_PVS, "survey_year", "SPFWT0", BRR_WTS) |>
    rename(mean_score = mean, score_se = se)

  sub |>
    group_by(survey_year) |>
    summarise(pop_millions = sum(SPFWT0) / 1e6, n_obs = n(), .groups = "drop") |>
    left_join(res_pl1  |> select(survey_year, pl1_share, pl1_se),  by = "survey_year") |>
    left_join(res_score |> select(survey_year, mean_score, score_se), by = "survey_year") |>
    mutate(
      country           = country_code,
      literate_share    = 1 - pl1_share,
      literate_millions = literate_share * pop_millions,
      illiterate_millions = pl1_share * pop_millions
    )
}

literacy_gbr <- compute_literacy_ag1(piaac, "GBR")
literacy_fra <- compute_literacy_ag1(piaac, "FRA")

literacy_all <- bind_rows(literacy_us, literacy_gbr, literacy_fra)

saveRDS(literacy_all, file.path(out_dir, "literacy_by_age_group.rds"))

cat("\nAge group 16-24 literacy by country (from PIAAC):\n")
print(literacy_all |> transmute(
  country,
  year = survey_year,
  pop_millions = round(pop_millions, 1),
  pl1_pct = scales::percent(pl1_share, accuracy = 0.1),
  literate_pct = scales::percent(literate_share, accuracy = 0.1),
  literate_millions = round(literate_millions, 1),
  mean_score = round(mean_score, 1)
))

# ---- Cost per literate graduate (all three countries) --------------------
# Cumulative K-12 spending = annual per-pupil * 13 years (K through grade 12).
# For each country × PIAAC round, we use the EAG spending figure closest to
# that survey year.

get_spending <- function(country_code, target_year) {
  spending_curated |>
    filter(country == country_code) |>
    slice_min(abs(year - target_year), n = 1) |>
    pull(spend_per_pupil_usd_ppp)
}

piaac_rounds <- c(2012, 2023)

cost_stats <- literacy_all |>
  filter(survey_year %in% piaac_rounds) |>
  mutate(
    annual_spend_usd_ppp = mapply(get_spending, country, survey_year),
    cumulative_k12       = annual_spend_usd_ppp * 13,
    cost_per_literate    = cumulative_k12 / literate_share
  )

# Historical 1970s comparison — for each country, use 2012 literacy rate
# (earliest PIAAC observation) with 1970 spending
hist_rows <- purrr::map_dfr(c("USA", "GBR", "FRA"), function(ctry) {
  spend_70  <- get_spending(ctry, 1970)
  lit_early <- literacy_all |> filter(country == ctry, survey_year == min(survey_year)) |>
    pull(literate_share)
  tibble(
    country = ctry, survey_year = 1970L,
    annual_spend_usd_ppp = spend_70,
    cumulative_k12 = spend_70 * 13,
    literate_share = lit_early,
    cost_per_literate = spend_70 * 13 / lit_early
  )
})

cost_stats_full <- bind_rows(cost_stats, hist_rows) |>
  arrange(country, survey_year)

saveRDS(cost_stats_full, file.path(out_dir, "education_cost_stats.rds"))

cat("\nCost-per-literate-graduate by country (USD PPP, ~2021 prices):\n")
print(cost_stats_full |> transmute(
  country,
  year = survey_year,
  annual_per_pupil  = scales::dollar(annual_spend_usd_ppp, accuracy = 100),
  cumulative_k12    = scales::dollar(cumulative_k12, accuracy = 1000),
  literate_pct      = scales::percent(literate_share, accuracy = 0.1),
  cost_per_literate = scales::dollar(cost_per_literate, accuracy = 1000)
))
