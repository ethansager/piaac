# 01_load_clean.r
# Load all PIAAC SPSS files, extract key variables, combine rounds, save clean RDS.
# Loads all 10 literacy PVs (PVLIT1-PVLIT10) and all 10 numeracy PVs (PVNUM1-PVNUM10).
# Derives pvlit_mean and pvnum_mean as row-wise averages (for exploration only;
# downstream analysis uses per-PV estimates via pv_group_mean() in 00_helpers.r).
# Includes all 80 BRR replicate weights (SPFWT1-SPFWT80) for downstream SE calculation.
# Includes demographic variables: gender, employment, immigration, education, income.
#
# Output: 02_output/piaac_clean.rds

set.seed(20260227)

library(haven)
library(dplyr)
library(purrr)
library(here)

#------------------------------------------------------------------------------#
#  Set up intital objects ----
#------------------------------------------------------------------------------#

out_dir <- here("02_output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
data_dir <- here("00_data")

sav_files <- list.files(
  data_dir,
  pattern = "\\.sav$",
  ignore.case = TRUE,
  full.names = TRUE
)

#------------------------------------------------------------------------------#
#  PIAAC country × cycle ----
#------------------------------------------------------------------------------#

# ---- Country-level survey years ----
# p1 files = each country's FIRST PIAAC participation (Cycle 1, three rounds):
#   C1 Round 1 (2012): most OECD countries
#   C1 Round 2 (2014): CHL, ECU, HUN, ISR, MEX, NZL, PER, RUS, SGP, TUR
#                      (AUT also did R2 but AUT's p1 = R1 = 2012)
#   C1 Round 3 (2017): GRC, KAZ, SVN
#
# p2 files = each country's SECOND PIAAC participation:
#   AUT: C1 Round 2 (2014) — AUT p1 was Round 1, p2 is Round 2
#   All others: Cycle 2 Round 1 (2023)
#   NZL p2 year set to 2023 pending confirmation (not shown in table but file exists)
CY1_YEAR_LOOKUP <- c(
  # C1 Round 1 (2012)
  AUT=2012L, BEL=2012L, CAN=2012L, CZE=2012L, DNK=2012L, EST=2012L,
  FIN=2012L, FRA=2012L, DEU=2012L, IRL=2012L, ITA=2012L, JPN=2012L,
  KOR=2012L, LTU=2012L, NLD=2012L, NOR=2012L, POL=2012L, SVK=2012L,
  ESP=2012L, SWE=2012L, GBR=2012L,
  # C1 Round 2 (2014)
  CHL=2014L, ECU=2014L, HUN=2014L, ISR=2014L, MEX=2014L, NZL=2014L,
  PER=2014L, RUS=2014L, SGP=2014L, TUR=2014L,
  # C1 Round 3 (2017)
  GRC=2017L, KAZ=2017L, SVN=2017L
)

CY2_YEAR_LOOKUP <- c(
  AUT = 2014L,   # AUT p2 = its C1 Round 2 participation
  # All others: Cycle 2 Round 1 (2023)
  BEL=2023L, CAN=2023L, CHL=2023L, CZE=2023L, DEU=2023L, DNK=2023L,
  ESP=2023L, EST=2023L, FIN=2023L, FRA=2023L, GBR=2023L, HUN=2023L,
  IRL=2023L, ISR=2023L, ITA=2023L, JPN=2023L, KOR=2023L, LTU=2023L,
  NZL=2023L, POL=2023L, SGP=2023L, SVK=2023L, USA=2023L
)

# ---- Round override for countries with 3+ participations ----
# USA has three survey rounds. We assign round numbers so that round 1 (2012)
# and round 2 (2023) align with the Cycle 1 / Cycle 2 structure used by all
# other countries, enabling a clean cohort comparison. The 2017 data gets
# round 3 and is available as an intermediate point.
# File naming: non-standard _YYYY suffix (prgusap1_2012.sav, prgusap1_2017.sav)
ROUND_OVERRIDE <- list(
  USA = c(`2012` = 1L, `2023` = 2L, `2017` = 3L)
)

# ---- Parse filename → country and round ----
# Standard pattern : PRG[ISO3]P[round].sav  (e.g. prgautp1.sav)
# Extended pattern : PRG[ISO3]P[round]_[YYYY].sav  (e.g. prgusap1_2012.sav)
parse_filename <- function(f) {
  fname <- basename(f)
  country   <- toupper(substr(fname, 4, 6))
  raw_round <- as.integer(substr(fname, 8, 8))

  # Detect optional _YYYY suffix; use it directly as survey_year if present
  year_suffix <- regmatches(fname,
    regexpr("_(\\d{4})\\.sav$", fname, ignore.case = TRUE))
  if (length(year_suffix) > 0) {
    survey_year <- as.integer(substr(year_suffix, 2, 5))
  } else {
    survey_year <- if (raw_round == 1L) {
      unname(CY1_YEAR_LOOKUP[country] %||% 2012L)
    } else {
      unname(CY2_YEAR_LOOKUP[country] %||% 2023L)
    }
  }

  # Apply round override for countries with non-standard participation structure
  round_num <- if (country %in% names(ROUND_OVERRIDE)) {
    unname(ROUND_OVERRIDE[[country]][as.character(survey_year)] %||% raw_round)
  } else {
    raw_round
  }

  list(country = country, round = round_num, survey_year = survey_year)
}

# %||% operator: return rhs if lhs is NA or NULL
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

parse_filename(sav_files[1])

# ---- Key variables (any_of handles round differences) ----
# Round 1: EDCAT7 (bare); Round 2: EDCAT7_TC1 (suffixed). Same pattern for EDCAT8.
# All 10 literacy and numeracy PVs, 80 BRR replicate weights, demographics.
LIT_PVS   <- paste0("PVLIT", 1:10)
NUM_PVS   <- paste0("PVNUM", 1:10)
BRR_WTS   <- paste0("SPFWT", 1:80)
# Demographic variables:
#   GENDER_R       = gender (1=male, 2=female)
#   C_D05          = employment status
#   I_Q08          = self-reported health status
#   J_Q01_C        = occupation (ISCO 1-digit)
#   J_Q03a         = industry sector
#   NATIVESPEAKER  = native speaker of test language (1=yes, 2=no)
#   MONTHLYINCPR   = monthly earnings percentile
#   PARED          = parental education (highest of two parents)
#   IMGEN          = immigration generation (1st, 2nd, native)
#   EDCAT8         = 8-category education (finer than EDCAT7)
DEMO_VARS <- c("GENDER_R", "C_D05", "I_Q08", "J_Q01_C", "J_Q03a",
               "NATIVESPEAKER", "MONTHLYINCPR", "PARED", "IMGEN",
               "EDCAT8", "EDCAT8_TC1")
KEY_VARS  <- c(LIT_PVS, NUM_PVS, BRR_WTS,
               "AGE_R", "AGEG10LFS", "AGEG10LFS_T", "EDCAT7", "EDCAT7_TC1", "SPFWT0",
               DEMO_VARS)

# ---- Load one file ----
load_one_file <- function(filepath) {
  meta <- parse_filename(filepath)

  d <- tryCatch(
    read_sav(filepath, col_select = any_of(KEY_VARS)),
    error = function(e) {
      warning(sprintf("Failed to read %s: %s", basename(filepath), e$message))
      return(NULL)
    }
  )
  if (is.null(d)) {
    return(NULL)
  }

  # Round 2 uses _TC1 suffixed names; standardize to base names
  if ("EDCAT7_TC1" %in% names(d) && !"EDCAT7" %in% names(d)) {
    d <- rename(d, EDCAT7 = EDCAT7_TC1)
  }
  if ("EDCAT8_TC1" %in% names(d) && !"EDCAT8" %in% names(d)) {
    d <- rename(d, EDCAT8 = EDCAT8_TC1)
  }

  # Some non-standard files (e.g. USA 2017) use AGEG10LFS_T instead of AGEG10LFS
  if ("AGEG10LFS_T" %in% names(d) && !"AGEG10LFS" %in% names(d)) {
    d <- rename(d, AGEG10LFS = AGEG10LFS_T)
  }

  # Some files lack AGE_R or suppress it (all NA) for privacy — approximate from age-band midpoint.
  # Applies to all USA files: individual ages are suppressed in the US PUF.
  # AGEG10LFS codes: 1=16-24, 2=25-34, 3=35-44, 4=45-54, 5=55-65
  age_r_suppressed <- !"AGE_R" %in% names(d) ||
    (all(is.na(d$AGE_R)) && "AGEG10LFS" %in% names(d))
  if (age_r_suppressed && "AGEG10LFS" %in% names(d)) {
    age_midpoints <- c(20L, 30L, 40L, 50L, 60L)
    d$AGE_R <- age_midpoints[as.integer(d$AGEG10LFS)]
    warning(sprintf(
      "[%s R%d] AGE_R suppressed/missing; approximated from AGEG10LFS midpoints (birth_cohort = approximate decade)",
      meta$country, meta$round
    ))
  }

  # Warn on missing key variables (check representative PVs + core vars)
  expected <- c("PVLIT1", "PVNUM1", "AGE_R", "AGEG10LFS", "EDCAT7", "SPFWT0")
  missing <- setdiff(expected, names(d))
  if (length(missing) > 0) {
    warning(sprintf(
      "[%s R%d] Missing vars: %s",
      meta$country,
      meta$round,
      paste(missing, collapse = ", ")
    ))
  }

  # Derive row-wise PV means across however many PVs are present
  lit_cols <- intersect(LIT_PVS, names(d))
  num_cols <- intersect(NUM_PVS, names(d))
  brr_cols <- intersect(BRR_WTS, names(d))

  if (length(lit_cols) < 10) {
    warning(sprintf("[%s R%d] Only %d/10 literacy PVs found",
                    meta$country, meta$round, length(lit_cols)))
  }
  if (length(num_cols) < 10) {
    warning(sprintf("[%s R%d] Only %d/10 numeracy PVs found",
                    meta$country, meta$round, length(num_cols)))
  }
  if (length(brr_cols) < 80) {
    warning(sprintf("[%s R%d] Only %d/80 BRR replicate weights found",
                    meta$country, meta$round, length(brr_cols)))
  }

  pvlit_mean <- if (length(lit_cols) > 0)
    rowMeans(d[, lit_cols, drop = FALSE], na.rm = FALSE) else NA_real_
  pvnum_mean <- if (length(num_cols) > 0)
    rowMeans(d[, num_cols, drop = FALSE], na.rm = FALSE) else NA_real_

  d |>
    mutate(
      country    = meta$country,
      round      = meta$round,
      survey_year = meta$survey_year,
      birth_cohort = survey_year - as.numeric(AGE_R),
      pvlit_mean = pvlit_mean,
      pvnum_mean = pvnum_mean,
      pvlit_n    = length(lit_cols),
      pvnum_n    = length(num_cols),
      brr_n      = length(brr_cols)
    )
}

# ---- Load all files ----
cat("Loading files (this may take a few minutes)...\n")
all_data <- map(sav_files, \(f) {
  cat(sprintf("  Reading %s...\n", basename(f)))
  load_one_file(f)
})

failed <- sum(map_lgl(all_data, is.null))
if (failed > 0) {
  cat(sprintf("WARNING: %d files failed to load\n", failed))
}

piaac <- bind_rows(compact(all_data))

# Convert haven-labelled columns to plain numeric
piaac <- mutate(piaac, across(where(haven::is.labelled), as.numeric))

# ---- Summary ----
cat(sprintf(
  "\nCombined dataset: %s rows, %d countries, %d rounds\n",
  format(nrow(piaac), big.mark = ","),
  n_distinct(piaac$country),
  n_distinct(piaac$round)
))

cat("\nRows by country × round:\n")
print(
  count(piaac, country, round) |>
    tidyr::pivot_wider(names_from = round, values_from = n),
  n = Inf
)

cat("\nVariable completeness:\n")
vars_to_check <- c("pvlit_mean", "pvnum_mean", "PVLIT1", "PVNUM1",
                   "SPFWT0", "SPFWT1", "AGE_R", "AGEG10LFS", "EDCAT7",
                   "GENDER_R", "C_D05", "I_Q08", "J_Q01_C", "J_Q03a",
                   "NATIVESPEAKER", "MONTHLYINCPR", "PARED", "IMGEN", "EDCAT8")
for (v in vars_to_check) {
  if (v %in% names(piaac)) {
    pct_missing <- mean(is.na(piaac[[v]])) * 100
    cat(sprintf("  %-16s: %.1f%% missing\n", v, pct_missing))
  } else {
    cat(sprintf("  %-16s: NOT FOUND\n", v))
  }
}

cat("\nPV counts (should be 10 each):\n")
cat(sprintf("  pvlit_n unique values: %s\n", paste(sort(unique(piaac$pvlit_n)), collapse = ", ")))
cat(sprintf("  pvnum_n unique values: %s\n", paste(sort(unique(piaac$pvnum_n)), collapse = ", ")))
cat(sprintf("  brr_n   unique values: %s\n", paste(sort(unique(piaac$brr_n)),   collapse = ", ")))

cat("\npvlit_mean range (non-missing):", range(piaac$pvlit_mean, na.rm = TRUE), "\n")
cat("pvnum_mean range (non-missing):", range(piaac$pvnum_mean, na.rm = TRUE), "\n")

# ---- Save ----
out_path <- file.path(out_dir, "piaac_clean.rds")
saveRDS(piaac, out_path)
cat(sprintf("\nSaved: %s (%.1f MB)\n", out_path, file.size(out_path) / 1e6))
