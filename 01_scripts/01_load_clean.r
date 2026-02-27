# 01_load_clean.r
# Load all PIAAC SPSS files, extract key variables, combine rounds, save clean RDS.
# Simplified first pass: PV1 only, no BRR/JK replicate weights.
#
# Output: 02_output/piaac_clean.rds

set.seed(20260227)

library(haven)
library(dplyr)
library(purrr)
library(here)

# ---- Output directory ----
out_dir <- here("02_output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- File inventory ----
data_dir <- here("00_data")
sav_files <- list.files(data_dir, pattern = "\\.sav$", ignore.case = TRUE, full.names = TRUE)
cat(sprintf("Found %d SAV files\n", length(sav_files)))

# ---- Parse filename → country and round ----
# Filename pattern: PRG[ISO3]P[round].sav (case-insensitive)
# e.g. prgautp1.sav → AUT, round 1; PRGAUTP2.sav → AUT, round 2
parse_filename <- function(f) {
  fname <- basename(f)
  country     <- toupper(substr(fname, 4, 6))
  round_num   <- as.integer(substr(fname, 8, 8))
  survey_year <- if (round_num == 1L) 2013L else 2017L
  list(country = country, round = round_num, survey_year = survey_year)
}

# ---- Key variables (any_of handles round differences) ----
# Round 1: EDCAT7 (bare); Round 2: EDCAT7_TC1 (suffixed)
KEY_VARS <- c("PVLIT1", "AGE_R", "AGEG10LFS", "EDCAT7", "EDCAT7_TC1", "SPFWT0")

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
  if (is.null(d)) return(NULL)

  # Round 2 uses EDCAT7_TC1 instead of EDCAT7; standardize name
  if ("EDCAT7_TC1" %in% names(d) && !"EDCAT7" %in% names(d)) {
    d <- rename(d, EDCAT7 = EDCAT7_TC1)
  }

  # Warn on missing key variables
  expected <- c("PVLIT1", "AGE_R", "AGEG10LFS", "EDCAT7", "SPFWT0")
  missing  <- setdiff(expected, names(d))
  if (length(missing) > 0) {
    warning(sprintf("[%s R%d] Missing vars: %s",
                    meta$country, meta$round, paste(missing, collapse = ", ")))
  }

  d |>
    mutate(
      country      = meta$country,
      round        = meta$round,
      survey_year  = meta$survey_year,
      birth_cohort = survey_year - as.numeric(AGE_R)
    )
}

# ---- Load all files ----
cat("Loading files (this may take a few minutes)...\n")
all_data <- map(sav_files, \(f) {
  cat(sprintf("  Reading %s...\n", basename(f)))
  load_one_file(f)
})

failed <- sum(map_lgl(all_data, is.null))
if (failed > 0) cat(sprintf("WARNING: %d files failed to load\n", failed))

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
print(count(piaac, country, round) |> tidyr::pivot_wider(names_from = round, values_from = n), n = Inf)

cat("\nVariable completeness:\n")
vars_to_check <- c("PVLIT1", "AGE_R", "AGEG10LFS", "EDCAT7", "SPFWT0")
for (v in vars_to_check) {
  if (v %in% names(piaac)) {
    pct_missing <- mean(is.na(piaac[[v]])) * 100
    cat(sprintf("  %-12s: %.1f%% missing\n", v, pct_missing))
  } else {
    cat(sprintf("  %-12s: NOT FOUND\n", v))
  }
}

cat("\nPVLIT1 range (non-missing):", range(piaac$PVLIT1, na.rm = TRUE), "\n")

# ---- Save ----
out_path <- file.path(out_dir, "piaac_clean.rds")
saveRDS(piaac, out_path)
cat(sprintf("\nSaved: %s (%.1f MB)\n", out_path, file.size(out_path) / 1e6))
