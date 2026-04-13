# 05a_cache_pisa.r
# One-time script: read each raw PISA file, keep only analysis columns
# (CNT, PVs, weights), and save as compressed RDS.
#
# Run once. Subsequent scripts load from cache in seconds.
# Cache lives in 00_data/pisa/cache/.

library(tidyverse)
library(haven)
library(here)

pisa_dir  <- here("00_data/pisa")
cache_dir <- file.path(pisa_dir, "cache")
docs_dir  <- here("docs")

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Helpers
# =============================================================================

parse_spss_layout <- function(setup_file) {
  lines   <- readLines(setup_file, encoding = "latin1", warn = FALSE)
  lines   <- iconv(lines, from = "latin1", to = "UTF-8", sub = "")
  pattern <- "^\\s*(\\w+)\\s+(\\d+)\\s*-\\s*(\\d+)\\s*\\("
  m       <- regmatches(lines, regexec(pattern, lines))
  valid   <- Filter(function(x) length(x) == 4, m)
  tibble(
    var   = sapply(valid, `[`, 2),
    start = as.integer(sapply(valid, `[`, 3)),
    end   = as.integer(sapply(valid, `[`, 4))
  )
}

read_pisa_txt <- function(data_file, setup_file, n_pvs = 5) {
  keep <- c("CNT", "AGE",
            paste0("PV", seq_len(n_pvs), "READ"),
            paste0("PV", seq_len(n_pvs), "MATH"),
            "W_FSTUWT",
            paste0("W_FSTR", 1:80))

  layout <- parse_spss_layout(setup_file) |> filter(var %in% keep)

  read_fwf(
    data_file,
    col_positions  = fwf_positions(layout$start, layout$end, layout$var),
    col_types      = cols(CNT = col_character(), .default = col_double()),
    progress       = TRUE,
    show_col_types = FALSE
  ) |>
    # Standardise BRR weight names: W_FSTR* -> W_FSTURWT* (matches SPSS files)
    rename_with(\(x) sub("^W_FSTR(\\d+)$", "W_FSTURWT\\1", x)) |>
    rename_with(tolower)
}

read_pisa_sav <- function(sav_file, n_pvs = 10) {
  keep <- c("CNT", "AGE",
            paste0("PV", seq_len(n_pvs), "READ"),
            paste0("PV", seq_len(n_pvs), "MATH"),
            "W_FSTUWT",
            paste0("W_FSTURWT", 1:80))

  read_sav(sav_file, col_select = any_of(keep)) |>
    zap_labels() |>
    rename_with(tolower)
}

cache_year <- function(year, read_fn, cache_dir) {
  cache_file <- file.path(cache_dir, paste0("pisa_", year, ".rds"))
  if (file.exists(cache_file)) {
    message("  Already cached: ", year)
    return(invisible(cache_file))
  }
  message("  Reading ", year, "...")
  df <- read_fn()
  message("  Saving cache: ", nrow(df), " rows, ",
          n_distinct(df$cnt), " countries")
  saveRDS(df, cache_file, compress = "xz")
  invisible(cache_file)
}

# =============================================================================
# Cache each year
# =============================================================================

message("=== Caching PISA data ===")

cache_year(2006, function() read_pisa_txt(
  file.path(pisa_dir, "INT_Stu06_Dec07.txt"),
  file.path(docs_dir, "PISA2006_SPSS_student.txt"),
  n_pvs = 5
), cache_dir)

cache_year(2009, function() read_pisa_txt(
  file.path(pisa_dir, "INT_STQ09_DEC11.txt"),
  file.path(docs_dir, "PISA2009_SPSS_student.txt"),
  n_pvs = 5
), cache_dir)

cache_year(2012, function() read_pisa_txt(
  file.path(pisa_dir, "INT_STU12_DEC03.txt"),
  file.path(docs_dir, "SPSS syntax to read in student questionnaire data file.txt"),
  n_pvs = 5
), cache_dir)

cache_year(2015, function() read_pisa_sav(
  file.path(pisa_dir, "CY6_MS_CMB_STU_QQQ.sav"),
  n_pvs = 10
), cache_dir)

cache_year(2018, function() read_pisa_sav(
  file.path(pisa_dir, "STU/CY07_MSU_STU_QQQ.sav"),
  n_pvs = 10
), cache_dir)

message("\nDone. Cache files:")
fs <- list.files(cache_dir, "*.rds", full.names = TRUE)
info <- file.info(fs)
cat(sprintf("  %-25s  %s MB\n",
            basename(fs),
            round(info$size / 1e6, 1)), sep = "")
