# 05a_cache_pisa.r
# One-time script: read each raw PISA file, keep only analysis columns
# (CNT, PVs, weights), and save as compressed RDS.
#
# Run once per new year added. Subsequent scripts load from cache in seconds.
# Cache lives in 00_data/pisa/cache/.
#
# Supported cycles:
#   2000 -- intstud_read_v3.txt + intstud_math_v3.txt (separate domain files)
#   2003 -- INT_stui_2003_v2.txt (combined, layout: PISA2003_SPSS_student.txt)
#   2006 -- INT_Stu06_Dec07.txt  (combined, layout: PISA2006_SPSS_student.txt)
#   2009 -- INT_STQ09_DEC11.txt  (combined, layout: PISA2009_SPSS_student.txt)
#   2012 -- INT_STU12_DEC03.txt  (combined, layout: SPSS syntax to read in...)
#   2015 -- CY6_MS_CMB_STU_QQQ.sav (SPSS .sav, self-describing)
#   2018 -- STU/CY07_MSU_STU_QQQ.sav
#
# BRR weights in cached files are renamed W_FSTR* -> w_fsturwt* to match
# the PISA 2015/2018 SPSS naming convention used downstream.

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

# Parse SPSS DATA LIST fixed-format syntax into a column-position tibble.
# Works for layouts with OR without trailing format specifiers like (F,4) or (a).
# (PISA 2000/2003 lack format specifiers on numeric variables; 2006+ have them.)
parse_spss_layout <- function(setup_file) {
  lines <- readLines(setup_file, encoding = "latin1", warn = FALSE)
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "")
  # Match: optional whitespace, variable name, start column, dash, end column.
  # Any trailing format spec (e.g. "(F,4)" or "(a)") is ignored.
  pattern <- "^\\s*(\\w+)\\s+(\\d+)\\s*-\\s*(\\d+)"
  m       <- regmatches(lines, regexec(pattern, lines))
  valid   <- Filter(function(x) length(x) == 4, m)
  tibble(
    var   = sapply(valid, `[`, 2),
    start = as.integer(sapply(valid, `[`, 3)),
    end   = as.integer(sapply(valid, `[`, 4))
  )
}

# Read a combined PISA fixed-width text file (2003, 2006, 2009, 2012).
# Keeps only CNT, AGE, reading/math PVs, and BRR weights.
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
    col_types      = cols(CNT = col_character(), AGE = col_double(),
                          .default = col_double()),
    progress       = TRUE,
    show_col_types = FALSE
  ) |>
    # Standardise BRR weight names: W_FSTR* -> w_fsturwt* (matches .sav files)
    rename_with(\(x) sub("^W_FSTR(\\d+)$", "W_FSTURWT\\1", x)) |>
    rename_with(tolower)
}

# Read a PISA SPSS .sav file (2015, 2018).
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

# =============================================================================
# PISA 2000: separate reading and math files, numeric country codes
# =============================================================================

# Column positions verified against PISA2000_SPSS_student_reading.txt and
# PISA2000_SPSS_student_mathematics.txt (docs/) and confirmed empirically.
#
# Reading file (intstud_read_v3.txt, 1565-char records, 228,784 rows):
#   COUNTRY (num char) : cols  2-4
#   SCHOOLID           : cols  5-9
#   STIDSTD            : cols 10-14
#   PV1READ-PV5READ    : cols 603-637  (7 chars each)
#   W_FSTUWT           : cols 779-787  (9 chars)
#   W_FSTR1-W_FSTR80   : cols 834-1553 (9 chars each)
#
# Math file (intstud_math_v3.txt, 1638-char records, 127,388 rows):
#   COUNTRY, SCHOOLID, STIDSTD  : same positions as reading
#   PV1MATH-PV5MATH             : cols 568-602 (7 chars each)
#   W_FSTUWT, W_FSTR1-80        : same positions as reading
#
# PISA 2000 used Fay BRR with k=0.5 (same as 2003-2018).
# Math was a minor domain: ~127k students (56%) have math PVs; the rest are NA.

# ISO 3166-1 numeric -> alpha-3 mapping for PISA 2000 participants.
# All 43 participating countries (OECD + partner) in one place.
PISA_2000_CNT_MAP <- c(
  "008" = "ALB", "032" = "ARG", "036" = "AUS", "040" = "AUT",
  "056" = "BEL", "076" = "BRA", "100" = "BGR", "124" = "CAN",
  "152" = "CHL", "203" = "CZE", "208" = "DNK", "246" = "FIN",
  "250" = "FRA", "276" = "DEU", "300" = "GRC", "344" = "HKG",
  "348" = "HUN", "352" = "ISL", "360" = "IDN", "372" = "IRL",
  "376" = "ISR", "380" = "ITA", "392" = "JPN", "410" = "KOR",
  "428" = "LVA", "438" = "LIE", "442" = "LUX", "484" = "MEX",
  "528" = "NLD", "554" = "NZL", "578" = "NOR", "604" = "PER",
  "616" = "POL", "620" = "PRT", "642" = "ROU", "643" = "RUS",
  "724" = "ESP", "752" = "SWE", "756" = "CHE", "764" = "THA",
  "807" = "MKD", "826" = "GBR", "840" = "USA"
)

read_pisa_2000 <- function(read_file, math_file) {
  brr_st <- 834L + seq(0L, 79L) * 9L
  brr_en <- brr_st + 8L

  message("    Reading reading file (~342 MB)...")
  df_read <- read_fwf(
    read_file,
    col_positions = fwf_positions(
      start = c(2L, 5L, 10L,
                603L, 610L, 617L, 624L, 631L,
                779L,
                brr_st),
      end   = c(4L, 9L, 14L,
                609L, 616L, 623L, 630L, 637L,
                787L,
                brr_en),
      col_names = c("country_num", "schoolid", "stidstd",
                    "pv1read", "pv2read", "pv3read", "pv4read", "pv5read",
                    "w_fstuwt",
                    paste0("w_fsturwt", 1:80))
    ),
    col_types = cols(
      country_num = col_character(),
      schoolid    = col_character(),
      stidstd     = col_character(),
      .default    = col_double()
    ),
    progress = TRUE, show_col_types = FALSE
  )

  message("    Reading math file (~200 MB)...")
  df_math <- read_fwf(
    math_file,
    col_positions = fwf_positions(
      start = c(2L, 5L, 10L,
                568L, 575L, 582L, 589L, 596L),
      end   = c(4L, 9L, 14L,
                574L, 581L, 588L, 595L, 602L),
      col_names = c("country_num", "schoolid", "stidstd",
                    "pv1math", "pv2math", "pv3math", "pv4math", "pv5math")
    ),
    col_types = cols(
      country_num = col_character(),
      schoolid    = col_character(),
      stidstd     = col_character(),
      .default    = col_double()
    ),
    progress = TRUE, show_col_types = FALSE
  )

  message("    Joining read + math (~56% of students have math PVs)...")
  df <- df_read |>
    left_join(df_math, by = c("country_num", "schoolid", "stidstd")) |>
    mutate(cnt = PISA_2000_CNT_MAP[str_trim(country_num)]) |>
    filter(!is.na(cnt)) |>
    select(-country_num, -schoolid, -stidstd)

  message("    Done: ", nrow(df), " students, ", n_distinct(df$cnt), " countries")
  message("    Math PV coverage: ",
          round(mean(!is.na(df$pv1math)) * 100, 1), "% of students")
  df
}

# =============================================================================
# Cache each year
# =============================================================================

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

message("=== Caching PISA data ===")

# 2000: two-file join (reading + math), 7-char PV fields, Fay k=0.5
cache_year(2000, function() read_pisa_2000(
  file.path(pisa_dir, "intstud_read_v3.txt"),
  file.path(pisa_dir, "intstud_math_v3.txt")
), cache_dir)

# 2003: combined file, 9-char PV/weight fields, Fay k=0.5
cache_year(2003, function() read_pisa_txt(
  file.path(pisa_dir, "INT_stui_2003_v2.txt"),
  file.path(docs_dir, "PISA2003_SPSS_student.txt"),
  n_pvs = 5
), cache_dir)

# 2006: combined file, layout has (F,4) format specifiers
cache_year(2006, function() read_pisa_txt(
  file.path(pisa_dir, "INT_Stu06_Dec07.txt"),
  file.path(docs_dir, "PISA2006_SPSS_student.txt"),
  n_pvs = 5
), cache_dir)

# 2009: combined file
cache_year(2009, function() read_pisa_txt(
  file.path(pisa_dir, "INT_STQ09_DEC11.txt"),
  file.path(docs_dir, "PISA2009_SPSS_student.txt"),
  n_pvs = 5
), cache_dir)

# 2012: combined file, different layout filename
cache_year(2012, function() read_pisa_txt(
  file.path(pisa_dir, "INT_STU12_DEC03.txt"),
  file.path(docs_dir, "SPSS syntax to read in student questionnaire data file.txt"),
  n_pvs = 5
), cache_dir)

# 2015: SPSS .sav (self-describing, 10 PVs)
cache_year(2015, function() read_pisa_sav(
  file.path(pisa_dir, "CY6_MS_CMB_STU_QQQ.sav"),
  n_pvs = 10
), cache_dir)

# 2018: SPSS .sav (self-describing, 10 PVs)
cache_year(2018, function() read_pisa_sav(
  file.path(pisa_dir, "STU/CY07_MSU_STU_QQQ.sav"),
  n_pvs = 10
), cache_dir)

message("\nDone. Cache files:")
fs   <- list.files(cache_dir, "*.rds", full.names = TRUE)
info <- file.info(fs)
cat(sprintf("  %-25s  %s MB\n",
            basename(fs),
            round(info$size / 1e6, 1)), sep = "")
