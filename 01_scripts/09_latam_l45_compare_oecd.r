#!/usr/bin/env Rscript

# 09_latam_l45_compare_oecd.r
# Recreate the Cycle 1 Latin America top-end proficiency table for Chile,
# Mexico, Ecuador, and Peru, and export the result as both CSV and an
# Excel-readable .xls workbook.
#
# The table reports:
#   - sample size
#   - weighted population
#   - Level 4/5 combined (score > 325)
#   - Level 5 only (score > 375)
#
# Outputs:
#   02_output/latam_top_levels.csv
#   02_output/latam_top_levels.xls

library(haven)
library(here)
library(dplyr)

# ---- Setup ----
root_candidates <- c(here(), here("piaac"))
piaac_root <- root_candidates[dir.exists(file.path(root_candidates, "01_scripts"))][1]
if (is.na(piaac_root)) {
  stop("Could not locate the PIAAC analysis root. Checked: . and ./piaac")
}

out_dir <- file.path(piaac_root, "02_output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_dir_candidates <- c(
  file.path(piaac_root, "0_data"),
  file.path(piaac_root, "00_data")
)
data_dir <- data_dir_candidates[file.exists(data_dir_candidates)][1]
if (is.na(data_dir)) {
  stop("Could not find raw PIAAC data directory. Checked: 0_data and 00_data")
}

country_files <- c(
  Chile = "prgchlp1.sav",
  Mexico = "prgmexp1.sav",
  Ecuador = "prgecup1.sav",
  Peru = "prgperp1.sav"
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
calc_unweighted_count <- function(data, prefix, cutoff = 325) {
  pv_names <- paste0(prefix, 1:10)
  mean(vapply(
    pv_names,
    function(v) sum(data[[v]] > cutoff, na.rm = TRUE),
    numeric(1)
  ))
}

calc_weighted_share <- function(data, prefix, cutoff = 325) {
  pv_names <- paste0(prefix, 1:10)
  mean(vapply(
    pv_names,
    function(v) weighted.mean(data[[v]] > cutoff, w = data$SPFWT0, na.rm = TRUE),
    numeric(1)
  ))
}

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&apos;", x, fixed = TRUE)
}

write_excel_xml <- function(data, path, sheet_name = "LATAM_L45") {
  cell_xml <- function(value, header = FALSE) {
    if (header || is.character(value)) {
      sprintf(
        "<Cell%s><Data ss:Type=\"String\">%s</Data></Cell>",
        if (header) " ss:StyleID=\"Header\"" else "",
        xml_escape(as.character(value))
      )
    } else if (is.numeric(value) && !is.na(value)) {
      sprintf(
        "<Cell><Data ss:Type=\"Number\">%s</Data></Cell>",
        format(value, scientific = FALSE, trim = TRUE)
      )
    } else if (is.logical(value) && !is.na(value)) {
      sprintf(
        "<Cell><Data ss:Type=\"String\">%s</Data></Cell>",
        if (value) "TRUE" else "FALSE"
      )
    } else {
      "<Cell><Data ss:Type=\"String\"></Data></Cell>"
    }
  }

  header_row <- paste0(
    "<Row>",
    paste(vapply(names(data), cell_xml, character(1), header = TRUE), collapse = ""),
    "</Row>"
  )

  data_rows <- vapply(seq_len(nrow(data)), function(i) {
    paste0(
      "<Row>",
      paste(vapply(data[i, ], function(x) cell_xml(x[[1]]), character(1)), collapse = ""),
      "</Row>"
    )
  }, character(1))

  workbook <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<?mso-application progid=\"Excel.Sheet\"?>\n",
    "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\" ",
    "xmlns:o=\"urn:schemas-microsoft-com:office:office\" ",
    "xmlns:x=\"urn:schemas-microsoft-com:office:excel\" ",
    "xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\" ",
    "xmlns:html=\"http://www.w3.org/TR/REC-html40\">\n",
    "<Styles>\n",
    "<Style ss:ID=\"Default\" ss:Name=\"Normal\"/>\n",
    "<Style ss:ID=\"Header\"><Font ss:Bold=\"1\"/></Style>\n",
    "</Styles>\n",
    "<Worksheet ss:Name=\"", xml_escape(sheet_name), "\">\n",
    "<Table>\n",
    header_row, "\n",
    paste(data_rows, collapse = "\n"), "\n",
    "</Table>\n",
    "</Worksheet>\n",
    "</Workbook>\n"
  )

  writeLines(workbook, con = path, useBytes = TRUE)
}

build_country_row <- function(country_name, filename) {
  data <- read_sav(
    file.path(data_dir, filename),
    col_select = any_of(c("SPFWT0", paste0("PVLIT", 1:10), paste0("PVNUM", 1:10)))
  )

  weighted_population <- sum(data$SPFWT0, na.rm = TRUE)
  lit_l45_share <- calc_weighted_share(data, "PVLIT", cutoff = 325)
  num_l45_share <- calc_weighted_share(data, "PVNUM", cutoff = 325)
  lit_l5_share <- calc_weighted_share(data, "PVLIT", cutoff = 375)
  num_l5_share <- calc_weighted_share(data, "PVNUM", cutoff = 375)

  tibble(
    country = country_name,
    sample_size = nrow(data),
    weighted_population = round(weighted_population),
    sample_l45_lit = round(calc_unweighted_count(data, "PVLIT", cutoff = 325), 1),
    weighted_l45_lit = round(weighted_population * lit_l45_share),
    l45_lit_pct = round(100 * lit_l45_share, 2),
    sample_l45_num = round(calc_unweighted_count(data, "PVNUM", cutoff = 325), 1),
    weighted_l45_num = round(weighted_population * num_l45_share),
    l45_num_pct = round(100 * num_l45_share, 2),
    sample_l5_lit = round(calc_unweighted_count(data, "PVLIT", cutoff = 375), 1),
    weighted_l5_lit = round(weighted_population * lit_l5_share),
    l5_lit_pct = round(100 * lit_l5_share, 2),
    sample_l5_num = round(calc_unweighted_count(data, "PVNUM", cutoff = 375), 1),
    weighted_l5_num = round(weighted_population * num_l5_share),
    l5_num_pct = round(100 * num_l5_share, 2)
  )
}

# ---- Build export table ----
top_levels_table <- bind_rows(lapply(
  names(country_files),
  function(country_name) build_country_row(country_name, country_files[[country_name]])
))

csv_path <- file.path(out_dir, "latam_top_levels.csv")
xls_path <- file.path(out_dir, "latam_top_levels.xls")

write.csv(top_levels_table, csv_path, row.names = FALSE, na = "")
write_excel_xml(top_levels_table, xls_path, sheet_name = "LATAM_TopLevels")

cat("\nWrote comparison table to:\n")
cat(" - ", csv_path, "\n", sep = "")
cat(" - ", xls_path, "\n", sep = "")
cat("\nPreview:\n")
print(top_levels_table)
