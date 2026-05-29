# 02_cohort_analysis_pl1.r
# Track birth cohorts across PIAAC rounds using the share scoring at or below
# Level 1 in literacy (PL1-VOTS) rather than mean literacy scores.
# Uses proper PV+BRR variance estimation (Rubin's rules) via pv_group_mean().
#
# Inputs:  02_output/piaac_clean.rds when available; otherwise raw .sav files
#          from 0_data/ or 00_data/
# Outputs: 02_output/cohort_pl1_scores.rds, 02_output/cohort_pl1_change.rds
#          02_output/cohort_pl1_scores_by_gender.rds
#          Figures/cohort_trends_pl1.pdf, Figures/cohort_trends_gender_pl1.pdf
#          Figures/cohort_change_bar_pl1.pdf

set.seed(20260227)

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(here)
library(purrr)

source(here("01_scripts/00_helpers.r"))

# ---- Setup ----
out_dir <- here("02_output")
fig_dir <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Minimal standalone loader ----
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

CY1_YEAR_LOOKUP <- c(
  AUT=2012L, BEL=2012L, CAN=2012L, CZE=2012L, DNK=2012L, EST=2012L,
  FIN=2012L, FRA=2012L, DEU=2012L, IRL=2012L, ITA=2012L, JPN=2012L,
  KOR=2012L, LTU=2012L, NLD=2012L, NOR=2012L, POL=2012L, SVK=2012L,
  ESP=2012L, SWE=2012L, GBR=2012L,
  CHL=2014L, ECU=2014L, HUN=2014L, ISR=2014L, MEX=2014L, NZL=2014L,
  PER=2014L, RUS=2014L, SGP=2014L, TUR=2014L,
  GRC=2017L, KAZ=2017L, SVN=2017L
)

CY2_YEAR_LOOKUP <- c(
  AUT=2023L,
  BEL=2023L, CAN=2023L, CHL=2023L, CZE=2023L, DEU=2023L, DNK=2023L,
  ESP=2023L, EST=2023L, FIN=2023L, FRA=2023L, GBR=2023L, HUN=2023L,
  IRL=2023L, ISR=2023L, ITA=2023L, JPN=2023L, KOR=2023L, LTU=2023L,
  NLD=2023L, NOR=2023L, NZL=2023L, POL=2023L, SGP=2023L, SVK=2023L,
  SWE=2023L, USA=2023L
)

ROUND_OVERRIDE <- list(
  USA = c(`2012` = 1L, `2014` = 1L, `2023` = 2L, `2017` = 3L)
)

parse_filename <- function(f) {
  fname <- basename(f)
  country   <- toupper(substr(fname, 4, 6))
  raw_round <- as.integer(substr(fname, 8, 8))

  year_suffix <- regmatches(
    fname,
    regexpr("_(\\d{4})\\.sav$", fname, ignore.case = TRUE)
  )

  if (length(year_suffix) > 0 && nzchar(year_suffix)) {
    survey_year <- as.integer(substr(year_suffix, 2, 5))
  } else {
    survey_year <- if (raw_round == 1L) {
      unname(CY1_YEAR_LOOKUP[country] %||% 2012L)
    } else {
      unname(CY2_YEAR_LOOKUP[country] %||% 2023L)
    }
  }

  round_num <- if (country %in% names(ROUND_OVERRIDE)) {
    unname(ROUND_OVERRIDE[[country]][as.character(survey_year)] %||% raw_round)
  } else {
    raw_round
  }

  list(country = country, round = round_num, survey_year = survey_year)
}

find_data_dir <- function() {
  candidates <- c(here("0_data"), here("00_data"))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Could not find raw PIAAC data in 0_data/ or 00_data/", call. = FALSE)
  }
  existing[1]
}

KEY_VARS <- c(LIT_PVS, BRR_WTS, "AGE_R", "AGEG10LFS", "AGEG10LFS_T", "SPFWT0", "GENDER_R", "DOORSTEP")

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

  if ("AGEG10LFS_T" %in% names(d) && !"AGEG10LFS" %in% names(d)) {
    d <- rename(d, AGEG10LFS = AGEG10LFS_T)
  }

  age_r_suppressed <- !"AGE_R" %in% names(d) ||
    (all(is.na(d$AGE_R)) && "AGEG10LFS" %in% names(d))

  if (age_r_suppressed && "AGEG10LFS" %in% names(d)) {
    age_midpoints <- c(20L, 30L, 40L, 50L, 60L)
    d$AGE_R <- age_midpoints[as.integer(d$AGEG10LFS)]
    warning(sprintf(
      "[%s R%d] AGE_R suppressed/missing; approximated from AGEG10LFS midpoints",
      meta$country, meta$round
    ))
  }

  d |>
    mutate(
      country      = meta$country,
      round        = meta$round,
      survey_year  = meta$survey_year,
      birth_cohort = survey_year - as.numeric(AGE_R)
    )
}

load_or_build_piaac <- function() {
  piaac_path <- file.path(out_dir, "piaac_clean.rds")

  if (file.exists(piaac_path)) {
    piaac_rds <- tryCatch(readRDS(piaac_path), error = function(e) NULL)
    if (!is.null(piaac_rds)) {
      cat("Loaded clean dataset from 02_output/piaac_clean.rds\n")
      return(exclude_doorstep(piaac_rds))
    }
    cat("02_output/piaac_clean.rds is not readable here; rebuilding from raw files...\n")
  } else {
    cat("02_output/piaac_clean.rds not found; building from raw files...\n")
  }

  data_dir <- find_data_dir()
  sav_files <- list.files(
    data_dir,
    pattern = "\\.sav$",
    ignore.case = TRUE,
    full.names = TRUE
  ) |>
    prefer_us_combined_round1()

  if (length(sav_files) == 0) {
    stop(sprintf("No .sav files found in %s", data_dir), call. = FALSE)
  }

  cat(sprintf("Reading %d raw .sav files from %s\n", length(sav_files), data_dir))
  piaac <- bind_rows(compact(map(sav_files, load_one_file))) |>
    mutate(across(where(haven::is.labelled), as.numeric))

  saveRDS(piaac, piaac_path)
  cat(sprintf("Saved rebuilt clean dataset to %s\n", piaac_path))
  exclude_doorstep(piaac)
}

# ---- Load ----
piaac <- load_or_build_piaac()
cat(sprintf("Loaded: %s rows\n", format(nrow(piaac), big.mark = ",")))

# ---- Birth cohort decades ----
piaac <- mutate(
  piaac,
  cohort_decade = floor(birth_cohort / 10) * 10
)

# Keep plausible cohorts (born 1930–2000) and valid observations
piaac_coh <- filter(
  piaac,
  cohort_decade >= 1930, cohort_decade <= 2000,
  !is.na(PVLIT1), !is.na(SPFWT0), SPFWT0 > 0
)

# ---- Build PL1 indicator plausible values ----
# Apply the threshold within each PV, then combine with Rubin's rules.
PL1_PVS <- paste0("pl1_pv", seq_along(LIT_PVS))
for (i in seq_along(LIT_PVS)) {
  piaac_coh[[PL1_PVS[i]]] <- as.numeric(piaac_coh[[LIT_PVS[i]]] <= 225)
}

# ---- Cohort PL1 shares: PV+BRR estimation ----
cat("Computing cohort PL1 shares (PV+BRR)...\n")
cohort_scores <- pv_group_mean(
  piaac_coh, PL1_PVS,
  c("country", "cohort_decade", "round", "survey_year"),
  "SPFWT0", BRR_WTS
) |>
  rename(share_pl1 = mean, n_obs = n)

saveRDS(cohort_scores, file.path(out_dir, "cohort_pl1_scores.rds"))
cat("Cohort PL1 shares sample (first 12 rows):\n")
print(head(cohort_scores, 12))

# ---- Countries present in both rounds ----
both_rounds <- cohort_scores |>
  distinct(country, round) |>
  group_by(country) |>
  filter(n() >= 2) |>
  pull(country) |>
  unique()

cat(sprintf(
  "\n%d countries in both rounds: %s\n",
  length(both_rounds), paste(sort(both_rounds), collapse = ", ")
))

# ---- Helper: build cohort trend plot ----
make_cohort_plot <- function(plot_data, facet_var = "country", subtitle_extra = "") {
  round_palette <- c("1" = "#012169", "2" = "#f2a900")

  ggplot(
    plot_data,
    aes(
      x = cohort_decade, y = share_pl1,
      color = factor(round), fill = factor(round),
      group = factor(round)
    )
  ) +
    geom_ribbon(
      aes(
        ymin = pmax(share_pl1 - 1.96 * se, 0),
        ymax = pmin(share_pl1 + 1.96 * se, 1)
      ),
      alpha = 0.15, color = NA
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(stats::as.formula(paste("~", facet_var)), ncol = 5) +
    scale_color_manual(
      values = round_palette,
      labels = c("1" = "cy1 (2012/14)", "2" = "cy2 (2023)"),
      name   = "Round"
    ) +
    scale_fill_manual(
      values = round_palette,
      labels = c("1" = "cy1 (2012/14)", "2" = "cy2 (2023)"),
      name   = "Round"
    ) +
    scale_x_continuous(
      breaks = seq(1940, 2000, 20),
      labels = c("1940s", "1960s", "1980s", "2000s")
    ) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1)
    ) +
    labs(
      title = "PIAAC Literacy PL1-VOTS by Birth Cohort: cy1 vs cy2",
      subtitle = paste0(
        "Higher values mean a larger share scoring at or below Level 1 in literacy. ",
        "Same birth decade is ~9-11 yrs older in cy2.",
        if (nchar(subtitle_extra) > 0) paste0("\n", subtitle_extra) else ""
      ),
      x = "Birth decade",
      y = "Share at Level 1 or below (PV+BRR)",
      caption = paste0(
        "Source: PIAAC cy1 & cy2 PUF. Share at or below Level 1 in literacy ",
        "(<=225), computed within each PV and combined with BRR SEs ",
        "(Rubin's rules), SPFWT0 weights.\nShading = ±1.96 SE. ",
        "Cells with n<50 suppressed. cy1 field dates: 2012 (most OECD), ",
        "2014 (CHL, ISR, SGP, etc.), 2017 (GRC, KAZ, SVN). cy2 = Cycle 2 (2023)."
      )
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title      = element_text(face = "bold", color = "#012169", size = 13),
      plot.subtitle   = element_text(size = 10),
      legend.position = "bottom",
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      strip.text      = element_text(size = 8, face = "bold")
    )
}

# ---- Plot: all-country cohort trends ----
plot_data <- cohort_scores |>
  filter(country %in% both_rounds, round %in% c(1, 2), cohort_decade >= 1940, n_obs >= 50)

p_cohort <- make_cohort_plot(plot_data)

fig_path <- file.path(fig_dir, "cohort_trends_pl1.pdf")
ggsave(fig_path, p_cohort, width = 14, height = 10)
ggsave(file.path(fig_dir, "cohort_trends_pl1.png"), p_cohort, width = 14, height = 10, dpi = 150)
cat(sprintf("Saved figure: %s\n", fig_path))

# ---- Cohort change: cy2 - cy1 for matched birth decades ----
cohort_change <- cohort_scores |>
  filter(country %in% both_rounds, n_obs >= 50) |>
  select(country, cohort_decade, round, share_pl1, se) |>
  pivot_wider(
    names_from = round, values_from = c(share_pl1, se),
    names_sep = "_r"
  ) |>
  filter(!is.na(share_pl1_r1), !is.na(share_pl1_r2)) |>
  mutate(
    change = share_pl1_r2 - share_pl1_r1,
    change_pp = change * 100,
    se_change = sqrt(se_r1^2 + se_r2^2),
    se_change_pp = se_change * 100,
    direction = if_else(change > 0, "increase", "decrease")
  )

saveRDS(cohort_change, file.path(out_dir, "cohort_pl1_change.rds"))

cat("\nAverage cohort PL1 change by country (cy2 - cy1, same birth decade):\n")
country_summary <- cohort_change |>
  group_by(country) |>
  summarise(
    mean_change_pp = mean(change_pp, na.rm = TRUE),
    n_cohorts      = n(),
    pct_increase   = mean(direction == "increase") * 100,
    .groups        = "drop"
  ) |>
  arrange(desc(mean_change_pp))
print(country_summary, n = Inf)

# ---- Ranked bar chart of mean cohort changes ----
country_bar <- country_summary |>
  mutate(
    country_fac = factor(country, levels = rev(country)),
    bar_color   = if_else(mean_change_pp > 0, "#b91c1c", "#15803d")
  )

p_change_bar <- ggplot(
  country_bar,
  aes(x = country_fac, y = mean_change_pp, fill = bar_color)
) +
  geom_col(width = 0.75) +
  geom_text(
    aes(
      label = sprintf("%.1f", mean_change_pp),
      hjust = if_else(mean_change_pp > 0, -0.15, 1.15)
    ),
    size = 3.0, color = "#222222"
  ) +
  geom_hline(yintercept = 0, color = "#222222", linewidth = 0.5) +
  coord_flip(clip = "off") +
  scale_fill_identity() +
  scale_y_continuous(
    limits = c(
      min(0, min(country_bar$mean_change_pp) * 1.18),
      max(0, max(country_bar$mean_change_pp) * 1.18)
    ),
    labels = function(x) paste0(x, " pp")
  ) +
  labs(
    title = "Mean Cohort PL1-VOTS Change: Cycle 2 vs. Cycle 1",
    subtitle = paste0(
      "Average change across matched birth-decade cohorts within each country.\n",
      "Positive = cohort had a higher PL1-VOTS share in Cycle 2 (2023) than in Cycle 1 (2012/14)."
    ),
    x = NULL,
    y = "Mean cohort PL1-VOTS change (percentage points)",
    caption = paste0(
      "Source: PIAAC cy1 & cy2 PUF. Average across all matched birth decades per country.\n",
      "Share at or below Level 1 in literacy, PV+BRR estimation, SPFWT0 weights. ",
      "Same birth decade is ~9-11 years older in cy2."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#012169", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "none",
    plot.margin = margin(5, 50, 5, 5)
  )

ggsave(file.path(fig_dir, "cohort_change_bar_pl1.pdf"), p_change_bar, width = 10, height = 8)
ggsave(file.path(fig_dir, "cohort_change_bar_pl1.png"), p_change_bar, width = 10, height = 8, dpi = 150)
cat(sprintf("Saved figure: %s\n", file.path(fig_dir, "cohort_change_bar_pl1.pdf")))

# ---- Gender stratification ----
cat("\nComputing cohort PL1 shares by gender (PV+BRR)...\n")
cohort_scores_gender <- pv_group_mean(
  piaac_coh |> filter(!is.na(GENDER_R)),
  PL1_PVS,
  c("country", "cohort_decade", "round", "survey_year", "GENDER_R"),
  "SPFWT0", BRR_WTS
) |>
  rename(share_pl1 = mean, n_obs = n) |>
  mutate(
    gender_label = if_else(GENDER_R == 1, "Male", "Female"),
    facet_label = paste0(country, " (", gender_label, ")")
  )

saveRDS(cohort_scores_gender, file.path(out_dir, "cohort_pl1_scores_by_gender.rds"))

# ---- Plot: gender cohort trends (first 10 countries for readability) ----
plot_gender <- cohort_scores_gender |>
  filter(country %in% both_rounds, round %in% c(1, 2), cohort_decade >= 1940, n_obs >= 50)

top_countries <- sort(unique(plot_gender$country))[1:min(10, length(unique(plot_gender$country)))]
plot_gender_top <- filter(plot_gender, country %in% top_countries)

p_gender <- make_cohort_plot(plot_gender_top, facet_var = "facet_label", subtitle_extra = "By gender")

fig_path_gender <- file.path(fig_dir, "cohort_trends_gender_pl1.pdf")
ggsave(fig_path_gender, p_gender, width = 14, height = 10)
ggsave(file.path(fig_dir, "cohort_trends_gender_pl1.png"), p_gender, width = 14, height = 10, dpi = 150)
cat(sprintf("Saved figure: %s\n", fig_path_gender))

cat("\nDone: PL1 cohort analysis complete.\n")
