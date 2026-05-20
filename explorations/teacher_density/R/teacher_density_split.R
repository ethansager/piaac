# teacher_density_split.R
# Split ISCO 23 teachers into non-tertiary (232/233/234/235) and
# tertiary (231 — university and higher education).
#
# Requires ISCO 3- or 4-digit; available in Cycle 1 PUFs for CHL/JPN/KOR/GBR.
# Sweden's PUF suppresses ISCO08_C entirely → omitted from this view.
# Cycle 2 ISCO08_C is suppressed for JPN/KOR/GBR; only CHL retains it.
#
# Outputs:
#   teacher_density_<domain>_c1_split.pdf/.png  Cycle 1, 4 countries
#   teacher_density_<domain>_chl_split.pdf/.png CHL pooled C1+C2 (bonus)
#   teacher_density_split_estimates.rds         Means + gap estimates

set.seed(20260503)

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(purrr)

source(here("01_scripts", "00_helpers.r"))

primary_blue <- "#012169"
primary_gold <- "#f2a900"
accent_gray  <- "#525252"

file_catalog <- tribble(
  ~country, ~cycle, ~file,
  "CHL", 1L, "PRGCHLP1.sav",
  "JPN", 1L, "PRGJPNP1.sav",
  "KOR", 1L, "PRGKORP1.sav",
  "GBR", 1L, "PRGGBRP1.sav",
  "CHL", 2L, "PRGCHLP2.sav"
)

COUNTRY_ORDER <- c("CHL", "JPN", "KOR", "GBR")

# ISCO-08 major group 2-digit codes that are NOT teaching (= "other workers")
OTHER_ISCO2 <- as.character(c(
  "01", "02", "03", "11", "12", "13", "14",
  "21", "22", "24", "25", "26",
  "31", "32", "33", "34", "35",
  "41", "42", "43", "44",
  "51", "52", "53", "54",
  "61", "62", "63",
  "71", "72", "73", "74", "75",
  "81", "82", "83",
  "91", "92", "93", "94", "95", "96"
))

# Classify ISCO08_C → tertiary / non-tertiary / NA
# Codes can be 3-digit (e.g. "234") or 4-digit (e.g. "2341"); use 3-char prefix.
classify_teacher <- function(isco08, isco2) {
  pref3 <- substr(isco08, 1, 3)
  case_when(
    pref3 == "231"                          ~ "Tertiary teachers",
    pref3 %in% c("232", "233", "234", "235") ~ "Non-tertiary teachers",
    isco2 == "23"                           ~ NA_character_,  # bare "23"
    isco2 %in% OTHER_ISCO2                  ~ "Other workers",
    TRUE                                    ~ NA_character_
  )
}

load_one <- function(iso, cyc, fname) {
  d <- read_sav(
    here("00_data", fname),
    col_select = any_of(c("ISCO2C", "ISCO08_C", "SPFWT0",
                          LIT_PVS, NUM_PVS, BRR_WTS))
  )
  d$ISCO2C   <- as.character(d$ISCO2C)
  d$ISCO08_C <- as.character(d$ISCO08_C)
  d <- mutate(d, across(where(haven::is.labelled), as.numeric))

  d |>
    mutate(
      country = iso,
      cycle   = cyc,
      group   = classify_teacher(ISCO08_C, ISCO2C)
    ) |>
    filter(!is.na(group), !is.na(SPFWT0))
}

cat("Loading files...\n")
dat_all <- pmap_dfr(file_catalog, function(country, cycle, file) {
  cat(sprintf("  %s C%d (%s)\n", country, cycle, file))
  load_one(country, cycle, file)
})

# Counts
counts <- dat_all |>
  count(country, cycle, group) |>
  pivot_wider(names_from = group, values_from = n, values_fill = 0)
print(counts)

# ---- Estimation ----
estimate_means <- function(d, pv_cols) {
  pv_group_mean(d, pv_cols, c("country", "group"), "SPFWT0", BRR_WTS)
}

# Gap = group - "Other workers" using premium estimator (BRR covariance preserved)
estimate_gap <- function(d, pv_cols, target_group) {
  d_sub <- d |>
    filter(group %in% c("Other workers", target_group)) |>
    mutate(is_target = as.integer(group == target_group))
  pv_group_premium(d_sub, pv_cols, "country", "is_target",
                   "SPFWT0", BRR_WTS) |>
    rename(gap = premium, n_target = n_college, n_other = n_non_college) |>
    mutate(target = target_group)
}

# ---- Plot ----
make_plot <- function(d, means, gaps, score_label, title_str, caption_str) {
  # Build facet labels with both gaps
  gap_wide <- gaps |>
    mutate(
      lab = sprintf("%s = %+.1f (%.1f)",
                    ifelse(target == "Non-tertiary teachers",
                           "Non-tert", "Tert"),
                    gap, se)
    ) |>
    select(country, target, lab, n_target) |>
    pivot_wider(names_from = target, values_from = c(lab, n_target))

  facet_lab <- gap_wide |>
    transmute(
      country,
      label = sprintf(
        "%s\n%s   (n=%d)\n%s   (n=%d)",
        country,
        `lab_Non-tertiary teachers`,
        `n_target_Non-tertiary teachers`,
        `lab_Tertiary teachers`,
        `n_target_Tertiary teachers`
      )
    )

  ord <- intersect(COUNTRY_ORDER, facet_lab$country)
  lvl <- facet_lab$label[match(ord, facet_lab$country)]

  group_levels <- c("Other workers",
                    "Non-tertiary teachers",
                    "Tertiary teachers")
  pal <- c(
    "Other workers"         = accent_gray,
    "Non-tertiary teachers" = primary_blue,
    "Tertiary teachers"     = primary_gold
  )

  d2 <- d |>
    inner_join(facet_lab, by = "country") |>
    mutate(
      label = factor(label, levels = lvl),
      group = factor(group, levels = group_levels)
    )

  mean_lines <- means |>
    inner_join(facet_lab, by = "country") |>
    mutate(
      label = factor(label, levels = lvl),
      group = factor(group, levels = group_levels)
    )

  ggplot(d2, aes(x = score, fill = group, colour = group,
                 weight = weight)) +
    geom_density(alpha = 0.35, adjust = 1.0, linewidth = 0.4) +
    geom_vline(data = mean_lines,
               aes(xintercept = mean, colour = group),
               linetype = "dashed", linewidth = 0.5,
               show.legend = FALSE) +
    geom_vline(xintercept = c(225, 275), linetype = "dotted",
               colour = accent_gray, linewidth = 0.3) +
    facet_wrap(~ label, ncol = 2) +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    coord_cartesian(xlim = c(100, 400)) +
    labs(
      title = title_str,
      subtitle = paste(
        "Tertiary = ISCO 231 (university/higher-ed teachers).",
        "Non-tertiary = ISCO 232/233/234/235.",
        "Density pooled across 10 PVs.",
        "Dashed lines: PV+BRR group means.",
        "Dotted: Levels 1/2 thresholds (225, 275)."
      ),
      x = score_label, y = "Density",
      fill = NULL, colour = NULL,
      caption = caption_str
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", colour = primary_blue),
      plot.subtitle = element_text(size = 9, colour = accent_gray),
      legend.position = "bottom",
      strip.text    = element_text(face = "bold", size = 9.5)
    )
}

make_pooled <- function(d, pv_cols) {
  d |>
    select(country, group, SPFWT0, all_of(pv_cols)) |>
    pivot_longer(all_of(pv_cols), names_to = "pv",
                 values_to = "score") |>
    filter(!is.na(score)) |>
    mutate(weight = SPFWT0 / length(pv_cols))
}

# ---- Build outputs ----
out_dir <- here("explorations", "teacher_density", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# View: Cycle 1, 4 countries
d_c1 <- dat_all |> filter(cycle == 1L)

# View: CHL pooled C1+C2 (bonus)
d_chl <- dat_all |> filter(country == "CHL")

results <- list()
views <- list(
  c1_split  = list(
    data = d_c1, country_order = COUNTRY_ORDER,
    title_suffix = "Cycle 1 (2012-2014)",
    caption = paste(
      "OECD PIAAC PUF.",
      "Sweden omitted: PUF suppresses ISCO08_C."
    )
  ),
  chl_split = list(
    data = d_chl, country_order = "CHL",
    title_suffix = "Chile, Cycles 1+2 pooled",
    caption = "OECD PIAAC PUF. CHL is the only country with ISCO08_C in C2."
  )
)

for (vname in names(views)) {
  v <- views[[vname]]
  cat(sprintf("\n=== %s ===\n", vname))
  for (dom in c("Literacy", "Numeracy")) {
    pv_cols <- if (dom == "Literacy") LIT_PVS else NUM_PVS
    d_dom <- v$data |> filter(!is.na(.data[[pv_cols[1]]]))

    means <- estimate_means(d_dom, pv_cols)
    gaps  <- bind_rows(
      estimate_gap(d_dom, pv_cols, "Non-tertiary teachers"),
      estimate_gap(d_dom, pv_cols, "Tertiary teachers")
    )

    pooled <- make_pooled(d_dom, pv_cols)
    title_str <- sprintf("PIAAC %s: tertiary vs non-tertiary teachers — %s",
                         tolower(dom), v$title_suffix)
    p <- make_plot(pooled, means, gaps,
                   sprintf("%s score", dom), title_str, v$caption)

    fname_base <- sprintf("teacher_density_%s_%s",
                          tolower(dom), vname)
    ggsave(file.path(out_dir, paste0(fname_base, ".pdf")),
           p, width = 11, height = 7)
    ggsave(file.path(out_dir, paste0(fname_base, ".png")),
           p, width = 11, height = 7, dpi = 150)

    results[[paste(vname, dom, sep = "_")]] <- list(
      means = means, gaps = gaps
    )
    cat(sprintf("  %s gaps:\n", dom))
    print(gaps)
  }
}

saveRDS(results, file.path(out_dir, "teacher_density_split_estimates.rds"))
cat("\nDone.\n")
