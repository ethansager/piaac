# teacher_density.R
# Kernel density of PIAAC literacy and numeracy: teachers vs other workers.
# Teachers = ISCO-08 major group 23 (Teaching Professionals).
#
# Methodology:
#   - Density curves: PV-pooled — stack all 10 PVs, each weighted SPFWT0/10.
#   - Group means + gap SEs: proper PV+BRR via pv_group_mean / pv_group_premium
#     (Rubin's rules for imputation variance + BRR sampling variance).
#
# Country coverage by cycle (ISCO 2-digit availability in PUF):
#   Cycle 1 (2012-2014): CHL, JPN, KOR, GBR, SWE — all available
#   Cycle 2 (2023):      CHL, JPN, KOR available; GBR suppressed; SWE absent
#   Finland: ISCO2C suppressed in BOTH cycles — substitute Sweden.
#
# Three figure sets produced for each domain (literacy, numeracy):
#   _c1.pdf       Cycle 1 only (5 countries)
#   _c2.pdf       Cycle 2 only (3 countries)
#   _combined.pdf C1+C2 pooled where available (5 countries; GBR/SWE C1 only)

set.seed(20260503)

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(purrr)

source(here("01_scripts", "00_helpers.r"))

primary_blue <- "#012169"
accent_gray  <- "#525252"

# ---- File catalog: country x cycle ----
file_catalog <- tribble(
  ~country, ~cycle, ~file,
  "CHL", 1L, "PRGCHLP1.sav",
  "JPN", 1L, "PRGJPNP1.sav",
  "KOR", 1L, "PRGKORP1.sav",
  "GBR", 1L, "PRGGBRP1.sav",
  "SWE", 1L, "PRGSWEP1.sav",
  "CHL", 2L, "PRGCHLP2.sav",
  "JPN", 2L, "PRGJPNP2.sav",
  "KOR", 2L, "PRGKORP2.sav"
)

COUNTRY_ORDER <- c("CHL", "JPN", "KOR", "GBR", "SWE")

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

load_one <- function(iso, cyc, fname) {
  d <- read_sav(
    here("00_data", fname),
    col_select = any_of(c("ISCO2C", "SPFWT0", LIT_PVS, NUM_PVS, BRR_WTS))
  )
  d$ISCO2C <- as.character(d$ISCO2C)
  d <- mutate(d, across(where(haven::is.labelled), as.numeric))

  d |>
    mutate(
      country = iso,
      cycle   = cyc,
      isco2   = ISCO2C,
      teacher = case_when(
        isco2 == "23"          ~ "Teachers (ISCO 23)",
        isco2 %in% OTHER_ISCO2 ~ "Other workers",
        TRUE                   ~ NA_character_
      )
    ) |>
    filter(!is.na(teacher), !is.na(SPFWT0))
}

cat("Loading files...\n")
dat_all <- pmap_dfr(file_catalog, function(country, cycle, file) {
  cat(sprintf("  %s cycle %d (%s)\n", country, cycle, file))
  load_one(country, cycle, file)
})

# ---- Subsets per view ----
make_subset <- function(view) {
  switch(view,
    c1       = dat_all |> filter(cycle == 1L),
    c2       = dat_all |> filter(cycle == 2L),
    combined = dat_all
  )
}

# ---- Estimation: per (country, teacher) means; per country gap ----
estimate_view <- function(d, pv_cols, view) {
  d <- d |> filter(!is.na(.data[[pv_cols[1]]]))

  means <- pv_group_mean(d, pv_cols, c("country", "teacher"),
                         "SPFWT0", BRR_WTS)

  gap <- pv_group_premium(
    d |> mutate(is_teacher = as.integer(teacher == "Teachers (ISCO 23)")),
    pv_cols, "country", "is_teacher", "SPFWT0", BRR_WTS
  ) |> rename(gap = premium, n_teach = n_college, n_other = n_non_college)

  list(means = means, gap = gap)
}

# ---- Pool PVs for density visualization ----
make_pooled <- function(d, pv_cols) {
  d |>
    select(country, teacher, SPFWT0, all_of(pv_cols)) |>
    pivot_longer(all_of(pv_cols), names_to = "pv",
                 values_to = "score") |>
    filter(!is.na(score)) |>
    mutate(weight = SPFWT0 / length(pv_cols))
}

# ---- Plot ----
make_plot <- function(pooled, est, view, score_label, title_dom) {
  facet_lab <- est$gap |>
    transmute(
      country,
      n_teach,
      label = sprintf("%s  gap = %+.1f (%.1f)\nteachers n=%d",
                      country, gap, se, n_teach)
    )

  ord <- intersect(COUNTRY_ORDER, facet_lab$country)
  lvl <- facet_lab$label[match(ord, facet_lab$country)]

  d <- pooled |>
    inner_join(facet_lab, by = "country") |>
    mutate(
      label   = factor(label, levels = lvl),
      teacher = factor(teacher,
        levels = c("Other workers", "Teachers (ISCO 23)"))
    )

  mean_lines <- est$means |>
    inner_join(facet_lab, by = "country") |>
    mutate(
      label   = factor(label, levels = lvl),
      teacher = factor(teacher,
        levels = c("Other workers", "Teachers (ISCO 23)"))
    )

  cycle_str <- switch(view,
    c1       = "Cycle 1 (2012-2014)",
    c2       = "Cycle 2 (2023)",
    combined = "Cycles 1 + 2 pooled"
  )

  caption <- if (view == "c2") {
    "OECD PIAAC public-use files. GBR and SWE omitted (no C2 ISCO 2-digit)."
  } else if (view == "combined") {
    paste0("OECD PIAAC public-use files. CHL/JPN/KOR pool C1+C2; ",
           "GBR and SWE are C1 only (no C2 ISCO 2-digit).")
  } else {
    "OECD PIAAC public-use files. SWE shown for FIN (FIN PUF suppresses ISCO2)."
  }

  ggplot(d, aes(x = score, fill = teacher, colour = teacher,
                weight = weight)) +
    geom_density(alpha = 0.40, adjust = 1.0, linewidth = 0.4) +
    geom_vline(data = mean_lines,
               aes(xintercept = mean, colour = teacher),
               linetype = "dashed", linewidth = 0.5,
               show.legend = FALSE) +
    geom_vline(xintercept = c(225, 275), linetype = "dotted",
               colour = accent_gray, linewidth = 0.3) +
    facet_wrap(~ label, ncol = 3) +
    scale_fill_manual(values = c(
      "Other workers"      = accent_gray,
      "Teachers (ISCO 23)" = primary_blue
    )) +
    scale_colour_manual(values = c(
      "Other workers"      = accent_gray,
      "Teachers (ISCO 23)" = primary_blue
    )) +
    coord_cartesian(xlim = c(100, 400)) +
    labs(
      title = sprintf("PIAAC %s: teachers vs other workers — %s",
                      tolower(title_dom), cycle_str),
      subtitle = paste(
        "Density pooled across 10 PVs.",
        "Dashed lines: PV+BRR group means.",
        "Dotted: Levels 1/2 thresholds (225, 275).",
        "Facet header: gap (SE) via Rubin's rules + BRR."
      ),
      x = score_label, y = "Density",
      fill = NULL, colour = NULL,
      caption = caption
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", colour = primary_blue),
      plot.subtitle = element_text(size = 9, colour = accent_gray),
      legend.position = "bottom",
      strip.text    = element_text(face = "bold", size = 10)
    )
}

out_dir <- here("explorations", "teacher_density", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()
for (view in c("c1", "c2", "combined")) {
  cat(sprintf("\n=== View: %s ===\n", view))
  d <- make_subset(view)

  for (dom in c("Literacy", "Numeracy")) {
    pv_cols <- if (dom == "Literacy") LIT_PVS else NUM_PVS
    est <- estimate_view(d, pv_cols, view)
    pooled <- make_pooled(
      d |> filter(!is.na(.data[[pv_cols[1]]])),
      pv_cols
    )
    score_label <- sprintf("%s score", dom)
    p <- make_plot(pooled, est, view, score_label, dom)

    suffix <- sprintf("%s_%s", tolower(dom), view)
    ggsave(file.path(out_dir, sprintf("teacher_density_%s.pdf", suffix)),
           p, width = 11, height = 6.5)
    ggsave(file.path(out_dir, sprintf("teacher_density_%s.png", suffix)),
           p, width = 11, height = 6.5, dpi = 150)

    est$view   <- view
    est$domain <- dom
    results[[paste(view, dom, sep = "_")]] <- est

    cat(sprintf("  %s — gap estimates:\n", dom))
    print(est$gap)
  }
}

saveRDS(results, file.path(out_dir, "teacher_density_estimates.rds"))
cat("\nDone. Files written to:", out_dir, "\n")
