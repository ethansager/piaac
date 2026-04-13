# 05_chile_pisa_piaac.r
# Compare PISA (2006-2018) and PIAAC (2014, 2023) literacy/numeracy trends for Chile.
#
# PISA variance: Fay's BRR with k=0.5, 80 replicates.
#   V_W = mean((theta_r - theta_0)^2) / (1 - 0.5)^2  [= 4x standard BRR]
# PIAAC variance: standard BRR (k=0), 80 replicates.
#
# Both use pv_group_mean() from 00_helpers.r -- no Rrepest dependency.
# Cache: pisa_all_countries.rds skips recomputation on subsequent runs.

set.seed(20260412)

library(tidyverse)
library(haven)
library(here)

source(here("01_scripts/00_helpers.r"))

cache_dir <- here("00_data/pisa/cache")
out_dir   <- here("02_output")
fig_dir   <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

primary_blue   <- "#012169"
primary_gold   <- "#f2a900"
accent_gray    <- "#525252"

theme_custom <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", color = primary_blue),
      plot.subtitle = element_text(color = accent_gray),
      legend.position = "bottom"
    )
}

# PISA weight/PV column specs by year (all lowercased in cache)
pisa_specs <- tribble(
  ~year, ~n_pvs,
  2006,  5,
  2009,  5,
  2012,  5,
  2015,  10,
  2018,  10
)
PISA_MAIN_WT  <- "w_fstuwt"
PISA_BRR_WTS  <- paste0("w_fsturwt", 1:80)
PISA_FAY_K    <- 0.5

# =============================================================================
# 1. PISA: all countries (cached after first run)
# =============================================================================

pisa_rds <- file.path(out_dir, "pisa_all_countries.rds")

if (file.exists(pisa_rds)) {
  message("Loading cached PISA estimates...")
  pisa_all <- readRDS(pisa_rds)
} else {
  message("Estimating PISA means (all countries, Fay BRR k=0.5)...")

  pisa_all <- pmap_dfr(pisa_specs, function(year, n_pvs) {
    message("  ", year, "...")
    df <- readRDS(file.path(cache_dir, paste0("pisa_", year, ".rds"))) |>
      # Cap PV values: 9997/9999 are PISA missing codes; scores > 900 are invalid
      mutate(across(starts_with("pv"), \(x) ifelse(x > 900, NA_real_, x)))

    pv_read <- paste0("pv", seq_len(n_pvs), "read")
    pv_math <- paste0("pv", seq_len(n_pvs), "math")

    bind_rows(
      pv_group_mean(df, pv_read, "cnt", PISA_MAIN_WT, PISA_BRR_WTS,
                    fay_k = PISA_FAY_K) |>
        mutate(subject = "Reading"),
      pv_group_mean(df, pv_math, "cnt", PISA_MAIN_WT, PISA_BRR_WTS,
                    fay_k = PISA_FAY_K) |>
        mutate(subject = "Mathematics")
    ) |>
      mutate(year = year, source = "PISA (age 15)")
  })

  message("PISA done. Rows: ", nrow(pisa_all),
          "  Countries: ", n_distinct(pisa_all$cnt))
  saveRDS(pisa_all, pisa_rds)
}

# =============================================================================
# 2. PIAAC: Chile only (standard BRR, k=0)
# =============================================================================

message("Estimating PIAAC means for Chile...")

piaac_raw <- readRDS(file.path(out_dir, "piaac_clean.rds")) |>
  filter(country == "CHL") |>
  filter(if_all(all_of(c(LIT_PVS, NUM_PVS)), \(x) !is.na(x)))

message("  Chile PIAAC rows after NA drop: ", nrow(piaac_raw),
        "  (", paste(sort(unique(piaac_raw$survey_year)), collapse = ", "), ")")

chile_piaac <- bind_rows(
  pv_group_mean(piaac_raw, LIT_PVS, "survey_year", "SPFWT0", BRR_WTS) |>
    mutate(subject = "Literacy"),
  pv_group_mean(piaac_raw, NUM_PVS, "survey_year", "SPFWT0", BRR_WTS) |>
    mutate(subject = "Numeracy")
) |>
  rename(year = survey_year) |>
  mutate(source = "PIAAC (adults 16-65)")

message("PIAAC done.")
print(chile_piaac)

saveRDS(chile_piaac, file.path(out_dir, "chile_piaac.rds"))

# =============================================================================
# 3. Filter PISA to Chile
# =============================================================================

chile_pisa <- pisa_all |>
  filter(cnt == "CHL") |>
  select(year, mean, se, V_W, V_B, n, subject, source)

saveRDS(chile_pisa, file.path(out_dir, "chile_pisa.rds"))

# =============================================================================
# 4. Figures
# =============================================================================

# --- 4a. Chile PISA trends ---------------------------------------------------

p_pisa <- chile_pisa |>
  mutate(ci_lo = mean - 1.96 * se, ci_hi = mean + 1.96 * se) |>
  ggplot(aes(year, mean, color = subject, fill = subject)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(Reading = primary_blue, Mathematics = primary_gold),
                     name = NULL) +
  scale_fill_manual(values  = c(Reading = primary_blue, Mathematics = primary_gold),
                    name = NULL) +
  scale_x_continuous(breaks = pisa_specs$year) +
  labs(title    = "Chile PISA scores, 2006-2018",
       subtitle = "15-year-olds; shaded band = 95% CI",
       x = "PISA cycle", y = "Score") +
  theme_custom()

ggsave(file.path(fig_dir, "chile_pisa_trends.pdf"), p_pisa, width = 8, height = 5)

# --- 4b. Chile PIAAC trends --------------------------------------------------

p_piaac <- chile_piaac |>
  mutate(ci_lo = mean - 1.96 * se, ci_hi = mean + 1.96 * se) |>
  ggplot(aes(year, mean, color = subject, fill = subject)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(Literacy = primary_blue, Numeracy = primary_gold),
                     name = NULL) +
  scale_fill_manual(values  = c(Literacy = primary_blue, Numeracy = primary_gold),
                    name = NULL) +
  scale_x_continuous(breaks = c(2014, 2023)) +
  labs(title    = "Chile PIAAC scores, 2014-2023",
       subtitle = "Adults 16-65; shaded band = 95% CI",
       x = "Survey year", y = "Score") +
  theme_custom()

ggsave(file.path(fig_dir, "chile_piaac_trends.pdf"), p_piaac, width = 8, height = 5)

# --- 4c. Normalised comparison -----------------------------------------------
# Index both surveys to their earliest Chile observation (change from baseline).

chile_pisa_norm <- chile_pisa |>
  mutate(subject = recode(subject, "Reading" = "Literacy",
                          "Mathematics" = "Numeracy")) |>
  group_by(subject) |>
  mutate(index = mean - mean[year == min(year)],
         ci_lo = index - 1.96 * se,
         ci_hi = index + 1.96 * se) |>
  ungroup() |>
  mutate(panel = "PISA (age 15, 2006 = 0)")

chile_piaac_norm <- chile_piaac |>
  group_by(subject) |>
  mutate(index = mean - mean[year == min(year)],
         ci_lo = index - 1.96 * se,
         ci_hi = index + 1.96 * se) |>
  ungroup() |>
  mutate(panel = "PIAAC (adults, 2014 = 0)")

p_norm <- bind_rows(chile_pisa_norm, chile_piaac_norm) |>
  ggplot(aes(year, index, color = subject, fill = subject)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = accent_gray, linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~panel, scales = "free_x") +
  scale_color_manual(values = c(Literacy = primary_blue, Numeracy = primary_gold),
                     name = NULL) +
  scale_fill_manual(values  = c(Literacy = primary_blue, Numeracy = primary_gold),
                    name = NULL) +
  labs(title    = "Chile skill trends: PISA vs PIAAC",
       subtitle = "Change relative to first available year; 95% CI bands",
       x = "Year", y = "Score change (points)") +
  theme_custom()

ggsave(file.path(fig_dir, "chile_pisa_piaac_normalised.pdf"), p_norm,
       width = 10, height = 5)

message("\nFigures saved:")
message("  chile_pisa_trends.pdf")
message("  chile_piaac_trends.pdf")
message("  chile_pisa_piaac_normalised.pdf")
