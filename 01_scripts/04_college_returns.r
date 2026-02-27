# 04_college_returns.r
# College literacy premium decomposition across PIAAC rounds.
# Simplified first pass: PV1 only, weighted means, no BRR.
#
# Inputs:  02_output/piaac_clean.rds
# Outputs: 02_output/college_returns.rds
#          Figures/college_premium.pdf

set.seed(20260227)

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# ---- Setup ----
out_dir <- here("02_output")
fig_dir <- here("Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load ----
piaac <- readRDS(file.path(out_dir, "piaac_clean.rds"))
cat(sprintf("Loaded: %s rows\n", format(nrow(piaac), big.mark = ",")))

# ---- Define college: EDCAT7 ≥ 6 (bachelor's or higher; ISCED 5A/6) ----
piaac_edu <- piaac |>
  filter(
    !is.na(PVLIT1), !is.na(EDCAT7), !is.na(SPFWT0),
    SPFWT0 > 0,
    EDCAT7 >= 1, EDCAT7 <= 7      # valid codes only
  ) |>
  mutate(college = as.integer(EDCAT7 >= 6))

cat(sprintf("Valid education obs: %s\n", format(nrow(piaac_edu), big.mark = ",")))

# ---- Weighted means by country × round × college group ----
college_means <- piaac_edu |>
  group_by(country, round, survey_year, college) |>
  summarise(
    mean_lit = weighted.mean(PVLIT1, SPFWT0, na.rm = TRUE),
    n_obs    = n(),
    .groups  = "drop"
  )

# ---- College premium ----
college_premium <- college_means |>
  select(country, round, survey_year, college, mean_lit) |>
  pivot_wider(
    names_from  = college,
    values_from = mean_lit,
    names_prefix = "edu_"
  ) |>
  rename(non_college = edu_0, college_score = edu_1) |>
  mutate(premium = college_score - non_college) |>
  filter(!is.na(premium))

saveRDS(college_premium, file.path(out_dir, "college_returns.rds"))

cat("\nCollege premium by country × round (sorted by cy1 premium):\n")
prem_wide <- college_premium |>
  select(country, round, premium) |>
  pivot_wider(names_from = round, values_from = premium, names_prefix = "r") |>
  mutate(change = r2 - r1) |>
  arrange(desc(r1))
print(prem_wide, n = Inf)

# ---- College share by country × round ----
college_share <- piaac_edu |>
  group_by(country, round, survey_year) |>
  summarise(
    college_share = weighted.mean(college, SPFWT0, na.rm = TRUE) * 100,
    mean_overall  = weighted.mean(PVLIT1, SPFWT0, na.rm = TRUE),
    n_obs         = n(),
    .groups       = "drop"
  )

cat("\nCollege share and overall mean by country × round:\n")
print(college_share, n = Inf)

# ---- Email back-of-envelope check (all countries pooled) ----
# cy1 reference: 40-45 yr olds in 2013 → birth cohort ≈ 1968-1973
# cy2 reference: 30-35 yr olds in 2017 → birth cohort ≈ 1982-1987
cat("\n--- Email back-of-envelope check ---\n")
cat("cy1 (round 1): birth cohort 1968-1973 (~40-45 yr olds in 2013)\n")
boe_cy1 <- piaac_edu |>
  filter(round == 1, birth_cohort >= 1968, birth_cohort <= 1973) |>
  summarise(
    mean_overall  = weighted.mean(PVLIT1, SPFWT0, na.rm = TRUE),
    college_share = weighted.mean(college, SPFWT0, na.rm = TRUE) * 100,
    n_obs         = n()
  )
print(boe_cy1)
cat(sprintf("  Implied: %.0f%% college, %.0f%% non-college → %.1f*X + %.1f*Y = %.1f\n",
            boe_cy1$college_share, 100 - boe_cy1$college_share,
            boe_cy1$college_share / 100, 1 - boe_cy1$college_share / 100,
            boe_cy1$mean_overall))

cat("\ncy2 (round 2): birth cohort 1982-1987 (~30-35 yr olds in 2017)\n")
boe_cy2 <- piaac_edu |>
  filter(round == 2, birth_cohort >= 1982, birth_cohort <= 1987) |>
  summarise(
    mean_overall  = weighted.mean(PVLIT1, SPFWT0, na.rm = TRUE),
    college_share = weighted.mean(college, SPFWT0, na.rm = TRUE) * 100,
    n_obs         = n()
  )
print(boe_cy2)
cat(sprintf("  Implied: %.0f%% college, %.0f%% non-college → %.1f*X1 + %.1f*Y1 = %.1f\n",
            boe_cy2$college_share, 100 - boe_cy2$college_share,
            boe_cy2$college_share / 100, 1 - boe_cy2$college_share / 100,
            boe_cy2$mean_overall))

# ---- Countries in both rounds ----
both_rounds <- college_premium |>
  distinct(country, round) |>
  group_by(country) |>
  filter(n() == 2) |>
  pull(country) |>
  unique()

cat(sprintf("\n%d countries in both rounds: %s\n",
            length(both_rounds), paste(sort(both_rounds), collapse = ", ")))

# ---- Plot: college premium cy1 vs cy2 ----
plot_data <- college_premium |>
  filter(country %in% both_rounds) |>
  mutate(round_label = if_else(round == 1, "cy1 (~2013)", "cy2 (~2017)"))

# Sort countries by cy1 premium descending
country_order <- plot_data |>
  filter(round == 1) |>
  arrange(desc(premium)) |>
  pull(country)
plot_data <- mutate(plot_data, country = factor(country, levels = rev(country_order)))

p_premium <- ggplot(plot_data,
                    aes(x = country, y = premium, fill = round_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c("cy1 (~2013)" = "#012169", "cy2 (~2017)" = "#f2a900"),
    name   = "Round"
  ) +
  labs(
    title    = "College Literacy Premium: cy1 (~2013) vs cy2 (~2017)",
    subtitle = "Premium = mean(bachelor's+) \u2013 mean(non-college). Countries in both rounds only.",
    x        = NULL,
    y        = "Score premium (PV1, weighted)",
    caption  = paste0("Source: PIAAC cy1 & cy2 PUF. PV1 only, SPFWT0 weights.\n",
                      "College = EDCAT7 \u2265 6 (bachelor's or higher).")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", color = "#012169", size = 13),
    plot.subtitle   = element_text(size = 10),
    legend.position = "bottom",
    axis.text.y     = element_text(size = 9)
  )

fig_path <- file.path(fig_dir, "college_premium.pdf")
ggsave(fig_path, p_premium, width = 10, height = 8)
cat(sprintf("\nSaved figure: %s\n", fig_path))
cat("Done: college returns complete.\n")
