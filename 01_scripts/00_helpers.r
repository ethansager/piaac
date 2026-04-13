# 00_helpers.r
# PV+BRR helper functions for proper OECD PIAAC variance estimation.
# Source this file at the top of each analysis script.
#
# Per OECD PIAAC Technical Report §15.2.3:
#   Never average PVs at the person level. Run the analysis M=10 times (once per PV),
#   average the M estimates, then combine sampling variance (BRR) and imputation
#   variance (Rubin's rules).
#
# Total variance formula:
#   V = V_W + (1 + 1/M) * V_B
#   V_W = (1/M) Σ_k V_BRR_k        mean BRR sampling variance across PVs
#   V_B = (1/(M-1)) Σ_k (θ̂_k - θ̄)² imputation variance across PVs

library(dplyr)

# ---- Constants ----
LIT_PVS <- paste0("PVLIT", 1:10)
NUM_PVS <- paste0("PVNUM", 1:10)
BRR_WTS <- paste0("SPFWT", 1:80)

# ---- pv_group_mean ----
# Compute group means with proper PV+BRR variance using Rubin's rules.
#
# @param data       Data frame with PV columns, weight columns, and group vars
# @param pv_cols    Character vector of PV column names (length M, typically 10)
# @param group_vars Character vector of grouping variable names
# @param final_wt   Name of the final (full-sample) weight column
# @param brr_wts    Character vector of BRR replicate weight column names (length R)
# @param fay_k      Fay's BRR coefficient (default 0 = standard BRR).
#                   Use 0.5 for PISA (Fay's method). The replicate variance is
#                   divided by (1 - fay_k)^2 per OECD Technical Reports.
# @return Tibble with one row per group: mean, se, V_W, V_B, n
pv_group_mean <- function(data, pv_cols, group_vars, final_wt, brr_wts,
                          fay_k = 0) {
  M        <- length(pv_cols)
  fay_corr <- (1 - fay_k)^2   # Fay correction: V_W_k /= (1-k)^2

  data |>
    group_by(across(all_of(group_vars))) |>
    group_modify(function(grp, key) {
      n_grp <- nrow(grp)
      w0    <- grp[[final_wt]]

      # BRR weight matrix: n x R
      W_brr <- as.matrix(grp[, brr_wts])
      W_brr_colsums <- colSums(W_brr)   # length R
      w0_sum <- sum(w0)

      theta_k <- numeric(M)
      V_W_k   <- numeric(M)

      for (k in seq_len(M)) {
        x     <- grp[[pv_cols[k]]]
        valid <- !is.na(x)

        # Point estimate: weighted mean over non-NA observations
        w0_v  <- w0[valid]
        x_v   <- x[valid]
        theta_k[k] <- sum(x_v * w0_v) / sum(w0_v)

        # BRR replicates: restrict to non-NA rows for each replicate
        W_v    <- W_brr[valid, , drop = FALSE]
        theta_r <- colSums(x_v * W_v) / colSums(W_v)  # length R

        # Fay-corrected replicate variance: divide by (1 - k)^2
        V_W_k[k] <- mean((theta_r - theta_k[k])^2) / fay_corr
      }

      # Rubin's rules
      theta_bar <- mean(theta_k)                            # point estimate
      V_B       <- sum((theta_k - theta_bar)^2) / (M - 1)  # imputation variance
      V_W       <- mean(V_W_k)                              # avg sampling variance
      V_total   <- V_W + (1 + 1 / M) * V_B

      tibble(
        mean = theta_bar,
        se   = sqrt(V_total),
        V_W  = V_W,
        V_B  = V_B,
        n    = n_grp
      )
    }) |>
    ungroup()
}

# ---- pv_group_premium ----
# Compute college literacy premium (college - non-college) with PV+BRR SEs.
# Variance is computed directly on the premium estimator (not difference of
# independently estimated means), so BRR covariance is captured correctly.
#
# @param data        Data frame
# @param pv_cols     Character vector of PV column names (length M)
# @param group_vars  Character vector of grouping variable names (e.g. country x round)
# @param college_var Name of 0/1 college indicator column
# @param final_wt    Name of the final weight column
# @param brr_wts     Character vector of BRR replicate weight column names
# @return Tibble: group vars + premium, se, V_W, V_B, n_college, n_non_college
pv_group_premium <- function(data, pv_cols, group_vars, college_var,
                             final_wt, brr_wts) {
  M <- length(pv_cols)

  data |>
    group_by(across(all_of(group_vars))) |>
    group_modify(function(grp, key) {
      d_col  <- grp[grp[[college_var]] == 1L, ]
      d_ncol <- grp[grp[[college_var]] == 0L, ]

      n_col  <- nrow(d_col)
      n_ncol <- nrow(d_ncol)

      if (n_col == 0 || n_ncol == 0) {
        return(tibble(
          premium = NA_real_, se = NA_real_,
          V_W = NA_real_, V_B = NA_real_,
          n_college = n_col, n_non_college = n_ncol
        ))
      }

      # Extract weights
      w0_col  <- d_col[[final_wt]];  w0_ncol <- d_ncol[[final_wt]]
      W_col   <- as.matrix(d_col[, brr_wts])
      W_ncol  <- as.matrix(d_ncol[, brr_wts])

      Wcs_col  <- colSums(W_col)
      Wcs_ncol <- colSums(W_ncol)

      prem_k <- numeric(M)
      V_W_k  <- numeric(M)

      for (k in seq_len(M)) {
        x_col  <- d_col[[pv_cols[k]]]
        x_ncol <- d_ncol[[pv_cols[k]]]

        # Point estimates
        mu_col_k  <- sum(x_col  * w0_col)  / sum(w0_col)
        mu_ncol_k <- sum(x_ncol * w0_ncol) / sum(w0_ncol)
        prem_k[k] <- mu_col_k - mu_ncol_k

        # BRR replicates (vectorized)
        mu_col_r  <- colSums(x_col  * W_col)  / Wcs_col
        mu_ncol_r <- colSums(x_ncol * W_ncol) / Wcs_ncol
        prem_r    <- mu_col_r - mu_ncol_r

        V_W_k[k] <- mean((prem_r - prem_k[k])^2)
      }

      # Rubin's rules
      prem_bar <- mean(prem_k)
      V_B      <- sum((prem_k - prem_bar)^2) / (M - 1)
      V_W      <- mean(V_W_k)
      V_total  <- V_W + (1 + 1 / M) * V_B

      tibble(
        premium       = prem_bar,
        se            = sqrt(V_total),
        V_W           = V_W,
        V_B           = V_B,
        n_college     = n_col,
        n_non_college = n_ncol
      )
    }) |>
    ungroup()
}
