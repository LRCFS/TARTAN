# =============================================================================
# 01_Calibration_Check.R — Session-Specific Calibration Check
# =============================================================================
#
# PIPELINE B — Step 1 of 3. Must be run before 02_Friction_Extraction.R.
#
# Purpose:
#   Computes a session-specific calibration correction factor (alpha_session)
#   for each experiment date using the before/after load-cell checks that
#   bracket the friction runs collected on that date.
#
#   The workflow is:
#     calibration check (before) -> friction experiments -> calibration check (after)
#
#   This script provides the calibration parameters used later in
#   03_Analysis.R to correct the extracted friction forces.
#
# Method:
#   For each experimental date:
#     1. Read the before and after calibration-check files.
#     2. Remove zero-value hardware artefacts (force_g == 0).
#     3. Use tare rows (target_g == 0) only as a diagnostic of zero-load
#        stability; they are excluded from the calibration fit.
#     4. For each timing (before / after), summarise the repeated readings at
#        each non-zero target mass by their mean force and standard error.
#     5. Estimate alpha by minimising the reduced chi-squared criterion:
#            chi2 = (1/(n-1)) * sum((M - alpha*X)^2 /
#                    (U_M^2 + (alpha*u_X)^2 + sigma_mechanical^2))
#        where M is the known reference mass, X the mean measured force,
#        u_X the standard error of the mean force, U_M the balance uncertainty,
#        and sigma_mechanical an additional uncertainty term if needed to bring
#        reduced chi-squared close to 1.
#     6. The fit-derived uncertainty on alpha_session is obtained from the
#        before/after alpha uncertainties.
#     7. sigma_alpha_combined = sqrt(u_alpha_mean^2 + sigma_within^2)
#     8. Quantify within-session stability by treating the observed before/after
#        alpha interval as a continuous rectangular (uniform) distribution:
#            sigma_within = abs(alpha_after - alpha_before) / sqrt(12)
#     9. Combine fit uncertainty and within-session stability in quadrature:
#            sigma_alpha_combined = sqrt(u_alpha_mean^2 + sigma_within^2)
#
# Notes:
#   - This script is distinct from 01_LoadCell_Characterisation.R.
#   - The characterisation script describes overall load-cell performance for
#     the paper; the present script produces the session-specific correction
#     actually used in the friction analysis.
#
# Output:
#   calibration_parameters.csv    one row per experiment date
#   calibration_check_summary.csv per-weight summary for diagnostics
#   calibration_check_tare.csv    tare stability diagnostics
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

data_dir   <- "Data/COF/"
output_dir <- "Results/Session-Specific_Calibration_Check/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01  # grams — resolution of the reference balance
u_balance <- balance_resolution / sqrt(12)

# ---- 1. LOAD BEFORE AND AFTER FILES -----------------------------------------

before_files <- list.files(data_dir,
                           pattern = "CalibrationRaw_LoadCell_before\\.csv$",
                           full.names = TRUE)
after_files  <- list.files(data_dir,
                           pattern = "CalibrationRaw_LoadCell_after\\.csv$",
                           full.names = TRUE)

cat(sprintf("Found %d before files and %d after files.\n\n",
            length(before_files), length(after_files)))

if (length(before_files) == 0 || length(after_files) == 0) {
  stop("No before/after calibration check files found in ", data_dir,
       "\nExpected filenames: YYYYMMDD_CalibrationRaw_LoadCell_before.csv\n",
       "                     YYYYMMDD_CalibrationRaw_LoadCell_after.csv")
}

read_check_file <- function(fp, timing_label) {
  date_str <- substr(basename(fp), 1, 8)
  read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      experiment_date = date_str,
      timing          = timing_label
    )
}

calib_checks <- bind_rows(
  map_dfr(before_files, read_check_file, timing_label = "before"),
  map_dfr(after_files,  read_check_file, timing_label = "after")
)

# ---- 2. ZERO REMOVAL --------------------------------------------------------
# Removes hardware artefacts (force_g == 0) from HX711 not-ready events.
# Must happen before any summarisation.
# This is SEPARATE from the tare rows (target_g == 0) which are legitimate.

zeros_removed <- calib_checks %>% filter(force_g == 0)

cat("=== Zero removal report ===\n")
cat(sprintf("Total measurements loaded: %d\n", nrow(calib_checks)))
cat(sprintf("Zero readings found:       %d\n", nrow(zeros_removed)))

if (nrow(zeros_removed) > 0) {
  cat("\nDetail of removed zero readings:\n")
  print(zeros_removed %>%
          dplyr::select(experiment_date, timing, target_g, sample_index, force_g),
        n = Inf, width = Inf)
} else {
  cat("No zero readings detected.\n")
}

calib_checks <- calib_checks %>% filter(force_g != 0)
cat(sprintf("Measurements retained: %d\n\n", nrow(calib_checks)))

# ---- 3. TARE STABILITY CHECK ------------------------------------------------
# Rows where target_g == 0 are the tare verification measurements.
# These are reported here as a diagnostic but excluded from chi-squared.
# A large tare drift indicates the zero-point shifted during the session,
# which is normal (thermal drift, creep) and handled by the Arduino tare.

tare_data <- calib_checks %>% filter(target_g == 0)

tare_summary <- tare_data %>%
  group_by(experiment_date, timing) %>%
  summarise(
    n          = n(),
    mean_zero  = mean(force_g, na.rm = TRUE),
    sd_zero    = sd(force_g,   na.rm = TRUE),
    .groups    = "drop"
  )

tare_drift <- tare_summary %>%
  dplyr::select(experiment_date, timing, mean_zero) %>%
  pivot_wider(names_from = timing, values_from = mean_zero) %>%
  mutate(tare_drift_g = after - before)

cat("=== Tare stability check (target_g == 0) ===\n")
cat("(Informational only — tare rows excluded from alpha computation)\n\n")

for (d in unique(tare_drift$experiment_date)) {
  row_b <- tare_summary %>% filter(experiment_date == d, timing == "before")
  row_a <- tare_summary %>% filter(experiment_date == d, timing == "after")
  drift <- tare_drift %>% filter(experiment_date == d)
  cat(sprintf("Session %s:\n", d))
  if (nrow(row_b) > 0)
    cat(sprintf("  Before — mean: %+.3f g, SD: %.3f g\n",
                row_b$mean_zero, row_b$sd_zero))
  if (nrow(row_a) > 0)
    cat(sprintf("  After  — mean: %+.3f g, SD: %.3f g\n",
                row_a$mean_zero, row_a$sd_zero))
  if (nrow(drift) > 0 && !is.na(drift$tare_drift_g))
    cat(sprintf("  Tare drift over session: %+.3f g\n\n", drift$tare_drift_g))
}

write_csv(tare_summary, file.path(output_dir, "Session_check_tare.csv"))

# ---- 4. CHI-SQUARED FUNCTIONS -----------------------------------------------
# Identical to those in 01_LoadCell_Characterisation.R.

compute_chi2 <- function(alpha, sigma_mech, M, X, sigma_X, u_M) {
  residuals_sq <- (M - alpha * X)^2
  denominators <- u_M^2 + (sigma_X * alpha)^2 + sigma_mech^2
  sum(residuals_sq / denominators)
}

compute_chi2_red <- function(chi2, n) {
  chi2 / (n - 1)
}

find_optimal_alpha <- function(sigma_mech, M, X, sigma_X, U_M) {
  result <- optimise(
    f        = function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M),
    interval = c(0.95, 1.05))
  list(alpha = result$minimum, chi2 = result$objective)
}

find_sigma_mechanical <- function(M, X, sigma_X, u_M) {
  n <- length(M)
  opt0 <- find_optimal_alpha(0, M, X, sigma_X, u_M)
  if (compute_chi2_red(opt0$chi2, n) <= 1.0) return(0)
  
  optimise(
    f = function(s) {
      opt_s <- find_optimal_alpha(s, M, X, sigma_X, u_M)
      (compute_chi2_red(opt_s$chi2, n) - 1.0)^2
    },
    interval = c(0, 10)
  )$minimum
}

compute_u_alpha <- function(alpha_opt, chi2_min, sigma_mech, M, X, sigma_X, u_M) {
  chi2_threshold <- chi2_min + 1
  
  alpha_lower <- tryCatch(
    uniroot(
      function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, u_M) - chi2_threshold,
      interval = c(alpha_opt - 0.05, alpha_opt)
    )$root,
    error = function(e) {
      warning("compute_u_alpha: lower bound not found, using fallback.")
      alpha_opt - 0.05
    }
  )
  
  alpha_upper <- tryCatch(
    uniroot(
      function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, u_M) - chi2_threshold,
      interval = c(alpha_opt, alpha_opt + 0.05)
    )$root,
    error = function(e) {
      warning("compute_u_alpha: upper bound not found, using fallback.")
      alpha_opt + 0.05
    }
  )
  
  list(
    u_alpha = (alpha_upper - alpha_lower) / 2,
    chi2_threshold = chi2_threshold,
    alpha_lower = alpha_lower,
    alpha_upper = alpha_upper
  )
}

run_chi2 <- function(dat) {
  summ <- dat %>%
    filter(target_g > 0) %>%
    group_by(target_g) %>%
    summarise(
      mean_force_g = mean(force_g, na.rm = TRUE),
      sd_force_g   = sd(force_g,   na.rm = TRUE),
      n_rep        = n(),
      se_force_g   = sd_force_g / sqrt(n_rep),
      .groups = "drop"
    )
  
  M   <- summ$target_g
  X   <- summ$mean_force_g
  sX  <- summ$se_force_g
  u_M <- rep(u_balance, length(M))
  n   <- length(M)
  
  sm  <- find_sigma_mechanical(M, X, sX, u_M)
  opt <- find_optimal_alpha(sm, M, X, sX, u_M)
  ua  <- compute_u_alpha(opt$alpha, opt$chi2, sm, M, X, sX, u_M)
  
  list(
    alpha            = opt$alpha,
    chi2_min         = opt$chi2,
    chi2_red_min     = compute_chi2_red(opt$chi2, n),
    chi2_threshold   = ua$chi2_threshold,
    alpha_lower      = ua$alpha_lower,
    alpha_upper      = ua$alpha_upper,
    u_alpha          = ua$u_alpha,
    sigma_mechanical = sm
  )
}

# ---- 5. COMPUTE SESSION-SPECIFIC ALPHA PER DATE -----------------------------

experiment_dates <- unique(calib_checks$experiment_date)
cat(sprintf("Computing session-specific alpha for %d experiment date(s).\n\n",
            length(experiment_dates)))

calib_params_list <- map_dfr(experiment_dates, function(d) {
  
  dat_before <- calib_checks %>%
    filter(experiment_date == d, timing == "before")
  dat_after  <- calib_checks %>%
    filter(experiment_date == d, timing == "after")
  
  if (nrow(dat_before) == 0 || nrow(dat_after) == 0) {
    warning(sprintf("Date %s: missing before or after file — skipping.", d))
    return(NULL)
  }
  
  chi2_before <- run_chi2(dat_before)
  chi2_after  <- run_chi2(dat_after)
  
  # Session alpha = mean of before and after
  alpha_session <- mean(c(chi2_before$alpha, chi2_after$alpha))
  
  # Stability term across the session
  # Standard uncertainty for a continuous rectangular (uniform) distribution
  # spanning the observed before/after alpha interval.
  sigma_within <- abs(chi2_after$alpha - chi2_before$alpha) / sqrt(12)
  
  # Standard uncertainty on the mean alpha from before/after chi-squared fits
  u_alpha_mean <- sqrt(
    chi2_before$u_alpha^2 + chi2_after$u_alpha^2
  ) / 2
  
  # Combined standard uncertainty on alpha_session
  u_alpha_session <- sqrt(u_alpha_mean^2 + sigma_within^2)
  
  # sigma_mechanical: use mean from before and after
  sigma_mech <- mean(c(chi2_before$sigma_mechanical,
                       chi2_after$sigma_mechanical))
  
  cat(sprintf("Date %s:\n", d))
  cat(sprintf("  alpha_before  = %.8f (chi2 = %.4f)\n",
              chi2_before$alpha, chi2_before$chi2_min))
  cat(sprintf("  alpha_after   = %.8f (chi2 = %.4f)\n",
              chi2_after$alpha,  chi2_after$chi2_min))
  cat(sprintf("  alpha_session = %.8f\n",  alpha_session))
  cat(sprintf("  sigma_within    = %.8f\n", sigma_within))
  cat(sprintf("  u_alpha_mean    = %.8f\n", u_alpha_mean))
  cat(sprintf("  u_alpha_session = %.8f\n\n", u_alpha_session))
  
  tibble(
    experiment_date       = d,
    alpha_before          = chi2_before$alpha,
    alpha_after           = chi2_after$alpha,
    
    chi2_min_before       = chi2_before$chi2_min,
    chi2_min_after        = chi2_after$chi2_min,
    chi2_red_min_before   = chi2_before$chi2_red_min,
    chi2_red_min_after    = chi2_after$chi2_red_min,
    
    chi2_threshold_before = chi2_before$chi2_threshold,
    chi2_threshold_after  = chi2_after$chi2_threshold,
    
    alpha_lower_before    = chi2_before$alpha_lower,
    alpha_upper_before    = chi2_before$alpha_upper,
    alpha_lower_after     = chi2_after$alpha_lower,
    alpha_upper_after     = chi2_after$alpha_upper,
    
    u_alpha_before        = chi2_before$u_alpha,
    u_alpha_after         = chi2_after$u_alpha,
    
    alpha_session         = alpha_session,
    sigma_within          = sigma_within,
    u_alpha_mean          = u_alpha_mean,
    u_alpha_session       = u_alpha_session,
    
    sigma_mechanical      = sigma_mech,
    u_balance             = u_balance
  )
})

cat("=== Session calibration parameters ===\n")
print(calib_params_list, width = Inf)

# ---- 6. SAVE OUTPUTS --------------------------------------------------------

# Full session parameter table (used by 03_Analysis.R for per-date join)
write_csv(calib_params_list,
          file.path(output_dir, "Session_calibration_parameters.csv"))

# Per-weight summary for diagnostics
calib_check_summary <- calib_checks %>%
  filter(target_g > 0) %>%
  group_by(experiment_date, timing, target_g) %>%
  summarise(n = n(), mean_g = mean(force_g, na.rm = TRUE),
            sd_g = sd(force_g, na.rm = TRUE),
            error_g = mean(force_g, na.rm = TRUE) - first(target_g),
            .groups = "drop")

write_csv(calib_check_summary,
          file.path(output_dir, "calibration_check_summary.csv"))

cat("\n=== 01_Calibration_Check.R complete ===\n")
cat(sprintf("calibration_parameters.csv written with %d session(s).\n",
            nrow(calib_params_list)))

# create a more readable table for the calibration parameters outputs
parameter_descriptions <- tribble(
  ~parameter,               ~description,
  "alpha_before",           "Best-fitting calibration correction factor obtained before the friction experiments.",
  "alpha_after",            "Best-fitting calibration correction factor obtained after the friction experiments.",
  "chi2_min_before",        "Minimum chi-squared value obtained for the before-session calibration fit.",
  "chi2_min_after",         "Minimum chi-squared value obtained for the after-session calibration fit.",
  "chi2_red_min_before",    "Minimum reduced chi-squared for the before-session fit; values near 1 indicate agreement with the stated uncertainties.",
  "chi2_red_min_after",     "Minimum reduced chi-squared for the after-session fit; values near 1 indicate agreement with the stated uncertainties.",
  "chi2_threshold_before",  "Chi-squared threshold used to determine the uncertainty interval around alpha_before (chi2_min + 1).",
  "chi2_threshold_after",   "Chi-squared threshold used to determine the uncertainty interval around alpha_after (chi2_min + 1).",
  "alpha_lower_before",     "Lower bound of the uncertainty interval for alpha_before.",
  "alpha_upper_before",     "Upper bound of the uncertainty interval for alpha_before.",
  "alpha_lower_after",      "Lower bound of the uncertainty interval for alpha_after.",
  "alpha_upper_after",      "Upper bound of the uncertainty interval for alpha_after.",
  "u_alpha_before",         "Fit-derived standard uncertainty associated with alpha_before.",
  "u_alpha_after",          "Fit-derived standard uncertainty associated with alpha_after.",
  "alpha_session",          "Session-specific correction factor, calculated as the mean of alpha_before and alpha_after.",
  "sigma_within",           "Standard uncertainty associated with slow within-session drift, assuming continuous uniform variation between alpha_before and alpha_after.",
  "u_alpha_mean",           "Standard uncertainty of the mean calibration factor, propagated from u_alpha_before and u_alpha_after.",
  "u_alpha_session",        "Combined standard uncertainty of alpha_session, combining u_alpha_mean and sigma_within in quadrature.",
  "sigma_mechanical",       "Additional mechanical or instrumental force uncertainty required to obtain a reduced chi-squared close to 1.",
  "u_balance",              "Standard uncertainty of the reference balance derived from its resolution."
)

calib_params_readable <- calib_params_list %>%
  pivot_longer(
    cols = -experiment_date,
    names_to = "parameter",
    values_to = "value"
  ) %>%
  left_join(parameter_descriptions, by = "parameter") %>%
  dplyr::select(parameter, description, experiment_date, value) %>%
  pivot_wider(
    names_from = experiment_date,
    values_from = value
  ) %>%
  arrange(match(parameter, parameter_descriptions$parameter))

write_csv(
  calib_params_readable,
  file.path(output_dir, "Session_calibration_parameters_readable.csv")
)

