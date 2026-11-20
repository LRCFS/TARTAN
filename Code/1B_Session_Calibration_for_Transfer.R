# =============================================================================
# 1B_Session_Calibration_for_Transfer.R — Session-Specific Calibration Check
# =============================================================================
#
# PIPELINE B — Step 1 of 3. Run before 2_Friction_extraction_Raw.R and
# 3_Transfer_Analysis_corrected.R.
#
# NOTE: this file was REBUILT because the previous copy had been overwritten
# with an old version of 1A_Loadcell_characterisation_SI.R. The chi-squared
# functions are copied verbatim from 1A (the 1A header states they are
# identical). The before/after AVERAGING and the u_alpha_session formula below
# are a reconstruction — check them against the Methods section of the paper.
#
# Purpose:
#   For every experiment day, the load cell was checked with known masses
#   BEFORE and AFTER the friction runs. Each check is fitted independently
#   with the chi-squared criterion (Galais 2024, Appendix 4) to obtain
#   alpha_before and alpha_after. The correction applied to that day's
#   friction runs is their mean:
#     alpha_session = (alpha_before + alpha_after) / 2
#
# Uncertainty on alpha_session (standard uncertainties):
#   (i)  fit-derived : u_fit  = sqrt(u_alpha_before^2 + u_alpha_after^2) / 2
#                      (uncertainty of the mean of two independent estimates)
#   (ii) stability   : u_stab = |alpha_after - alpha_before| / (2 * sqrt(3))
#                      (drift within the session treated as a rectangular
#                       distribution spanning the before/after interval)
#   u_alpha_session  = sqrt(u_fit^2 + u_stab^2)
#
#   sigma_mechanical for the session is the root-mean-square of the before
#   and after values.
#
# Input files (in data_dir = Data/COF/):
#   YYYYMMDD_..._CalibrationRaw_LoadCell_before.csv
#   YYYYMMDD_..._CalibrationRaw_LoadCell_after.csv
#   Expected columns (same format as the calibration-curve files in 1A):
#     date, label, weight_index, target_g, sample_index, force_g
#   Only target_g and force_g are required.
#
# Outputs (in output_dir = Results/Session-Specific_Calibration_Check/):
#   Calibration_check_fits.csv            — one row per check (before / after)
#   Session_calibration_parameters.csv    — one row per experiment date
#                                            (read by Script 3)
#   calibration_check_alpha_by_session.png
#
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

data_dir   <- "Data/COF/"
output_dir <- "Results/Session-Specific_Calibration_Check/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01  # grams — resolution of the reference balance
U_balance <- balance_resolution / sqrt(12)

# Mass used to set the firmware scale factor and tare rows are excluded,
# exactly as in 1A (they cannot independently check the converted response).
scale_factor_mass <- 899.45

# ---- 1. CHI-SQUARED FUNCTIONS (identical to 1A) -----------------------------

compute_chi2 <- function(alpha, sigma_mech, M, X, sigma_X, U_M) {
  residuals_sq <- (M - alpha * X)^2
  denominators <- U_M^2 + (sigma_X * alpha)^2 + sigma_mech^2
  sum(residuals_sq / denominators)
}

compute_chi2_red <- function(chi2, n) {
  chi2 / (n - 1)
}

find_optimal_alpha <- function(sigma_mech, M, X, sigma_X, U_M) {
  result <- optimise(
    f = function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M),
    interval = c(0.95, 1.05),
    tol      = 1e-12
  )
  list(alpha = result$minimum, chi2 = result$objective)
}

find_sigma_mechanical <- function(M, X, sigma_X, U_M) {
  n    <- length(M)
  opt0 <- find_optimal_alpha(0, M, X, sigma_X, U_M)
  
  if (compute_chi2_red(opt0$chi2, n) <= 1.0) return(0)
  
  optimise(
    f = function(s) {
      opt_s <- find_optimal_alpha(s, M, X, sigma_X, U_M)
      (compute_chi2_red(opt_s$chi2, n) - 1.0)^2
    },
    interval = c(0, 10),
    tol      = 1e-10
  )$minimum
}

compute_u_alpha <- function(alpha_opt, chi2_min, sigma_mech, M, X, sigma_X, U_M) {
  threshold <- chi2_min + 1
  
  lower <- tryCatch(
    uniroot(
      function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M) - threshold,
      interval = c(alpha_opt - 0.05, alpha_opt),
      tol      = 1e-14
    )$root,
    error = function(e) {
      warning("compute_u_alpha: lower bound not found, using fallback.")
      alpha_opt - 0.05
    }
  )
  
  upper <- tryCatch(
    uniroot(
      function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M) - threshold,
      interval = c(alpha_opt, alpha_opt + 0.05),
      tol      = 1e-14
    )$root,
    error = function(e) {
      warning("compute_u_alpha: upper bound not found, using fallback.")
      alpha_opt + 0.05
    }
  )
  
  list(
    u_alpha     = (upper - lower) / 2,
    threshold   = threshold,
    alpha_min   = lower,
    alpha_max   = upper,
    sigma_mech  = sigma_mech
  )
}

# ---- 2. FIND BEFORE / AFTER CHECK FILES -------------------------------------

check_files <- list.files(
  data_dir,
  pattern = "Calibration.*(before|after)\\.csv$",
  full.names = TRUE, ignore.case = TRUE
)

cat(sprintf("Found %d before/after calibration check files.\n", length(check_files)))
if (length(check_files) == 0) {
  stop("No before/after calibration check files found in ", data_dir,
       ". Expected names like YYYYMMDD_..._CalibrationRaw_LoadCell_before.csv")
}

check_index <- tibble(
  filepath        = check_files,
  filename        = basename(check_files),
  experiment_date = substr(basename(check_files), 1, 8),
  check           = tolower(stringr::str_extract(basename(check_files),
                                                 "(?i)(before|after)(?=\\.csv$)"))
)

bad_date <- check_index %>% filter(!grepl("^\\d{8}$", experiment_date))
if (nrow(bad_date) > 0) {
  stop("File name(s) do not start with YYYYMMDD:\n  ",
       paste(bad_date$filename, collapse = "\n  "))
}

dupes <- check_index %>% count(experiment_date, check) %>% filter(n > 1)
if (nrow(dupes) > 0) {
  stop("More than one '", paste(unique(dupes$check), collapse = "/"),
       "' file for date(s): ", paste(unique(dupes$experiment_date), collapse = ", "),
       ". Remove duplicates/copies from ", data_dir)
}

print(check_index %>% dplyr::select(experiment_date, check, filename), n = Inf)

# ---- 3. READ, CLEAN AND SUMMARISE EACH CHECK --------------------------------

check_raw <- pmap_dfr(check_index, function(filepath, filename, experiment_date, check) {
  df <- read_csv(filepath, show_col_types = FALSE)
  missing_cols <- setdiff(c("target_g", "force_g"), names(df))
  if (length(missing_cols) > 0) {
    stop(filename, " is missing column(s): ", paste(missing_cols, collapse = ", "),
         "\n  Columns found: ", paste(names(df), collapse = ", "))
  }
  df %>%
    transmute(experiment_date = experiment_date,
              check           = check,
              filename        = filename,
              target_g        = as.numeric(target_g),
              force_g         = as.numeric(force_g))
})

# force_g == 0 readings are HX711 artefacts (same rule as 1A and Script 2)
n_zero <- sum(check_raw$force_g == 0, na.rm = TRUE)
cat(sprintf("\nZero readings removed from checks: %d of %d\n", n_zero, nrow(check_raw)))
check_raw <- check_raw %>% filter(force_g != 0, !is.na(force_g))

check_summary <- check_raw %>%
  filter(target_g > 0, !dplyr::near(target_g, scale_factor_mass, tol = 1e-6)) %>%
  group_by(experiment_date, check, filename, target_g) %>%
  summarise(
    n_samples    = n(),
    mean_force_g = mean(force_g),
    sd_force_g   = sd(force_g),
    se_force_g   = sd_force_g / sqrt(n_samples),
    .groups = "drop"
  )

# ---- 4. CHI-SQUARED FIT FOR EACH CHECK --------------------------------------

check_fits <- check_summary %>%
  group_by(experiment_date, check, filename) %>%
  group_modify(~ {
    M  <- .x$target_g
    X  <- .x$mean_force_g
    sX <- .x$se_force_g
    UM <- rep(U_balance, length(M))
    
    if (length(M) < 2) {
      stop("Check ", .y$filename, " has fewer than 2 usable masses after ",
           "excluding 0 g and ", scale_factor_mass, " g.")
    }
    
    sm  <- find_sigma_mechanical(M, X, sX, UM)
    opt <- find_optimal_alpha(sm, M, X, sX, UM)
    ua  <- compute_u_alpha(opt$alpha, opt$chi2, sm, M, X, sX, UM)
    
    tibble(
      n_points         = length(M),
      sigma_mechanical = sm,
      alpha            = opt$alpha,
      chi2_min         = opt$chi2,
      chi2_red_min     = compute_chi2_red(opt$chi2, length(M)),
      u_alpha          = ua$u_alpha
    )
  }) %>%
  ungroup() %>%
  arrange(experiment_date, desc(check == "before"))

cat("\n=== Chi-squared fit per calibration check ===\n")
print(check_fits %>% dplyr::select(-filename), n = Inf, width = Inf)
write_csv(check_fits, file.path(output_dir, "Calibration_check_fits.csv"))

# ---- 5. COMBINE BEFORE + AFTER INTO ONE ALPHA PER SESSION -------------------

check_wide <- check_fits %>%
  dplyr::select(experiment_date, check, alpha, u_alpha, sigma_mechanical,
                chi2_red_min, n_points) %>%
  pivot_wider(names_from = check,
              values_from = c(alpha, u_alpha, sigma_mechanical, chi2_red_min, n_points))

incomplete <- check_wide %>%
  filter(is.na(alpha_before) | is.na(alpha_after))
if (nrow(incomplete) > 0) {
  stop("Missing a before or after check for date(s): ",
       paste(incomplete$experiment_date, collapse = ", "))
}

session_params <- check_wide %>%
  mutate(
    alpha_session    = (alpha_before + alpha_after) / 2,
    u_alpha_fit      = sqrt(u_alpha_before^2 + u_alpha_after^2) / 2,
    u_alpha_stab     = abs(alpha_after - alpha_before) / (2 * sqrt(3)),
    u_alpha_session  = sqrt(u_alpha_fit^2 + u_alpha_stab^2),
    sigma_mechanical = sqrt((sigma_mechanical_before^2 + sigma_mechanical_after^2) / 2),
    u_balance        = U_balance,
    alpha_drift_pct  = 100 * (alpha_after / alpha_before - 1)
  ) %>%
  dplyr::select(experiment_date,
                alpha_before, u_alpha_before, alpha_after, u_alpha_after,
                alpha_drift_pct,
                alpha_session, u_alpha_fit, u_alpha_stab, u_alpha_session,
                sigma_mechanical_before, sigma_mechanical_after, sigma_mechanical,
                chi2_red_min_before, chi2_red_min_after,
                n_points_before, n_points_after,
                u_balance) %>%
  arrange(experiment_date)

cat("\n=== Session calibration parameters (used by Script 3) ===\n")
print(session_params %>%
        dplyr::select(experiment_date, alpha_before, alpha_after, alpha_drift_pct,
                      alpha_session, u_alpha_session, sigma_mechanical),
      n = Inf, width = Inf)

write_csv(session_params, file.path(output_dir, "Session_calibration_parameters.csv"))

# ---- 6. FIGURE: alpha before / after per session ----------------------------

alpha_plot_dat <- check_fits %>%
  mutate(check = factor(check, levels = c("before", "after")),
         date  = as.Date(experiment_date, format = "%Y%m%d"))

p_alpha_sessions <- ggplot(alpha_plot_dat,
                           aes(x = factor(date), y = alpha, shape = check)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = alpha - u_alpha, ymax = alpha + u_alpha),
                width = 0.15, colour = "grey20",
                position = position_dodge(width = 0.4)) +
  geom_point(size = 3, colour = "grey20",
             position = position_dodge(width = 0.4)) +
  geom_point(data = session_params %>%
               mutate(date = as.Date(experiment_date, format = "%Y%m%d")),
             aes(x = factor(date), y = alpha_session),
             inherit.aes = FALSE, shape = 4, size = 4, colour = "firebrick4") +
  labs(x = "\nExperiment date", y = expression(alpha), shape = "Check") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_alpha_sessions)

ggsave(file.path(output_dir, "calibration_check_alpha_by_session.png"),
       p_alpha_sessions, width = 7, height = 4.5, dpi = 600)

cat("\n=== 1B_Session_Calibration_for_Transfer.R complete ===\n")
cat("Session_calibration_parameters.csv written with",
    nrow(session_params), "session(s) to", output_dir, "\n")