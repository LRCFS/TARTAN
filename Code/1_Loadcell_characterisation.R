# =============================================================================
# 01_LoadCell_Characterisation.R — Load Cell Characterisation Study
# =============================================================================
#
# PIPELINE A — Standalone. Does NOT feed into the friction data analysis.
#
# Purpose:
#   Characterises the TARTAN load cell using full multi-weight calibration
#   curves collected across multiple sessions (morning / midday / evening,
#   across several days). Quantifies:
#     - Linearity (linear regression per session and load range)
#     - Metrological validity (chi-squared criterion, following
#       Galais 2024, Appendix 4)
#     - Session-to-session drift in the calibration factor alpha
#
#   Results are used in the paper to document load cell behaviour.
#   They do NOT correct the friction experiment data — that correction
#   is handled by 01_Calibration_Check.R using the before/after files.
#
# Input files (in data_dir):
#   YYYYMMDD_CalibrationRaw_HighLoadRange_weights_[morning|midday|evening].csv
#   YYYYMMDD_CalibrationRaw_LowLoadRange_weights_[morning|midday|evening].csv
#
#   Each file contains repeated measurements at several known masses,
#   collected over 20 samples per mass. Columns:
#     date, label, weight_index, target_g, sample_index, force_g
#
# Outputs (in output_dir):
#   calibration_linearity_paper.png       — paper figure (one representative session)
#   calibration_linearity_supp_*.png      — supplementary (one figure per session)
#   calibration_alpha_drift.png           — alpha drift across sessions
#   calibration_residuals.png             — residuals
#   calibration_raw_scatter_paper.png     — paper scatter (single weight example)
#   calibration_raw_scatter_LowLoad.png   — supplementary scatter (Low-load, A4)
#   calibration_raw_scatter_HighLoad.png  — supplementary scatter (High-load, A4)
#   linear_models_by_session.csv
#   chi2_results_all_sessions.csv
#   calibration_drift_across_sessions.csv
#   error_range_by_weight_and_session.csv
#
# This script is self-contained and can be run independently of all others.
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

data_dir   <- "Data/Calibration curve/"
output_dir <- "Results"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01  # grams — resolution of the reference balance
U_balance <- balance_resolution / sqrt(12)

# Actual measured masses of the transfer weights (precision balance)
M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

# Time-of-day ordering for sorting sessions within a day
tod_levels <- c("morning", "midday", "evening")

# Representative session for the PAPER linearity figure (one per range).
# Set to the date string and time-of-day of the session you want to highlight.
paper_session_date <- "20260312"
paper_session_tod  <- "morning"

# Representative weight for the PAPER raw scatter example figure.
paper_scatter_range  <- "Low-load range"  # "High-load range" or "Low-load range"
paper_scatter_weight <- 249.20             # must match one of your target_g values

# ---- 1. LOAD ALL CALIBRATION CURVE FILES ------------------------------------

calib_files <- list.files(data_dir,
                          pattern = "CalibrationRaw_.*LoadRange.*\\.csv$",
                          full.names = TRUE)
cat(sprintf("Found %d calibration curve files.\n\n", length(calib_files)))

calib_all_raw <- map_dfr(calib_files, function(fp) {
  fname       <- basename(fp)
  date_str    <- substr(fname, 1, 8)
  range_type  <- ifelse(grepl("LowLoad", fname), "Low-load range", "High-load range")
  range_short <- ifelse(grepl("LowLoad", fname), "Low", "High")
  tod <- stringr::str_extract(fname, "(morning|midday|evening)(?=\\.csv$)")
  if (is.na(tod)) tod <- "unknown"
  
  read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      calib_date          = date_str,
      date                = paste0(substr(date_str,1,4),"-",
                                   substr(date_str,5,6),"-",
                                   substr(date_str,7,8)),
      time_of_day         = factor(tod, levels = tod_levels),
      range_short         = range_short,
      session             = paste0(date_str, "_", tod, "_", range_short),
      session_label       = paste0(date, " (", tod, ", ", range_short, "-load)"),
      session_label_short = paste0(date, "\n(", tod, ")"),
      type                = range_type
    )
})

# ---- 2. ZERO REMOVAL --------------------------------------------------------
# Removes hardware artefacts (force_g == 0) caused by the HX711 occasionally
# returning a zero when not ready. This is a data quality filter — it is NOT
# the same as the tare check (target_g == 0), which is a legitimate measurement.
# Zero removal must happen before any summarisation to prevent zeros from
# contaminating means.

zeros_removed <- calib_all_raw %>% filter(force_g == 0)

cat("=== Zero removal report ===\n")
cat(sprintf("Total measurements loaded: %d\n", nrow(calib_all_raw)))
cat(sprintf("Zero readings found:       %d\n", nrow(zeros_removed)))

if (nrow(zeros_removed) > 0) {
  cat("\nDetail of removed zero readings:\n")
  print(zeros_removed %>%
          dplyr::select(session_label, type, target_g, sample_index, force_g),
        n = Inf, width = Inf)
} else {
  cat("No zero readings detected.\n")
}

calib_all_raw <- calib_all_raw %>% filter(force_g != 0)
cat(sprintf("Measurements retained: %d\n\n", nrow(calib_all_raw)))

# ---- 3. ORDER SESSIONS CHRONOLOGICALLY --------------------------------------

session_order <- calib_all_raw %>%
  distinct(session, session_label, session_label_short, calib_date, time_of_day) %>%
  arrange(calib_date, time_of_day) %>%
  pull(session)

label_levels <- calib_all_raw %>%
  distinct(session, session_label) %>%
  arrange(match(session, session_order)) %>%
  pull(session_label)

label_short_levels <- calib_all_raw %>%
  distinct(session, session_label_short) %>%
  arrange(match(session, session_order)) %>%
  pull(session_label_short) %>%
  unique()

calib_all_raw <- calib_all_raw %>%
  mutate(
    session             = factor(session, levels = session_order),
    session_label       = factor(session_label, levels = label_levels),
    session_label_short = factor(session_label_short, levels = label_short_levels)
  )

# Define dat_raw for scatter plots (excludes target_g == 0 tare rows)
dat_raw <- calib_all_raw %>% filter(target_g > 0)

# ---- 4. SUMMARY (after zero removal) ----------------------------------------

calib_summary <- calib_all_raw %>%
  group_by(session, session_label, session_label_short,
           calib_date, date, time_of_day, range_short, type,
           target_g) %>%
  summarise(
    n_samples    = n(),
    mean_force_g = mean(force_g, na.rm = TRUE),
    sd_force_g   = sd(force_g,  na.rm = TRUE),
    se_force_g   = sd_force_g / sqrt(n_samples),
    .groups = "drop"
  ) %>%
  mutate(error_g = mean_force_g - target_g)

# Exclude tare row (target_g == 0) from regression and chi-squared
calib_for_lm <- calib_summary %>% filter(target_g > 0)

# ---- 5. LINEAR REGRESSION (per session and range) ---------------------------

lm_tbl <- calib_for_lm %>%
  group_by(session_label, type) %>%
  do({
    fit <- lm(mean_force_g ~ target_g, data = .)
    data.frame(
      R2 = formatC(summary(fit)$r.squared, format = "f", digits = 8),
      Equation = paste0("y = ",
                        formatC(coef(fit)[2], format = "f", digits = 6),
                        "x + ",
                        formatC(coef(fit)[1], format = "f", digits = 4))
    )
  }) %>%
  ungroup() %>%
  transmute(Session = session_label, `Weight range` = type,
            `R-squared` = R2, Equation)

cat("\n=== Linear models per session ===\n")
print(lm_tbl, width = Inf, n = 30)
write_csv(lm_tbl, file.path(output_dir, "linear_models_by_session.csv"))

# ---- 6. CHI-SQUARED VALIDATION ----------------------------------------------
# Computes the optimal calibration factor alpha and its uncertainty for each
# session × range combination, following the chi-squared criterion described
# in Galais 2024, Appendix 4.

compute_chi2 <- function(alpha, sigma_mech, M, X, sigma_X, U_M) {
  n <- length(M)
  residuals_sq <- (M - alpha * X)^2
  denominators <- U_M^2 + (sigma_X * alpha)^2 + sigma_mech^2
  (1 / (n - 1)) * sum(residuals_sq / denominators)
}

find_optimal_alpha <- function(sigma_mech, M, X, sigma_X, U_M) {
  result <- optimise(
    f        = function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M),
    interval = c(0.95, 1.05))
  list(alpha = result$minimum, chi2 = result$objective)
}

find_sigma_mechanical <- function(M, X, sigma_X, U_M) {
  opt0 <- find_optimal_alpha(0, M, X, sigma_X, U_M)
  if (opt0$chi2 <= 1.0) return(0)
  optimise(
    f        = function(s) (find_optimal_alpha(s, M, X, sigma_X, U_M)$chi2 - 1.0)^2,
    interval = c(0, 10))$minimum
}

compute_sigma_alpha <- function(alpha_opt, chi2_min, sigma_mech, M, X, sigma_X, U_M) {
  # GUM-compatible 1-sigma interval:
  # The 1-sigma confidence interval on alpha is the set of alpha values for
  # which the reduced chi-squared exceeds its minimum by no more than 1/(n-1).
  # This follows directly from chi-squared statistics and is consistent with
  # the GUM framework for parameter uncertainty from a least-squares fit.
  n <- length(M)
  threshold <- chi2_min + 1 / (n - 1)
  
  lower <- tryCatch(
    uniroot(function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M) - threshold,
            interval = c(alpha_opt - 0.05, alpha_opt))$root,
    error = function(e) {
      warning("compute_sigma_alpha: lower bound not found, using fallback.")
      alpha_opt - 0.05
    }
  )
  
  upper <- tryCatch(
    uniroot(function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M) - threshold,
            interval = c(alpha_opt, alpha_opt + 0.05))$root,
    error = function(e) {
      warning("compute_sigma_alpha: upper bound not found, using fallback.")
      alpha_opt + 0.05
    }
  )
  
  list(sigma_alpha = (upper - lower) / 2,
       threshold   = threshold,
       alpha_min   = lower,
       alpha_max   = upper,
       sigma_mech  = sigma_mech)
}

chi2_results <- calib_for_lm %>%
  group_by(session, session_label, session_label_short,
           calib_date, date, time_of_day, range_short, type) %>%
  group_modify(~ {
    M  <- .x$target_g;  X  <- .x$mean_force_g
    sX <- .x$se_force_g; UM <- rep(U_balance, length(M))
    sm  <- find_sigma_mechanical(M, X, sX, UM)
    opt <- find_optimal_alpha(sm, M, X, sX, UM)
    sa  <- compute_sigma_alpha(opt$alpha, opt$chi2, sm, M, X, sX, UM)
    tibble(n_points = length(M), sigma_mechanical = sm,
           alpha = opt$alpha, chi2_min = opt$chi2,
           chi2_threshold = sa$threshold,
           alpha_min = sa$alpha_min,
           alpha_max = sa$alpha_max,
           sigma_alpha = sa$sigma_alpha)
  }) %>%
  ungroup()

cat("\n=== Chi-squared results ===\n")
print(chi2_results %>%
        dplyr::select(session_label, type, n_points,
                      sigma_mechanical, alpha, chi2_min, sigma_alpha),
      width = Inf, n = 30)
write_csv(chi2_results, file.path(output_dir, "chi2_results_all_sessions.csv"))

# ---- 7. DRIFT ANALYSIS ACROSS SESSIONS --------------------------------------

drift_summary <- chi2_results %>%
  group_by(type) %>%
  summarise(
    n_sessions      = n(),
    alpha_mean      = mean(alpha),
    alpha_sd        = sd(alpha),
    alpha_range     = max(alpha) - min(alpha),
    sigma_mech_mean = mean(sigma_mechanical),
    .groups = "drop"
  )

cat("\n=== Drift across calibration sessions ===\n")
print(drift_summary, width = Inf)
write_csv(drift_summary,
          file.path(output_dir, "calibration_drift_across_sessions.csv"))

# ---- 8. FIGURES -------------------------------------------------------------

library(patchwork)

# Alpha correction factor used for residuals figure (most recent High-load session)
alpha_for_residuals <- chi2_results %>%
  filter(type == "High-load range") %>%
  arrange(calib_date, match(time_of_day, tod_levels)) %>%
  slice_tail(n = 1) %>%
  pull(alpha)

# -- Fig 1a: Linearity — paper (one representative session) -------------------
calib_paper <- calib_for_lm %>%
  filter(calib_date == paper_session_date,
         as.character(time_of_day) == paper_session_tod)

if (nrow(calib_paper) == 0) {
  warning(sprintf(
    "No data for paper_session_date='%s', paper_session_tod='%s'. Check values.",
    paper_session_date, paper_session_tod))
} else {
  linearity_panel <- function(range_label) {
    dat <- calib_paper %>% filter(type == range_label)
    fit <- lm(mean_force_g ~ target_g, data = dat)
    r2  <- summary(fit)$r.squared
    cf  <- coef(fit)
    eq_label <- sprintf("y = %.4fx + %.3f\nR² = %.6f",
                        cf[2], cf[1], r2)
    ggplot(dat, aes(x = target_g, y = mean_force_g)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey40") +
      geom_errorbar(aes(ymin = mean_force_g - sd_force_g,
                        ymax = mean_force_g + sd_force_g),
                    width = 0, colour = "grey20") +
      geom_point(size = 2.5, colour = "grey20") +
      annotate("text", x = -Inf, y = Inf, label = eq_label,
               hjust = -0.1, vjust = 1.3, size = 3.2, colour = "grey20") +
      labs(x = "\nKnown weight added (g)",
           y = "Average load cell reading (g)\n",
           title = range_label) +
      theme_bw(base_size = 12) +
      theme(plot.title = element_text(size = 11, hjust = 0.5))
  }
  p_linearity_paper <- linearity_panel("Low-load range") |
    linearity_panel("High-load range")
  print(p_linearity_paper)
  ggsave(file.path(output_dir, "calibration_linearity_paper.png"),
         p_linearity_paper, width = 8, height = 4, dpi = 600)
}

# -- Fig 1b: Linearity — supplementary (one figure per session) ---------------
all_sessions <- calib_for_lm %>%
  distinct(calib_date, time_of_day) %>%
  arrange(calib_date, time_of_day)

for (i in seq_len(nrow(all_sessions))) {
  s_date <- all_sessions$calib_date[i]
  s_tod  <- as.character(all_sessions$time_of_day[i])
  dat_sess <- calib_for_lm %>%
    filter(calib_date == s_date, as.character(time_of_day) == s_tod)
  
  linearity_supp_panel <- function(range_label) {
    dat <- dat_sess %>% filter(type == range_label)
    if (nrow(dat) == 0) return(NULL)
    ggplot(dat, aes(x = target_g, y = mean_force_g)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey40") +
      geom_errorbar(aes(ymin = mean_force_g - sd_force_g,
                        ymax = mean_force_g + sd_force_g),
                    width = 0, colour = "grey20") +
      geom_point(size = 2, colour = "grey20") +
      labs(x = "\nKnown weight added (g)",
           y = "Average load cell reading (g)\n",
           title = range_label) +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(size = 10, hjust = 0.5))
  }
  p_low  <- linearity_supp_panel("Low-load range")
  p_high <- linearity_supp_panel("High-load range")
  p_supp <- if (!is.null(p_low) && !is.null(p_high)) (p_low | p_high) else
    (p_low %||% p_high)
  p_supp <- p_supp + plot_annotation(title = paste0(s_date, " — ", s_tod))
  ggsave(file.path(output_dir,
                   paste0("calibration_linearity_supp_", s_date, "_", s_tod, ".png")),
         p_supp, width = 8, height = 4, dpi = 600)
}
cat("\nSupplementary linearity figures saved (one per session).\n")

# -- Fig 2: Alpha drift — stacked, x-axis = date + time only -----------------
drift_panel <- function(range_label, show_x = TRUE) {
  dat <- chi2_results %>% filter(type == range_label)
  p <- ggplot(dat, aes(x = session_label_short, y = alpha, group = 1)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.6, colour = "grey40") +
    geom_point(size = 3, colour = "grey20") +
    geom_errorbar(aes(ymin = alpha - sigma_alpha,
                      ymax = alpha + sigma_alpha),
                  width = 0.2, colour = "grey20") +
    labs(x = NULL,
         y = bquote(alpha ~ .(paste0("(", range_label, ")")))) +
    theme_bw(base_size = 12)
  if (!show_x) p <- p + theme(axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  else         p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}
p_drift_combined <- drift_panel("Low-load range",  show_x = FALSE) /
  drift_panel("High-load range", show_x = TRUE)
print(p_drift_combined)
ggsave(file.path(output_dir, "calibration_alpha_drift.png"),
       p_drift_combined, width = 8, height = 7, dpi = 600)

# -- Fig 3: Residuals ---------------------------------------------------------
calib_residuals <- calib_for_lm %>%
  mutate(residual = mean_force_g - alpha_for_residuals * target_g)

p_residuals <- ggplot(calib_residuals,
                      aes(x = target_g, y = residual, colour = session_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_x") +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "\nKnown weight added (g)", y = "Residual (g)\n", colour = "Session") +
  theme_bw(base_size = 12)
print(p_residuals)
ggsave(file.path(output_dir, "calibration_residuals.png"),
       p_residuals, width = 8, height = 4, dpi = 600)

# -- Fig 4: Raw scatter — all weights, split by range (supplementary, A4) ----
mean_tbl_all <- dat_raw %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(session_label, session_label_short, type, target_g) %>%
  summarise(mean_error = mean(error_g), .groups = "drop")

scatter_full <- function(range_label) {
  dat_sub  <- dat_raw %>% filter(type == range_label) %>%
    mutate(error_g = force_g - target_g)
  mean_sub <- mean_tbl_all %>% filter(type == range_label)
  ggplot(dat_sub, aes(x = session_label_short, y = error_g)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_jitter(width = 0.12, height = 0, size = 1.5, alpha = 0.6, colour = "grey30") +
    geom_point(data = mean_sub, aes(x = session_label_short, y = mean_error),
               colour = "red", size = 2.5) +
    geom_text(data  = mean_sub, aes(x = session_label_short, y = mean_error,
                                    label = round(mean_error, 2)),
              colour = "red", vjust = -1.2, hjust = 0.5, size = 2.5) +
    facet_wrap(~ paste0(target_g, " g"), scales = "fixed", ncol = 3) +
    labs(x = "\nCalibration session",
         y = paste0(range_label, "\nmeasurement error (g)")) +
    theme_bw(base_size = 11) +
    theme(strip.background  = element_rect(fill = "grey90"),
          axis.text.x       = element_text(angle = 45, hjust = 1))
}

p_scatter_low_full  <- scatter_full("Low-load range")
p_scatter_high_full <- scatter_full("High-load range")
print(p_scatter_low_full)
ggsave(file.path(output_dir, "calibration_raw_scatter_LowLoad.png"),
       p_scatter_low_full, width = 8.27, height = 11.69, dpi = 600)
print(p_scatter_high_full)
ggsave(file.path(output_dir, "calibration_raw_scatter_HighLoad.png"),
       p_scatter_high_full, width = 8.27, height = 11.69, dpi = 600)
cat("\nSupplementary scatter figures saved (one per load range, A4).\n")

# -- Fig 5: Raw scatter — paper example (single weight × single range) --------
dat_paper_scatter <- dat_raw %>%
  filter(type == paper_scatter_range, target_g == paper_scatter_weight) %>%
  mutate(error_g = force_g - target_g)

mean_paper_scatter <- mean_tbl_all %>%
  filter(type == paper_scatter_range, target_g == paper_scatter_weight) %>%
  mutate(session_label_short = factor(session_label_short,
                                      levels = label_short_levels))

if (nrow(dat_paper_scatter) == 0) {
  warning(sprintf(
    "No data for paper_scatter_range='%s', paper_scatter_weight=%g. Check values.",
    paper_scatter_range, paper_scatter_weight))
} else {
  p_scatter_paper <- ggplot(dat_paper_scatter,
                            aes(x = session_label_short, y = error_g)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_jitter(width = 0.12, height = 0, size = 1.8, alpha = 0.6, colour = "grey30") +
    geom_point(data = mean_paper_scatter,
               aes(x = session_label_short, y = mean_error),
               colour = "red", size = 3) +
    geom_text(data  = mean_paper_scatter,
              aes(x = session_label_short, y = mean_error,
                  label = round(mean_error, 2)),
              colour = "red", vjust = 0, hjust = -1.2, size = 3) +
    labs(x = "\nCalibration session",
         y = paste0("Measurement error (g)")) +
    theme_bw(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_scatter_paper)
  ggsave(file.path(output_dir, "calibration_raw_scatter_paper.png"),
         p_scatter_paper, width = 7, height = 6, dpi = 600)
  cat(sprintf("Paper scatter figure saved (%g g, %s).\n",
              paper_scatter_weight, paper_scatter_range))
}

# ---- Error range table ------------------------------------------------------
error_range_tbl <- dat_raw %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(session_label, target_g) %>%
  summarise(min_error_g = min(error_g), max_error_g = max(error_g),
            range_g = max_error_g - min_error_g, .groups = "drop")
write_csv(error_range_tbl,
          file.path(output_dir, "error_range_by_weight_and_session.csv"))

cat("\n=== 01_LoadCell_Characterisation.R complete ===\n")