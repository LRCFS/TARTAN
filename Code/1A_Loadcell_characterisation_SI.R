# =============================================================================
# 1A_LoadCell_characterisation_SI.R — Load Cell Characterisation Study
# =============================================================================
#
# PIPELINE A — Standalone. Does NOT feed into the friction data analysis.
#
# Purpose:
#   Characterises the TARTAN load cell in two complementary stages.
#
#   1. Raw K-factor stability characterisation
#      Five repeated calibration curves are recorded from 0 to 1000 g, with
#      50 raw HX711/load-cell measurements collected at each applied mass.
#      This analysis quantifies:
#        - Raw-counts-per-gram calibration factor K
#        - Repeat-to-repeat variation in K
#        - Zero-offset drift
#        - Linearity of the raw load-cell response
#
#      This distinguishes zero-offset drift, which can be corrected by taring,
#      from sensitivity drift, which would affect the calibration factor K.
#
#   2. Calibrated load-cell characterisation across measurement sessions
#      Multi-weight calibration curves collected across several sessions
#      (morning / midday / evening, across several days) are used to quantify
#      the following. The tare rows (0 g) and the scale-factor mass (899.45 g)
#      are excluded, as they were used to configure the load cell:
#        - Linearity (linear regression per session and load range)
#        - Metrological validity (chi-squared criterion, following
#          Galais 2024, Appendix 4)
#        - Session-to-session drift in calibration factor alpha
#
#   Results are used in the paper to document load-cell behaviour.
#   They do NOT correct the friction experiment data — that correction
#   is handled by 1B_Session_Calibration_for_Transfer.R using the before/after files.
#
# IMPORTANT:
#   In the session characterisation, each calibration curve is treated as an
#   independent object. Each session × load-range curve therefore gets its own:
#     - alpha
#     - u_alpha
#     - chi2_min (full) and chi2_red_min (reduced)
#
#   This script does NOT compute an averaged alpha_session from before/after
#   calibrations. That averaging belongs only in the transfer-data correction
#   workflow.
#
# Input files:
#
#   Raw K-factor stability:
#     Data/K_characterisation.csv
#     Columns:
#       Mass = applied reference mass (g)
#       raw  = raw HX711/load-cell reading
#       Rep  = repeated calibration curve (Rep1–Rep5)
#     Fifty raw measurements are recorded at each mass within each repeat.
#
#   Session characterisation (in data_dir):
#     YYYYMMDD_CalibrationRaw_HighLoadRange_weights_[morning|midday|evening].csv
#     YYYYMMDD_CalibrationRaw_LowLoadRange_weights_[morning|midday|evening].csv
#     Each file contains repeated measurements at several known masses,
#     collected over 20 samples per mass. Columns:
#       date, label, weight_index, target_g, sample_index, force_g
#
# Main outputs:
#   kchar_drift_by_repeat.csv
#   kchar_drift_summary.csv
#   kchar_and_zero_drift_paper.png
#   kchar_Rep1_Rep5.png
#   kchar_all_repeats.png
#   calibration_linearity_paper.png
#   calibration_linearity_supp_*.png
#   calibration_alpha_drift.png
#   calibration_residuals.png
#   calibration_raw_scatter_paper.png
#   calibration_raw_scatter_LowLoad.png
#   calibration_raw_scatter_HighLoad.png
#   linear_models_by_session.csv
#   chi2_results_Load-cell_characterisation.csv
#   Load-cell_characterisation_drift_across_sessions.csv
#   error_range_by_weight_and_session.csv
#   points_outside_display_window.csv (only if any points fall outside ±1 g)
#
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

data_dir   <- "Data/Calibration curve/"
output_dir <- "Results/Load-cell_characterisation/"
k_file     <- "Data/K_characterisation.csv"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01  # grams — resolution of the reference balance
U_balance <- balance_resolution / sqrt(12)

# Explicit calibrated-mass order for plotting
low_load_levels  <- c(49.65, 99.47, 149.32, 199.28, 249.20,
                      298.95, 349.00, 399.01, 448.85, 498.81)

high_load_levels <- c(99.90, 200.00, 299.67, 399.86, 499.47,
                      599.45, 699.41, 799.37)

# Reference mass used to set the firmware scale factor k (Section 2.4.2).
# Because it defines the conversion from raw counts to grams, it cannot provide
# an independent check of the converted response and is therefore excluded from
# the session assessment, together with the tare rows (0 g).
scale_factor_mass <- 899.45

# Time-of-day ordering for sorting sessions within a day
tod_levels <- c("morning", "midday", "evening")

# Representative session for the PAPER linearity figure
paper_session_date <- "20260312"
paper_session_tod  <- "morning"

# Representative weight for the PAPER raw scatter example figure
paper_scatter_range  <- "Low-load range"
paper_scatter_weight <- 249.20

# ---- 1. RAW LOAD-CELL RESPONSE AND kchar STABILITY --------------------------

k_raw <- read_csv(k_file, show_col_types = FALSE) %>%
  mutate(raw = as.numeric(raw), Mass = as.numeric(Mass),
         Rep = factor(Rep, levels = paste0("Rep", 1:5)))

k_summary <- k_raw %>%
  group_by(Rep, Mass) %>%
  summarise(n = n(), mean_raw = mean(raw), sd_raw = sd(raw), .groups = "drop")

k_models <- k_summary %>%
  group_by(Rep) %>%
  group_modify(~{
    fit <- lm(mean_raw ~ Mass, data = .x)
    tibble(k_char = coef(fit)[2], intercept = coef(fit)[1],
           R2 = summary(fit)$r.squared)
  }) %>% ungroup()

k_zero <- k_raw %>%
  filter(Mass == 0) %>%
  group_by(Rep) %>%
  summarise(zero_mean_raw = mean(raw), zero_sd_raw = sd(raw), .groups = "drop")

k_drift <- k_models %>%
  left_join(k_zero, by = "Rep") %>%
  mutate(k_char_change_pct_vs_Rep1 = 100 * (k_char / first(k_char) - 1),
         zero_shift_raw_vs_Rep1 = zero_mean_raw - first(zero_mean_raw),
         zero_shift_g_vs_Rep1 = zero_shift_raw_vs_Rep1 / first(k_char))

k_drift_overall <- k_drift %>%
  summarise(k_char_mean = mean(k_char), k_char_sd = sd(k_char),
            k_char_min = min(k_char), k_char_max = max(k_char),
            k_char_range = k_char_max - k_char_min,
            k_char_range_pct_of_mean = 100 * k_char_range / k_char_mean,
            zero_drift_raw_Rep1_to_Rep5 = last(zero_mean_raw) - first(zero_mean_raw),
            zero_drift_g_Rep1_to_Rep5 = zero_drift_raw_Rep1_to_Rep5 / first(k_char))

print(k_drift, width = Inf)
print(k_drift_overall, width = Inf)
write_csv(k_drift, file.path(output_dir, "kchar_drift_by_repeat.csv"))
write_csv(k_drift_overall, file.path(output_dir, "kchar_drift_summary.csv"))

# kchar and zero-offset drift
p_k_drift <- ggplot(k_drift, aes(Rep, k_char, group = 1)) +
  geom_line(colour = "grey40") + geom_point(size = 3, colour = "grey20") +
  labs(x = "Repeat", y = expression(k[char]~"(raw counts/g)")) + theme_bw(base_size = 14)+
  theme(axis.title.y = element_text(margin = margin(r = 12)))
p_k_drift

p_zero_drift <- ggplot(k_drift, aes(Rep, zero_mean_raw, group = 1)) +
  geom_line(colour = "grey40") + geom_point(size = 3, colour = "grey20") +
  labs(x = "Repeat", y = "Mean raw reading at 0 g") + theme_bw(base_size = 14)+
  theme(axis.title.y = element_text(margin = margin(r = 12)))
p_zero_drift

p_k_combined <- (p_zero_drift | p_k_drift) + plot_annotation(tag_levels = "A")
p_k_combined

ggsave(file.path(output_dir, "kchar_and_zero_drift_paper.png"),
       p_k_combined, width = 9, height = 4, dpi = 600)

# Rep1 and Rep5 linearity
k_15 <- k_summary %>% filter(Rep %in% c("Rep1", "Rep5"))
labels_15 <- k_models %>% filter(Rep %in% c("Rep1", "Rep5")) %>%
  mutate(label = sprintf("y = %.4fx + %.2f\nR² = %.8f", k_char, intercept, R2))

p_15 <- ggplot(k_15, aes(Mass, mean_raw)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_text(aes(label = round(mean_raw)), colour = "firebrick4",
            nudge_x = -30, nudge_y = 20000, hjust = 1, size = 3) +
  geom_text(data = labels_15, aes(x = 500, y = -Inf, label = label),
            inherit.aes = FALSE, vjust = -0.8, size = 3.2) +
  facet_wrap(~Rep, nrow = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.20, 0.02))) +  # room for the 0 g label (Reviewer 2)
  labs(x = "Applied mass (g)", y = "Mean raw load-cell signal") +
  theme_bw(base_size = 14)+
  theme(axis.title.y = element_text(margin = margin(r = 12)))+
  theme(plot.margin = margin(t = 0, r = 0.6, b = 0, l = 0, unit = "cm"))
p_15

ggsave(file.path(output_dir, "kchar_Rep1_Rep5.png"),
       p_15, width = 8, height = 5, dpi = 600)

# All repeats linearity (for SI)
labels_all <- k_models %>%
  mutate(label = sprintf("y = %.4fx + %.2f\nR² = %.8f", k_char, intercept, R2))

p_all <- ggplot(k_summary, aes(Mass, mean_raw)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_text(aes(label = round(mean_raw)), colour = "firebrick4",
            nudge_x = -30, nudge_y = 20000, hjust = 1, size = 3) +
  geom_text(data = labels_all, aes(x = 500, y = -Inf, label = label),
            inherit.aes = FALSE, vjust = -0.8, size = 3.2) +
  facet_wrap(~Rep, ncol = 2) +
  scale_x_continuous(expand = expansion(mult = c(0.20, 0.02))) +
  labs(x = "Applied mass (g)", y = "Mean raw load-cell signal") +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(margin = margin(r = 12)),
        plot.margin = margin(t = 0, r = 0.6, b = 0, l = 0, unit = "cm"))

p_all

ggsave(file.path(output_dir, "kchar_all_repeats.png"),
       p_all, width = 8, height = 10, dpi = 600)

# ---- 2. LOAD ALL CALIBRATION CURVE FILES ------------------------------------

calib_files <- list.files(
  data_dir,
  pattern = "CalibrationRaw_.*LoadRange.*\\.csv$",
  full.names = TRUE
)

cat(sprintf("Found %d calibration curve files.\n\n", length(calib_files)))

calib_all_raw <- map_dfr(calib_files, function(fp) {
  fname       <- basename(fp)
  date_str    <- substr(fname, 1, 8)
  range_type  <- ifelse(grepl("LowLoad", fname), "Low-load range", "High-load range")
  range_short <- ifelse(grepl("LowLoad", fname), "Low", "High")
  tod <- stringr::str_extract(fname, "(morning|midday|evening)(?=\\.csv$)")
  # Stop rather than silently creating an NA session: a name like
  # "..._evening - Copy.csv" or "..._evening (1).csv" is usually a duplicate.
  if (is.na(tod)) {
    stop("Unexpected calibration file name (duplicate/copy?): ", fname,
         "\n  Expected ..._weights_[morning|midday|evening].csv")
  }
  
  read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      calib_date          = date_str,
      date                = paste0(substr(date_str, 1, 4), "-",
                                   substr(date_str, 5, 6), "-",
                                   substr(date_str, 7, 8)),
      time_of_day         = factor(tod, levels = tod_levels),
      range_short         = range_short,
      session             = paste0(date_str, "_", tod, "_", range_short),
      session_label       = paste0(date, " (", tod, ", ", range_short, "-load)"),
      session_label_short = paste0(date, "\n(", tod, ")"),
      type                = range_type
    )
})

# ---- 3. ZERO REMOVAL --------------------------------------------------------

zeros_removed <- calib_all_raw %>% filter(force_g == 0)

cat("=== Zero removal report ===\n")
cat(sprintf("Total measurements loaded: %d\n", nrow(calib_all_raw)))
cat(sprintf("Zero readings found:       %d\n", nrow(zeros_removed)))

if (nrow(zeros_removed) > 0) {
  cat("\nDetail of removed zero readings:\n")
  print(
    zeros_removed %>%
      dplyr::select(session_label, type, target_g, sample_index, force_g),
    n = Inf, width = Inf
  )
} else {
  cat("No zero readings detected.\n")
}

calib_all_raw <- calib_all_raw %>% filter(force_g != 0)
cat(sprintf("Measurements retained: %d\n\n", nrow(calib_all_raw)))

# ---- 4. ORDER SESSIONS CHRONOLOGICALLY --------------------------------------

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

# Define dat_raw for scatter plots and error tables
# (excludes tare rows and the scale-factor mass)
dat_raw <- calib_all_raw %>%
  filter(target_g > 0, !dplyr::near(target_g, scale_factor_mass, tol = 1e-6))

# ---- 5. SUMMARY (after zero removal) ----------------------------------------

calib_summary <- calib_all_raw %>%
  group_by(session, session_label, session_label_short,
           calib_date, date, time_of_day, range_short, type,
           target_g) %>%
  summarise(
    n_samples    = n(),
    mean_force_g = mean(force_g, na.rm = TRUE),
    sd_force_g   = sd(force_g, na.rm = TRUE),
    se_force_g   = sd_force_g / sqrt(n_samples),
    .groups = "drop"
  ) %>%
  mutate(error_g = mean_force_g - target_g)

# Exclude tare rows (target_g == 0) and the scale-factor mass (899.45 g)
# from regression and chi-squared
calib_for_lm <- calib_summary %>%
  filter(target_g > 0, !dplyr::near(target_g, scale_factor_mass, tol = 1e-6))

cat(sprintf("Excluded from session assessment: tare (0 g) and scale-factor mass (%.2f g).\n",
            scale_factor_mass))

# ---- 6. LINEAR REGRESSION (per session and range) ---------------------------

lm_tbl <- calib_for_lm %>%
  group_by(session, session_label, type) %>%
  group_modify(~ {
    fit <- lm(mean_force_g ~ target_g, data = .x)
    tibble(
      R2 = formatC(summary(fit)$r.squared, format = "f", digits = 8),
      Equation = paste0(
        "y = ",
        formatC(coef(fit)[2], format = "f", digits = 6),
        "x + ",
        formatC(coef(fit)[1], format = "f", digits = 4)
      )
    )
  }) %>%
  ungroup() %>%
  transmute(
    Session = session_label,
    `Weight range` = type,
    `R-squared` = R2,
    Equation
  )

cat("\n=== Linear models per session ===\n")
print(lm_tbl, width = Inf, n = 30)
write_csv(lm_tbl, file.path(output_dir, "linear_models_by_session.csv"))

# ---- 7. CHI-SQUARED VALIDATION ----------------------------------------------
# NUMERICAL TOLERANCE: optimise() and uniroot() are called with explicit, tight
# tolerances. Their default (~1.2e-4) is larger than the uncertainty on alpha
# (~1e-5), which made u_alpha collapse to 0 or become numerical noise.
# Each calibration curve (session × range) is fitted independently.
# Same chi-squared functions as in 1B_Session_Calibration_for_Transfer.R:
#   - compute_chi2 returns the FULL chi-squared;
#   - sigma_mechanical is chosen so that the REDUCED chi-squared,
#     chi2 / (n - 1), is close to 1;
#   - the uncertainty on alpha is the half-width of the interval where the
#     FULL chi-squared rises by 1 above its minimum (chi2_min + 1).

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

chi2_results <- calib_for_lm %>%
  group_by(session, session_label, session_label_short,
           calib_date, date, time_of_day, range_short, type) %>%
  group_modify(~ {
    M  <- .x$target_g
    X  <- .x$mean_force_g
    sX <- .x$se_force_g
    UM <- rep(U_balance, length(M))
    
    sm  <- find_sigma_mechanical(M, X, sX, UM)
    opt <- find_optimal_alpha(sm, M, X, sX, UM)
    ua  <- compute_u_alpha(opt$alpha, opt$chi2, sm, M, X, sX, UM)
    
    tibble(
      n_points          = length(M),
      sigma_mechanical  = sm,
      alpha             = opt$alpha,
      chi2_min          = opt$chi2,
      chi2_red_min      = compute_chi2_red(opt$chi2, length(M)),
      chi2_threshold    = ua$threshold,
      alpha_min         = ua$alpha_min,
      alpha_max         = ua$alpha_max,
      u_alpha           = ua$u_alpha
    )
  }) %>%
  ungroup()

cat("\n=== Chi-squared results ===\n")
print(
  chi2_results %>%
    dplyr::select(session_label, type, n_points,
                  sigma_mechanical, alpha, chi2_min, chi2_red_min,
                  chi2_threshold, u_alpha),
  width = Inf, n = 30
)

write_csv(
  chi2_results,
  file.path(output_dir, "chi2_results_Load-cell_characterisation.csv")
)

# ---- 8. DRIFT ANALYSIS ACROSS SESSIONS --------------------------------------

drift_summary <- chi2_results %>%
  group_by(type) %>%
  summarise(
    n_sessions      = n(),
    alpha_mean      = mean(alpha, na.rm = TRUE),
    alpha_sd        = sd(alpha, na.rm = TRUE),
    alpha_range     = max(alpha, na.rm = TRUE) - min(alpha, na.rm = TRUE),
    sigma_mech_mean = mean(sigma_mechanical, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Drift across calibration sessions ===\n")
print(drift_summary, width = Inf)

write_csv(
  drift_summary,
  file.path(output_dir, "Load-cell_characterisation_drift_across_sessions.csv")
)

# ---- 9. FIGURES -------------------------------------------------------------

# -- Fig 1a: Linearity — paper (one representative session) -------------------

calib_paper <- calib_for_lm %>%
  filter(calib_date == paper_session_date,
         as.character(time_of_day) == paper_session_tod)

if (nrow(calib_paper) == 0) {
  warning(sprintf(
    "No data for paper_session_date='%s', paper_session_tod='%s'. Check values.",
    paper_session_date, paper_session_tod
  ))
} else {
  linearity_panel <- function(range_label) {
    dat <- calib_paper %>% filter(type == range_label)
    fit <- lm(mean_force_g ~ target_g, data = dat)
    r2  <- summary(fit)$r.squared
    cf  <- coef(fit)
    
    eq_label <- sprintf("y = %.4fx + %.3f\nR² = %.6f", cf[2], cf[1], r2)
    
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
  
  ggsave(
    file.path(output_dir, "calibration_linearity_paper.png"),
    p_linearity_paper,
    width = 8, height = 4, dpi = 600
  )
}

# -- Fig 1b: Linearity — supplementary (one figure per session) ---------------

all_sessions <- calib_for_lm %>%
  distinct(calib_date, time_of_day) %>%
  arrange(calib_date, time_of_day)

linearity_supp_panel <- function(dat_sess, range_label) {
  dat <- dat_sess %>% filter(type == range_label)
  if (nrow(dat) < 2) return(NULL)   # lm needs >= 2 points
  
  fit <- lm(mean_force_g ~ target_g, data = dat)
  r2  <- summary(fit)$r.squared
  cf  <- coef(fit)
  
  eq_label <- sprintf("y = %.4fx + %.3f\nR² = %.6f", cf[2], cf[1], r2)
  
  ggplot(dat, aes(x = target_g, y = mean_force_g)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(ymin = mean_force_g - sd_force_g,
                      ymax = mean_force_g + sd_force_g),
                  width = 0, colour = "grey20") +
    geom_point(size = 2, colour = "grey20") +
    annotate("text", x = -Inf, y = Inf, label = eq_label,
             hjust = -0.1, vjust = 1.3, size = 3.0, colour = "grey20") +
    labs(x = "\nKnown weight added (g)",
         y = "Average load cell reading (g)\n",
         title = range_label) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
}

for (i in seq_len(nrow(all_sessions))) {
  s_date <- all_sessions$calib_date[i]
  s_tod  <- as.character(all_sessions$time_of_day[i])
  
  dat_sess <- calib_for_lm %>%
    filter(calib_date == s_date, as.character(time_of_day) == s_tod)
  
  panels <- list(
    linearity_supp_panel(dat_sess, "Low-load range"),
    linearity_supp_panel(dat_sess, "High-load range")
  )
  panels <- Filter(Negate(is.null), panels)
  
  if (length(panels) == 0) {
    warning(sprintf("No linearity panels for %s %s — skipped.", s_date, s_tod))
    next
  }
  
  p_supp <- wrap_plots(panels, nrow = 1) +
    plot_annotation(title = paste0(s_date, " — ", s_tod))
  
  ggsave(
    file.path(output_dir,
              paste0("calibration_linearity_supp_", s_date, "_", s_tod, ".png")),
    p_supp,
    width = 4 * length(panels), height = 4, dpi = 600
  )
}

cat("\nSupplementary linearity figures saved (one per session).\n")

# -- Fig 2: Alpha drift -------------------------------------------------------

drift_panel <- function(range_label, show_x = TRUE) {
  dat <- chi2_results %>% filter(type == range_label)
  
  p <- ggplot(dat, aes(x = session_label_short, y = alpha, group = 1)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.6, colour = "grey40") +
    geom_point(size = 3, colour = "grey20") +
    geom_errorbar(aes(ymin = alpha - u_alpha,
                      ymax = alpha + u_alpha),
                  width = 0.2, colour = "grey20") +
    labs(x = NULL,
         y = bquote(alpha ~ .(paste0(" (", range_label, ")")))) +
    theme_bw(base_size = 12)
  
  if (!show_x) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  p
}

p_drift_combined <- drift_panel("Low-load range", show_x = FALSE) /
  drift_panel("High-load range", show_x = TRUE)

print(p_drift_combined)

ggsave(
  file.path(output_dir, "calibration_alpha_drift.png"),
  p_drift_combined,
  width = 8, height = 7, dpi = 600
)

# -- Fig 3: Residuals ---------------------------------------------------------
# Residuals are calculated using the curve-specific alpha for each session × range.

calib_residuals <- calib_for_lm %>%
  left_join(
    chi2_results %>%
      dplyr::select(session, type, alpha),
    by = c("session", "type")
  ) %>%
  mutate(residual = mean_force_g - alpha * target_g)

p_residuals <- ggplot(
  calib_residuals,
  aes(x = target_g, y = residual, colour = session_label)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_x") +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "\nKnown weight added (g)",
       y = "Residual (g)\n",
       colour = "Session") +
  theme_bw(base_size = 12)

print(p_residuals)

ggsave(
  file.path(output_dir, "calibration_residuals.png"),
  p_residuals,
  width = 8, height = 4, dpi = 600
)

# ---- Display window for raw scatter figures ---------------------------------
# The +/- 1 g window is applied ONLY through coord_cartesian() in the plots,
# so boxplot statistics (median, quartiles, whiskers) are computed from ALL
# measurements. Points outside the window are reported below so that the
# figure captions can state how many fall outside the displayed range.

scatter_display_limits <- c(-1, 1)

dat_raw_scatter_display <- dat_raw %>%
  mutate(error_g = force_g - target_g)

outside_display <- dat_raw_scatter_display %>%
  filter(error_g < scatter_display_limits[1] |
           error_g > scatter_display_limits[2])

cat("\n=== Points outside the displayed error range ===\n")
cat(sprintf("Display window: %+.1f g to %+.1f g\n",
            scatter_display_limits[1], scatter_display_limits[2]))
cat(sprintf("Points outside window: %d of %d (all included in boxplot statistics)\n",
            nrow(outside_display), nrow(dat_raw_scatter_display)))

if (nrow(outside_display) > 0) {
  outside_summary <- outside_display %>%
    group_by(type, target_g, session_label) %>%
    summarise(n_outside = n(),
              min_error_g = min(error_g),
              max_error_g = max(error_g),
              .groups = "drop")
  print(outside_summary, n = Inf, width = Inf)
  write_csv(outside_summary,
            file.path(output_dir, "points_outside_display_window.csv"))
} else {
  cat("No points outside the display window.\n")
}

mean_tbl_scatter_display <- dat_raw_scatter_display %>%
  group_by(session_label, session_label_short, type, target_g) %>%
  summarise(mean_error = mean(error_g), .groups = "drop")

# -- Fig 4: Raw scatter — all weights, split by range -------------------------

scatter_full <- function(range_label) {
  
  if (range_label == "Low-load range") {
    weight_levels <- low_load_levels
  } else if (range_label == "High-load range") {
    weight_levels <- high_load_levels
  } else {
    stop("Unknown range_label")
  }
  
  dat_sub <- dat_raw_scatter_display %>%
    filter(type == range_label) %>%
    mutate(
      target_f = factor(
        sprintf("%.2f g", target_g),
        levels = sprintf("%.2f g", weight_levels)
      )
    )
  
  mean_sub <- mean_tbl_scatter_display %>%
    filter(type == range_label) %>%
    mutate(
      target_f = factor(
        sprintf("%.2f g", target_g),
        levels = sprintf("%.2f g", weight_levels)
      )
    )
  
  ggplot(dat_sub, aes(x = session_label_short, y = error_g)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_boxplot(outlier.shape = NA, width = 0.55, fill = "white", colour = "grey30") +
    geom_jitter(width = 0.12, height = 0, size = 1.2, alpha = 0.5, colour = "grey30") +
    facet_wrap(~ target_f, scales = "fixed", ncol = 3) +
    coord_cartesian(ylim = scatter_display_limits) +
    labs(x = "\nCalibration session",
         y = paste0(range_label, "\nmeasurement error (g)")) +
    theme_bw(base_size = 11) +
    theme(strip.background = element_rect(fill = "grey90"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_scatter_low_full  <- scatter_full("Low-load range")
p_scatter_high_full <- scatter_full("High-load range")

print(p_scatter_low_full)
ggsave(
  file.path(output_dir, "calibration_raw_scatter_LowLoad.png"),
  p_scatter_low_full,
  width = 8.27, height = 11.69, dpi = 600
)

print(p_scatter_high_full)
ggsave(
  file.path(output_dir, "calibration_raw_scatter_HighLoad.png"),
  p_scatter_high_full,
  width = 8.27, height = 11.69, dpi = 600
)

cat("\nSupplementary scatter figures saved (one per load range, A4).\n")

# -- Fig 5: Raw scatter — paper example ---------------------------------------

dat_paper_scatter <- dat_raw_scatter_display %>%
  filter(type == paper_scatter_range,
         dplyr::near(target_g, paper_scatter_weight, tol = 1e-6)) %>%
  mutate(session_label_short = factor(session_label_short,
                                      levels = label_short_levels))

paper_session_labels <- c(
  "2026-03-10\n(evening)" = "D1 (E)",
  "2026-03-11\n(morning)" = "D2 (M)",
  "2026-03-11\n(midday)" = "D2 (MD)",
  "2026-03-11\n(evening)" = "D2 (E)",
  "2026-03-12\n(morning)" = "D3 (M)",
  "2026-03-12\n(midday)" = "D3 (MD)"
)

if (nrow(dat_paper_scatter) == 0) {
  warning(sprintf(
    "No data for paper_scatter_range='%s', paper_scatter_weight=%g. Check values.",
    paper_scatter_range, paper_scatter_weight
  ))
} else {
  p_scatter_paper <- ggplot(dat_paper_scatter,
                            aes(x = session_label_short, y = error_g)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_boxplot(outlier.shape = NA, width = 0.55,
                 fill = "white", colour = "grey30") +
    geom_jitter(width = 0.12, height = 0, size = 1.8,
                alpha = 0.6, colour = "grey30") +
    scale_x_discrete(labels = paper_session_labels) +
    coord_cartesian(ylim = scatter_display_limits) +
    labs(x = "\nSession", y = "Measurement error (g)") +
    theme_bw(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_scatter_paper)
  
  ggsave(
    file.path(output_dir, "calibration_raw_scatter_paper.png"),
    p_scatter_paper, width = 7, height = 6, dpi = 600
  )
  
  cat(sprintf("Paper scatter figure saved (%g g, %s).\n",
              paper_scatter_weight, paper_scatter_range))
  cat(sprintf("  Points outside the displayed range in this figure: %d of %d\n",
              sum(dat_paper_scatter$error_g < scatter_display_limits[1] |
                    dat_paper_scatter$error_g > scatter_display_limits[2]),
              nrow(dat_paper_scatter)))
}

# ---- Error range table ------------------------------------------------------

error_range_tbl <- dat_raw %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(session_label, target_g) %>%
  summarise(
    min_error_g = min(error_g),
    max_error_g = max(error_g),
    range_g = max_error_g - min_error_g,
    .groups = "drop"
  )

write_csv(
  error_range_tbl,
  file.path(output_dir, "error_range_by_weight_and_session.csv")
)

cat("\n=== 1A_Loadcell_characterisation_SI.R complete ===\n")