# =============================================================================
# 01_calibration.R — Calibration Validation
# =============================================================================
# Validates the transfer device load cell using full calibration curves.
# Combines linear regression (for paper figures) with chi-squared criterion
# (for metrological validation, following Galais 2024, Appendix 4).
#
# Inputs:  Calibration curve CSVs
#          (*_CalibrationRaw_*LoadRange_weights_[morning/midday/evening].csv)
# Outputs: calibration_parameters.csv, chi2_results_all_sessions.csv,
#          linearity/residual/drift/scatter plots
#
# NOTE: Multiple calibration sessions per day are supported.
#       Each unique date × time-of-day × range combination is treated as a
#       separate calibration session.
#       Zero removal occurs BEFORE any summarisation.
#
# Run this FIRST, before 02_friction_extraction.R
# =============================================================================

# ---- SHARED CONFIGURATION ---------------------------------------------------

data_dir   <- "Data/Calibration curve/"
output_dir <- "Results"
output_dir2 <- "Results/Calibration_curve"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01  # grams
U_balance <- balance_resolution / sqrt(12)

# Actual measured masses of the transfer weights (precision balance)
M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

# Time-of-day ordering for sorting sessions within a day
tod_levels <- c("morning", "midday", "evening")

# Representative session for the PAPER linearity figure (one per range).
# Set to the date string and time-of-day of the session you want to highlight.
# All other sessions are relegated to the supplementary figures.
paper_session_date <- "20260312"   # e.g. most recent date
paper_session_tod  <- "morning"    # morning / midday / evening

# ---- 1. LOAD ALL CALIBRATION CURVES -----------------------------------------

calib_files <- list.files(data_dir,
                          pattern = "CalibrationRaw_.*LoadRange.*\\.csv$",
                          full.names = TRUE)
cat(sprintf("Found %d calibration curve files.\n\n", length(calib_files)))

calib_all_raw <- map_dfr(calib_files, function(fp) {
  fname       <- basename(fp)
  date_str    <- substr(fname, 1, 8)
  range_type  <- ifelse(grepl("LowLoad", fname), "Low-load range",
                        "High-load range")
  range_short <- ifelse(grepl("LowLoad", fname), "Low", "High")
  
  tod <- stringr::str_extract(fname, "(morning|midday|evening)(?=\\.csv$)")
  if (is.na(tod)) tod <- "unknown"
  
  read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      calib_date    = date_str,
      date          = paste0(substr(date_str, 1, 4), "-",
                             substr(date_str, 5, 6), "-",
                             substr(date_str, 7, 8)),
      time_of_day   = factor(tod, levels = tod_levels),
      range_short   = range_short,
      # Full unique session ID (used internally)
      session       = paste0(date_str, "_", tod, "_", range_short),
      # Label with range — used in legends
      session_label = paste0(date_str %>%
                               {paste0(substr(., 1, 4), "-",
                                       substr(., 5, 6), "-",
                                       substr(., 7, 8))},
                             " (", tod, ", ", range_short, "-load)"),
      # Short label WITHOUT range — used on drift x-axis (panel already
      # encodes range)
      session_label_short = paste0(date_str %>%
                                     {paste0(substr(., 1, 4), "-",
                                             substr(., 5, 6), "-",
                                             substr(., 7, 8))},
                                   "\n(", tod, ")"),
      type          = range_type
    )
})

# ---- 2. ZERO REMOVAL (before ANY summarisation) -----------------------------
# This must happen here so that zeroes never enter means or residuals.

zeros_removed <- calib_all_raw %>% filter(force_g == 0)

cat("=== Zero removal report ===\n")
cat(sprintf("Total measurements loaded: %d\n", nrow(calib_all_raw)))
cat(sprintf("Zero readings found:       %d\n", nrow(zeros_removed)))

if (nrow(zeros_removed) > 0) {
  cat("\nDetail of removed zero readings:\n")
  print(zeros_removed %>%
          dplyr::select(session_label, type, target_g,
                        sample_index, force_g),
        n = Inf, width = Inf)
} else {
  cat("No zero readings detected.\n")
}

calib_all_raw <- calib_all_raw %>% filter(force_g != 0)
cat(sprintf("Measurements retained: %d\n\n", nrow(calib_all_raw)))

# ---- 3. ORDER SESSIONS CHRONOLOGICALLY --------------------------------------

session_order <- calib_all_raw %>%
  distinct(session, session_label, session_label_short,
           calib_date, time_of_day) %>%
  arrange(calib_date, time_of_day) %>%
  pull(session)

calib_all_raw <- calib_all_raw %>%
  mutate(
    session = factor(session, levels = session_order),
    session_label = factor(
      session_label,
      levels = calib_all_raw %>%
        distinct(session, session_label) %>%
        arrange(match(session, session_order)) %>%
        pull(session_label)
    ),
    session_label_short = factor(
      session_label_short,
      levels = calib_all_raw %>%
        distinct(session, session_label_short) %>%
        arrange(match(session, session_order)) %>%
        pull(session_label_short) %>%
        unique()
    )
  )

# Define dat_raw here so it is available throughout the script
dat_raw <- calib_all_raw %>% filter(target_g > 0)

# ---- 4. SUMMARY (after zero removal) ----------------------------------------

calib_summary <- calib_all_raw %>%
  group_by(session, session_label, session_label_short,
           calib_date, date, time_of_day, range_short, type,
           weight_index, target_g) %>%
  summarise(
    n_samples    = n(),
    mean_force_g = mean(force_g, na.rm = TRUE),
    sd_force_g   = sd(force_g,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(error_g = mean_force_g - target_g)

cat("Calibration summary:\n")
print(calib_summary, width = Inf, n = 50)

# ---- 5. LINEAR REGRESSION (per session and range) ---------------------------

calib_for_lm <- calib_summary %>% filter(target_g > 0)

lm_tbl <- calib_for_lm %>%
  group_by(session_label, type) %>%
  do({
    fit <- lm(mean_force_g ~ target_g, data = .)
    data.frame(
      R2 = formatC(summary(fit)$r.squared, format = "f", digits = 6),
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
    Session        = session_label,
    `Weight range` = type,
    `R-squared`    = R2,
    Equation
  )

cat("\n=== Linear models per session ===\n")
print(lm_tbl, width = Inf, n = 30)
write_csv(lm_tbl, file.path(output_dir, "linear_models_by_session.csv"))

# ---- 6. CHI-SQUARED VALIDATION (per session and range) ----------------------

compute_chi2 <- function(alpha, sigma_mech, M, X, sigma_X, U_M) {
  n <- length(M)
  residuals_sq <- (M - alpha * X)^2
  denominators <- U_M^2 + (sigma_X * alpha)^2 + sigma_mech^2
  (1 / (n - 1)) * sum(residuals_sq / denominators)
}

find_optimal_alpha <- function(sigma_mech, M, X, sigma_X, U_M) {
  result <- optimise(
    f = function(a) compute_chi2(a, sigma_mech, M, X, sigma_X, U_M),
    interval = c(0.95, 1.05)
  )
  list(alpha = result$minimum, chi2 = result$objective)
}

find_sigma_mechanical <- function(M, X, sigma_X, U_M) {
  opt0 <- find_optimal_alpha(0, M, X, sigma_X, U_M)
  if (opt0$chi2 <= 1.0) return(0)
  optimise(
    f = function(s) (find_optimal_alpha(s, M, X, sigma_X, U_M)$chi2 - 1.0)^2,
    interval = c(0, 10)
  )$minimum
}

compute_sigma_alpha <- function(alpha_opt, chi2_min, sigma_mech,
                                M, X, sigma_X, U_M) {
  n <- length(M)
  chi2_3sigma <- chi2_min * (1 + (3 / (n - 1))^2)
  
  find_at <- function(side) {
    iv <- if (side == "lower") c(alpha_opt - 0.01, alpha_opt) else
      c(alpha_opt, alpha_opt + 0.01)
    optimise(function(a)
      (compute_chi2(a, sigma_mech, M, X, sigma_X, U_M) - chi2_3sigma)^2,
      interval = iv)$minimum
  }
  
  alpha_min <- find_at("lower")
  alpha_max <- find_at("upper")
  list(chi2_3sigma = chi2_3sigma,
       alpha_min   = alpha_min, alpha_max = alpha_max,
       sigma_alpha = (alpha_max - alpha_min) / 6)
}

chi2_results <- calib_for_lm %>%
  group_by(session, session_label, session_label_short,
           calib_date, date, time_of_day, range_short, type) %>%
  group_modify(~ {
    M  <- .x$target_g
    X  <- .x$mean_force_g
    sX <- .x$sd_force_g
    UM <- rep(U_balance, length(M))
    
    sm  <- find_sigma_mechanical(M, X, sX, UM)
    opt <- find_optimal_alpha(sm, M, X, sX, UM)
    sa  <- compute_sigma_alpha(opt$alpha, opt$chi2, sm, M, X, sX, UM)
    
    tibble(
      n_points         = length(M),
      sigma_mechanical = sm,
      alpha            = opt$alpha,
      chi2_min         = opt$chi2,
      chi2_3sigma      = sa$chi2_3sigma,
      sigma_alpha      = sa$sigma_alpha
    )
  }) %>%
  ungroup()

cat("\n=== Chi-squared results ===\n")
print(chi2_results %>%
        dplyr::select(session_label, type, n_points, sigma_mechanical,
                      alpha, chi2_min, sigma_alpha),
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

# ---- 8. SELECT ALPHA FOR EXPERIMENT -----------------------------------------

latest_high <- chi2_results %>%
  filter(type == "High-load range") %>%
  arrange(calib_date, time_of_day) %>%
  slice_tail(n = 1)

alpha_calib          <- latest_high$alpha
sigma_alpha_chi      <- latest_high$sigma_alpha
sigma_drift          <- drift_summary %>%
  filter(type == "High-load range") %>% pull(alpha_sd)
sigma_alpha_combined <- sqrt(sigma_alpha_chi^2 + sigma_drift^2)
sigma_mech_cal       <- latest_high$sigma_mechanical

cat(sprintf(
  "\n-> Selected alpha = %.8f (latest session: %s, High-load range)\n",
  alpha_calib, as.character(latest_high$session_label)))
cat(sprintf(
  "   sigma_alpha = %.8f (chi2: %.8f + drift: %.8f)\n",
  sigma_alpha_combined, sigma_alpha_chi, sigma_drift))

calib_params <- tibble(
  parameter = c("alpha", "sigma_alpha", "sigma_alpha_chi2", "sigma_drift",
                "sigma_mechanical", "U_balance",
                "M_transfer_200", "M_transfer_500", "M_transfer_800"),
  value = c(alpha_calib, sigma_alpha_combined, sigma_alpha_chi, sigma_drift,
            sigma_mech_cal, U_balance,
            M_transfer[["200"]], M_transfer[["500"]], M_transfer[["800"]])
)
write_csv(calib_params, file.path(output_dir, "calibration_parameters.csv"))

# ---- 9. FIGURES --------------------------------------------------------------

# ---------------------------------------------------------------------------
# Fig 1: LINEARITY — paper figure (one representative session per range)
# ---------------------------------------------------------------------------
# Select the representative session defined at the top of the script
paper_session_id <- paste0(paper_session_date, "_", paper_session_tod)

calib_paper <- calib_for_lm %>%
  filter(calib_date == paper_session_date,
         as.character(time_of_day) == paper_session_tod)

if (nrow(calib_paper) == 0) {
  warning(sprintf(
    "No data found for paper_session_date = '%s' and paper_session_tod = '%s'.\n",
    paper_session_date, paper_session_tod),
    "Check these values at the top of the script.")
} else {
  linearity_panel <- function(range_label) {
    dat <- calib_paper %>% filter(type == range_label)
    ggplot(dat, aes(x = target_g, y = mean_force_g)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                  colour = "grey40") +
      geom_errorbar(aes(ymin = mean_force_g - sd_force_g,
                        ymax = mean_force_g + sd_force_g),
                    width = 0, colour = "grey20") +
      geom_point(size = 2.5, colour = "grey20") +
      labs(x = "\nKnown weight added (g)",
           y = "Average load cell reading (g)\n",
           title = range_label) +
      theme_bw(base_size = 12) +
      theme(plot.title = element_text(size = 11, hjust = 0.5))
  }
  
  p_linearity_paper <- linearity_panel("Low-load range") |
    linearity_panel("High-load range")
  
  ggsave(file.path(output_dir, "calibration_linearity_paper.png"),
         p_linearity_paper, width = 8, height = 4, dpi = 600)
  cat(sprintf("\nPaper linearity figure saved (%s, %s).\n",
              paper_session_date, paper_session_tod))
}
p_linearity_paper

# ---------------------------------------------------------------------------
# Fig 2: LINEARITY — supplementary (all sessions, one figure per session,
#         High-load and Low-load side by side)
# ---------------------------------------------------------------------------
all_sessions <- calib_for_lm %>%
  distinct(calib_date, time_of_day, session_label) %>%
  arrange(calib_date, time_of_day)

for (i in seq_len(nrow(all_sessions))) {
  s_date  <- all_sessions$calib_date[i]
  s_tod   <- as.character(all_sessions$time_of_day[i])
  s_label <- as.character(all_sessions$session_label[i])
  # Remove range suffix for a clean file name
  s_clean <- gsub(", (Low|High)-load\\)", ")", s_label) %>%
    gsub("[^A-Za-z0-9_-]", "_", .)
  
  dat_sess <- calib_for_lm %>%
    filter(calib_date == s_date,
           as.character(time_of_day) == s_tod)
  
  linearity_supp_panel <- function(range_label) {
    dat <- dat_sess %>% filter(type == range_label)
    if (nrow(dat) == 0) return(NULL)
    ggplot(dat, aes(x = target_g, y = mean_force_g)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                  colour = "grey40") +
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
  
  if (!is.null(p_low) && !is.null(p_high)) {
    p_supp_sess <- (p_low | p_high) +
      plot_annotation(title = paste0(s_date, " — ", s_tod))
  } else {
    p_supp_sess <- (p_low %||% p_high) +
      plot_annotation(title = paste0(s_date, " — ", s_tod))
  }
  
  fname <- file.path(output_dir2,
                     paste0("calibration_linearity_supp_",
                            s_date, "_", s_tod, ".png"))
  ggsave(fname, p_supp_sess, width = 8, height = 4, dpi = 600)
}
cat("\nSupplementary linearity figures saved (one per session).\n")

# ---------------------------------------------------------------------------
# Fig 3: ALPHA DRIFT — stacked panels, x-axis = date + time only (no range)
# ---------------------------------------------------------------------------
drift_panel <- function(range_label) {
  dat <- chi2_results %>% filter(type == range_label)
  
  ggplot(dat, aes(x = session_label_short, y = alpha, group = 1)) +
    geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.6, colour = "grey40") +
    geom_point(size = 3, colour = "grey20") +
    geom_errorbar(aes(ymin = alpha - sigma_alpha,
                      ymax = alpha + sigma_alpha),
                  width = 0.2, colour = "grey20") +
    labs(x = NULL,
         y = bquote(alpha ~ .(paste0("(", range_label, ")")))) +
    theme_bw(base_size = 12)
}

p_drift_low  <- drift_panel("Low-load range") +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p_drift_high <- drift_panel("High-load range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_drift_combined <- p_drift_low / p_drift_high
p_drift_combined

ggsave(file.path(output_dir, "calibration_alpha_drift.png"),
       p_drift_combined, width = 8, height = 7, dpi = 600)

# ---------------------------------------------------------------------------
# Fig 4: RESIDUALS
# ---------------------------------------------------------------------------
calib_residuals <- calib_for_lm %>%
  mutate(residual = mean_force_g - alpha_calib * target_g)

p_residuals <- ggplot(calib_residuals,
                      aes(x = target_g, y = residual,
                          colour = session_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_x") +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "\nKnown weight added (g)", y = "Residual (g)\n",
       colour = "Session") +
  theme_bw(base_size = 12)
p_residuals

ggsave(file.path(output_dir, "calibration_residuals.png"), p_residuals,
       width = 8, height = 4, dpi = 600)

# ---------------------------------------------------------------------------
# Fig 5: RAW SCATTER
# ---------------------------------------------------------------------------
# Configure which single weight + range to use for the PAPER example figure.
# The two full figures (all weights, one per range) go to supplementary.
paper_scatter_range  <- "Low-load range"   # "High-load range" or "Low-load range"
paper_scatter_weight <- 249.20              # set to one of your target_g values

# -- Helper: build mean error table ------------------------------------------
mean_tbl_all <- dat_raw %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(session_label, session_label_short, type, target_g) %>%
  summarise(mean_error = mean(error_g), .groups = "drop")

# -- Helper: full scatter for one range (all weights faceted) -----------------
scatter_full <- function(range_label) {
  dat_sub  <- dat_raw      %>% filter(type == range_label) %>%
    mutate(error_g = force_g - target_g)
  mean_sub <- mean_tbl_all %>% filter(type == range_label)
  
  ggplot(dat_sub, aes(x = session_label_short, y = error_g)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_jitter(width = 0.12, height = 0, size = 1.5,
                alpha = 0.6, colour = "grey30") +
    geom_point(data = mean_sub,
               aes(x = session_label_short, y = mean_error),
               colour = "red", size = 2.5) +
    geom_text(data  = mean_sub,
              aes(x = session_label_short, y = mean_error,
                  label = round(mean_error, 2)),
              colour = "red", vjust = -1.2, hjust = -0.2, size = 2.5) +
    facet_wrap(~ paste0(target_g, " g"), scales = "fixed", ncol = 3) +
    labs(x        = "\nCalibration session",
         y        = "Measurement error (g)\n",
         subtitle = range_label) +
    theme_bw(base_size = 11) +
    theme(strip.background = element_rect(fill = "grey90"),
          axis.text.x      = element_text(angle = 45, hjust = 1),
          plot.subtitle    = element_text(hjust = 0.5, face = "bold"))
}

# Save one A4-sized figure per range
p_scatter_low_full  <- scatter_full("Low-load range")
p_scatter_high_full <- scatter_full("High-load range")

print(p_scatter_low_full)
ggsave(file.path(output_dir, "calibration_raw_scatter_LowLoad.png"),
       p_scatter_low_full,
       width = 8.27, height = 11.69, dpi = 600)

print(p_scatter_high_full)
ggsave(file.path(output_dir, "calibration_raw_scatter_HighLoad.png"),
       p_scatter_high_full,
       width = 8.27, height = 11.69, dpi = 600)

cat("\nSupplementary scatter figures saved (one per load range, A4).\n")

# -- Paper example: single weight × single range ------------------------------
dat_paper_scatter  <- dat_raw %>%
  filter(type == paper_scatter_range, target_g == paper_scatter_weight) %>%
  mutate(error_g = force_g - target_g)

mean_paper_scatter <- mean_tbl_all %>%
  filter(type == paper_scatter_range, target_g == paper_scatter_weight)

if (nrow(dat_paper_scatter) == 0) {
  warning(sprintf(
    "No data for paper_scatter_range = '%s' and paper_scatter_weight = %g. ",
    paper_scatter_range, paper_scatter_weight),
    "Check these values.")
} else {
  p_scatter_paper <- ggplot(dat_paper_scatter,
                            aes(x = session_label_short, y = error_g)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_jitter(width = 0.12, height = 0, size = 1.8,
                alpha = 0.6, colour = "grey30") +
    geom_point(data = mean_paper_scatter,
               aes(x = session_label_short, y = mean_error),
               colour = "red", size = 3) +
    geom_text(data  = mean_paper_scatter,
              aes(x = session_label_short, y = mean_error,
                  label = round(mean_error, 2)),
              colour = "red",
              size   = 3,
              vjust  = -1,    # always directly above the red point
              hjust  = -1) +   # centred over the point
    labs(x = "\nCalibration session",
         y = paste0("Measurement error at ",
                    paper_scatter_weight, " g (", paper_scatter_range, ")\n")) +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Print AFTER building, not before
  print(p_scatter_paper)

  ggsave(file.path(output_dir, "calibration_raw_scatter_paper.png"),
         p_scatter_paper, width = 8, height = 5, dpi = 600)
  cat(sprintf("Paper scatter figure saved (%g g, %s).\n",
              paper_scatter_weight, paper_scatter_range))
}

# ---------------------------------------------------------------------------
# Error range table
# ---------------------------------------------------------------------------
error_range_tbl <- dat_raw %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(session_label, target_g) %>%
  summarise(min_error_g = min(error_g), max_error_g = max(error_g),
            range_g = max_error_g - min_error_g, .groups = "drop")
write_csv(error_range_tbl,
          file.path(output_dir, "error_range_by_weight_and_session.csv"))

cat("\n=== 01_calibration.R complete ===\n")
