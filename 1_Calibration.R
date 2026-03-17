# =============================================================================
# 01_calibration.R — Calibration Validation
# =============================================================================
# Validates the transfer device load cell using full calibration curves.
# Combines linear regression (for paper figures) with chi-squared criterion
# (for metrological validation, following Galais 2024, Appendix 4).
#
# Inputs:  Calibration curve CSVs (*_CalibrationRaw_*LoadRange_weights.csv)
# Outputs: calibration_parameters.csv, chi2_results_all_days.csv,
#          linearity/residual/drift/error plots
#
# Run this FIRST, before 02_friction_extraction.R
# =============================================================================

library(tidyverse)

# ---- SHARED CONFIGURATION ---------------------------------------------------
# (same across all three scripts)

data_dir   <- "Data/Calibration curve/"
output_dir <- "Results"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01  # grams
U_balance <- balance_resolution / sqrt(12)

# Actual measured masses of the transfer weights (precision balance)
M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

# ---- 1. LOAD ALL CALIBRATION CURVES -----------------------------------------

calib_files <- list.files(data_dir,
                          pattern = "CalibrationRaw_.*LoadRange.*\\.csv$",
                          full.names = TRUE)
cat(sprintf("Found %d calibration curve files.\n\n", length(calib_files)))

calib_all_raw <- map_dfr(calib_files, function(fp) {
  fname <- basename(fp)
  date_str <- substr(fname, 1, 8)
  range_type <- ifelse(grepl("LowLoad", fname), "Low-load range",
                       "High-load range")
  read_csv(fp, show_col_types = FALSE) %>%
    mutate(calib_date = date_str,
           date = paste0(substr(date_str, 1, 4), "-",
                         substr(date_str, 5, 6), "-",
                         substr(date_str, 7, 8)),
           type = range_type)
})

# Summary per date, range, and weight
calib_summary <- calib_all_raw %>%
  group_by(date, calib_date, type, weight_index, target_g) %>%
  summarise(
    n_samples     = n(),
    mean_force_g  = mean(force_g, na.rm = TRUE),
    sd_force_g    = sd(force_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(error_g = mean_force_g - target_g)

cat("Calibration summary:\n")
print(calib_summary, width = Inf, n = 30)

# ---- 2. LINEAR REGRESSION (per date and range) ------------------------------

# Exclude 0g for regression
calib_for_lm <- calib_summary %>% filter(target_g > 0)

lm_tbl <- calib_for_lm %>%
  group_by(date, type) %>%
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
    Date = date,
    `Weight range` = type,
    `R-squared` = R2,
    Equation
  )

cat("\n=== Linear models per date ===\n")
print(lm_tbl, width = Inf, n = 20)
write_csv(lm_tbl, file.path(output_dir, "linear_models_by_date.csv"))

# ---- 3. CHI-SQUARED VALIDATION (per date and range) -------------------------

# Helper functions
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
       alpha_min = alpha_min, alpha_max = alpha_max,
       sigma_alpha = (alpha_max - alpha_min) / 6)
}

# Run chi-squared for each date x range
chi2_results <- calib_for_lm %>%
  group_by(date, calib_date, type) %>%
  group_modify(~ {
    M <- .x$target_g
    X <- .x$mean_force_g
    sX <- .x$sd_force_g
    UM <- rep(U_balance, length(M))
    
    sm <- find_sigma_mechanical(M, X, sX, UM)
    opt <- find_optimal_alpha(sm, M, X, sX, UM)
    sa <- compute_sigma_alpha(opt$alpha, opt$chi2, sm, M, X, sX, UM)
    
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

cat("\n=== Chi-squared results (cf. thesis Table 7-4) ===\n")
print(chi2_results %>%
        select(date, type, n_points, sigma_mechanical, alpha,
               chi2_min, sigma_alpha),
      width = Inf, n = 20)

write_csv(chi2_results, file.path(output_dir, "chi2_results_all_days.csv"))

# ---- 4. DRIFT ANALYSIS ACROSS DAYS ------------------------------------------

drift_summary <- chi2_results %>%
  group_by(type) %>%
  summarise(
    n_days      = n(),
    alpha_mean  = mean(alpha),
    alpha_sd    = sd(alpha),
    alpha_range = max(alpha) - min(alpha),
    sigma_mech_mean = mean(sigma_mechanical),
    .groups = "drop"
  )

cat("\n=== Drift across calibration days ===\n")
print(drift_summary, width = Inf)
write_csv(drift_summary, file.path(output_dir, "calibration_drift_across_days.csv"))

# ---- 5. SELECT ALPHA FOR EXPERIMENT -----------------------------------------
# Use latest calibration day (closest to experiment date 20260217).
# Add day-to-day drift SD in quadrature for honest uncertainty.

latest_date <- max(chi2_results$calib_date)
latest_high <- chi2_results %>%
  filter(calib_date == latest_date, type == "High-load range")

alpha_calib     <- latest_high$alpha
sigma_alpha_chi <- latest_high$sigma_alpha
sigma_drift     <- drift_summary %>%
  filter(type == "High-load range") %>% pull(alpha_sd)
sigma_alpha_combined <- sqrt(sigma_alpha_chi^2 + sigma_drift^2)
sigma_mech_cal  <- latest_high$sigma_mechanical

cat(sprintf("\n-> Selected alpha = %.8f (latest: %s, High-load range)\n",
            alpha_calib, latest_date))
cat(sprintf("   sigma_alpha = %.8f (chi2: %.8f + drift: %.8f)\n",
            sigma_alpha_combined, sigma_alpha_chi, sigma_drift))

# Save parameters for use by other scripts
calib_params <- tibble(
  parameter = c("alpha", "sigma_alpha", "sigma_alpha_chi2", "sigma_drift",
                "sigma_mechanical", "U_balance",
                "M_transfer_200", "M_transfer_500", "M_transfer_800"),
  value = c(alpha_calib, sigma_alpha_combined, sigma_alpha_chi, sigma_drift,
            sigma_mech_cal, U_balance,
            M_transfer[["200"]], M_transfer[["500"]], M_transfer[["800"]])
)
write_csv(calib_params, file.path(output_dir, "calibration_parameters.csv"))

# ---- 6. FIGURES --------------------------------------------------------------

# Fig: Linearity plot (faceted by range)
p_linearity <- ggplot(calib_for_lm,
                      aes(x = target_g, y = mean_force_g, shape = date)) +
  geom_point(size = 2, colour = "grey20") +
  geom_errorbar(aes(ymin = mean_force_g - sd_force_g,
                    ymax = mean_force_g + sd_force_g),
                width = 0, colour = "grey20") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ type, scales = "free_x") +
  scale_shape_manual(values = c(16, 17, 15, 18, 3)) +
  labs(x = "\nKnown weight added (g)",
       y = "Average load cell reading (g)\n",
       shape = "Date") +
  theme_bw(base_size = 12)
p_linearity

ggsave(file.path(output_dir, "calibration_linearity.png"), p_linearity,
       width = 8, height = 4, dpi = 600)

# Fig: Alpha drift over days
p_drift <- ggplot(chi2_results,
                  aes(x = date, y = alpha, colour = type, group = type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.6) +
  geom_errorbar(aes(ymin = alpha - sigma_alpha, ymax = alpha + sigma_alpha),
                width = 0.2) +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "grey50") +
  labs(x = "\nCalibration date", y = expression(alpha ~ "\n"),
       colour = "Load range") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")
p_drift

ggsave(file.path(output_dir, "calibration_alpha_drift.png"), p_drift,
       width = 7, height = 5, dpi = 600)

# Fig: Residuals
calib_residuals <- calib_for_lm %>%
  mutate(residual = mean_force_g - alpha_calib * target_g)

p_residuals <- ggplot(calib_residuals,
                      aes(x = target_g, y = residual, colour = date)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_x") +
  labs(x = "\nKnown weight added (g)", y = "Residual (g)\n",
       colour = "Date") +
  theme_bw(base_size = 12)
p_residuals

ggsave(file.path(output_dir, "calibration_residuals.png"), p_residuals,
       width = 8, height = 4, dpi = 600)

# Fig: Raw scatter for a single weight (example)
dat_raw <- calib_all_raw %>% filter(target_g > 0)

# Pick the weight closest to 250g for the raw scatter
available_weights <- sort(unique(dat_raw$target_g))
mass_of_interest <- available_weights[which.min(abs(available_weights - 250))]

mean_tbl <- dat_raw %>%
  filter(target_g == mass_of_interest) %>%
  group_by(date) %>%
  summarise(mean_error = mean(force_g - target_g), .groups = "drop")

p_raw_scatter <- ggplot(dat_raw %>% filter(target_g == mass_of_interest),
                        aes(x = date, y = force_g - target_g)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_jitter(width = 0.12, height = 0, size = 1.8, alpha = 0.6,
              colour = "grey30") +
  geom_point(data = mean_tbl, aes(x = date, y = mean_error),
             colour = "red", size = 3) +
  geom_text(data = mean_tbl,
            aes(x = date, y = mean_error, label = round(mean_error, 2)),
            colour = "red", vjust = -1.2, hjust = -1.5, size = 3) +
  labs(x = "\nDate",
       y = paste0("Raw measurement error at ", mass_of_interest, " g (g)\n")) +
  theme_bw(base_size = 12)
p_raw_scatter

ggsave(file.path(output_dir, "calibration_raw_scatter.png"), p_raw_scatter,
       width = 9, height = 5, dpi = 600)

# Supplementary: error per weight faceted
p_supp_error <- ggplot(dat_raw %>% mutate(error_g = force_g - target_g),
                       aes(x = date, y = error_g, colour = date)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(width = 0.12, height = 0, alpha = 0.6, size = 1.2) +
  facet_wrap(~ paste0(target_g, " g"), scales = "free_y") +
  labs(x = "Date", y = "Raw measurement error (g)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey90"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "calibration_error_by_weight_supp.png"),
       p_supp_error, width = 10, height = 8, dpi = 600)

# Error range table
error_range_tbl <- dat_raw %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(date, target_g) %>%
  summarise(min_error_g = min(error_g), max_error_g = max(error_g),
            range_g = max_error_g - min_error_g, .groups = "drop")
write_csv(error_range_tbl,
          file.path(output_dir, "error_range_by_weight_and_date.csv"))

cat("\n=== 01_calibration.R complete ===\n")
