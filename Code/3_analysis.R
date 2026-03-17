# =============================================================================
# 03_Analysis.R — Correction, Uncertainty Propagation, Statistics & Publication Figures
# =============================================================================
#
# PIPELINE B — Step 3 of 3. Must be run after 01_Calibration_Check.R and
# 02_Friction_Extraction.R.
#
# Purpose:
#   Applies the session-specific calibration correction (alpha_session)
#   computed by 01_Calibration_Check.R to the raw friction force readings
#   extracted in 02_Friction_Extraction.R. Propagates measurement
#   uncertainties following the GUM framework (k = 2 for expanded uncertainty),
#   runs the statistical analyses, and produces publication-ready figures.
#
# Calibration correction:
#   Each friction run is matched to its session alpha by experiment date.
#   The alpha applied to a given run is the mean of the before and after
#   load-cell checks conducted on the same day as that run:
#     F_corrected = F_raw × alpha_session
#     mu = F_corrected / M_actual
#
#   This traceability ensures the correction is temporally tied to each
#   experiment rather than derived from a separate characterisation study.
#
# Statistical approach:
#   - Normality tested via Shapiro-Wilk on model residuals.
#   - Homogeneity of variance tested via Levene's test.
#   - mu_s: standard two-way ANOVA (assumptions verified).
#   - mu_d: Aligned Rank Transform (ART) ANOVA (normality and homogeneity
#     violated; ART is the non-parametric equivalent for factorial designs).
#   - Post-hoc: Tukey HSD (standard ANOVA) / ART + emmeans (ART ANOVA).
#
# Inputs (from Results/):
#   calibration_parameters.csv   — session-specific alpha from Script 01
#   all_results.csv              — raw extracted friction results from Script 02
#
# Outputs (in Results/):
#   all_results_corrected.csv        — corrected friction coefficients + uncertainties
#   summary_corrected.csv            — mean ± SD per condition
#   example_force_curve_corrected.png — corrected example force curve (paper figure)
#   boxplot_mu_combined.png          — combined mu_s / mu_d boxplot (paper figure)
#   scatter_static_vs_dynamic.png    — mu_s vs mu_d scatter plot
#
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

output_dir <- "Results"

# ---- 1. LOAD CALIBRATION PARAMETERS ----------------------------------------

cat("=== LOADING CALIBRATION PARAMETERS ===\n\n")

calib_params <- read_csv(file.path(output_dir, "calibration_parameters.csv"),
                         show_col_types = FALSE)

cat(sprintf("Session-specific alpha values (%d session(s)):\n",
            nrow(calib_params)))
print(calib_params %>%
        dplyr::select(experiment_date, alpha_before, alpha_after,
                      alpha_session, sigma_alpha_combined, sigma_mechanical),
      width = Inf)
cat("\n")

# ---- 2. LOAD FRICTION RESULTS -----------------------------------------------

results_raw <- read_csv(file.path(output_dir, "all_results.csv"),
                        show_col_types = FALSE)
cat(sprintf("Loaded %d friction results.\n\n", nrow(results_raw)))

# ---- 3. JOIN SESSION ALPHA BY EXPERIMENT DATE -------------------------------
# Each run is matched to the calibration session conducted on the same date.
# If a run date has no matching calibration parameters, it will produce NA
# and a warning is issued.

results <- results_raw %>%
  left_join(calib_params %>%
              dplyr::select(experiment_date, alpha_session,
                            sigma_alpha_combined, sigma_mechanical,
                            U_balance),
            by = c("date_str" = "experiment_date"))

unmatched <- results %>% filter(is.na(alpha_session))
if (nrow(unmatched) > 0) {
  warning(sprintf(
    "%d run(s) could not be matched to a calibration session:\n%s\n",
    nrow(unmatched),
    paste(unique(unmatched$date_str), collapse = ", ")))
}

# ---- 4. APPLY SESSION-SPECIFIC CALIBRATION & PROPAGATE UNCERTAINTY ----------
#
# Force correction:
#   F_corrected = F_raw * alpha_session
#
# Friction coefficients:
#   mu = F_corrected / M_actual
#
# Uncertainty propagation follows the GUM framework.
#
# Components:
#   U_F      = sigma_mechanical
#              Mechanical / instrumental force uncertainty carried forward from
#              the calibration check.
#
#   U_alpha  = sigma_alpha_combined
#              Combined calibration uncertainty on alpha_session, including:
#                (i) fit-derived uncertainty on alpha from the chi-squared
#                    profile, and
#                (ii) within-session stability from the before/after difference.
#
#   U_M      = U_balance
#              Uncertainty on the applied transfer mass from the balance
#              resolution.
#
# Corrected-force uncertainty:
#   U(F_corrected) / F_corrected =
#       sqrt((U_F / F_raw)^2 + (U_alpha / alpha_session)^2)
#
# Friction-coefficient uncertainty:
#   U(mu) / mu =
#       sqrt((U(F_corrected) / F_corrected)^2 + (U_M / M_actual)^2)
#
# Expanded uncertainties are reported with k = 2.

M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

results <- results %>%
  mutate(
    # Session-specific corrected forces
    Fstat_corrected   = force_static_g  * alpha_session,
    Fdyn_corrected    = force_dynamic_g * alpha_session,
    
    # Force uncertainty (mechanical component from load cell characterisation)
    U_F = sigma_mechanical,
    
    # Corrected force uncertainty (GUM quadrature: force + alpha components)
    U_Fstat_corrected = Fstat_corrected * sqrt(
      (U_F / force_static_g)^2 + (sigma_alpha_combined / alpha_session)^2),
    U_Fdyn_corrected  = Fdyn_corrected * sqrt(
      (U_F / force_dynamic_g)^2 + (sigma_alpha_combined / alpha_session)^2),
    
    # Actual transfer mass and its uncertainty
    M_actual = M_transfer[as.character(weight_g)],
    U_M      = U_balance,
    
    # Static coefficient of friction
    mu_s = Fstat_corrected / M_actual,
    U_mu_s = mu_s * sqrt(
      (U_Fstat_corrected / Fstat_corrected)^2 + (U_M / M_actual)^2),
    U_mu_s_expanded = 2 * U_mu_s,   # k = 2 for 95% expanded uncertainty
    
    # Dynamic coefficient of friction
    mu_d = Fdyn_corrected / M_actual,
    U_mu_d = mu_d * sqrt(
      (U_Fdyn_corrected / Fdyn_corrected)^2 + (U_M / M_actual)^2),
    U_mu_d_expanded = 2 * U_mu_d,
    
    # Factors for plotting and ANOVA
    weight_f = factor(weight_g,   levels = c(200, 500, 800)),
    speed_f  = factor(speed_cm_s, levels = c(1.0, 1.5, 2.0))
  )

cat("Corrected results preview (first 10 rows):\n")
print(results %>%
        dplyr::select(filename, weight_g, speed_cm_s, date_str,
                      alpha_session, Fstat_corrected,
                      mu_s, U_mu_s_expanded,
                      mu_d, U_mu_d_expanded) %>%
        head(10),
      width = Inf)

write_csv(results, file.path(output_dir, "all_results_corrected.csv"))

# ---- 5. CORRECTED FORCE-CURVE FIGURES (paper-ready) -------------------------
# Unlike the diagnostic plots created in 02_Friction_Extraction.R, the force
# curves plotted here are corrected using the session-specific alpha_session.
# The annotated Fstat and Fdyn values therefore correspond to the corrected
# forces reported in the final analysis.

# Use the same raw data directory as in 02_Friction_Extraction.R
data_dir <- "Data/COF/"

# Global corrected y-axis maximum across all files
all_forces_corr <- map2_dbl(results$filename, results$alpha_session, function(fname, alpha) {
  fp <- file.path(data_dir, fname)
  df <- suppressWarnings(read_csv(fp, show_col_types = FALSE))
  max(na_if(df$force_g, 0) * alpha, na.rm = TRUE)
})
y_max_corr <- ceiling(max(all_forces_corr, na.rm = TRUE) * 1.15)

plot_one_rep_corrected <- function(fname, alpha_session, y_max) {
  fp <- file.path(data_dir, fname)
  
  df <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      sample = row_number(),
      force_g = na_if(force_g, 0),
      force_corr_g = force_g * alpha_session
    )
  
  motion <- df %>% filter(steps > 0, !is.na(force_corr_g))
  forces <- motion$force_corr_g
  
  if (length(forces) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Too few samples") +
        theme_void()
    )
  }
  
  local_max_idx  <- which(diff(sign(diff(forces))) == -2) + 1
  threshold_stat <- mean(forces, na.rm = TRUE)
  valid_max_idx  <- local_max_idx[forces[local_max_idx] > threshold_stat]
  peak_idx       <- if (length(valid_max_idx) > 0) valid_max_idx[1] else which.max(forces)
  
  fstat       <- forces[peak_idx]
  peak_sample <- motion$sample[peak_idx]
  
  if (peak_idx < length(forces)) {
    fdyn      <- mean(forces[(peak_idx + 1):length(forces)], na.rm = TRUE)
    dyn_start <- motion$sample[peak_idx + 1]
    dyn_end   <- max(motion$sample, na.rm = TRUE)
  } else {
    fdyn <- dyn_start <- dyn_end <- NA_real_
  }
  
  y_fs_label <- min(fstat + 0.05 * y_max, y_max * 0.88)
  y_fd_label <- if (!is.na(fdyn)) max(fdyn - 0.08 * y_max, y_max * 0.10) else NA_real_
  
  p <- ggplot(df, aes(x = sample, y = force_corr_g)) +
    geom_line(colour = "black", linewidth = 0.35, na.rm = TRUE) +
    geom_vline(xintercept = peak_sample, linetype = "dashed",
               colour = "orange", linewidth = 0.4) +
    annotate("text", x = peak_sample + 2, y = y_fs_label,
             label = paste0("F[stat] == ", round(fstat, 1), " ~ g"),
             hjust = 0, size = 3.2, parse = TRUE) +
    coord_cartesian(ylim = c(0, y_max)) +
    theme_bw(base_size = 10) +
    theme(axis.title   = element_blank(),
          axis.text    = element_text(size = 8),
          plot.margin  = margin(1, 1, 1, 1))
  
  if (!is.na(fdyn)) {
    p <- p +
      geom_segment(x = dyn_start, xend = dyn_end, y = fdyn, yend = fdyn,
                   colour = "red", linetype = "dotted", linewidth = 0.4) +
      annotate("text", x = (dyn_start + dyn_end) / 2, y = y_fd_label,
               label = paste0("F[dyn] == ", round(fdyn, 1), " ~ g"),
               size = 3.2, parse = TRUE)
  }
  
  p
}

# ---- 5B. PAPER EXAMPLE: SINGLE CORRECTED FORCE CURVE ------------------------

example_row <- results %>%
  filter(weight_g == 500, speed_cm_s == 1.5) %>%
  arrange(rep_num) %>%
  slice(1)

p_example_corr <- plot_one_rep_corrected(
  fname         = example_row$filename,
  alpha_session = example_row$alpha_session,
  y_max         = y_max_corr
) +
  labs(title = paste0(example_row$weight_g, " g, ",
                      example_row$speed_cm_s, " cm/s"),
       x = "Sample number",
       y = "Friction force (g)") +
  theme(plot.title = element_text(size = 11, hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 10))

ggsave(file.path(output_dir, "example_force_curve_corrected.png"),
       p_example_corr, width = 15, height = 10, units = "cm", dpi = 600)

# ---- 5.B SUMMARY STATISTICS --------------------------------------------------

summary_stats <- results %>%
  group_by(weight_f, speed_f) %>%
  summarise(
    n              = n(),
    mu_s_mean      = mean(mu_s,            na.rm = TRUE),
    mu_s_sd        = sd(mu_s,              na.rm = TRUE),
    mu_s_U_mean    = mean(U_mu_s_expanded, na.rm = TRUE),
    mu_d_mean      = mean(mu_d,            na.rm = TRUE),
    mu_d_sd        = sd(mu_d,              na.rm = TRUE),
    mu_d_U_mean    = mean(U_mu_d_expanded, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== SUMMARY (mean ± SD, mean expanded U at k = 2) ===\n")
print(summary_stats, width = Inf)
write_csv(summary_stats, file.path(output_dir, "summary_corrected.csv"))

# ---- 6. STATISTICAL ANALYSIS ------------------------------------------------

cat("\n=== ASSUMPTION CHECKS ===\n")

# Fit standard two-way ANOVA models for assumption testing
aov_static  <- aov(mu_s ~ weight_f * speed_f, data = results)
aov_dynamic <- aov(mu_d ~ weight_f * speed_f, data = results)

# Normality of residuals (Shapiro-Wilk)
cat("\n--- Shapiro-Wilk: mu_s residuals ---\n")
print(shapiro.test(residuals(aov_static)))

cat("\n--- Shapiro-Wilk: mu_d residuals ---\n")
print(shapiro.test(residuals(aov_dynamic)))

# Q-Q plots for visual inspection
par(mfrow = c(1, 2))
qqnorm(residuals(aov_static),  main = "Q-Q: mu_s residuals")
qqline(residuals(aov_static))
qqnorm(residuals(aov_dynamic), main = "Q-Q: mu_d residuals")
qqline(residuals(aov_dynamic))
par(mfrow = c(1, 1))

# Homogeneity of variance (Levene's test)
cat("\n--- Levene's test: mu_s ---\n")
print(leveneTest(mu_s ~ weight_f * speed_f, data = results))

cat("\n--- Levene's test: mu_d ---\n")
print(leveneTest(mu_d ~ weight_f * speed_f, data = results))

# ---- mu_s: standard two-way ANOVA -------------------------------------------
# Used when normality and homogeneity assumptions are met (verify above).

cat("\n=== TWO-WAY ANOVA: mu_s ~ weight * speed ===\n")
print(summary(aov_static))

cat("\n=== Tukey HSD: mu_s ===\n")
print(TukeyHSD(aov_static))

# ---- mu_d: ART ANOVA --------------------------------------------------------
# Used because normality and/or homogeneity are violated for mu_d.
# ART (Aligned Rank Transform) is the non-parametric equivalent of a
# two-way ANOVA and correctly handles factorial designs with interactions.
# Reference: Kay & Wobbrock (2016), ARTool R package.

cat("\n=== ART ANOVA: mu_d ~ weight * speed ===\n")
art_dynamic <- art(mu_d ~ weight_f * speed_f, data = results)
print(anova(art_dynamic))

cat("\n=== ART post-hoc: mu_d ~ weight_f ===\n")
print(contrast(emmeans(artlm(art_dynamic, "weight_f"), ~ weight_f),
               method = "pairwise", adjust = "tukey"))

cat("\n=== ART post-hoc: mu_d ~ speed_f ===\n")
print(contrast(emmeans(artlm(art_dynamic, "speed_f"), ~ speed_f),
               method = "pairwise", adjust = "tukey"))

# ---- 7. FIGURES -------------------------------------------------------------

# Shared y-axis range
mu_range   <- range(c(results$mu_s, results$mu_d), na.rm = TRUE)
mu_padding <- diff(mu_range) * 0.05
mu_ylim    <- c(mu_range[1] - mu_padding, mu_range[2] + mu_padding)

# -- Boxplot: mu_s by weight and speed ----------------------------------------
p_static <- ggplot(results, aes(x = weight_f, y = mu_s, fill = speed_f)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(aes(colour = speed_f),
              position = position_jitterdodge(jitter.width = 0.15),
              size = 1.5, alpha = 0.6) +
  labs(x = NULL,
       y = expression(mu[s] ~ ""),
       fill   = "Speed (cm/s)",
       colour = "Speed (cm/s)") +
  coord_cartesian(ylim = mu_ylim) +
  theme_bw(base_size = 14) +
  scale_fill_brewer(palette   = "Set2") +
  scale_colour_brewer(palette = "Set2")
p_static

# -- Boxplot: mu_d by weight and speed ----------------------------------------
p_dynamic <- ggplot(results, aes(x = weight_f, y = mu_d, fill = speed_f)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(aes(colour = speed_f),
              position = position_jitterdodge(jitter.width = 0.15),
              size = 1.5, alpha = 0.6) +
  labs(x      = "Weight (g)",
       y      = expression(mu[d] ~ ""),
       fill   = "Speed (cm/s)",
       colour = "Speed (cm/s)") +
  coord_cartesian(ylim = mu_ylim) +
  theme_bw(base_size = 14) +
  scale_fill_brewer(palette   = "Set2") +
  scale_colour_brewer(palette = "Set2")
p_dynamic

# -- Combined figure (paper) --------------------------------------------------
p_combined <- (p_static + theme(legend.position = "none")) /
  (p_dynamic + theme(legend.position = "bottom")) +
  plot_layout(nrow = 2)
p_combined

ggsave(file.path(output_dir, "boxplot_mu_combined.png"),
       p_combined, width = 8, height = 10, dpi = 300)

# -- Scatter: mu_s vs mu_d ----------------------------------------------------
p_scatter <- ggplot(results, aes(x = mu_d, y = mu_s,
                                 colour = weight_f, shape = speed_f)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3, alpha = 0.7) +
  labs(x      = expression(mu[d]),
       y      = expression(mu[s]),
       colour = "Weight (g)",
       shape  = "Speed (cm/s)") +
  theme_bw(base_size = 13) +
  scale_colour_brewer(palette = "Set2") +
  coord_equal()
p_scatter

ggsave(file.path(output_dir, "scatter_static_vs_dynamic.png"),
       p_scatter, width = 7, height = 6, dpi = 600)

cat("\n=== 03_Analysis.R complete ===\n")
cat("All outputs saved to:", output_dir, "\n")