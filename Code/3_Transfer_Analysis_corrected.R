# =============================================================================
# 3_Transfer_Analysis_corrected.R — Correction, Uncertainty Propagation, Statistics & Publication Figures
# =============================================================================
#
# PIPELINE B — Step 3 of 3. Must be run after 1B_Session_Calibration_for_Transfer.R
# and 2_Friction_extraction_Raw.R.
#
# Purpose:
#   Applies the session-specific calibration correction (alpha_session)
#   computed by 1B_Session_Calibration_for_Transfer.R to the raw friction force
#   readings extracted in 2_Friction_extraction_Raw.R. Propagates measurement
#   uncertainties following the GUM framework (k = 2 for expanded uncertainty),
#   runs the statistical analyses, and produces publication-ready figures.
#
# Calibration correction:
#   Each friction run is matched to its session alpha by experiment date.
#   (Session_calibration_parameters.csv, produced by Script 1B.)
#   The alpha applied to a given run is the mean of the before and after
#   load-cell checks conducted on the same day as that run:
#     F_corrected = F_raw × alpha_session
#     mu = F_corrected / M_actual
#
#   This traceability ensures the correction is temporally tied to each
#   experiment rather than derived from a separate characterisation study.
#
# Statistical approach:
#   - mu_s and mu_d are analysed separately, and the effects of applied mass
#     and transfer speed are assessed independently (one-factor comparisons).
#   - Group differences are tested with Kruskal-Wallis rank-sum tests.
#   - Where the omnibus test is significant (p < 0.05), Dunn's post-hoc
#     pairwise comparisons are performed with Holm adjustment for multiple
#     testing (FSA::dunnTest, method = "holm").
#   - Descriptive statistics (n, mean, SD, median, IQR) are reported per group.
#
# Inputs (from Results/Session-Specific_Calibration_Check/):
#   Session_calibration_parameters.csv — session-specific alpha from Script 1B
#   all_results_uncorrected.csv        — raw extracted friction results from Script 2
#   Raw experiment files are also re-read from Data/COF/ for the force-curve figures.
#
# Outputs (in Results/Session-Specific_Calibration_Check/):
#   all_results_corrected.csv          — corrected friction coefficients + uncertainties
#   summary_corrected.csv              — mean ± SD per condition
#   example_force_curve_corrected.png  — corrected example force curve (paper figure)
#   force_curves_corrected/            — corrected force curves, one figure per condition
#   boxplot_mu_combined.png            — combined mu_s / mu_d boxplot (paper figure)
#   scatter_static_vs_dynamic.png      — mu_s vs mu_d scatter plot (paper figure)
#
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

output_dir <- "Results/Session-Specific_Calibration_Check/"

# ---- 1. LOAD CALIBRATION PARAMETERS (from Script 1B) ------------------------

cat("=== LOADING CALIBRATION PARAMETERS ===\n\n")

# Dates are read as character on both sides so the join cannot fail on type
calib_params <- read_csv(file.path(output_dir, "Session_calibration_parameters.csv"),
                         col_types = cols(experiment_date = col_character()),
                         show_col_types = FALSE)

cat(sprintf("Session-specific alpha values (%d session(s)):\n",
            nrow(calib_params)))
print(calib_params %>%
        dplyr::select(experiment_date, alpha_before, alpha_after,
                      alpha_session, u_alpha_session, sigma_mechanical),
      width = Inf)
cat("\n")

# ---- 2. LOAD FRICTION RESULTS -----------------------------------------------

results_raw <- read_csv(file.path(output_dir, "all_results_uncorrected.csv"),
                        col_types = cols(date_str = col_character()),
                        show_col_types = FALSE)
cat(sprintf("Loaded %d friction results.\n\n", nrow(results_raw)))

# ---- 3. JOIN SESSION ALPHA BY EXPERIMENT DATE -------------------------------
# Each run is matched to the calibration session conducted on the same date.
# If a run date has no matching calibration parameters, the script stops.

results <- results_raw %>%
  left_join(calib_params %>%
              dplyr::select(
                experiment_date,
                alpha_session,
                u_alpha_session,
                sigma_mechanical,
                u_balance
              ),
            by = c("date_str" = "experiment_date"))

unmatched <- results %>% filter(is.na(alpha_session))
if (nrow(unmatched) > 0) {
  # Stop: an unmatched run would silently become NA and drop out of the stats
  stop(sprintf(
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
# Components (all standard uncertainties):
#   u_F_mech       = sigma_mechanical
#                    Mechanical / instrumental force uncertainty carried
#                    forward from the calibration check.
#
#   u_alpha_session = u_alpha_session
#                    Combined standard uncertainty on alpha_session, including:
#                      (i) fit-derived uncertainty from the chi-squared profile,
#                      (ii) within-session stability from the before/after difference.
#
#   u_M            = u_balance
#                    Standard uncertainty on the applied transfer mass from the
#                    balance resolution.
#
# Corrected-force standard uncertainty:
#   u(F_corrected) / F_corrected =
#       sqrt((u_F_mech / F_raw)^2 + (u_alpha_session / alpha_session)^2)
#
# Friction-coefficient standard uncertainty:
#   u(mu) / mu =
#       sqrt((u(F_corrected) / F_corrected)^2 + (u_M / M_actual)^2)
#
# Expanded uncertainties are then reported with k = 2.

M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

results <- results %>%
  mutate(
    Fstat_corrected = force_static_g  * alpha_session,
    Fdyn_corrected  = force_dynamic_g * alpha_session,
    
    u_F_mech = sigma_mechanical,
    
    u_Fstat_corrected = Fstat_corrected * sqrt(
      (u_F_mech / force_static_g)^2 + (u_alpha_session / alpha_session)^2
    ),
    u_Fdyn_corrected = Fdyn_corrected * sqrt(
      (u_F_mech / force_dynamic_g)^2 + (u_alpha_session / alpha_session)^2
    ),
    
    M_actual = M_transfer[as.character(weight_g)],
    u_M      = u_balance,
    
    mu_s = Fstat_corrected / M_actual,
    u_mu_s = mu_s * sqrt(
      (u_Fstat_corrected / Fstat_corrected)^2 + (u_M / M_actual)^2
    ),
    U_mu_s = 2 * u_mu_s,
    
    mu_d = Fdyn_corrected / M_actual,
    u_mu_d = mu_d * sqrt(
      (u_Fdyn_corrected / Fdyn_corrected)^2 + (u_M / M_actual)^2
    ),
    U_mu_d = 2 * u_mu_d,
    
    weight_f = factor(weight_g,   levels = c(200, 500, 800)),
    speed_f  = factor(speed_cm_s, levels = c(1.0, 1.5, 2.0))
  )

cat("Corrected results preview (first 10 rows):\n")
print(results %>%
        dplyr::select(filename, weight_g, speed_cm_s, date_str,
                      alpha_session, Fstat_corrected,
                      mu_s, U_mu_s,
                      mu_d, U_mu_d) %>%
        head(10),
      width = Inf)

write_csv(results, file.path(output_dir, "all_results_corrected.csv"))

# ---- 5. CORRECTED FORCE-CURVE FIGURES (paper-ready) -------------------------
# Unlike the diagnostic plots created in 2_Friction_extraction_Raw.R, the force
# curves plotted here are corrected using the session-specific alpha_session.
# The annotated Fstat and Fdyn values therefore correspond to the corrected
# forces reported in the final analysis.

# Use the same raw data directory as in 2_Friction_extraction_Raw.R
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

# ---- 5C. SAVE CORRECTED FORCE-CURVE FIGURES BY CONDITION --------------------

corrected_curve_dir <- file.path(output_dir, "force_curves_corrected")
dir.create(corrected_curve_dir, showWarnings = FALSE)

conditions <- results %>%
  distinct(weight_g, speed_cm_s) %>%
  arrange(weight_g, speed_cm_s)

for (i in seq_len(nrow(conditions))) {
  w <- conditions$weight_g[i]
  s <- conditions$speed_cm_s[i]
  
  condition_rows <- results %>%
    filter(weight_g == w, speed_cm_s == s) %>%
    arrange(rep_num)
  
  cat(sprintf("  %dg, %.1f cm/s: %d replicates\n", w, s, nrow(condition_rows)))
  
  plots <- map2(
    condition_rows$filename,
    condition_rows$alpha_session,
    ~ plot_one_rep_corrected(.x, .y, y_max = y_max_corr)
  )
  
  combined <- wrap_plots(plots, ncol = 2) +
    plot_annotation(
      title = paste0(w, " g, ", s, " cm/s"),
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )
  
  fname_out <- sprintf("force_curves_corrected_%dg_%.0fcms.png", w, s * 10)
  
  ggsave(
    file.path(corrected_curve_dir, fname_out),
    combined,
    width = 8,
    height = 12,
    dpi = 600
  )
}

# ---- 6. SUMMARY STATISTICS --------------------------------------------------

summary_stats <- results %>%
  group_by(weight_f, speed_f) %>%
  summarise(
    n           = n(),
    mu_s_mean   = mean(mu_s,  na.rm = TRUE),
    mu_s_sd     = sd(mu_s,    na.rm = TRUE),
    U_mu_s_mean = mean(U_mu_s, na.rm = TRUE),
    mu_d_mean   = mean(mu_d,  na.rm = TRUE),
    mu_d_sd     = sd(mu_d,    na.rm = TRUE),
    U_mu_d_mean = mean(U_mu_d, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== SUMMARY (mean ± SD, mean expanded U at k = 2) ===\n")
print(summary_stats, width = Inf)
write_csv(summary_stats, file.path(output_dir, "summary_corrected.csv"))

# ---- 7. STATISTICAL ANALYSIS --------------------------------------------------
cat("\n==============================\n")
cat("STATISTICAL ANALYSIS\n")
cat("==============================\n")

# Ensure factors are ordered as intended
results <- results %>%
  mutate(
    weight_f = factor(weight_f, levels = c("200", "500", "800")),
    speed_f  = factor(speed_f,  levels = c("1", "1.5", "2"))
  )

# ------------------------------------------------------------------------------
# 7.1 DESCRIPTIVE SUMMARIES
# ------------------------------------------------------------------------------

cat("\n=== DESCRIPTIVE SUMMARIES ===\n")

summary_mu_s_weight <- results %>%
  group_by(weight_f) %>%
  summarise(
    n = n(),
    mean_mu_s   = mean(mu_s, na.rm = TRUE),
    sd_mu_s     = sd(mu_s, na.rm = TRUE),
    median_mu_s = median(mu_s, na.rm = TRUE),
    IQR_mu_s    = IQR(mu_s, na.rm = TRUE),
    .groups = "drop"
  )

summary_mu_s_speed <- results %>%
  group_by(speed_f) %>%
  summarise(
    n = n(),
    mean_mu_s   = mean(mu_s, na.rm = TRUE),
    sd_mu_s     = sd(mu_s, na.rm = TRUE),
    median_mu_s = median(mu_s, na.rm = TRUE),
    IQR_mu_s    = IQR(mu_s, na.rm = TRUE),
    .groups = "drop"
  )

summary_mu_d_weight <- results %>%
  group_by(weight_f) %>%
  summarise(
    n = n(),
    mean_mu_d   = mean(mu_d, na.rm = TRUE),
    sd_mu_d     = sd(mu_d, na.rm = TRUE),
    median_mu_d = median(mu_d, na.rm = TRUE),
    IQR_mu_d    = IQR(mu_d, na.rm = TRUE),
    .groups = "drop"
  )

summary_mu_d_speed <- results %>%
  group_by(speed_f) %>%
  summarise(
    n = n(),
    mean_mu_d   = mean(mu_d, na.rm = TRUE),
    sd_mu_d     = sd(mu_d, na.rm = TRUE),
    median_mu_d = median(mu_d, na.rm = TRUE),
    IQR_mu_d    = IQR(mu_d, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- mu_s by weight ---\n")
print(summary_mu_s_weight)

cat("\n--- mu_s by speed ---\n")
print(summary_mu_s_speed)

cat("\n--- mu_d by weight ---\n")
print(summary_mu_d_weight)

cat("\n--- mu_d by speed ---\n")
print(summary_mu_d_speed)

# ------------------------------------------------------------------------------
# 7.2 KRUSKAL-WALLIS TESTS
# ------------------------------------------------------------------------------

cat("\n=== KRUSKAL-WALLIS: mu_s by weight ===\n")
kw_mu_s_weight <- kruskal.test(mu_s ~ weight_f, data = results)
print(kw_mu_s_weight)

cat("\n=== KRUSKAL-WALLIS: mu_s by speed ===\n")
kw_mu_s_speed <- kruskal.test(mu_s ~ speed_f, data = results)
print(kw_mu_s_speed)

cat("\n=== KRUSKAL-WALLIS: mu_d by weight ===\n")
kw_mu_d_weight <- kruskal.test(mu_d ~ weight_f, data = results)
print(kw_mu_d_weight)

cat("\n=== KRUSKAL-WALLIS: mu_d by speed ===\n")
kw_mu_d_speed <- kruskal.test(mu_d ~ speed_f, data = results)
print(kw_mu_d_speed)

# ------------------------------------------------------------------------------
# 7.3 DUNN POST-HOC TESTS (ONLY IF OMNIBUS TEST IS SIGNIFICANT)
# ------------------------------------------------------------------------------

cat("\n=== DUNN POST-HOC TESTS (Holm adjustment) ===\n")

dunn_mu_s_weight <- NULL
dunn_mu_s_speed  <- NULL
dunn_mu_d_weight <- NULL
dunn_mu_d_speed  <- NULL

if (kw_mu_s_weight$p.value < 0.05) {
  cat("\n--- mu_s by weight ---\n")
  dunn_mu_s_weight <- dunnTest(mu_s ~ weight_f, data = results, method = "holm")
  print(dunn_mu_s_weight)
} else {
  cat("\n--- mu_s by weight: omnibus test not significant, no post-hoc performed ---\n")
}

if (kw_mu_s_speed$p.value < 0.05) {
  cat("\n--- mu_s by speed ---\n")
  dunn_mu_s_speed <- dunnTest(mu_s ~ speed_f, data = results, method = "holm")
  print(dunn_mu_s_speed)
} else {
  cat("\n--- mu_s by speed: omnibus test not significant, no post-hoc performed ---\n")
}

if (kw_mu_d_weight$p.value < 0.05) {
  cat("\n--- mu_d by weight ---\n")
  dunn_mu_d_weight <- dunnTest(mu_d ~ weight_f, data = results, method = "holm")
  print(dunn_mu_d_weight)
} else {
  cat("\n--- mu_d by weight: omnibus test not significant, no post-hoc performed ---\n")
}

if (kw_mu_d_speed$p.value < 0.05) {
  cat("\n--- mu_d by speed ---\n")
  dunn_mu_d_speed <- dunnTest(mu_d ~ speed_f, data = results, method = "holm")
  print(dunn_mu_d_speed)
} else {
  cat("\n--- mu_d by speed: omnibus test not significant, no post-hoc performed ---\n")
}

# ------------------------------------------------------------------------------
# 7.4 SAVE TABLES
# ------------------------------------------------------------------------------

kw_table <- data.frame(
  response = c("mu_s", "mu_s", "mu_d", "mu_d"),
  factor   = c("weight", "speed", "weight", "speed"),
  chi_sq   = c(
    unname(kw_mu_s_weight$statistic),
    unname(kw_mu_s_speed$statistic),
    unname(kw_mu_d_weight$statistic),
    unname(kw_mu_d_speed$statistic)
  ),
  df = c(
    unname(kw_mu_s_weight$parameter),
    unname(kw_mu_s_speed$parameter),
    unname(kw_mu_d_weight$parameter),
    unname(kw_mu_d_speed$parameter)
  ),
  p_value = c(
    kw_mu_s_weight$p.value,
    kw_mu_s_speed$p.value,
    kw_mu_d_weight$p.value,
    kw_mu_d_speed$p.value
  )
)

cat("\n=== KRUSKAL-WALLIS SUMMARY TABLE ===\n")
print(kw_table)

# Convert Dunn outputs to data frames if they exist
dunn_mu_s_weight_table <- if (!is.null(dunn_mu_s_weight)) dunn_mu_s_weight$res else NULL
dunn_mu_s_speed_table  <- if (!is.null(dunn_mu_s_speed))  dunn_mu_s_speed$res  else NULL
dunn_mu_d_weight_table <- if (!is.null(dunn_mu_d_weight)) dunn_mu_d_weight$res else NULL
dunn_mu_d_speed_table  <- if (!is.null(dunn_mu_d_speed))  dunn_mu_d_speed$res  else NULL

# Uncomment if needed
# write.csv(summary_mu_s_weight, "summary_mu_s_weight.csv", row.names = FALSE)
# write.csv(summary_mu_s_speed,  "summary_mu_s_speed.csv",  row.names = FALSE)
# write.csv(summary_mu_d_weight, "summary_mu_d_weight.csv", row.names = FALSE)
# write.csv(summary_mu_d_speed,  "summary_mu_d_speed.csv",  row.names = FALSE)
# write.csv(kw_table,            "kruskal_wallis_summary.csv", row.names = FALSE)
# if (!is.null(dunn_mu_s_weight_table)) write.csv(dunn_mu_s_weight_table, "dunn_mu_s_weight.csv", row.names = FALSE)
# if (!is.null(dunn_mu_s_speed_table))  write.csv(dunn_mu_s_speed_table,  "dunn_mu_s_speed.csv",  row.names = FALSE)
# if (!is.null(dunn_mu_d_weight_table)) write.csv(dunn_mu_d_weight_table, "dunn_mu_d_weight.csv", row.names = FALSE)
# if (!is.null(dunn_mu_d_speed_table))  write.csv(dunn_mu_d_speed_table,  "dunn_mu_d_speed.csv",  row.names = FALSE)

cat("\n==============================\n")
cat("END OF STATISTICAL ANALYSIS\n")
cat("==============================\n")

# ---- 8. FIGURES -------------------------------------------------------------

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
  theme_bw(base_size = 16) +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2")

# -- Boxplot: mu_d by weight and speed ----------------------------------------
p_dynamic <- ggplot(results, aes(x = weight_f, y = mu_d, fill = speed_f)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(aes(colour = speed_f),
              position = position_jitterdodge(jitter.width = 0.15),
              size = 1.5, alpha = 0.6) +
  labs(x = "Weight (g)",
       y = expression(mu[d] ~ ""),
       fill   = "Speed (cm/s)",
       colour = "Speed (cm/s)") +
  coord_cartesian(ylim = mu_ylim) +
  theme_bw(base_size = 16) +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2")

# -- Combined figure (paper) --------------------------------------------------
p_combined_pending <- ggarrange(p_static + labs(x = NULL), 
                                p_dynamic + labs(x = NULL),
                                common.legend = TRUE,
                                legend        = "right",
                                ncol          = 1, nrow = 2,
                                align         = "hv") +
  theme(legend.key.size  = unit(0.4, "cm"),
        legend.text       = element_text(size = 10),
        legend.title      = element_text(size = 10))

p_combined <- annotate_figure(p_combined_pending,
                              bottom = textGrob("Weight (g)",
                                                gp = gpar(cex = 1.35)))
p_combined

ggsave(file.path(output_dir, "boxplot_mu_combined.png"),
       p_combined, width = 7, height = 8, dpi = 600)

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

cat("\n=== 3_Transfer_Analysis_corrected.R complete ===\n")
cat("All outputs saved to:", output_dir, "\n")