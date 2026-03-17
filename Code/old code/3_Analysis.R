# =============================================================================
# 03_analysis.R — Correction, Uncertainty Propagation & Statistical Analysis
# =============================================================================
# Applies the calibration correction (alpha) from 01_calibration.R to the
# friction data from 02_friction_extraction.R. Propagates all uncertainties
# (GUM framework, k=2 for 95% CI). Runs ANOVA + Tukey HSD and produces
# publication figures.
#
# Inputs:  Results/calibration_parameters.csv, Results/all_results.csv
# Outputs: Corrected results, summary tables, boxplots, ANOVA
#
# Run AFTER 01_calibration.R and 02_friction_extraction.R
# =============================================================================
# ---- CONFIGURATION -----------------------------------------------------------
# This script reads ONLY from the Results folder (outputs of scripts 01 and 02):
#   - Results/calibration_parameters.csv  (from 01_calibration.R)
#   - Results/all_results.csv             (from 02_friction_extraction.R)
#   - Results/session_calibration_summary.csv (from 02, if available)

output_dir <- "Results"

# ---- 1. LOAD CALIBRATION PARAMETERS AND RESULTS -----------------------------

cat("=== LOADING DATA ===\n\n")

params <- read_csv(file.path(output_dir, "calibration_parameters.csv"),
                   show_col_types = FALSE) %>%
  deframe()  # converts to named vector

alpha_use       <- unname(params["alpha"])
sigma_alpha     <- unname(params["sigma_alpha"])
sigma_mech      <- unname(params["sigma_mechanical"])
U_balance       <- unname(params["U_balance"])
M_transfer      <- c("200" = unname(params["M_transfer_200"]),
                     "500" = unname(params["M_transfer_500"]),
                     "800" = unname(params["M_transfer_800"]))

# Estimate U_Fstat from session reference readings
# (if session data exists, use it; otherwise fall back to sigma_mechanical)
session_file <- list.files(output_dir,
                           pattern = "session_calibration_summary",
                           full.names = TRUE)
if (length(session_file) > 0) {
  session_data <- read_csv(session_file[1], show_col_types = FALSE)
  # Use first non-zero weight from "Before" as reference
  ref <- session_data %>%
    filter(timing == "Before", target_g > 0) %>%
    slice(1)
  sigma_X_combined <- sqrt(ref$sd_g^2 + sigma_mech^2)
  U_Fstat_est <- sigma_X_combined / sqrt(ref$n)
} else {
  # Fallback: use sigma_mechanical as estimate
  U_Fstat_est <- sigma_mech
}

cat(sprintf("alpha = %.8f\n", alpha_use))
cat(sprintf("sigma_alpha = %.8f\n", sigma_alpha))
cat(sprintf("U_Fstat (estimated) = %.4f g\n", U_Fstat_est))
cat(sprintf("Transfer masses: 200g=%.2f, 500g=%.2f, 800g=%.2f\n\n",
            M_transfer[["200"]], M_transfer[["500"]], M_transfer[["800"]]))

results <- read_csv(file.path(output_dir, "all_results.csv"),
                    show_col_types = FALSE)
cat(sprintf("Loaded %d friction results.\n\n", nrow(results)))

# ---- 2. APPLY CORRECTION & PROPAGATE UNCERTAINTIES --------------------------
# F_corrected = F_raw × alpha
# mu = F_corrected / M_transfer
# Uncertainties propagated following GUM (Galais 2024, Appendix 4)

results <- results %>%
  mutate(
    # Corrected forces
    Fstat_corrected = force_static_g * alpha_use,
    Fdyn_corrected  = force_dynamic_g * alpha_use,
    
    # U(Fstat_corrected) / Fstat_corrected = sqrt((U_F/F)^2 + (sigma_alpha/alpha)^2)
    U_Fstat_corrected = Fstat_corrected * sqrt(
      (U_Fstat_est / force_static_g)^2 + (sigma_alpha / alpha_use)^2
    ),
    U_Fdyn_corrected = Fdyn_corrected * sqrt(
      (U_Fstat_est / force_dynamic_g)^2 + (sigma_alpha / alpha_use)^2
    ),
    
    # Actual transfer mass
    M_actual = M_transfer[as.character(weight_g)],
    U_M = U_balance,
    
    # Static coefficient of friction
    mu_s = Fstat_corrected / M_actual,
    U_mu_s = mu_s * sqrt(
      (U_Fstat_corrected / Fstat_corrected)^2 + (U_M / M_actual)^2
    ),
    U_mu_s_expanded = 2 * U_mu_s,  # k=2 for 95% CI
    
    # Dynamic coefficient of friction
    mu_d = Fdyn_corrected / M_actual,
    U_mu_d = mu_d * sqrt(
      (U_Fdyn_corrected / Fdyn_corrected)^2 + (U_M / M_actual)^2
    ),
    U_mu_d_expanded = 2 * U_mu_d,
    
    # Factors for plotting/ANOVA
    weight_f = factor(weight_g, levels = c(200, 500, 800)),
    speed_f  = factor(speed_cm_s, levels = c(1.0, 1.5, 2.0))
  )

cat("Corrected results (first 10 rows):\n")
print(results %>%
        dplyr::select(filename, weight_g, speed_cm_s,
                      Fstat_corrected, U_Fstat_corrected,
                      mu_s, U_mu_s_expanded,
                      mu_d, U_mu_d_expanded) %>%
        head(10),
      width = Inf)

write_csv(results, file.path(output_dir, "all_results_corrected.csv"))

# ---- 3. SUMMARY STATISTICS --------------------------------------------------

summary_stats <- results %>%
  group_by(weight_f, speed_f) %>%
  summarise(
    n = n(),
    mu_s_mean      = mean(mu_s, na.rm = TRUE),
    mu_s_sd        = sd(mu_s, na.rm = TRUE),
    mu_s_U_mean    = mean(U_mu_s_expanded, na.rm = TRUE),
    mu_d_mean      = mean(mu_d, na.rm = TRUE),
    mu_d_sd        = sd(mu_d, na.rm = TRUE),
    mu_d_U_mean    = mean(U_mu_d_expanded, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== SUMMARY (mean +/- SD, mean expanded U at k=2) ===\n")
print(summary_stats, width = Inf)
write_csv(summary_stats, file.path(output_dir, "summary_corrected.csv"))

# ---- 4. STATISTICAL ANALYSIS -----------------------------------------------
# 1. FIT MODELS
aov_static  <- aov(mu_s ~ weight_f * speed_f, data = results)
aov_dynamic <- aov(mu_d ~ weight_f * speed_f, data = results)

# 2. NORMALITY OF RESIDUALS
cat("=== Shapiro-Wilk: mu_s residuals ===\n")
print(shapiro.test(residuals(aov_static)))

cat("\n=== Shapiro-Wilk: mu_d residuals ===\n")
print(shapiro.test(residuals(aov_dynamic)))

# Q-Q plots
par(mfrow = c(1, 2))
qqnorm(residuals(aov_static),  main = "Q-Q plot: mu_s residuals"); qqline(residuals(aov_static))
qqnorm(residuals(aov_dynamic), main = "Q-Q plot: mu_d residuals"); qqline(residuals(aov_dynamic))
par(mfrow = c(1, 1))

# 3. HOMOGENEITY OF VARIANCES (LEVENE'S TEST)
cat("\n=== Levene's test: mu_s ===\n")
print(leveneTest(mu_s ~ weight_f * speed_f, data = results))

cat("\n=== Levene's test: mu_d ===\n")
print(leveneTest(mu_d ~ weight_f * speed_f, data = results))

# 4. IF ASSUMPTIONS ARE VIOLATED: ART ANOVA
art_static  <- art(mu_s ~ weight_f * speed_f, data = results)
art_dynamic <- art(mu_d ~ weight_f * speed_f, data = results)

cat("\n=== ART ANOVA: mu_s ===\n")
print(anova(art_static))

cat("\n=== ART ANOVA: mu_d ===\n")
print(anova(art_dynamic))

# Post-hoc for ART (if needed)
# install.packages("emmeans")
library(emmeans)

cat("\n=== ART post-hoc: mu_s ~ weight_f ===\n")
print(contrast(emmeans(artlm(art_static, "weight_f"), ~ weight_f), method = "pairwise", adjust = "tukey"))

cat("\n=== ART post-hoc: mu_d ~ weight_f ===\n")
print(contrast(emmeans(artlm(art_dynamic, "weight_f"), ~ weight_f), method = "pairwise", adjust = "tukey"))

cat("\n=== ART post-hoc: mu_d ~ speed_f ===\n")
print(contrast(emmeans(artlm(art_dynamic, "speed_f"), ~ speed_f), method = "pairwise", adjust = "tukey"))


# ---- 5. FIGURES --------------------------------------------------------------
# Shared y-axis range across mu_s and mu_d
mu_range <- range(c(results$mu_s, results$mu_d), na.rm = TRUE)
mu_padding <- diff(mu_range) * 0.05
mu_ylim <- c(mu_range[1] - mu_padding, mu_range[2] + mu_padding)

# Boxplot: Static coefficient by weight and speed
p_static <- ggplot(results, aes(x = weight_f, y = mu_s, fill = speed_f)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(aes(colour = speed_f),
              position = position_jitterdodge(jitter.width = 0.15),
              size = 1.5, alpha = 0.6) +
  labs(x = NULL,
       y = expression(mu[s]~""),
       fill = "Speed (cm/s)", colour = "Speed (cm/s)") +
  coord_cartesian(ylim = mu_ylim) +
  theme_bw(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2")
p_static

# Boxplot: Dynamic coefficient by weight and speed
p_dynamic <- ggplot(results, aes(x = weight_f, y = mu_d, fill = speed_f)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(aes(colour = speed_f),
              position = position_jitterdodge(jitter.width = 0.15),
              size = 1.5, alpha = 0.6) +
  labs(x = "Weight (g)",
       y = expression(mu[d]~""),
       fill = "Speed (cm/s)", colour = "Speed (cm/s)") +
  coord_cartesian(ylim = mu_ylim) +
  theme_bw(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2")
p_dynamic

# Combined: two rows, shared y scale, shared x label, single legend
p_static_noleg <- p_static + theme(legend.position = "none")

p_dynamic_leg  <- p_dynamic + theme(legend.position = "bottom")

p_combined <- p_static_noleg + p_dynamic_leg +
  plot_layout(nrow = 2)

ggsave(file.path(output_dir, "boxplot_mu_combined.png"), p_combined,
       width = 8, height = 10, dpi = 300)

# Scatter: Static vs Dynamic
p_scatter <- ggplot(results, aes(x = mu_d, y = mu_s,
                                 colour = weight_f, shape = speed_f)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  labs(x = expression(mu[d]), y = expression(mu[s]),
       colour = "Weight (g)", shape = "Speed (cm/s)") +
  theme_bw(base_size = 13) +
  scale_colour_brewer(palette = "Set2") +
  coord_equal()
p_scatter

ggsave(file.path(output_dir, "scatter_static_vs_dynamic.png"), p_scatter,
       width = 7, height = 6, dpi = 600)

cat("\n=== 03_analysis.R complete ===\n")
cat("All outputs in:", output_dir, "\n")

# =============================================================================
# Generates 9 figures (3 weights × 3 speeds). Each figure contains 10
# annotated force curve panels (one per replicate) with Fstat and Fdyn marked.
# =============================================================================
# ---- CONFIGURATION -----------------------------------------------------------

data_dir   <- "Data/COF/"

output_dir <- "Results/force_curves"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

results_file <- "Results/all_results.csv"

# ---- LOAD RESULTS -----------------------------------------------------------

results <- read_csv(results_file, show_col_types = FALSE)
cat(sprintf("Loaded %d results.\n\n", nrow(results)))

# ---- GLOBAL Y LIMITS (same scale for all panels) -----------------------------

all_files <- results$filename

all_forces <- map_dbl(all_files, function(fname) {
  fp <- file.path(data_dir, fname)
  df <- read_csv(fp, show_col_types = FALSE)
  max(df$force_g, na.rm = TRUE)
})

y_max <- ceiling(max(all_forces) * 1.15)  # add ~15% headroom
cat(sprintf("Global y-axis max set to %.1f g\n", y_max))

# ---- PLOT FUNCTION (single replicate panel) ----------------------------------

plot_one_rep <- function(fname, data_dir, y_max) {
  fp <- file.path(data_dir, fname)
  
  df <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      sample  = row_number(),
      force_g = na_if(force_g, 0)
    )
  
  motion <- df %>%
    filter(steps > 0, !is.na(force_g))
  
  forces <- motion$force_g
  
  if (length(forces) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Too few samples") +
        theme_void()
    )
  }
  
  peak_idx    <- which.max(forces)
  fstat       <- forces[peak_idx]
  peak_sample <- motion$sample[peak_idx]
  
  if (peak_idx < length(forces)) {
    fdyn      <- mean(forces[(peak_idx + 1):length(forces)], na.rm = TRUE)
    dyn_start <- motion$sample[peak_idx + 1]
    dyn_end   <- max(motion$sample, na.rm = TRUE)
  } else {
    fdyn      <- NA_real_
    dyn_start <- NA_real_
    dyn_end   <- NA_real_
  }
  
  y_fs_label <- min(fstat + 0.05 * y_max, y_max * 0.88)
  y_fd_label <- if (!is.na(fdyn)) max(fdyn - 0.08 * y_max, y_max * 0.10) else NA_real_
  
  p <- ggplot(df, aes(x = sample, y = force_g)) +
    geom_line(colour = "black", linewidth = 0.35, na.rm = TRUE) +
    geom_vline(xintercept = peak_sample, linetype   = "dashed",
      colour     = "orange", linewidth  = 0.4) +
    annotate(
      "text",
      x      = peak_sample + 2,
      y      = y_fs_label,
      label  = paste0("F[stat] == ", round(fstat, 1), " ~ g"),
      hjust  = 0,
      size   = 3.2,
      parse  = TRUE
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    coord_cartesian(ylim = c(0, y_max)) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_line(linewidth = 0.2),
      panel.grid.major = element_line(linewidth = 0.25),
      axis.title       = element_blank(),
      axis.text        = element_text(size = 8),
      axis.text.x      = element_text(margin = margin(t = 1)),
      axis.text.y      = element_text(margin = margin(r = 1)),
      plot.margin      = margin(1, 1, 1, 1)
    )
  
  if (!is.na(fdyn)) {
    p <- p +geom_segment(x = dyn_start, xend = dyn_end, y = fdyn, yend = fdyn,
        colour = "red", linetype = "dotted", linewidth = 0.4) +
      annotate(
        "text",
        x     = (dyn_start + dyn_end) / 2,
        y     = y_fd_label,
        label = paste0("F[dyn] == ", round(fdyn, 1), " ~ g"),
        size  = 3.2,
        parse = TRUE
      )
  }
  
  p
}

# ---- EXAMPLE FORCE CURVE: single panel --------------------------------------

# Choose one example condition and one replicate
example_row <- results %>%
  filter(weight_g == 500, speed_cm_s == 1.5) %>%   # change if needed
  arrange(rep_num) %>%
  slice(1)

example_file <- example_row$filename

example_plot <- plot_one_rep(example_file, data_dir = data_dir, y_max = y_max) +
  labs(
    title = paste0(example_row$weight_g, " g, ", example_row$speed_cm_s, " cm/s"),
    x = "Sample number",
    y = "Friction force (g)"
  ) +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 10)
  )

example_plot

ggsave(file.path("Results", "example_force_curve.png"),example_plot,
  width = 15,height = 10,units = "cm",dpi = 600)

# ---- GENERATE 9 FIGURES (one per condition) ----------------------------------

conditions <- results %>%
  distinct(weight_g, speed_cm_s) %>%
  arrange(weight_g, speed_cm_s)

for (i in seq_len(nrow(conditions))) {
  w <- conditions$weight_g[i]
  s <- conditions$speed_cm_s[i]
  
  condition_files <- results %>%
    filter(weight_g == w, speed_cm_s == s) %>%
    arrange(rep_num) %>%
    pull(filename)
  
  cat(sprintf("  %dg, %.1f cm/s: %d replicates\n", w, s, length(condition_files)))
  
  plots <- map(condition_files, plot_one_rep, data_dir = data_dir, y_max = y_max)
  
  combined <- wrap_plots(plots, ncol = 2) +
    plot_annotation(
      title = paste0(w, " g, ", s, " cm/s"),
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )
  
  fname_out <- sprintf("force_curves_%dg_%.0fcms.png", w, s * 10)
  ggsave(file.path(output_dir, fname_out), combined,
         width = 8, height = 12, dpi = 600)
}

cat(sprintf("\n=== Done: 9 figures saved to %s ===\n", output_dir))
