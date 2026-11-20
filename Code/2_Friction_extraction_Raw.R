# =============================================================================
# 2_Friction_extraction_Raw.R — Raw Friction Extraction & Diagnostic Plots
# =============================================================================
#
# PIPELINE B — Step 2 of 3. Must be run after 1B_Session_Calibration_for_Transfer.R.
#
# Purpose:
#   Extracts raw static and dynamic friction forces from all TARTAN experiment
#   CSV files. Also generates annotated raw force-curve figures for visual
#   inspection and quality control of each replicate.
#
# Method:
#   Static friction  : first local maximum of the force signal during motion,
#                      detected using sign change of the second difference
#                      (diff(sign(diff(forces))) == -2). Falls back to global
#                      maximum if no local peak is detected.
#   Dynamic friction : mean of the plateau after the static peak.
#
#   IMPORTANT:
#   All quantities extracted in this script are based on the raw force_g
#   values. No session-specific alpha correction is applied here.
#   The definitive corrected forces, corrected friction coefficients, and
#   publication-ready figures are generated later in 3_Transfer_Analysis_corrected.R.
#
# Input files (in data_dir = Data/COF/):
#   YYYYMMDD_HHMMSS_[weight]g_Rep[n]_run[speed].csv
#   Columns: date, time, speed_cm_s, steps, force_g
#
#   Calibration check files (before/after) in the same folder are automatically
#   excluded from processing by filename pattern.
#
# Outputs (in output_dir = Results/Session-Specific_Calibration_Check/):
#   all_results_uncorrected.csv         — raw extracted friction forces and
#                                         preliminary friction coefficients
#                                         (diagnostic only; no alpha correction)
#   example_force_curve_uncorrected.png — raw single example force curve (QC)
#   force_curves_uncorrected/           — raw replicate-level force-curve
#                                         figures for diagnostic inspection
#
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------

data_dir   <- "Data/COF/"
output_dir <- "Results/Session-Specific_Calibration_Check/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Actual measured masses of the transfer weights (precision balance)
# Used here only to compute preliminary raw mu values for diagnostics/QC.
# Definitive corrected forces and friction coefficients are computed later
# in 3_Transfer_Analysis_corrected.R after application of alpha_session.
M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

# ---- 1. HELPERS -------------------------------------------------------------

# Extract weight, replicate, and speed from TARTAN filename convention:
# YYYYMMDD_HHMMSS_[weight]g_Rep[n]_run[speed].csv
parse_filename <- function(filepath) {
  fname      <- basename(filepath)
  date_str   <- substr(fname, 1, 8)
  weight_g   <- as.numeric(stringr::str_extract(fname, "\\d+(?=g)"))
  rep_num    <- as.numeric(stringr::str_extract(fname, "(?<=Rep)\\d+"))
  run_label  <- stringr::str_extract(fname, "run[0-9]+(-[0-9]+)?")
  speed_cm_s <- dplyr::case_when(
    run_label == "run1"   ~ 1.0,
    run_label == "run1-5" ~ 1.5,
    run_label == "run2"   ~ 2.0,
    TRUE                  ~ NA_real_
  )
  tibble(filename      = fname,
         date_str      = date_str,
         weight_g      = weight_g,
         rep_num       = rep_num,
         speed_cm_s    = speed_cm_s)
}

# Extract static and dynamic friction from a single experiment file.
# force_g == 0 readings are treated as NA (HX711 hardware artefacts).
extract_friction <- function(filepath) {
  df <- read_csv(filepath, show_col_types = FALSE) %>%
    mutate(force_g = na_if(force_g, 0))
  
  motion <- df %>% filter(steps > 0, !is.na(force_g))
  
  if (nrow(motion) < 5) {
    warning(paste("Too few motion samples in", basename(filepath)))
    return(tibble(force_static_g = NA_real_, force_dynamic_g = NA_real_,
                  n_motion_samples = nrow(motion), n_dynamic_samples = 0L))
  }
  
  forces   <- motion$force_g
  n_motion <- length(forces)
  
  # Static peak: first local maximum (sign change of second difference)
  # that exceeds a minimum threshold (mean of the full motion signal).
  # This filters out spurious noise-induced micro-peaks at the start of motion
  # before the true static peak has built up.
  local_max_idx  <- which(diff(sign(diff(forces))) == -2) + 1
  threshold_stat <- mean(forces, na.rm = TRUE)
  valid_max_idx  <- local_max_idx[forces[local_max_idx] > threshold_stat]
  peak_idx       <- if (length(valid_max_idx) > 0) valid_max_idx[1] else which.max(forces)
  force_static_g <- forces[peak_idx]
  
  # Dynamic: mean of plateau after the static peak
  if (peak_idx < n_motion) {
    dynamic_forces  <- forces[(peak_idx + 1):n_motion]
    force_dynamic_g <- mean(dynamic_forces, na.rm = TRUE)
    n_dynamic       <- length(dynamic_forces)
  } else {
    force_dynamic_g <- NA_real_
    n_dynamic       <- 0L
  }
  
  tibble(force_static_g   = force_static_g,
         force_dynamic_g  = force_dynamic_g,
         n_motion_samples = n_motion,
         n_dynamic_samples = n_dynamic)
}

# ---- 2. FIND AND PROCESS EXPERIMENT FILES -----------------------------------
# Calibration check files (before/after) share the same folder but are
# excluded by pattern matching on their filename.

csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
csv_files <- csv_files[!grepl("Calibration", basename(csv_files), ignore.case = TRUE)]
cat(sprintf("Found %d experiment CSV files.\n", length(csv_files)))

results <- map_dfr(csv_files, function(fp) {
  meta    <- parse_filename(fp)
  friction <- extract_friction(fp)
  bind_cols(meta, friction)
})

# Preliminary uncorrected mu (for diagnostics — alpha correction in Script 3)
results <- results %>%
  mutate(
    M_actual         = M_transfer[as.character(weight_g)],
    mu_s_uncorrected = force_static_g  / M_actual,
    mu_d_uncorrected = force_dynamic_g / M_actual
  )

cat(sprintf("Processed %d files.\n\n", nrow(results)))
cat("Results preview (first 15 rows, uncorrected mu):\n")
print(results %>%
        dplyr::select(weight_g, speed_cm_s, rep_num,
                      force_static_g, force_dynamic_g,
                      mu_s_uncorrected, mu_d_uncorrected) %>%
        head(15),
      width = Inf)

write_csv(results, file.path(output_dir, "all_results_uncorrected.csv"))

# ---- 3. FORCE CURVE PLOT FUNCTION -------------------------------------------

# Computes global y-axis maximum across all files for consistent scaling
all_forces <- map_dbl(results$filename, function(fname) {
  fp <- file.path(data_dir, fname)
  df <- suppressWarnings(read_csv(fp, show_col_types = FALSE))
  max(df$force_g, na.rm = TRUE)
})
y_max <- ceiling(max(all_forces, na.rm = TRUE) * 1.15)
cat(sprintf("Global y-axis max set to %.1f g\n", y_max))

plot_one_rep <- function(fname, y_max) {
  fp <- file.path(data_dir, fname)
  df <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(sample  = row_number(),
           force_g = na_if(force_g, 0))
  
  motion <- df %>% filter(steps > 0, !is.na(force_g))
  forces <- motion$force_g
  
  if (length(forces) < 5)
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "Too few samples") +
             theme_void())
  
  local_max_idx  <- which(diff(sign(diff(forces))) == -2) + 1
  threshold_stat <- mean(forces, na.rm = TRUE)
  valid_max_idx  <- local_max_idx[forces[local_max_idx] > threshold_stat]
  peak_idx       <- if (length(valid_max_idx) > 0) valid_max_idx[1] else which.max(forces)
  fstat          <- forces[peak_idx]
  peak_sample   <- motion$sample[peak_idx]
  
  if (peak_idx < length(forces)) {
    fdyn      <- mean(forces[(peak_idx + 1):length(forces)], na.rm = TRUE)
    dyn_start <- motion$sample[peak_idx + 1]
    dyn_end   <- max(motion$sample, na.rm = TRUE)
  } else {
    fdyn <- dyn_start <- dyn_end <- NA_real_
  }
  
  y_fs_label <- min(fstat + 0.05 * y_max, y_max * 0.88)
  y_fd_label <- if (!is.na(fdyn)) max(fdyn - 0.08 * y_max, y_max * 0.10) else NA_real_
  
  p <- ggplot(df, aes(x = sample, y = force_g)) +
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
  
  if (!is.na(fdyn))
    p <- p +
    geom_segment(x = dyn_start, xend = dyn_end, y = fdyn, yend = fdyn,
                 colour = "red", linetype = "dotted", linewidth = 0.4) +
    annotate("text", x = (dyn_start + dyn_end) / 2, y = y_fd_label,
             label = paste0("F[dyn] == ", round(fdyn, 1), " ~ g"),
             size = 3.2, parse = TRUE)
  p
}

# ---- 4. PAPER EXAMPLE: SINGLE FORCE CURVE -----------------------------------

example_row <- results %>%
  filter(weight_g == 500, speed_cm_s == 1.5) %>%
  arrange(rep_num) %>%
  slice(1)

p_example <- plot_one_rep(example_row$filename, y_max) +
  labs(title = paste0(example_row$weight_g, " g, ",
                      example_row$speed_cm_s, " cm/s"),
       x = "Sample number", y = "Friction force (g)") +   # x is row_number(), not seconds
  theme(plot.title = element_text(size = 11, hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 10))

print(p_example)
ggsave(file.path(output_dir, "example_force_curve_uncorrected.png"),
       p_example, width = 15, height = 10, units = "cm", dpi = 600)

# ---- 5. FULL DIAGNOSTIC: 9 FIGURES (one per condition, all replicates) ------

force_curves_dir <- file.path(output_dir, "force_curves_uncorrected")
dir.create(force_curves_dir, showWarnings = FALSE)

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
  plots    <- map(condition_files, plot_one_rep, y_max = y_max)
  combined <- wrap_plots(plots, ncol = 2) +
    plot_annotation(title = paste0(w, " g, ", s, " cm/s"),
                    theme = theme(plot.title = element_text(size = 13, face = "bold")))
  fname_out <- sprintf("force_curves_%dg_%.0fcms.png", w, s * 10)
  ggsave(file.path(force_curves_dir, fname_out),
         combined, width = 8, height = 12, dpi = 600)
}

cat(sprintf("\n=== 2_Friction_extraction_Raw.R complete ===\n"))
cat(sprintf("all_results_uncorrected.csv written with %d rows.\n", nrow(results)))
cat(sprintf("Force curve figures saved to %s\n", force_curves_dir))