# =============================================================================
# 02_friction_extraction.R — Session Check & Friction Extraction
# =============================================================================
# Verifies load cell stability during the experiment session, then extracts
# static and dynamic friction from all 90 transfer experiment files.
#
# Inputs:  Session before/after CSVs, experiment run CSVs
# Outputs: all_results.csv, session drift check, example force curves
#
# Run AFTER 01_calibration.R
# =============================================================================

# ---- SHARED CONFIGURATION ---------------------------------------------------

data_dir   <- "Data/COF/"
output_dir <- "Results"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

balance_resolution <- 0.01
U_balance <- balance_resolution / sqrt(12)
M_transfer <- c("200" = 200.00, "500" = 498.85, "800" = 799.37)

# ---- 1. SESSION DRIFT CHECK (before vs after) -------------------------------

cat("=== SESSION DRIFT CHECK ===\n\n")

session_files_before <- list.files(data_dir,
                                   pattern = "CalibrationRaw_LoadCell_before\\.csv$", full.names = TRUE)
session_files_after <- list.files(data_dir,
                                  pattern = "CalibrationRaw_LoadCell_before_after\\.csv$", full.names = TRUE)

if (length(session_files_before) > 0 & length(session_files_after) > 0) {
  calib_before <- read_csv(session_files_before[1], show_col_types = FALSE) %>%
    mutate(timing = "Before")
  calib_after <- read_csv(session_files_after[1], show_col_types = FALSE) %>%
    mutate(timing = "After")
  
  calib_all <- bind_rows(calib_before, calib_after) %>%
    mutate(timing = factor(timing, levels = c("Before", "After")))
  
  # Summary per weight and timing
  calib_summary <- calib_all %>%
    group_by(timing, weight_index, target_g) %>%
    summarise(
      n = n(), mean_g = mean(force_g), sd_g = sd(force_g),
      error_g = mean(force_g) - first(target_g),
      .groups = "drop"
    )
  cat("Session calibration summary:\n")
  print(calib_summary, width = Inf)
  
  # Paired t-tests for drift
  calib_drift <- calib_all %>%
    filter(target_g > 0) %>%
    group_by(target_g) %>%
    group_modify(~ {
      b <- .x %>% filter(timing == "Before") %>% pull(force_g)
      a <- .x %>% filter(timing == "After")  %>% pull(force_g)
      tt <- t.test(b, a, paired = TRUE)
      tibble(mean_before = mean(b), mean_after = mean(a),
             drift_g = mean(a) - mean(b),
             t_stat = tt$statistic, p_value = tt$p.value)
    })
  cat("\nDrift tests:\n")
  print(calib_drift, width = Inf)
  
  max_drift <- max(abs(calib_drift$drift_g))
  cat(sprintf("Max absolute drift: %.3f g\n", max_drift))
  if (max_drift < 1.0) cat("Drift < 1g: session stable.\n\n")
  
  # Session check plot
  p_session <- ggplot(calib_all, aes(x = timing, y = force_g, fill = timing)) +
    geom_boxplot(alpha = 0.7, width = 0.5) +
    geom_hline(data = calib_all %>% distinct(target_g),
               aes(yintercept = target_g),
               linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    facet_wrap(~ paste0("Target: ", target_g, " g"), scales = "free_y") +
    labs(x = NULL, y = "Measured Force (g)", fill = "Timing") +
    theme_bw(base_size = 12) +
    scale_fill_manual(values = c("Before" = "#66c2a5", "After" = "#fc8d62"))
  ggsave(file.path(output_dir, "session_before_after.png"), p_session,
         width = 10, height = 6, dpi = 600)
  
  write_csv(calib_summary, file.path(output_dir, "session_calibration_summary.csv"))
  write_csv(calib_drift, file.path(output_dir, "session_drift_tests.csv"))
} else {
  cat("No session before/after files found. Skipping.\n\n")
}

# ---- 2. HELPERS: PARSE FILENAME & EXTRACT FRICTION ---------------------------

parse_filename <- function(filepath) {
  fname <- basename(filepath)
  weight_g <- as.numeric(str_extract(fname, "\\d+(?=g)"))
  rep_num  <- as.numeric(str_extract(fname, "(?<=Rep)\\d+"))
  run_label <- str_extract(fname, "run[0-9](-[0-9])?")
  speed_cm_s <- case_when(
    run_label == "run1"   ~ 1.0,
    run_label == "run1-5" ~ 1.5,
    run_label == "run2"   ~ 2.0,
    TRUE ~ NA_real_
  )
  tibble(filename = fname, weight_g = weight_g,
         rep_num = rep_num, speed_cm_s = speed_cm_s)
}

# Static friction: first local maximum after motion starts
# Dynamic friction: mean of plateau after the static peak
# (follows Galais 2024 thesis methodology)
extract_friction <- function(filepath) {
  df <- read_csv(filepath, show_col_types = FALSE) %>%
    mutate(force_g = na_if(force_g, 0))
  
  motion <- df %>% filter(steps > 0, !is.na(force_g))
  
  if (nrow(motion) < 5) {
    warning(paste("Too few motion samples in", basename(filepath)))
    return(tibble(force_static_g = NA_real_, force_dynamic_g = NA_real_,
                  n_motion_samples = nrow(motion), n_dynamic_samples = 0L))
  }
  
  forces <- motion$force_g
  n_motion <- length(forces)
  
  # Static peak = global maximum of the motion phase
  peak_idx       <- which.max(forces)
  force_static_g <- forces[peak_idx]
  
  # Dynamic = mean of plateau after the peak
  if (peak_idx < n_motion) {
    dynamic_forces    <- forces[(peak_idx + 1):n_motion]
    force_dynamic_g   <- mean(dynamic_forces, na.rm = TRUE)
    n_dynamic_samples <- length(dynamic_forces)
  } else {
    force_dynamic_g   <- NA_real_
    n_dynamic_samples <- 0L
  }
  
  tibble(force_static_g = force_static_g, force_dynamic_g = force_dynamic_g,
         n_motion_samples = n_motion, n_dynamic_samples = n_dynamic_samples)
}

# ---- 3. PROCESS ALL EXPERIMENT FILES ----------------------------------------

csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
csv_files <- csv_files[!grepl("Calibration", basename(csv_files), ignore.case = TRUE)]
cat(sprintf("Found %d experiment CSV files.\n", length(csv_files)))

results <- map_dfr(csv_files, function(fp) {
  meta <- parse_filename(fp)
  friction <- extract_friction(fp)
  bind_cols(meta, friction)
})

# Compute mu using actual masses
results <- results %>%
  mutate(
    weight_actual = M_transfer[as.character(weight_g)],
    mu_static     = force_static_g / weight_actual,
    mu_dynamic    = force_dynamic_g / weight_actual
  )

cat(sprintf("Processed %d files.\n\n", nrow(results)))
cat("Results (first 15 rows):\n")
print(results %>%
        dplyr::select(weight_g, speed_cm_s, rep_num,
               force_static_g, force_dynamic_g, mu_static, mu_dynamic) %>%
        head(15),
      width = Inf)

# Save
write_csv(results, file.path(output_dir, "all_results.csv"))

# ---- 4. EXAMPLE FORCE CURVES ------------------------------------------------

example_files <- results %>%
  mutate(weight_g_f = factor(weight_g), speed_cm_s_f = factor(speed_cm_s)) %>%
  group_by(weight_g_f, speed_cm_s_f) %>%
  slice(1) %>%
  pull(filename)

plot_force_curve <- function(fname) {
  fp <- file.path(data_dir, fname)
  df <- read_csv(fp, show_col_types = FALSE) %>% 
    mutate(sample = row_number(),
           force_g = na_if(force_g, 0))
  meta <- parse_filename(fp)
  motion <- df %>% filter(steps > 0)
  forces <- motion$force_g
  
  local_max_idx <- which(diff(sign(diff(forces))) == -2) + 1
  peak_motion_idx <- if (length(local_max_idx) > 0) local_max_idx[1] else
    which.max(forces)
  peak_sample <- df$sample[df$steps > 0][peak_motion_idx]
  fstat <- forces[peak_motion_idx]
  fdyn <- mean(forces[(peak_motion_idx + 1):length(forces)], na.rm = TRUE)
  dyn_start <- df$sample[df$steps > 0][peak_motion_idx + 1]
  dyn_end <- max(df$sample[df$steps > 0])
  
  ggplot(df, aes(x = sample, y = force_g)) +
    geom_line(colour = "steelblue") +
    geom_vline(xintercept = peak_sample, linetype = "dashed", colour = "red") +
    annotate("text", x = peak_sample + 3, y = fstat * 0.95,
             label = paste0("Fstat = ", round(fstat, 1), " g"),
             colour = "red", hjust = 0, size = 3.5) +
    geom_segment(x = dyn_start, xend = dyn_end, y = fdyn, yend = fdyn,
                 colour = "darkorange", linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = (dyn_start + dyn_end) / 2, y = fdyn * 1.05,
             label = paste0("Fdyn = ", round(fdyn, 1), " g"),
             colour = "darkorange", size = 3.5) +
    labs(title = paste0(meta$weight_g, "g @ ", meta$speed_cm_s, " cm/s"),
         x = "Sample #", y = "Force (g)") +
    theme_bw(base_size = 11)
}

example_plots <- map(example_files, plot_force_curve)
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined <- wrap_plots(example_plots, ncol = 3)
  ggsave(file.path(output_dir, "example_force_curves.png"), combined,
         width = 15, height = ceiling(length(example_plots) / 3) * 4, dpi = 300)
} else {
  walk2(example_plots, seq_along(example_plots), function(p, i) {
    ggsave(file.path(output_dir, paste0("example_force_curve_", i, ".png")),
           p, width = 7, height = 4, dpi = 300)
  })
}

cat("\n=== 02_friction_extraction.R complete ===\n")
