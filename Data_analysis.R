library(ggplot2)
library(dplyr)
windowsFonts(Arial = windowsFont("Arial"))


# =========================
# 1) Load data 
# =========================
LowLoadRange_02 <- read.csv("./Data/20260202_CalibrationRaw_LowLoadRange_weights.csv")
HighLoadRange_02   <- read.csv("./Data/20260202_CalibrationRaw_HighLoadRange_weights.csv")

LowLoadRange_03 <- read.csv("./Data/20260203_CalibrationRaw_LowLoadRange_weights.csv")
HighLoadRange_03   <- read.csv("./Data/20260203_CalibrationRaw_HighLoadRange_weights.csv")

LowLoadRange_04 <- read.csv("./Data/20260204_CalibrationRaw_LowLoadRange_weights.csv")
HighLoadRange_04  <- read.csv("./Data/20260204_CalibrationRaw_HighLoadRange_weights.csv")


# Add weight type + combine
LowLoadRange_02$type <- "LowLoadRange"; LowLoadRange_02$date <- "2026-02-02"
LowLoadRange_03$type <- "LowLoadRange"; LowLoadRange_03$date <- "2026-02-03"
LowLoadRange_04$type <- "LowLoadRange"; LowLoadRange_04$date <- "2026-02-04"

HighLoadRange_02$type <- "HighLoadRange"; HighLoadRange_02$date <- "2026-02-02"
HighLoadRange_03$type <- "HighLoadRange"; HighLoadRange_03$date <- "2026-02-03"
HighLoadRange_04$type <- "HighLoadRange"; HighLoadRange_04$date <- "2026-02-04"

# Combine + clean
dat <- rbind(LowLoadRange_02, LowLoadRange_03, LowLoadRange_04, HighLoadRange_02, HighLoadRange_03, HighLoadRange_04) 

dat$target_g <- as.numeric(dat$target_g)
dat$force_g  <- as.numeric(dat$force_g)

# Exclude tare and calibration weight
dat <- dat %>%
  filter(!target_g %in% c(0, 498.81))


# Summary table (mean, SD, error)
summary_tbl <- dat %>%
  group_by(date, type, target_g) %>%
  summarise(
    mean_force_g = mean(force_g, na.rm = TRUE),
    sd_force_g   = sd(force_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    error_g = mean_force_g - target_g
  )
summary_tbl

# ===========================================
# 2) Linear models per day and per load range
# ===========================================
# table for article
lm_tbl <- summary_tbl %>%
  group_by(date, type) %>%
  do({
    fit <- lm(mean_force_g ~ target_g, data = .)
    data.frame(
      R2 = formatC(summary(fit)$r.squared, format = "f", digits = 3),
      Equation = paste0(
        "y = ",
        formatC(coef(fit)[2], format = "f", digits = 4),
        "x + ",
        formatC(coef(fit)[1], format = "f", digits = 2)
      )
    )
  }) %>%
  ungroup() %>%
  transmute(
    Date = date,
    `Weight range` = recode(
      type,
      HighLoadRange = "High-load range",
      LowLoadRange  = "Low-load range"
    ),
    `R-squared` = R2,
    Equation
  )
write.csv(lm_tbl, "./Results/linear_models_by_date.csv", row.names = FALSE)

# ggplot — two graphs side by side - Figure XXX
summary_tbl$type <-gsub("HighLoadRange", "High-load range", summary_tbl$type)
summary_tbl$type <-gsub("LowLoadRange", "Low-load range", summary_tbl$type)

Linear_reg <- ggplot(summary_tbl,aes(x = target_g, y = mean_force_g, shape = date)) +
  geom_point(size = 2, colour = "grey20") +
  geom_errorbar(aes(ymin = mean_force_g - sd_force_g, ymax = mean_force_g + sd_force_g),width = 0,colour = "grey20") +
  geom_abline(intercept = 0,slope = 1,linetype = "dashed",colour = "grey40") +
  facet_wrap(~ type, scales = "free_x") +
  scale_shape_manual(values = c(16, 17, 15, 18, 3)) +
  labs(
    x = "\nKnown weight added (g)",
    y = "Average load cell reading (g)\n",
    shape = "Date") +
  theme_bw(base_family = "Arial", base_size = 12)
Linear_reg
ggsave("./Results/Linear_reg.png", plot = Linear_reg, width = 8, height = 4, dpi = 600)

# Raw scatter: force vs applied weight - Figure XXX
mass_of_interest <- 249.20

mean_tbl <- dat %>% 
  filter(target_g == mass_of_interest) %>% 
  group_by(date) %>% 
  summarise(mean_error = mean(force_g - target_g), .groups = "drop")

Raw_scatter <- ggplot(dat %>% filter(target_g == mass_of_interest),
                      aes(x = date, y = force_g - target_g)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_jitter(width = 0.12, height = 0, size = 1.8, alpha = 0.6, colour = "grey30") +
  geom_point(data = mean_tbl, aes(x = date, y = mean_error),
             colour = "red", size = 3) +
  geom_text(data = mean_tbl,
            aes(x = date, y = mean_error, label = round(mean_error, 2)),
            colour = "red", vjust = -1.2, hjust=-1.5, size = 3) +
  labs(x = "\nDate", y = "Raw measurement error (g)\n") +
  theme_bw(base_family = "Arial", base_size = 13) +
  theme(legend.position = "none")
Raw_scatter
ggsave("./Results/Raw_scatter.png", plot = Raw_scatter, width = 9, height = 5, dpi = 600)


# Range of error per weight and per date
error_range_tbl <- dat %>%
  mutate(error_g = force_g - target_g) %>%
  group_by(date, target_g) %>%
  summarise(
    min_error_g = min(error_g),
    max_error_g = max(error_g),
    range_g     = max_error_g - min_error_g,
    .groups = "drop"
  )
error_range_tbl
write.csv(error_range_tbl,"./Results/error_range_by_weight_and_date.csv",row.names = FALSE)

# Figure for supp info
# Compute raw error
dat$error_g <- dat$force_g - dat$target_g

# Low-load range limits
error_lim_low <- range(dat$error_g[dat$type == "LowLoadRange"],na.rm = TRUE)
error_lim_low <- c(floor(error_lim_low[1]),ceiling(error_lim_low[2]))

# High-load range limits
error_lim_high <- range(dat$error_g[dat$type == "HighLoadRange"],na.rm = TRUE)
error_lim_high <- c(floor(error_lim_high[1]),ceiling(error_lim_high[2]))

# Supplementary figure — Low-load range (fixed axes)
ggplot(dat %>% filter(type == "LowLoadRange"),aes(x = date, y = error_g, colour = date)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(width = 0.12, height = 0, alpha = 0.6, size = 1.2) +
  facet_wrap(~ target_g) +
  coord_cartesian(ylim = error_lim_low) +
  labs(
    x = "Date",
    y = "Raw measurement error (g)",
    title = "Supplementary Figure Sx: Raw measurement error (Low-load range)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey90")
  )

# Supplementary figure — High-load range (same axes)
ggplot(dat %>% filter(type == "HighLoadRange"),aes(x = date, y = error_g, colour = date)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(width = 0.12, height = 0, alpha = 0.6, size = 1.2) +
  facet_wrap(~ target_g) +
  coord_cartesian(ylim = error_lim_low) +
  labs(
    x = "Date",
    y = "Raw measurement error (g)",
    title = "Supplementary Figure Sx: Raw measurement error (Low-load range)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey90")
  )

# Error (bias) plot — much more diagnostic
ggplot(summary_tbl,
       aes(x = target_g, y = error_g, colour = date)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  facet_wrap(~ type, scales = "free_x") +
  labs(
    x = "Known weight added (g)",
    y = "Error (g) = mean(reading) − known weight",
    title = "Calibration bias by date",
    colour = "Date"
  ) +
  theme_bw(base_size = 13)

# Day-to-day shift analysis (quantified)
day_shift_tbl <- summary_tbl %>%
  group_by(date, type) %>%
  summarise(
    mean_bias_g = mean(error_g),
    sd_bias_g   = sd(error_g),
    .groups = "drop"
  )

day_shift_tbl

#Optional: drift visualisation
ggplot(day_shift_tbl,
       aes(x = date, y = mean_bias_g, colour = type, group = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "date",
    y = "Mean calibration bias (g)",
    title = "Day-to-day calibration shift"
  ) +
  theme_minimal(base_size = 13)


