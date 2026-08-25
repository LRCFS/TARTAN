# TARTAN: a transparent, calibratable fibre transfer platform based on repurposed motion systems

[DOI](https://doi.org/PLACEHOLDER)

**Authors:** Virginie Galais¹\*, Cameron Anderson², Niamh Nic Daéid¹, Hervé Ménard¹

¹ Leverhulme Research Centre for Forensic Science, Department of Science and Engineering, University of Dundee, Dundee, DD1 4HN, UK  
² Science and Engineering Office, School of Science and Engineering, University of Dundee, Dundee, UK  
\*Corresponding author

This repository contains the R scripts, Arduino firmware, and Python control software for the TARTAN device - a bench-top platform, built from a repurposed 3D-printer motion system, for measuring the frictional properties of textiles (static and dynamic coefficients of friction) under controlled contact. The R analysis covers load-cell characterisation, session-specific calibration, friction extraction, and the corrected, uncertainty-propagated analysis, providing insight into forensic textile analysis. The repository is structured to allow for reproducible analysis and figure generation.

## 0 - Global Script (Global.R)

This is the primary script that needs to be run first. It loads the required packages and automatically calls and executes the other scripts in the correct sequence. Make sure to start by running the Global.R script to ensure the correct data is loaded and processed, so that the analyses are performed in the proper order.

The analysis is organised into two independent pipelines:

- **Pipeline A (standalone, Supplementary Information):** `1A_Loadcell_characterisation_SI.R`
- **Pipeline B (friction analysis, run in order):** `1B_Session_Calibration_for_Transfer.R` → `2_Friction_extraction_Raw.R` → `3_Transfer_Analysis_corrected.R`

The `Code/` folder contains the following R scripts:

## 1A - Loadcell_characterisation_SI.R

Standalone characterisation of the TARTAN load cell for the Supplementary Information; it does **not** feed into the friction analysis. Using full multi-weight calibration curves collected across several sessions (morning / midday / evening, over multiple days), it quantifies linearity (per-session linear regression), metrological validity (reduced chi-squared criterion), and session-to-session drift in the calibration factor α.

- **Input:** `Data/Calibration curve/`
- **Outputs (`Results/Load-cell_characterisation/`):** linearity, α-drift, residual and raw-scatter figures, plus the linear-model, chi-squared and drift summary tables.

## 1B - Session_Calibration_for_Transfer.R

**Pipeline B, Step 1 of 3.** Computes a session-specific calibration correction factor (α_session) for each experiment date from the before/after load-cell checks that bracket the friction runs collected that day. α is estimated by minimising a reduced chi-squared criterion, and its uncertainty combines the fit uncertainty with the within-session (before/after) stability.

- **Input:** `Data/COF/` (before/after calibration-check files)
- **Output (`Results/Session-Specific_Calibration_Check/`):** `Session_calibration_parameters.csv` (one row per date) plus per-weight and tare diagnostic summaries.
- Must be run before scripts 2 and 3.

## 2 - Friction_extraction_Raw.R

**Pipeline B, Step 2 of 3.** Extracts the raw static and dynamic friction forces from every TARTAN experiment file and produces annotated force-curve figures for quality control. Static friction is taken as the first local maximum of the force signal during motion (detected via the sign change of the second difference); dynamic friction is the mean of the plateau after the static peak. No calibration correction is applied here - these values are diagnostic only.

- **Input:** `Data/COF/` (experiment CSVs; calibration-check files are auto-excluded by filename)
- **Outputs:** `all_results_uncorrected.csv` and replicate-level force-curve figures.

## 3 - Transfer_Analysis_corrected.R

**Pipeline B, Step 3 of 3.** Applies the session-specific correction (F_corrected = F_raw × α_session; μ = F_corrected / actual mass), propagates measurement uncertainties following the GUM framework (k = 2 expanded uncertainty), runs the statistics, and produces the publication figures. Statistics: normality (Shapiro–Wilk) and homogeneity of variance (Levene); a two-way ANOVA for the static coefficient and an Aligned Rank Transform (ART) ANOVA for the dynamic coefficient; post-hoc Tukey HSD / ART + emmeans.

- **Inputs (from `Results/`):** `Session_calibration_parameters.csv` (Script 1B) and `all_results_uncorrected.csv` (Script 2)
- **Outputs:** `all_results_corrected.csv`, `summary_corrected.csv`, and the paper figures (corrected example force curve, combined static/dynamic μ boxplot, static-vs-dynamic scatter).

## Data

- `Data/COF/` - friction experiment recordings together with the before/after load-cell calibration checks (used by scripts 1B, 2 and 3).
- `Data/Calibration curve/` - multi-weight calibration curves used for load-cell characterisation (used by script 1A).

## Requirements

R (≥ 4.x) with the packages loaded by `Global.R`, including `readr`, `dplyr`, `tidyr`, `ggplot2`, `car` (Levene's test), `ARTool` (ART ANOVA) and `emmeans` (post-hoc). [Confirm against your Global.R.]

## Arduino firmware (`Arduino codes/`)

The device is driven by two independent Arduino boards, each connected to the host computer on its own USB serial port with no direct communication between them. This separation keeps the continuous load-cell acquisition from interfering with the timing-critical motor stepping.

### `TARTAN_Uno_MotorControl` - Arduino Uno (motion)
Controls the two stepper motors that drive the vertical lead screws and reads the limit switches. It is **command-driven and event-based**: the main loop does nothing but generate step pulses during motion (no serial traffic while stepping, to protect timing), and it emits short event messages only when the state changes.
- **Commands:** `MOVE <mm> <UP|DOWN> <speed_cm_s>`, `HOME_HIT <speed_cm_s>`, `OFF`, `HELP`.
- **Events:** `MOVE_START`, `MOVE_DONE`, `HOME_START`, `HOME_DONE`, `HOME_TIMEOUT`, `OFF` (and `ESTOP` for the upper safety limit switch).
- **Key settings:** 400 steps/mm; active-low limit switches on pins D8 (lower/home) and D9 (upper/safety stop); 30 s homing timeout.

### `TARTAN_Nano_LoadCell` - Arduino Nano (force acquisition)
Reads the load cell through an HX711 24-bit amplifier (data D7, clock D6) and streams calibrated force to the host at 10 Hz as `t_ms,force_g`. It handles taring, applies a scale/calibration factor, and stores that factor in EEPROM so it persists across power cycles.
- **Commands:** `TARE`, `STREAM_ON`, `STREAM_OFF`, `RAW <n>`, `SET_SCALE <value>`, `GET_SCALE`, `SAVE_SCALE`, `LOAD_SCALE`.

### `Limit_swtich_test` - standalone diagnostic
A minimal sketch that prints the raw state of the two limit switches to the serial monitor, used to verify the switches and wiring during assembly.

## Control software (`Code/GUI code`)

`TARTAN_GUI.py` is a Python (Tkinter) graphical interface that connects to both boards over serial (the Uno for motion, the Nano for the load cell). It provides load-cell calibration and taring, platform homing and controlled moves, and runs a transfer measurement while recording force and platform-position data to a timestamped CSV file. Requires Python 3 with `pyserial` and `Pillow` (`tkinter` ships with most Python installations).


## Funding and acknowledgements

This work was carried out at the Leverhulme Research Centre for Forensic Science, University of Dundee, and was supported by the Leverhulme Trust (RC-2015-011) and Wellcome (grant reference 228087/Z/23/Z).
