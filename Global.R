###########################################################################
# TARTAN: a transparent, calibratable device for controlled 
#         fibre transfer experiments and reproducible ground-truth data generation
#
# Virginie Galais1*, Cameron Anderson2, Niamh Nic Daéid1, Hervé Ménard1
# 1 Leverhulme Research Centre for Forensic Science,
#   Department of Science and Engineering, University of Dundee,
#   Dundee, DD1 4HN, UK
# 2 Science and Engineering Office, School of Science and Engineering,
#   University of Dundee, Dundee, UK
# * Correspondence: vgalais001@dundee.ac.uk
#
# Website: https://github.com/LRCFS/
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.
# If not, see <https://www.gnu.org/licenses/>.
###########################################################################

# ---- OVERVIEW ---------------------------------------------------------------
#
# This repository contains two distinct analytical pipelines:
#
#   PIPELINE A — Load cell characterisation (for the paper / device validation)
#   ─────────────────────────────────────────────────────────────────────────
#   Purpose : Characterises the TARTAN load cell: raw K-factor stability, and
#             multi-weight calibration curves across several sessions.
#             Quantifies drift, linearity, and metrological validity via the
#             chi-squared criterion (following Galais 2024, Appendix 4).
#   Input   : Data/K_characterisation.csv
#             Data/Calibration curve/*_CalibrationRaw_*LoadRange_weights_*.csv
#   Output  : Results/Load-cell_characterisation/
#             Does NOT feed into the friction data analysis.
#   Script  : Code/1A_Loadcell_characterisation_SI.R
#
#   PIPELINE B — Friction data analysis (for reproducing experimental results)
#   ─────────────────────────────────────────────────────────────────────────
#   Purpose : Processes the fibre transfer friction experiments.
#             Computes a session-specific calibration correction (alpha) from
#             the before/after load cell checks bracketing each experiment,
#             extracts static and dynamic friction coefficients, propagates
#             measurement uncertainties (GUM framework), and runs statistical
#             analyses (Kruskal-Wallis, Dunn post-hoc with Holm adjustment).
#   Input   : Data/COF/*_CalibrationRaw_LoadCell_before.csv / _after.csv
#             Data/COF/*_[weight]g_Rep[n]_run[speed].csv
#   Output  : Results/Session-Specific_Calibration_Check/
#   Scripts : Code/1B_Session_Calibration_for_Transfer.R
#                                    → Session_calibration_parameters.csv
#             Code/2_Friction_extraction_Raw.R
#                                    → all_results_uncorrected.csv
#             Code/3_Transfer_Analysis_corrected.R
#                                    → corrected results, statistics + figures
#
# ---- HOW TO USE -------------------------------------------------------------
#
#   source("Global.R") runs Pipeline A and then Pipeline B (1B → 2 → 3).
#   Set run_pipeline_A / run_pipeline_B below to run only one of them.
#   Scripts must be run in order within each pipeline.
#   Pipeline A and Pipeline B are fully independent of each other.
#
# ---- REQUIRED FOLDER STRUCTURE ----------------------------------------------
#
#   Data/
#     K_characterisation.csv
#     Calibration curve/   ← multi-weight drift study files (Pipeline A)
#     COF/                 ← experiment files + before/after checks (Pipeline B)
#   Results/               ← created automatically
#   Code/
#     1A_Loadcell_characterisation_SI.R
#     1B_Session_Calibration_for_Transfer.R
#     2_Friction_extraction_Raw.R
#     3_Transfer_Analysis_corrected.R
#
# -----------------------------------------------------------------------------

# ---- Clean environment ------------------------------------------------------
rm(list = ls())

# ---- Libraries --------------------------------------------------------------
library(tidyverse)   # dplyr, readr, tidyr, purrr, stringr, ggplot2
library(patchwork)   # |, /, wrap_plots(), plot_annotation()
library(grid)        # textGrob(), gpar() in Script 3
library(FSA)         # dunnTest() in Script 3
library(ggpubr)      # ggarrange(), annotate_figure() in Script 3
library(RColorBrewer)

# ---- Output directory -------------------------------------------------------
Results.dir <- "Results/"
dir.create(Results.dir, showWarnings = FALSE, recursive = TRUE)

# ---- Run pipelines ----------------------------------------------------------
run_pipeline_A <- TRUE
run_pipeline_B <- TRUE

if (run_pipeline_A) {
  source("Code/1A_Loadcell_characterisation_SI.R")
}

if (run_pipeline_B) {
  source("Code/1B_Session_Calibration_for_Transfer.R")
  source("Code/2_Friction_extraction_Raw.R")
  source("Code/3_Transfer_Analysis_corrected.R")
}
