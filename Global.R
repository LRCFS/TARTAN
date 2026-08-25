###########################################################################
# TARTAN: a transparent, calibratable fibre transfer platform
#         based on repurposed motion systems
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
#   Purpose : Characterises the TARTAN load cell across multiple sessions
#             using full multi-weight calibration curves. Quantifies drift,
#             linearity, and metrological validity via chi-squared criterion
#             (following Galais 2024, Appendix 4).
#   Input   : Data/Calibration curve/*_CalibrationRaw_*LoadRange_weights_*.csv
#   Output  : Figures and tables documenting load cell behaviour.
#             Does NOT feed into the friction data analysis.
#   Script  : Code/01_LoadCell_Characterisation.R
#
#   PIPELINE B — Friction data analysis (for reproducing experimental results)
#   ─────────────────────────────────────────────────────────────────────────
#   Purpose : Processes the fibre transfer friction experiments.
#             Computes a session-specific calibration correction (alpha) from
#             the before/after load cell checks bracketing each experiment,
#             extracts static and dynamic friction coefficients, propagates
#             measurement uncertainties (GUM framework), and runs statistical
#             analyses (ART ANOVA, Tukey post-hoc).
#   Input   : Data/COF/*_CalibrationRaw_LoadCell_before/after.csv
#             Data/COF/*_[weight]g_Rep[n]_run[speed].csv
#   Output  : Corrected friction coefficients, summary statistics,
#             boxplots, ANOVA results.
#   Scripts : Code/01_Calibration_Check.R  → calibration_parameters.csv
#             Code/02_Friction_Extraction.R → all_results.csv
#             Code/03_Analysis.R            → corrected results + figures
#
# ---- HOW TO USE -------------------------------------------------------------
#
#   To reproduce the paper's load cell characterisation figures only:
#     source("Code/01_LoadCell_Characterisation.R")
#
#   To reproduce the full friction data analysis:
#     source("Global.R")   ← runs scripts B1, B2, B3 in sequence
#
#   Scripts must be run in order within each pipeline.
#   Pipeline A and Pipeline B are fully independent of each other.
#
# ---- REQUIRED FOLDER STRUCTURE ----------------------------------------------
#
#   Data/
#     Calibration curve/   ← multi-weight drift study files (Pipeline A)
#     COF/                 ← experiment files + before/after checks (Pipeline B)
#   Results/               ← created automatically
#   Code/
#     01_LoadCell_Characterisation.R
#     01_Calibration_Check.R
#     02_Friction_Extraction.R
#     03_Analysis.R
#
# -----------------------------------------------------------------------------

# ---- Clean environment ------------------------------------------------------
rm(list = ls())

# ---- Libraries --------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(grid)
library(car)
library(FSA)
library(multcomp)
library(ARTool)
library(emmeans)
library(ggrepel)
library(ggpubr)

# ---- Output directory -------------------------------------------------------
Results.dir <- "Results/"
dir.create(Results.dir, showWarnings = FALSE, recursive = TRUE)

# ---- Run Pipeline B (friction data analysis) --------------------------------
# To run Pipeline A instead, source 01_LoadCell_Characterisation.R directly.

source("Code/1A_Loadcell_characterisation_SI.R")
source("Code/1B_Session_Calibration_for_Transfer.R")
source("Code/2_Friction_extraction_Raw.R")
source("Code/3_Transfer_Analysis_corrected.R")
