# TARTAN: a transparent, calibratable fibre transfer platform based on repurposed motion systems

[DOI](https://doi.org/PLACEHOLDER)

**Authors:** Virginie Galais¹\*, Cameron Anderson², Niamh Nic Daéid¹, Hervé Ménard¹

¹ Leverhulme Research Centre for Forensic Science, Department of Science and Engineering, University of Dundee, Dundee, DD1 4HN, UK  
² Science and Engineering Office, School of Science and Engineering, University of Dundee, Dundee, UK  
\*Corresponding author

This repository contains the R scripts used for analysing the data from a study investigating the transfer of fibres between textiles during controlled contact, and its relationship with the frictional properties of the fabrics, measured using the TARTAN device. The repository is structured to allow for reproducible analysis and figure generation, providing insight into forensic textile analysis.

## Global Script (Global.R)

This is the primary script that needs to be run first. It automatically calls and executes all the other scripts in the correct sequence:

- Load-cell characterisation
- Session calibration for transfer
- Friction extraction
- Transfer analysis

Make sure to start by running the Global.R script to ensure the correct data is loaded and processed. This will handle the automatic execution of all other scripts, ensuring the analyses are performed in the proper order.

The `Code/` folder contains the following R scripts, each focusing on a different aspect of the study:

## 1A - Loadcell_characterisation_SI.R

Characterises the load cell and generates the load-cell characterisation figures and tables presented in the Supplementary Information.

## 1B - Session_Calibration_for_Transfer.R

Applies the per-session calibration used for the transfer experiments, based on the calibration-curve data (`Data/Calibration curve`).

## 2 - Friction_extraction_Raw.R

Processes the raw force recordings to extract the friction data, including the coefficient of friction (`Data/COF`).

## 3 - Transfer_Analysis_corrected.R

Performs the (corrected) analysis of fibre transfer, generating the associated figures and statistical tables.

## Data

- `Data/COF` — coefficient-of-friction measurements.
- `Data/Calibration curve` — load-cell calibration data used by the session calibration.

## Device firmware and control software

The `Arduino codes/` folder contains the microcontroller firmware and the control interface:

- `TARTAN_Uno_MotorControl` — Arduino Uno firmware for motor control and the limit switches (lower switch = home; upper switch = safety stop).
- `TARTAN_Nano_LoadCell` — Arduino Nano firmware reading the load cell via the HX711 amplifier.
- `Limit_swtich_test` — standalone sketch for testing the limit switches.
- `GUI code` — Python control GUI used to operate the device.

See the construction and operating manual for details on building and running the device.
