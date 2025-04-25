#!/bin/bash

#SBATCH --job-name=assemble_az_stage3_030425
#SBATCH --mem=24G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/assemble_az_stage3_030425.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/assemble_calibration.R ehe C.35620 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.33100 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.31080 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.12060 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.26420 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.19100 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.16980 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.47900 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.37980 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.36740 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.41860 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.38060 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.45300 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.40140 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.19820 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.12580 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.29820 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.14460 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.41740 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.16740 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.41700 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.27260 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.35380 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.32820 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.42660 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.12420 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.26900 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.17140 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.18140 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.12940 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.40900 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.17460 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R
Rscript cluster_scripts/assemble_calibration.R ehe C.41180 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R