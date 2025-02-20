#!/bin/bash

#SBATCH --job-name=run_ehe_C.19820_full.with.covid2_1
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.19820/run_full.with.covid2_1.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/run_calibration.R ehe C.19820 full.with.covid2 1 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R