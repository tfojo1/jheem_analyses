#!/bin/bash

#SBATCH --job-name=R_C.41700_1
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.41700/run_full.with.covid2_1.out
#SBATCH --time=36:00:00
#SBATCH --partition=parallel
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/run_calibration.R ehe C.41700 full.with.covid2 1 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R