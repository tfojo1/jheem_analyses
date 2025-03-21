#!/bin/bash

#SBATCH --job-name=R_C.12060_1
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs2/ehe/C.12060/run_full.ehe_1.out
#SBATCH --time=36:00:00
#SBATCH --partition=parallel
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/run_calibration.R ehe C.12060 full.ehe 1 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R