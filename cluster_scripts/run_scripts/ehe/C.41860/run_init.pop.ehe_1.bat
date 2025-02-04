#!/bin/bash

#SBATCH --job-name=run_ehe_C.41860_init.pop.ehe_1
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1//jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.41860/run_init.pop.ehe_1.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/run_calibration.R ehe C.41860 init.pop.ehe 1 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R