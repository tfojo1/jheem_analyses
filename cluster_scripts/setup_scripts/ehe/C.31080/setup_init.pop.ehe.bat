#!/bin/bash

#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1//jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.31080/setup_init.pop.ehe.bat/setup_init.pop.ehe.bat/.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/set_up_calibration.R ehe C.31080 init.pop.ehe applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R