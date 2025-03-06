#!/bin/bash

#SBATCH --job-name=S_C.40900
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.40900/setup_init.pop.ehe.bat/setup_init.pop.ehe.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/set_up_calibration.R ehe C.40900 init.pop.ehe applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R