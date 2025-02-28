#!/bin/bash

#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.38060/setup_final.bat/setup_final.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/set_up_calibration.R ehe C.38060 final applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R