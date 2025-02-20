#!/bin/bash

#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.18140/setup_full.with.covid2.bat/setup_full.with.covid2.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/set_up_calibration.R ehe C.18140 full.with.covid2 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R