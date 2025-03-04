#!/bin/bash

#SBATCH --job-name=assemble_az_stage3_Balt_030425
#SBATCH --mem=24G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/assemble_az_stage3_Balt_030425.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/assemble_calibration.R ehe C.12580 full.with.covid2 150 0 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R