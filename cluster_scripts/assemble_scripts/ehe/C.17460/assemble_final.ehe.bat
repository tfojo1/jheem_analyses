#!/bin/bash

#SBATCH --job-name=S_C.17460
#SBATCH --cpus-per-task=2
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs2/ehe/C.17460/assemble_final.ehe.out
#SBATCH --time=2:00:00
#SBATCH --partition=parallel
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/assemble_calibration.R ehe C.17460 final.ehe 0 1000 ../jheem_analyses/applications/EHE/ehe_specification.R ../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R