#!/bin/bash

#SBATCH --job-name=M_C.31080_1
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cluster_scripts/outputs/ehe/C.31080/multiphase_pop.ehe__trans.ehe__full.ehe1.out
#SBATCH --time=36:00:00
#SBATCH --partition=parallel
#SBATCH --account=tfojo1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/do_multiphase_calibration.R ehe C.31080 pop.ehe__trans.ehe__full.ehe 1 applications/EHE/ehe_specification.R applications/EHE/calibration_runs/ehe_register_calibrations.R