#!/bin/bash

#SBATCH --job-name=run_shield_C.12580_pop.demog.shield.US_4
#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/cluster_scripts/outputs/shield/C.12580/run_pop.demog.shield.US_4.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/run_calibration.R shield C.12580 pop.demog.shield.US 4 /scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/applications/SHIELD/shield_specification.R /scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/applications/SHIELD/calibration/shield_calib_register.R