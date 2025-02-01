#!/bin/bash

#SBATCH --mem=16G
#SBATCH --output=/scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/cluster_scripts/outputs/shield/US/setup_pop.demog.shield.US.bat/setup_pop.demog.shield.US.bat/.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/set_up_calibration.R shield US pop.demog.shield.US /scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/applications/SHIELD/shield_specification.R /scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/applications/SHIELD/calibration/shield_calib_register.R