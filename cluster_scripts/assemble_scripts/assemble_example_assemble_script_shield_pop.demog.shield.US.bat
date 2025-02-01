#!/bin/bash

#SBATCH --job-name=assemble_example_assemble_script_shield_pop.demog.shield.US
#SBATCH --mem=24G
#SBATCH --output=/scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/cluster_scripts/outputs/assemble_example_assemble_script_shield_pop.demog.shield.US.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

source cluster_scripts/rockfish_module_loads.sh
Rscript cluster_scripts/assemble_calibration.R shield US pop.demog.shield.US 0.6 0.2 /scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/applications/SHIELD/shield_specification.R /scratch4/pkasaie1/nsizemo1/jheem/code/jheem_analyses/applications/SHIELD/calibration/shield_calib_register.R