#!/bin/bash

#SBATCH --job-name=setup_C.12580_1
#SBATCH --mem=16G
#SBATCH --output=../jheem_analyses/cluster_scripts/run_C.12580_1.out
#SBATCH --time=168:00:00
#SBATCH --partition=unlimited
#SBATCH --account=pkasaie1

ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
Rscript cluster_scripts/run_calibration.R ehe C.12580 init.pop.ehe 1