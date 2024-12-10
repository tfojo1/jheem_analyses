#!/bin/bash

#SBATCH --job-name=C.12580_setup
#SBATCH --output=../jheem_analyses/cluster_scripts/init_C.12580.out
#SBATCH --time=48:00:00
#SBATCH --partition=shared
#SBATCH --account=azalesak

ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
Rscript cluster_scripts/set_up_calibration.r ehe C.12580 init.pop.ehe