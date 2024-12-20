#!/bin/bash

#SBATCH --job-name=C.12060_setup
#SBATCH --mem=16G
#SBATCH --output=../jheem_analyses/cluster_scripts/init_C.12060.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
Rscript cluster_scripts/set_up_calibration.R ehe C.12060 init.pop.ehe