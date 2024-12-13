#!/bin/bash

#SBATCH --job-name=run_C.35620_1
#SBATCH --mem=16G
#SBATCH --output=../jheem_analyses/cluster_scripts/run_C.35620_1.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
Rscript cluster_scripts/run_calibration.R ehe C.35620 init.pop.ehe 1