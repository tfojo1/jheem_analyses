#!/bin/bash

#SBATCH --job-name=assemble_C.12580
#SBATCH --mem=16G
#SBATCH --output=../jheem_analyses/cluster_scripts/assemble_C.12580.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
Rscript cluster_scripts/run_calibration.R ehe C.12580 init.pop.ehe 0.5 0