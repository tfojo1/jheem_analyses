#!/bin/bash

#SBATCH --job-name=assemble_group1
#SBATCH --mem=24G
#SBATCH --output=../jheem_analyses/cluster_scripts/assemble_group1.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
Rscript cluster_scripts/assemble_calibration.R ehe C.12580 init.pop.ehe 0.5 0
Rscript cluster_scripts/assemble_calibration.R ehe C.12060 init.pop.ehe 0.5 0
Rscript cluster_scripts/assemble_calibration.R ehe C.35620 init.pop.ehe 0.5 0