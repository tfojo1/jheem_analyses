#!/bin/bash -l
#SBATCH --account=pkasaie1
#SBATCH --job-name=shield
#SBATCH --time=48:00:00
#SBATCH --partition=parallel
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=4G
#SBATCH --output=out.out

#load modules and check
module load r
Rscript shield_calib_setup_and_run.R "rockfish"