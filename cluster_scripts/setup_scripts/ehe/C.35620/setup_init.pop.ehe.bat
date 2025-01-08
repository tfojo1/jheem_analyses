#!/bin/bash

#SBATCH --mem=16G
#SBATCH --output=../jheem_analyses/cluster_scripts/ehe/C.35620/setup_init.pop.ehe.bat/setup_init.pop.ehe.bat/.out
#SBATCH --time=12:00:00
#SBATCH --partition=shared
#SBATCH --account=pkasaie1

source cluster_scripts/Rockfish_module_loads.sh
Rscript cluster_scripts/set_up_calibration.R ehe C.35620 init.pop.ehe