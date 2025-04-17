#!/usr/bin/bash

mkdir -p $HOME/rlibs/R-4.3.3
module load git
module load gfbf/2023b
module load R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
export ON_CLUSTER=true