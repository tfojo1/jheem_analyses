#!/usr/bin/bash

mkdir -p $HOME/rlibs/R-4.3.3
ml gfbf/2023b
ml R/4.3.3-gfbf-2023b
export R_LIBS_USER=$HOME/rlibs/R-4.3.3