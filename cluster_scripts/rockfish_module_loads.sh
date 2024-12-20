#!/usr/bin/bash

echo "First ml"
ml gfbf/2023b
echo "Second ml"
ml R/4.3.3-gfbf-2023b
echo "Export"
export R_LIBS_USER=$HOME/rlibs/R-4.3.3
echo "Complete"