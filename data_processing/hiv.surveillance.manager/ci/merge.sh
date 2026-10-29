#!/usr/bin/env bash
# Merge the HIV surveillance sections. A step of the shared manager build
# (.github/workflows/manager-build.yml), which sets SECTION_DIR, CACHED_DIR and
# WRITE_SHARED_OUTPUT=false and measures this process.
set -euo pipefail
export CENSUS_MANAGER_CACHE_FILE="$GITHUB_WORKSPACE/_ci_inputs/deps/census.manager.rdata"
export CENSUS_MANAGER_SHARED_FILE="$GITHUB_WORKSPACE/_ci_inputs/deps/census.manager.rdata"
export COUNTY_TO_COUNTY_DIR="$GITHUB_WORKSPACE/_ci_inputs/raw/data_raw/movement/county.to.county"
exec Rscript -e '
  library(jheem2)
  library(locations)
  library(dplyr)
  library(readxl)
  library(stringr)
  library(tidyr)
  source("data_processing/hiv.surveillance.manager/hiv.surveillance.processing.for.merging/hiv.surveillance.manager.merge.R")
  writeLines(capture.output(sessionInfo()), "_ci_outputs/session_info.txt")
'
