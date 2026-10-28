#!/usr/bin/env bash
# Merge the census sections. A step of the shared manager build
# (.github/workflows/manager-build.yml), which sets SECTION_DIR, CACHED_DIR and
# WRITE_SHARED_OUTPUT=false and measures this process.
set -euo pipefail
exec Rscript -e '
  library(jheem2)
  source("data_processing/census.manager/census.processing.for.merging/census.manager.merge.R")
  writeLines(capture.output(sessionInfo()), "_ci_outputs/session_info.txt")
'
