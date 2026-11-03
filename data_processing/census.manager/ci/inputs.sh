#!/usr/bin/env bash
# Census build inputs beyond the sections and the baseline: the syphilis release SHIELD
# declares on this commit, for the SHIELD consumer check. A step of the shared manager
# build (.github/workflows/manager-build.yml), run from the repository root.
set -euo pipefail
source scripts/ci/manager_build.sh

SYPHILIS=$(Rscript -e '
  m <- read.csv("applications/SHIELD/data_managers.csv", comment.char = "#",
                colClasses = "character", strip.white = TRUE)
  cat(m$release[m$name == "syphilis"])')
[[ "$SYPHILIS" =~ ^syphilis-manager-v[0-9][0-9.]*$ ]]
download_verified "$SYPHILIS" syphilis.manager.rdata _ci_inputs/consumer
echo "SYPHILIS_RELEASE=$SYPHILIS" >> "$GITHUB_ENV"

record_input shield_syphilis_release "$SYPHILIS"
record_digests _ci_inputs/consumer/syphilis.manager.rdata
build_info_row "SHIELD check syphilis" '`applications/SHIELD/data_managers.csv`' "\`$SYPHILIS\`"
