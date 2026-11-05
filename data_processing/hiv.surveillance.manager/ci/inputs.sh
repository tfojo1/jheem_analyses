#!/usr/bin/env bash
# HIV build inputs beyond the sections and the baseline: county-to-county movement from
# the raw data, the census from a census release (the promoted census by default), and
# the promoted syphilis manager for the consumer check. A step of the shared manager
# build (.github/workflows/manager-build.yml), run from the repository root.
set -euo pipefail
source scripts/ci/manager_build.sh
[[ "$RAW_RELEASE" =~ ^data-raw-[a-zA-Z0-9._-]+$ ]]
[[ "$CENSUS_RELEASE" =~ ^census-manager-[a-zA-Z0-9._-]+$ ]]

download_verified "$RAW_RELEASE" data-raw-movement.tar.gz _ci_inputs/raw
tar -xzf _ci_inputs/raw/data-raw-movement.tar.gz -C _ci_inputs/raw \
  --no-same-owner --no-same-permissions \
  data_raw/movement/county.to.county
download_verified "$CENSUS_RELEASE" census.manager.rdata _ci_inputs/census
# The syphilis manager's source registry is used by the adult-population import.
SYPHILIS=$(dated_release syphilis-manager-latest "Promoted from")
download_verified syphilis-manager-latest syphilis.manager.rdata _ci_inputs/consumer

RAW_EXACT=$(dated_release "$RAW_RELEASE" "Points to")
CENSUS_EXACT=$(dated_release "$CENSUS_RELEASE" "Promoted from")
record_input raw_data_release "$RAW_RELEASE"
record_input raw_data_exact "$RAW_EXACT"
record_input census_release "$CENSUS_RELEASE"
record_input census_exact "$CENSUS_EXACT"
record_input syphilis_consumer_release "$SYPHILIS"
record_digests _ci_inputs/raw/data-raw-movement.tar.gz _ci_inputs/census/census.manager.rdata \
  _ci_inputs/consumer/syphilis.manager.rdata

digest12() { sha256sum "$1" | cut -c1-12; }
build_info_row "Raw data" "\`$RAW_RELEASE\`" \
  "\`${RAW_EXACT:-unknown}\`, movement \`$(digest12 _ci_inputs/raw/data-raw-movement.tar.gz)\`"
build_info_row "Census" "\`$CENSUS_RELEASE\`" \
  "\`${CENSUS_EXACT:-unknown}\`, \`$(digest12 _ci_inputs/census/census.manager.rdata)\`"
build_info_row "Consumer check syphilis" '`syphilis-manager-latest`' "\`$SYPHILIS\`"
