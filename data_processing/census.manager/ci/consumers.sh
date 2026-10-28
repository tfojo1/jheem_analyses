#!/usr/bin/env bash
# Check that SHIELD starts with the baseline census and with the candidate. A step of
# the shared manager build (.github/workflows/manager-build.yml).
set -euo pipefail
export JHEEM2_PATH="$GITHUB_WORKSPACE/_jheem2"
export JHEEM_ANALYSES_REF="$GITHUB_SHA"
export JHEEM2_REF="$(git -C _jheem2 rev-parse HEAD)"
export LOCATIONS_REF="$(git -C _locations rev-parse HEAD)"
export DISTRIBUTIONS_REF="$(git -C _distributions rev-parse HEAD)"
SYPHILIS_FILE=_ci_inputs/consumer/syphilis.manager.rdata
# The baseline is the control: if SHIELD fails with it, the check is broken.
Rscript data_processing/census.manager/check_consumers.R \
  _ci_inputs/baseline/census.manager.rdata "$BASELINE_RELEASE" \
  "$SYPHILIS_FILE" "$SYPHILIS_RELEASE" _ci_outputs/consumer_baseline_report.json \
  2>&1 | tee _ci_outputs/consumer_baseline_summary.txt
Rscript data_processing/census.manager/check_consumers.R \
  _ci_outputs/census.manager.rdata "census-manager-v$BUILD_DATE-candidate" \
  "$SYPHILIS_FILE" "$SYPHILIS_RELEASE" _ci_outputs/consumer_candidate_report.json \
  2>&1 | tee _ci_outputs/consumer_candidate_summary.txt
