#!/usr/bin/env bash
# Exercise the queries the HIV surveillance manager's consumers make. A step of the
# shared manager build (.github/workflows/manager-build.yml).
set -euo pipefail
Rscript data_processing/hiv.surveillance.manager/check_consumers.R \
  _ci_outputs/surveillance.manager.rdata \
  _ci_inputs/baseline/surveillance.manager.rdata \
  _ci_inputs/consumer/syphilis.manager.rdata \
  _ci_outputs/consumer_report.json \
  2>&1 | tee _ci_outputs/consumer_summary.txt
