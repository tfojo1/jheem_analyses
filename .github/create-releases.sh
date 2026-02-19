#!/usr/bin/env bash
# Create GitHub Releases for syphilis manager build inputs.
#
# Usage:
#   .github/create-releases.sh [--dry-run] [--q-drive /path/to/q/drive]
#
# Defaults to /Volumes/jheem$ for the Q drive mount point.
# Creates a release: syphilis-manager-inputs-vX.Y.Z
#   Contents: section files, birth data, strat results
#
# Prerequisites: gh CLI authenticated (gh auth status)

set -euo pipefail

# --- Configuration ---
Q_DRIVE="${Q_DRIVE:-/Volumes/jheem\$}"
INPUTS_TAG="${INPUTS_TAG:-syphilis-manager-inputs-v1.0.0}"

# --- Parse args ---
DRY_RUN=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run) DRY_RUN=1; shift ;;
    --q-drive) Q_DRIVE="$2"; shift 2 ;;
    --inputs-tag) INPUTS_TAG="$2"; shift 2 ;;
    *) echo "Unknown arg: $1"; exit 1 ;;
  esac
done

SECTION_DIR="$Q_DRIVE/data_managers/data.manager.merge"
BIRTH_DIR="$Q_DRIVE/data_raw/syphilis.manager/births.for.congenital.syphilis.proportion"
STRAT_RESULTS="$Q_DRIVE/data_managers/stratification_analysis_results_county_based.rdata"

# --- Verify files exist ---
echo "Checking files..."
MISSING=0
for f in \
  "$SECTION_DIR/syphilis.manager_section1.rdata" \
  "$SECTION_DIR/syphilis.manager_section2.rdata" \
  "$SECTION_DIR/syphilis.manager_section3.rdata" \
  "$SECTION_DIR/syphilis.manager_section4.rdata" \
  "$STRAT_RESULTS"; do
  if [[ ! -f "$f" ]]; then
    echo "  MISSING: $f"
    MISSING=1
  fi
done

BIRTH_FILES=("$BIRTH_DIR"/*.xlsx)
if [[ ${#BIRTH_FILES[@]} -eq 0 ]]; then
  echo "  MISSING: no .xlsx files in $BIRTH_DIR"
  MISSING=1
fi

if [[ $MISSING -eq 1 ]]; then
  echo "Some files are missing. Aborting."
  exit 1
fi
echo "All files found."

# --- Show what we're about to do ---
echo ""
echo "=== $INPUTS_TAG ==="
echo "  Section files:"
for i in 1 2 3 4; do
  f="$SECTION_DIR/syphilis.manager_section${i}.rdata"
  echo "    $(basename "$f") ($(du -h "$f" | cut -f1))"
done
echo "  Birth data: ${#BIRTH_FILES[@]} .xlsx files ($(du -sh "$BIRTH_DIR" | cut -f1) total)"
echo "  Strat results: $(basename "$STRAT_RESULTS") ($(du -h "$STRAT_RESULTS" | cut -f1))"

if [[ $DRY_RUN -eq 1 ]]; then
  echo ""
  echo "(dry run — no releases created)"
  exit 0
fi

echo ""
read -p "Create these releases? [y/N] " CONFIRM
if [[ "$CONFIRM" != "y" && "$CONFIRM" != "Y" ]]; then
  echo "Aborted."
  exit 0
fi

# --- Create inputs release ---
echo ""
echo "Creating $INPUTS_TAG..."
gh release create "$INPUTS_TAG" \
  --title "Syphilis Manager Build Inputs $INPUTS_TAG" \
  --notes "$(cat <<'NOTES'
Build inputs for the syphilis manager merge pipeline.

**Contents:**
- 4 section `.rdata` files (from `data.manager.merge/`)
- 9 birth data `.xlsx` files (for congenital syphilis proportions)
- `stratification_analysis_results_county_based.rdata` (for data quality removals)

Used by the `build-syphilis-manager` workflow.
NOTES
)" \
  "$SECTION_DIR/syphilis.manager_section1.rdata" \
  "$SECTION_DIR/syphilis.manager_section2.rdata" \
  "$SECTION_DIR/syphilis.manager_section3.rdata" \
  "$SECTION_DIR/syphilis.manager_section4.rdata" \
  "$STRAT_RESULTS" \
  "${BIRTH_FILES[@]}"

echo "Created $INPUTS_TAG"

echo ""
echo "Done: https://github.com/tfojo1/jheem_analyses/releases/tag/$INPUTS_TAG"
