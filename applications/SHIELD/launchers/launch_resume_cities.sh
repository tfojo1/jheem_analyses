#!/usr/bin/env bash
#
# RESUME SINGLE-CHAIN STAGES FOR MANY CITIES
#   Batch version of `launch_resume_chain.sh <city> <calib_code>` (no chain).
#   For each city in CITIES:
#     1. Resume RESUME_CODE from its cache (run chain 1, then assemble).
#        Setup is never rerun - it would clear the cache.
#     2. Then run each of FOLLOW_CODES from scratch ('all'), in order, the way
#        launch_pipeline_stages_0_to_3.sh does in phase 1.
#   A failure skips the rest of that city's codes; other cities carry on.
#   Up to MAX_CITIES cities run in parallel (1 core each).
#
#   Single-chain stages only - for stage3 chains use launch_resume_chain.sh
#   with an explicit chain number.
#
# USAGE
#   Edit the config block below, then:
#       nohup bash applications/SHIELD/launchers/launch_resume_cities.sh > /dev/null 2>&1 &
#
# Kill:
#   pkill -u pkasaie1 -x R
#   pkill -u pkasaie1 -f "Rscript"
#
# Monitor:
#   ls -lt /home/jheem-shared/logs/launcher_resume_cities_*.out   # newest first
#   tail -f /home/jheem-shared/logs/<city>_<calib_code>.out
#
# NOTE ON LOGGING
#   The resumed code appends (>>) to the log the original pipeline run used
#   (<city>_<calib_code>.out), so the history stays in one place. FOLLOW_CODES
#   start fresh (>), same as the pipeline launcher.

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed city must not abort the launcher

# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts + logs are one level up

# ── where the logs go (see launch_pipeline_stages_0_to_3.sh for details) ───────
LOG_DIR="${JHEEM_LOG_DIR:-/home/jheem-shared/logs}"
mkdir -p "$LOG_DIR"
umask 002   # new log files stay group-readable for the other members

# ── master log ──────────────────────────────────────────────────────────────
RUN_ID="${USER:-$(id -un)}_$(date +%Y%m%d_%H%M%S)"
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_resume_cities_${RUN_ID}.out" 2>&1

SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"

# ── thread settings ────────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ── config ─────────────────────────────────────────────────────────────────────
ten_cities=(
    C.12060 C.12580 C.16980 C.26420 C.31080
    C.33100 C.35620 C.37980 C.38060 C.42660
)

# ── set active cities and calibration codes here ───────────────────────────────
CITIES=("${ten_cities[@]}")

# The code that was interrupted - resumed from its cache, then assembled.
RESUME_CODE=calib.10.5.stage0.1x

# Codes to run afterwards from scratch (setup + run + assemble), in order.
# Leave empty - FOLLOW_CODES=() - to only resume.
FOLLOW_CODES=(
    calib.10.5.stage1.1x
)

# MAX_CITIES = max cities in flight (1 core each) -> peak cores = MAX_CITIES.
MAX_CITIES=32

# ── preflight ──────────────────────────────────────────────────────────────────
if [[ ! -f "$SCRIPT" ]]; then
    echo "Error: R script not found at $SCRIPT" >&2
    exit 1
fi
# `set -u` does NOT catch a typo'd array name (it expands to empty), so check explicitly
if (( ${#CITIES[@]} == 0 )); then
    echo "Error: CITIES is empty — check the array name in the config block above" >&2
    exit 1
fi

# ── per-city pipeline ──────────────────────────────────────────────────────────
# Arguments: $1 = city code
resume_city() {
    local loc="$1"
    local log="$LOG_DIR/${loc}_${RESUME_CODE}.out"
    local rc

    # 1. resume chain 1 of RESUME_CODE, appending to the original log
    echo "[$(date '+%F %T')] RESUME  $loc :: $RESUME_CODE"
    Rscript "$SCRIPT" "$loc" "$RESUME_CODE" run 1 >> "$log" 2>&1
    rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $RESUME_CODE :: run (exit $rc) — skipping remaining codes" >&2
        return 1
    fi

    # ... then assemble, as the original 'all' run would have
    Rscript "$SCRIPT" "$loc" "$RESUME_CODE" assemble >> "$log" 2>&1
    rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $RESUME_CODE :: assemble (exit $rc) — skipping remaining codes" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    $loc :: $RESUME_CODE"

    # 2. later stages from scratch
    for calib_code in "${FOLLOW_CODES[@]}"; do
        echo "[$(date '+%F %T')] START   $loc :: $calib_code"
        Rscript "$SCRIPT" "$loc" "$calib_code" all \
            > "$LOG_DIR/${loc}_${calib_code}.out" 2>&1
        rc=$?
        if (( rc != 0 )); then
            echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code (exit $rc) — skipping remaining codes" >&2
            return 1
        fi
        echo "[$(date '+%F %T')] DONE    $loc :: $calib_code"
    done

    echo "[$(date '+%F %T')] COMPLETE $loc"
}

# ── launch ─────────────────────────────────────────────────────────────────────
echo "[$(date '+%F %T')] ===== RESUME $RESUME_CODE, then: ${FOLLOW_CODES[*]:-(nothing)} ====="

for loc in "${CITIES[@]}"; do

    wait_for_slot "$MAX_CITIES" cities

    resume_city "$loc" &
    echo "[$(date '+%F %T')] LAUNCHED $loc (PID $!, running_cities=$(running_slots))"

done

wait
echo "[$(date '+%F %T')] ===== ALL CITIES DONE ====="
