#!/usr/bin/env bash
#
# ASSEMBLE + THIN, up to MAX_CITIES cities at a time
#   For each city, saves three simsets (see shield_assemble_and_thin.R):
#   the full chain, thinned with no burn, and burned + thinned.
#   Never runs setup, so it cannot clear the chains. A city with an unfinished
#   chain fails with an error instead of saving a partial simset, and a simset
#   already on disk is skipped, so rerun this after the last chains finish.
#
# HOW MANY CITIES RUN AT ONCE
#   1. Set MAX_CITIES in the config block below.
#   2. MAX_CITIES=1 runs one city at a time.
#      MAX_CITIES=2 runs two cities side by side.
#   3. Each running city holds one full simset in memory.
#      So peak memory is about MAX_CITIES x one full simset.
#      Example: one city peaks at 60 GB -> MAX_CITIES=2 needs about 120 GB free.
#   4. Check free memory with `free -g` before raising it.
#   5. When a city finishes, the next city in CITIES starts right away.
#
# USAGE (from jheem_analyses, on a server with enough memory for MAX_CITIES full simsets)
#   nohup bash applications/SHIELD/launchers/launch_assemble_and_thin.sh > /dev/null 2>&1 &
#
# Kill:
#   pkill -u $USER -f launch_assemble_and_thin.sh
#   pkill -u $USER -f shield_assemble_and_thin.R
#
# Monitor:
#   ls -lt /home/jheem-shared/logs/launcher_assemble_thin_*.out   # newest first
#   tail -f /home/jheem-shared/logs/<the file you want>
#   tail -f /home/jheem-shared/logs/<loc>_<calib_code>_assemble_thin.out

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed city must not abort the launcher

# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts are one level up
SCRIPT="$PARENT_DIR/shield_assemble_and_thin.R"
source "$SCRIPT_DIR/_shield_slots.sh"   # wait_for_slot, running_slots

# ── where the logs go ─────────────────────────────────────────────────────────
# Set JHEEM_LOG_DIR before launching on a server without this shared folder.
LOG_DIR="${JHEEM_LOG_DIR:-/home/jheem-shared/logs}"
mkdir -p "$LOG_DIR"
umask 002   # new log files stay group-readable for the other members

# ── master log ────────────────────────────────────────────────────────────────
RUN_ID="${USER:-$(id -un)}_$(date +%Y%m%d_%H%M%S)"
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_assemble_thin_${RUN_ID}.out" 2>&1

# ── thread settings ───────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ── config: EDIT THESE for each run ───────────────────────────────────────────
CITIES=(
   C.37980 C.38060 C.42660   # stage 4: these three are missing the thinned (2,000) simset
)
CALIB_CODE=calib.10.20.stage4
THIN_EVERY=500   # iterations between kept simulations
BURN_KEEP=0.5    # fraction of each chain to keep
MAX_CITIES=2     # cities assembled at once; 1 = one at a time (see header for memory)

# ── preflight ─────────────────────────────────────────────────────────────────
if [[ ! -f "$SCRIPT" ]]; then
    echo "Error: R script not found at $SCRIPT" >&2
    exit 1
fi

# ── per city assemble + thin job ──────────────────────────────────────────────
run_city() {
    local loc="$1"
    echo "[$(date '+%F %T')] START   $loc :: $CALIB_CODE"
    Rscript "$SCRIPT" "$loc" "$CALIB_CODE" "$THIN_EVERY" "$BURN_KEEP" \
        > "$LOG_DIR/${loc}_${CALIB_CODE}_assemble_thin.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $CALIB_CODE (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    $loc :: $CALIB_CODE"
}

# ── up to MAX_CITIES at a time: each full simset needs a lot of memory ────────
for loc in "${CITIES[@]}"; do
    wait_for_slot "$MAX_CITIES" cities
    run_city "$loc" &
    echo "[$(date '+%F %T')] LAUNCHED $loc :: $CALIB_CODE (PID $!, running_cities=$(running_slots))"
done

wait
echo "[$(date '+%F %T')] ALL CITIES ATTEMPTED (see FAILED lines above)"
