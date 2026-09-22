#!/usr/bin/env bash
#
# STEP 2 of 4: RUN
#   Runs chains 1..N_CHAINS for each city, for each calibration code.
#   Run this AFTER launch_setup.sh has completed and you've manually verified
#   the setup was correct.
#
#   If one or more (city, chain) combos fail, do NOT rerun this whole script —
#   use launch_resume_chain.sh with the specific failed (city, chain) pairs.
#
# USAGE
#   nohup bash applications/SHIELD/launchers/launch_run_chains.sh > applications/SHIELD/logs/launcher_run.out 2>&1 &
#
# Kill:
#   pkill -u pkasaie1 -x R
#   pkill -u pkasaie1 -f "Rscript"
#
# Monitor:
#   tail -f applications/SHIELD/logs/launcher_run.out
#   tail -f applications/SHIELD/logs/<loc>_<calib_code>_chain<N>.out
#
# ON FAILURE
#   Check logs/<loc>_<calib_code>_chain<N>.out for the R-level error.
#   Note the city name and chain number, then use launch_resume_chain.sh.

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed chain must not abort the launcher

# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts + logs are one level up
LOG_DIR="$PARENT_DIR/logs"
mkdir -p "$LOG_DIR"

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"

# ── thread settings ────────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ── config: EDIT THESE for each run ────────────────────────────────────────────
allLocs=(
    C.35620 C.33100 C.31080 C.12060 C.26420 C.19100 C.16980 C.47900
    C.37980 C.36740 C.41860 C.38060 C.45300 C.40140 C.19820 C.12580
    C.29820 C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820
    C.42660 C.12420 C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
)

ten_cities=(
    C.12060 C.12580 C.16980 C.26420 C.31080
    C.33100 C.35620 C.37980 C.38060 C.42660
)

shield2_cities=(
    C.12060 C.12580 C.16980 C.26420 C.31080
)
shield1_cities=(
     C.33100 C.37980 C.38060 C.42660
)
shield3_cities=(
    C.12060 C.12580 C.16980 C.26420 C.31080
)

# ── set active cities here ─────────────────────────────────────────────────────
CITIES=("${shield1_cities[@]}")

CALIBRATION_CODES=(
    calib.9.19.stage3
)

N_CHAINS=4   # chains 1..N_CHAINS will be launched for every city x calibration code

# MAX_JOBS = max concurrent Rscript processes on this machine (1 core each).
# Here one job == one chain, so peak cores = MAX_JOBS.
MAX_JOBS=20

SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

# ── preflight ──────────────────────────────────────────────────────────────────
if [[ ! -f "$SCRIPT" ]]; then
    echo "Error: R script not found at $SCRIPT" >&2
    exit 1
fi

# non-empty config guard: `set -u` does NOT catch a typo'd array name
# (an undefined array expands to empty), so check explicitly.
if (( ${#CITIES[@]} == 0 )); then
    echo "Error: CITIES is empty — check the array name in the config block above" >&2
    exit 1
fi
if (( ${#CALIBRATION_CODES[@]} == 0 )); then
    echo "Error: CALIBRATION_CODES is empty — check the array name in the config block above" >&2
    exit 1
fi

# ── per city+calibration code+chain run job ────────────────────────────────────
run_chain() {
    local loc="$1"
    local calib_code="$2"
    local chain="$3"
    echo "[$(date '+%F %T')] START   RUN $loc :: $calib_code :: chain $chain"
    Rscript "$SCRIPT" "$loc" "$calib_code" run "$chain" \
        > "$LOG_DIR/${loc}_${calib_code}_chain${chain}.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  RUN $loc :: $calib_code :: chain $chain (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    RUN $loc :: $calib_code :: chain $chain"
}

# ── job-slot manager ───────────────────────────────────────────────────────────
for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do
        for (( chain=1; chain<=N_CHAINS; chain++ )); do

            wait_for_slot "$MAX_JOBS" jobs

            run_chain "$loc" "$calib_code" "$chain" &
            echo "[$(date '+%F %T')] LAUNCHED RUN $loc :: $calib_code :: chain $chain (PID $!, running_jobs=$(running_slots))"

        done
    done
done

wait
echo "[$(date '+%F %T')] ALL RUN JOBS DONE"
