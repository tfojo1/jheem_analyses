#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launchers/launch_multiChain_stage3.sh > applications/SHIELD/logs/launcher.out 2>&1 &
#   Kill Runs:
#       pkill -u pkasaie1 -x R
#       pkill -u pkasaie1 -f "Rscript"
#
#   Connect to server via SSH:
#       ssh username@10.253.170.91  (SHIELD1)
#       ssh username@10.253.170.89  (SHIELD2)
#
#   Monitor overall progress:
#       tail -f applications/SHIELD/logs/launcher.out
#
#   Check a specific city+calibration code log:
#       tail -f applications/SHIELD/logs/<loc>_<calib_code>_setup.out
#       tail -f applications/SHIELD/logs/<loc>_<calib_code>_chain<n>.out
#       tail -f applications/SHIELD/logs/<loc>_<calib_code>_assemble.out
#
# HOW IT WORKS
#   Each city gets its own subshell that runs all phases sequentially.
#   The outer loop keeps at most MAX_CITIES subshells alive at once.
#   Within each city: setup runs blocking on core 1, then chains 2-N_CHAINS launch
#   in parallel while chain 1 runs on the same core as setup. Assemble runs
#   blocking after all N_CHAINS chains complete. Peak cores = MAX_CITIES x N_CHAINS.
#
# ON FAILURE
#   A failed chain skips assemble and releases its city slot.
#   All other cities keep running.
#   Check logs/<loc>_<calib_code>_chain<n>.out for the R-level error message.

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed city/chain must not abort the launcher

# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts + logs are one level up
LOG_DIR="$PARENT_DIR/logs"
mkdir -p "$LOG_DIR"

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"
SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

# ── thread settings ────────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ── config ─────────────────────────────────────────────────────────────────────
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

shield3_cities=(
    C.12060 C.12580 C.16980 C.26420 C.31080
    C.33100 C.37980 
)
shield1_cities=(
    C.38060 C.42660
)

N_CHAINS=4

# ── set active cities and calibration codes here ───────────────────────────────
# MAX_CITIES = max cities in flight at once. Each city holds N_CHAINS cores, so
# peak cores = MAX_CITIES x N_CHAINS (5 x 4 = 20), leaving headroom on a 24-core box.
MAX_CITIES=7

CITIES=("${shield1_cities[@]}")

CALIBRATION_CODES=(
    calib.9.22.stage3
)

# ── preflight ──────────────────────────────────────────────────────────────────
if [[ ! -f "$SCRIPT" ]]; then
    echo "[$(date '+%F %T')] Error: R script not found at $SCRIPT" >&2
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



# ── per-city orchestration ─────────────────────────────────────────────────────
run_city_calib_code() {
    local loc="$1"
    local calib_code="$2"
    local rc rc1 rc_chain any_failed
    local -a chain_pids=()

    # PHASE 1: Setup — blocking on this core
    echo "[$(date '+%F %T')] SETUP   $loc :: $calib_code"
    Rscript "$SCRIPT" "$loc" "$calib_code" setup \
        > "$LOG_DIR/${loc}_${calib_code}_setup.out" 2>&1
    rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code :: setup (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] SETUP DONE  $loc :: $calib_code"

    # PHASE 2: Launch chains 2-N_CHAINS in background, run chain 1 on this same core
    for (( chain=2; chain<=N_CHAINS; chain++ )); do
        Rscript "$SCRIPT" "$loc" "$calib_code" run "$chain" \
            > "$LOG_DIR/${loc}_${calib_code}_chain${chain}.out" 2>&1 &
        chain_pids+=("$!")
        echo "[$(date '+%F %T')] LAUNCH CHAIN $chain  $loc :: $calib_code (PID $!)"
    done

    # Chain 1 runs blocking on this core (setup's core)
    echo "[$(date '+%F %T')] LAUNCH CHAIN 1  $loc :: $calib_code (this core)"
    Rscript "$SCRIPT" "$loc" "$calib_code" run 1 \
        > "$LOG_DIR/${loc}_${calib_code}_chain1.out" 2>&1
    rc1=$?

    # Wait for chains 2-N_CHAINS
    any_failed=$(( rc1 != 0 ))
    for pid in "${chain_pids[@]}"; do
        wait "$pid"
        rc_chain=$?
        if (( rc_chain != 0 )); then any_failed=1; fi
    done

    if (( any_failed )); then
        echo "[$(date '+%F %T')] SKIPPING ASSEMBLE $loc :: $calib_code (chain failure)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] ALL CHAINS DONE  $loc :: $calib_code"

    # PHASE 3: Assemble — blocking on this core
    echo "[$(date '+%F %T')] ASSEMBLE  $loc :: $calib_code"
    Rscript "$SCRIPT" "$loc" "$calib_code" assemble \
        > "$LOG_DIR/${loc}_${calib_code}_assemble.out" 2>&1
    rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code :: assemble (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE  $loc :: $calib_code"
}

# ── job-slot manager (city-level) ──────────────────────────────────────────────
for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do

        wait_for_slot "$MAX_CITIES" cities

        run_city_calib_code "$loc" "$calib_code" &
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $calib_code (PID $!, running_cities=$(running_slots))"

    done
done

wait
echo "[$(date '+%F %T')] ALL CITIES DONE"
