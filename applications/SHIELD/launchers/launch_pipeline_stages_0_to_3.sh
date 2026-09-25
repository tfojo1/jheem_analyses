#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launchers/launch_pipeline_stages_0_to_3.sh > /dev/null 2>&1 &
#   Kill Runs:
#       pkill -u pkasaie1 -x R
#       pkill -u pkasaie1 -f "Rscript"
#
#   Connect to server via SSH:
#       ssh username@10.253.170.91  (SHIELD1)
#       ssh username@10.253.170.89  (SHIELD2)
#
#   Monitor overall progress:
#       tail -f /home/jheem-shared/logs/launcher_pipeline_<user>.out
#
#   Check a specific city+calibration code log:
#       tail -f /home/jheem-shared/logs/<loc>_<calib_code>.out
#       tail -f /home/jheem-shared/logs/<loc>_<calib_code>_chain<n>.out
#
# HOW IT WORKS
#   PHASE 1 (sequential, single-chain calibration codes):
#     For each city, run SEQ_CALIBRATION_CODES in order (stage0 -> stage1 -> stage2).
#     If any calibration code fails for a city, the remaining ones for that city
#     are skipped and the city is recorded in FAILED_CITIES_LOG.
#     Up to SEQ_MAX_CITIES cities run in parallel (1 core each).
#     The script BLOCKS here (`wait`) until every city has finished phase 1.
#
#   PHASE 2 (parallel, multi-chain stage3):
#     Runs only after ALL cities have completed (or failed) phase 1.
#     Cities listed in FAILED_CITIES_LOG are skipped.
#     For each remaining city: setup runs blocking on core 1, chains 2-N_CHAINS
#     launch in parallel while chain 1 runs on the setup core, then assemble
#     runs blocking after all chains complete.
#     Up to PAR_MAX_CITIES cities run in parallel (N_CHAINS cores each).
#
# ON FAILURE
#   Phase 1: a failed calibration code skips the rest of that city's sequential
#            codes and adds the city to FAILED_CITIES_LOG (phase 2 skip list).
#   Phase 2: a failed chain skips assemble for that city; other cities are unaffected.

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed city/chain must not abort the launcher

# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts + logs are one level up
# ── where the logs go ─────────────────────────────────────────────────────────
# Logs are written to the shared folder on this machine, so any member logged
# into this server can follow the runs without going through the repo.
#   1. The folder is shared per machine; it is not shared across machines,
#      so log names from different servers never collide.
#   2. Set JHEEM_LOG_DIR before launching to send one run's logs elsewhere.
#   3. TO REVERT: comment the shared line and uncomment the repo line above it.
# LOG_DIR="$PARENT_DIR/logs"          # previous setup: logs/ inside the repo
LOG_DIR="${JHEEM_LOG_DIR:-/home/jheem-shared/logs}"
mkdir -p "$LOG_DIR"
umask 002   # new log files stay group-readable for the other members

# ── master log ──────────────────────────────────────────────────────────────
# Everything this launcher prints goes to one file inside LOG_DIR, so the launch
# command carries no path of its own and cannot drift away from LOG_DIR.
#   1. Launch with:  nohup bash <this script> > /dev/null 2>&1 &
#      The > /dev/null only stops nohup from creating an empty nohup.out.
#   2. Watch with:   tail -f $LOG_DIR/launcher_pipeline_<user>.out
#   3. Run in the foreground and the output stays on your screen instead; the
#      guard below only redirects when stdout is not a terminal.
#   4. TO REVERT: comment the exec line and put the redirect back on the launch
#      command:  nohup bash <this script> > applications/SHIELD/logs/launcher.out 2>&1 &
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_pipeline_${USER:-$(id -un)}.out" 2>&1

FAILED_CITIES_LOG="$LOG_DIR/phase1_failed_cities.txt"
: > "$FAILED_CITIES_LOG"   # truncate/create fresh at start of each run

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"

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

all_except_ten_cities=(
    C.19100 C.47900 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820
    C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.12420
    C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
)

# ── set active cities and calibration codes here ───────────────────────────────
CITIES=("${ten_cities[@]}")

# Phase 1: sequential, single-chain calibration codes (one Rscript process per
# code, so the OS fully reclaims memory between them).
SEQ_SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"
SEQ_CALIBRATION_CODES=(
    calib.7.5.stage2.az
)
# SEQ_MAX_CITIES = max cities in flight in phase 1 (1 core each) -> peak cores = SEQ_MAX_CITIES.
SEQ_MAX_CITIES=20

# Phase 2: parallel, multi-chain stage3.
PAR_SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"
PAR_CALIBRATION_CODES=(
    calib.7.5.stage3.az
)
N_CHAINS=4
# PAR_MAX_CITIES = max cities in flight in phase 2. Each holds N_CHAINS cores, so
# peak cores = PAR_MAX_CITIES x N_CHAINS (5 x 4 = 20).
PAR_MAX_CITIES=5

# ── preflight ──────────────────────────────────────────────────────────────────
for s in "$SEQ_SCRIPT" "$PAR_SCRIPT"; do
    if [[ ! -f "$s" ]]; then
        echo "Error: R script not found at $s" >&2
        exit 1
    fi
done

# non-empty config guard: `set -u` does NOT catch a typo'd array name
# (an undefined array expands to empty), so check explicitly.
if (( ${#CITIES[@]} == 0 )); then
    echo "Error: CITIES is empty — check the array name in the config block above" >&2
    exit 1
fi
if (( ${#SEQ_CALIBRATION_CODES[@]} == 0 )); then
    echo "Error: SEQ_CALIBRATION_CODES is empty — check the array name in the config block above" >&2
    exit 1
fi
if (( ${#PAR_CALIBRATION_CODES[@]} == 0 )); then
    echo "Error: PAR_CALIBRATION_CODES is empty — check the array name in the config block above" >&2
    exit 1
fi


# ── PHASE 1: per-city sequential pipeline (stages 0-2) ─────────────────────────
# Arguments: $1 = city code, $2..$N = calibration code names
run_city_sequential() {
    local loc="$1"
    shift   # drop city arg so "$@" contains only calibration codes
    local rc

    for calib_code in "$@"; do

        echo "[$(date '+%F %T')] START   $loc :: $calib_code"

        Rscript "$SEQ_SCRIPT" "$loc" "$calib_code" all \
            > "$LOG_DIR/${loc}_${calib_code}.out" 2>&1
        rc=$?

        if (( rc != 0 )); then
            echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code (exit $rc) — skipping remaining codes and stage3" >&2
            flock "$FAILED_CITIES_LOG" -c "echo '$loc' >> '$FAILED_CITIES_LOG'"
            return 1
        fi

        echo "[$(date '+%F %T')] DONE    $loc :: $calib_code"

    done

    echo "[$(date '+%F %T')] PHASE1 COMPLETE $loc (stages 0-2)"
}


# ── PHASE 2: per-city parallel-chain pipeline (stage3) ─────────────────────────
# Arguments: $1 = city code, $2 = calibration code
run_city_parallel() {
    local loc="$1"
    local calib_code="$2"
    local rc rc1 rc_chain any_failed
    local -a chain_pids=()

    # PHASE 2a: Setup — blocking on this core
    echo "[$(date '+%F %T')] SETUP   $loc :: $calib_code"
    Rscript "$PAR_SCRIPT" "$loc" "$calib_code" setup \
        > "$LOG_DIR/${loc}_${calib_code}_setup.out" 2>&1
    rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code :: setup (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] SETUP DONE  $loc :: $calib_code"

    # PHASE 2b: Launch chains 2-N_CHAINS in background, run chain 1 on this same core
    for (( chain=2; chain<=N_CHAINS; chain++ )); do
        Rscript "$PAR_SCRIPT" "$loc" "$calib_code" run "$chain" \
            > "$LOG_DIR/${loc}_${calib_code}_chain${chain}.out" 2>&1 &
        chain_pids+=("$!")
        echo "[$(date '+%F %T')] LAUNCH CHAIN $chain  $loc :: $calib_code (PID $!)"
    done

    # Chain 1 runs blocking on this core (setup's core)
    echo "[$(date '+%F %T')] LAUNCH CHAIN 1  $loc :: $calib_code (this core)"
    Rscript "$PAR_SCRIPT" "$loc" "$calib_code" run 1 \
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

    # PHASE 2c: Assemble — blocking on this core
    echo "[$(date '+%F %T')] ASSEMBLE  $loc :: $calib_code"
    Rscript "$PAR_SCRIPT" "$loc" "$calib_code" assemble \
        > "$LOG_DIR/${loc}_${calib_code}_assemble.out" 2>&1
    rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code :: assemble (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE  $loc :: $calib_code"
}


# ════════════════════════════════════════════════════════════════════════════
# PHASE 1 — run stages 0-2 sequentially for every city (bounded parallelism)
# ════════════════════════════════════════════════════════════════════════════
echo "[$(date '+%F %T')] ===== PHASE 1: stages 0-2 (sequential per city) ====="

for loc in "${CITIES[@]}"; do

    wait_for_slot "$SEQ_MAX_CITIES" cities

    run_city_sequential "$loc" "${SEQ_CALIBRATION_CODES[@]}" &
    echo "[$(date '+%F %T')] LAUNCHED $loc (PID $!, running_cities=$(running_slots))"

done

wait   # block here until EVERY city has finished (or failed) phase 1
echo "[$(date '+%F %T')] ===== PHASE 1 COMPLETE FOR ALL CITIES ====="


# ════════════════════════════════════════════════════════════════════════════
# PHASE 2 — run stage3 (N_CHAINS parallel chains) for cities that passed phase 1
# ════════════════════════════════════════════════════════════════════════════
echo "[$(date '+%F %T')] ===== PHASE 2: stage3 (parallel chains per city) ====="

# Cities that failed phase 1 are skipped here
mapfile -t FAILED_CITIES < "$FAILED_CITIES_LOG"
if (( ${#FAILED_CITIES[@]} > 0 )); then
    echo "[$(date '+%F %T')] Skipping phase 2 for failed cities: ${FAILED_CITIES[*]}"
fi

for loc in "${CITIES[@]}"; do

    # skip cities recorded as failed during phase 1
    skip=0
    for f in "${FAILED_CITIES[@]}"; do
        if [[ "$loc" == "$f" ]]; then skip=1; break; fi
    done
    if (( skip )); then continue; fi

    for calib_code in "${PAR_CALIBRATION_CODES[@]}"; do

        wait_for_slot "$PAR_MAX_CITIES" cities

        run_city_parallel "$loc" "$calib_code" &
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $calib_code (PID $!, running_cities=$(running_slots))"

    done

done

wait
echo "[$(date '+%F %T')] ===== ALL CITIES DONE (PHASE 1 + PHASE 2) ====="
