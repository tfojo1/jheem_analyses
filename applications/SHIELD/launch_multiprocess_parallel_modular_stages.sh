#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launch_multiprocess_parallel_modular_stages.sh > applications/SHIELD/logs/launcher.out 2>&1 &
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
#   Check a specific city+stage log:
#       tail -f applications/SHIELD/logs/xxxx
#
# HOW IT WORKS
#   Each city gets its own subshell that runs all stages sequentially.
#   The outer loop keeps at most MAX_CITIES subshells alive at once.
#   Within each city: setup runs blocking on core 1, then chains 2-4 launch
#   in parallel while chain 1 runs on the same core as setup. Assemble runs
#   blocking after all 4 chains complete. Peak usage = MAX_CITIES x 4 cores.
#
# ON FAILURE
#   A failed chain skips assemble and releases its city slot.
#   All other cities keep running_cities untouched.
#   Check logs/<loc>_<stage>_chain<n>.out for the R-level error message.


# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$SCRIPT_DIR/logs"
mkdir -p "$LOG_DIR"


# ── thread settings ────────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1


# ── config ─────────────────────────────────────────────────────────────────────
# allLocs=(
#     C.35620 C.33100 C.31080 C.12060 C.26420 C.19100 C.16980 C.47900
#     C.37980 C.36740 C.41860 C.38060 C.45300 C.40140 C.19820 C.12580
#     C.29820 C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820
#     C.42660 C.12420 C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
# )
# 
# ten_cities=(
#     C.12060 C.12580 C.16980 C.26420 C.31080
#     C.33100 C.35620 C.37980 C.38060 C.42660
# )
# 
# five_cities=(
#     C.12060 C.12580 C.16980 C.26420 C.31080
# )
# 
# all_except_ten_cities=(
#     C.19100 C.47900 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820
#     C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.12420
#     C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
# )

# at the locations here
CITIES=(
    C.12060 C.12580 C.16980 C.26420 C.31080
    C.33100 C.35620 C.37980 C.38060 C.42660
)
CALIBRATION_CODES=(
    calib.5.12.stage3.pk
)
N_CHAINS=4
SCRIPT="$SCRIPT_DIR/shield_calib_setup_and_run_modular.R"

# Peak core usage = MAX_CITIES x N_CHAINS (e.g. 20/4 = 5)
# each city would require four cores to run 4 parallel chains and we limit total cores to 20 to leave some cores open for Rscript work
MAX_CITIES=5 

# ── per-city orchestration ─────────────────────────────────────────────────────
run_city_stage() {
    local loc="$1"
    local stage="$2"

    # PHASE 1: Setup — blocking on this core
    echo "[$(date '+%F %T')] SETUP   $loc :: $stage"
    Rscript "$SCRIPT" "$loc" "$stage" setup \
        > "$LOG_DIR/${loc}_${stage}_setup.out" 2>&1
    if (( $? != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $stage :: setup" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] SETUP DONE  $loc :: $stage"

    # PHASE 2: Launch chains 2-xxx in background, run chain 1 on this same core
    chain_pids=()
    for chain in $(seq 2 $N_CHAINS); do
        Rscript "$SCRIPT" "$loc" "$stage" run "$chain" \
            > "$LOG_DIR/${loc}_${stage}_chain${chain}.out" 2>&1 &
        chain_pids+=($!)
        echo "[$(date '+%F %T')] LAUNCH CHAIN $chain  $loc :: $stage (PID $!)"
    done

    # Chain 1 runs blocking on this core (setup's core)
    echo "[$(date '+%F %T')] LAUNCH CHAIN 1  $loc :: $stage (this core)"
    Rscript "$SCRIPT" "$loc" "$stage" run 1 \
        > "$LOG_DIR/${loc}_${stage}_chain1.out" 2>&1
    local rc1=$?

    # Wait for chains 2-NCHAINS
    local any_failed=$(( rc1 != 0 ))
    for pid in "${chain_pids[@]}"; do
        wait "$pid"
        if (( $? != 0 )); then any_failed=1; fi
    done

    if (( any_failed )); then
        echo "[$(date '+%F %T')] SKIPPING ASSEMBLE $loc :: $stage (chain failure)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] ALL CHAINS DONE  $loc :: $stage"

    # PHASE 3: Assemble — blocking on this core
    echo "[$(date '+%F %T')] ASSEMBLE  $loc :: $stage"
    Rscript "$SCRIPT" "$loc" "$stage" assemble \
        > "$LOG_DIR/${loc}_${stage}_assemble.out" 2>&1
    if (( $? != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $stage :: assemble" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE  $loc :: $stage"
}


# ── job-slot manager (city-level) ──────────────────────────────────────────────
running_cities=0
for loc in "${CITIES[@]}"; do
    for stage in "${CALIBRATION_CODES[@]}"; do
        while (( running_cities >= MAX_CITIES )); do
            wait -n -p done_pid
            (( running_cities-- ))
            echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running_cities=$running_cities)"
        done

        run_city_stage "$loc" "$stage" &
        (( running_cities++ ))
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $stage (PID $!, running_cities=$running_cities)"
    done
done

wait
echo "[$(date '+%F %T')] ALL CITIES DONE"