#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launch_pipeline_stages_0_to_3.sh > applications/SHIELD/logs/launcher.out 2>&1 &
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
#       tail -f applications/SHIELD/logs/<loc>_<calib_code>.out
#       tail -f applications/SHIELD/logs/<loc>_<stage>_chain<n>.out
#
# HOW IT WORKS
#   PHASE 1 (sequential, single-chain stages):
#     For each city, run SEQ_CALIBRATION_CODES in order (stage0 -> stage1 -> stage2).
#     If any stage fails for a city, remaining stages for that city are skipped
#     and the city is recorded in FAILED_CITIES_LOG.
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
#            stages and adds the city to FAILED_CITIES_LOG (phase 2 skip list).
#   Phase 2: a failed chain skips assemble for that city; other cities are unaffected.


# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$SCRIPT_DIR/logs"
mkdir -p "$LOG_DIR"

FAILED_CITIES_LOG="$LOG_DIR/phase1_failed_cities.txt"
: > "$FAILED_CITIES_LOG"   # truncate/create fresh at start of each run


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

# Phase 1: sequential, single-chain stages (one Rscript process per stage,
# so the OS fully reclaims memory between them).
SEQ_SCRIPT="$SCRIPT_DIR/shield_calib_setup_and_run.R"
SEQ_CALIBRATION_CODES=(
    calib.7.5.stage2.az
)
SEQ_MAX_CITIES=20

# Phase 2: parallel, multi-chain stage3.
PAR_SCRIPT="$SCRIPT_DIR/shield_calib_setup_and_run_modular.R"
PAR_CALIBRATION_CODES=(
    calib.7.5.stage3.az
)
N_CHAINS=4
# Peak core usage = PAR_MAX_CITIES x N_CHAINS (e.g. 5 x 4 = 20)
PAR_MAX_CITIES=5


# ── PHASE 1: per-city sequential pipeline (stages 0-2) ─────────────────────────
# Arguments: $1 = city code, $2..$N = calibration code names
run_city_sequential() {
    local loc="$1"
    shift   # drop city arg so "$@" contains only calibration codes
    
    for calib_code in "$@"; do
    
    echo "[$(date '+%F %T')] START   $loc :: $calib_code"
    
    Rscript "$SEQ_SCRIPT" "$loc" "$calib_code" \
    > "$LOG_DIR/${loc}_${calib_code}.out" 2>&1
    local rc=$?
        
        if (( rc != 0 )); then
    echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code (exit $rc) — skipping remaining stages and stage3" >&2
    flock "$FAILED_CITIES_LOG" -c "echo '$loc' >> '$FAILED_CITIES_LOG'"
    return 1
    fi
    
    echo "[$(date '+%F %T')] DONE    $loc :: $calib_code"
    
    done
    
    echo "[$(date '+%F %T')] PHASE1 COMPLETE $loc (stages 0-2)"
}


# ── PHASE 2: per-city parallel-chain pipeline (stage3) ─────────────────────────
# Arguments: $1 = city code, $2 = stage/calibration code
run_city_stage_parallel() {
    local loc="$1"
    local stage="$2"
    
    # PHASE 2a: Setup — blocking on this core
    echo "[$(date '+%F %T')] SETUP   $loc :: $stage"
    Rscript "$PAR_SCRIPT" "$loc" "$stage" setup \
    > "$LOG_DIR/${loc}_${stage}_setup.out" 2>&1
    if (( $? != 0 )); then
    echo "[$(date '+%F %T')] FAILED  $loc :: $stage :: setup" >&2
    return 1
    fi
    echo "[$(date '+%F %T')] SETUP DONE  $loc :: $stage"
    
    # PHASE 2b: Launch chains 2-N_CHAINS in background, run chain 1 on this same core
    chain_pids=()
    for chain in $(seq 2 $N_CHAINS); do
    Rscript "$PAR_SCRIPT" "$loc" "$stage" run "$chain" \
    > "$LOG_DIR/${loc}_${stage}_chain${chain}.out" 2>&1 &
        chain_pids+=($!)
    echo "[$(date '+%F %T')] LAUNCH CHAIN $chain  $loc :: $stage (PID $!)"
    done
    
    # Chain 1 runs blocking on this core (setup's core)
    echo "[$(date '+%F %T')] LAUNCH CHAIN 1  $loc :: $stage (this core)"
    Rscript "$PAR_SCRIPT" "$loc" "$stage" run 1 \
    > "$LOG_DIR/${loc}_${stage}_chain1.out" 2>&1
    local rc1=$?
        
        # Wait for chains 2-N_CHAINS
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
    
    # PHASE 2c: Assemble — blocking on this core
    echo "[$(date '+%F %T')] ASSEMBLE  $loc :: $stage"
    Rscript "$PAR_SCRIPT" "$loc" "$stage" assemble \
    > "$LOG_DIR/${loc}_${stage}_assemble.out" 2>&1
    if (( $? != 0 )); then
    echo "[$(date '+%F %T')] FAILED  $loc :: $stage :: assemble" >&2
    return 1
    fi
    echo "[$(date '+%F %T')] DONE  $loc :: $stage"
}


# ════════════════════════════════════════════════════════════════════════════
# PHASE 1 — run stages 0-2 sequentially for every city (bounded parallelism)
# ════════════════════════════════════════════════════════════════════════════
echo "[$(date '+%F %T')] ===== PHASE 1: stages 0-2 (sequential per city) ====="

running_cities=0
for loc in "${CITIES[@]}"; do

while (( running_cities >= SEQ_MAX_CITIES )); do
wait -n -p done_pid
(( running_cities-- ))
echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running_cities=$running_cities)"
done

run_city_sequential "$loc" "${SEQ_CALIBRATION_CODES[@]}" &
    (( running_cities++ ))
echo "[$(date '+%F %T')] LAUNCHED $loc (PID $!, running_cities=$running_cities)"

done

wait   # block here until EVERY city has finished (or failed) phase 1
echo "[$(date '+%F %T')] ===== PHASE 1 COMPLETE FOR ALL CITIES ====="


# ════════════════════════════════════════════════════════════════════════════
# PHASE 2 — run stage3 (4 parallel chains) for cities that completed phase 1
# ════════════════════════════════════════════════════════════════════════════
echo "[$(date '+%F %T')] ===== PHASE 2: stage3 (parallel chains per city) ====="

# Cities that failed phase 1 are skipped here
mapfile -t FAILED_CITIES < "$FAILED_CITIES_LOG"
if (( ${#FAILED_CITIES[@]} > 0 )); then
    echo "[$(date '+%F %T')] Skipping phase 2 for failed cities: ${FAILED_CITIES[*]}"
    fi
    
    running_cities=0
    for loc in "${CITIES[@]}"; do
    
    # skip cities recorded as failed during phase 1
    skip=0
    for f in "${FAILED_CITIES[@]}"; do
    if [[ "$loc" == "$f" ]]; then skip=1; break; fi
    done
    if (( skip )); then continue; fi
    
    for stage in "${PAR_CALIBRATION_CODES[@]}"; do
    while (( running_cities >= PAR_MAX_CITIES )); do
    wait -n -p done_pid
    (( running_cities-- ))
    echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running_cities=$running_cities)"
    done
    
    run_city_stage_parallel "$loc" "$stage" &
        (( running_cities++ ))
    echo "[$(date '+%F %T')] LAUNCHED $loc :: $stage (PID $!, running_cities=$running_cities)"
    done
    
    done
    
    wait
    echo "[$(date '+%F %T')] ===== ALL CITIES DONE (PHASE 1 + PHASE 2) ====="