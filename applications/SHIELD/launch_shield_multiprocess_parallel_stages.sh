#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launch_shield_multiprocess_parallel_stages.sh > applications/SHIELD/logs/launcher.out 2>&1 &
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
#       tail -f applications/SHIELD/logs/C.19100_calib.4.24.stage1.az.out
#
# HOW IT WORKS
#   Each city gets its own subshell that runs all stages sequentially.
#   The outer loop keeps at most MAX_JOBS subshells alive at once.
#   When a city finishes (or fails early), its slot is freed and the
#   next city launches. No city ever starts a stage before its own
#   prior stage completes. Memory is fully released between stages
#   because each stage is a separate Rscript process.
#
# ON FAILURE
#   The failed city prints to stderr and releases its slot.
#   All other cities keep running untouched.
#   Check logs/<loc>_<stage>.out for the R-level error message.


# ── resolve paths relative to this script's location ──────────────────────────
# This ensures logs/ is always created next to the script,
# regardless of which directory you launch from.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$SCRIPT_DIR/logs"
mkdir -p "$LOG_DIR"


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

five_cities=(
    C.12060 C.12580 C.16980 C.26420 C.31080
)

all_except_ten_cities=(
    C.19100 C.47900 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820
    C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.12420
    C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
)

# Stages run sequentially per city — each is a separate Rscript process
# so the OS fully reclaims memory between them
stages=(
    calib.5.6.stage3.2.p2
    calib.5.6.stage3.2.p4
    calib.5.6.stage3.2.p8
    calib.5.6.stage3.4.p2
    calib.5.6.stage3.4.p4
    calib.5.6.stage3.4.p8
    calib.5.6.stage3.8.p2
)

script="$SCRIPT_DIR/shield_calib_setup_and_run.R"
MAX_JOBS=20

run_stage() {
    local loc="$1"
    local stage="$2"
    echo "[$(date '+%F %T')] START   $loc :: $stage"
    Rscript "$script" "$loc" "$stage" > "$LOG_DIR/${loc}_${stage}.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $stage (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    $loc :: $stage"
}

# ── job-slot manager ───────────────────────────────────────────────────────────
running=0
for loc in "${five_cities[@]}"; do
    for stage in "${stages[@]}"; do
        while (( running >= MAX_JOBS )); do
            wait -n -p done_pid
            (( running-- ))
            echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running=$running)"
        done

        run_stage "$loc" "$stage" &
        (( running++ ))
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $stage (PID $!, running=$running)"
    done
done

# Wait for all remaining background jobs to finish
wait
echo "[$(date '+%F %T')] ALL DONE"

# The for-loop is exhausted (all cities launched) but up to MAX_JOBS
# pipelines may still be running. Wait here until every last one finishes.
wait
echo "All jobs finished"