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
#   Check a specific city+calibration log:
#       tail -f applications/SHIELD/logs/C.19100_calib.5.6.stage3.2.p2.out
#
# HOW IT WORKS
#   Each city+calibration code combination gets its own subshell.
#   The outer loop keeps at most MAX_JOBS subshells alive at once.
#   When a city finishes (or fails early), its slot is freed and the
#   next city launches. Memory is fully released between calibration codes
#   because each is a separate Rscript process.
#
# ON FAILURE
#   The failed city prints to stderr and releases its slot.
#   All other cities keep running_jobs untouched.
#   Check logs/<loc>_<calibration_code>.out for the R-level error message.


# ── resolve paths relative to this script's location ──────────────────────────
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

all_except_ten_cities=(
    C.19100 C.47900 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820
    C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.12420
    C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
)

# ── set active cities and calibration codes here ───────────────────────────────
CITIES=("${ten_cities[@]}")

CALIBRATION_CODES=(
    calib.5.6.stage3.2.p2
)

SCRIPT="$SCRIPT_DIR/shield_calib_setup_and_run.R"
MAX_JOBS=20

# ── per city+calibration code runner ──────────────────────────────────────────
run_calib_code() {
    local loc="$1"
    local calib_code="$2"
    echo "[$(date '+%F %T')] START   $loc :: $calib_code"
    Rscript "$SCRIPT" "$loc" "$calib_code" \
        > "$LOG_DIR/${loc}_${calib_code}.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    $loc :: $calib_code"
}


# ── job-slot manager ───────────────────────────────────────────────────────────
running_jobs=0

for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do

        while (( running_jobs >= MAX_JOBS )); do
            wait -n -p done_pid
            (( running_jobs-- ))
            echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running_jobs=$running_jobs)"
        done

        run_calib_code "$loc" "$calib_code" &
        (( running_jobs++ ))
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $calib_code (PID $!, running_jobs=$running_jobs)"

    done
done

wait
echo "[$(date '+%F %T')] ALL CITIES DONE"