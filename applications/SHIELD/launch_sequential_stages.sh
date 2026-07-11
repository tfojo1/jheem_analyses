#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launch_sequential_stages.sh > applications/SHIELD/logs/launcher.out 2>&1 &
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
#       tail -f applications/SHIELD/logs/C.19100_calib.5.7.stage0.pk.out
#
# HOW IT WORKS
#   Each city gets its own subshell that runs all calibration codes sequentially.
#   The outer loop keeps at most MAX_CITIES subshells alive at once.
#   When a city finishes (or fails early), its slot is freed and the
#   next city launches. No city ever starts a calibration code before its own
#   prior one completes. Memory is fully released between calibration codes
#   because each is a separate Rscript process.
#
# ON FAILURE
#   The failed city prints to stderr and releases its slot.
#   All other cities keep running_cities untouched.
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

# Calibration codes run sequentially per city — each is a separate Rscript process
# so the OS fully reclaims memory between them
CALIBRATION_CODES=(
    calib.7.10.stage0.pk
    calib.7.10.stage1.pk
)

SCRIPT="$SCRIPT_DIR/shield_calib_setup_and_run.R"
MAX_CITIES=20


# ── per-city pipeline ──────────────────────────────────────────────────────────
# Arguments: $1 = city code, $2..$N = calibration code names
# Runs all calibration codes in sequence for one city.
# If any calibration code fails, logs the error and skips remaining ones for that city.
run_city() {
    local loc="$1"
    shift   # drop city arg so "$@" contains only calibration codes

    for calib_code in "$@"; do

        echo "[$(date '+%F %T')] START   $loc :: $calib_code"

        Rscript "$SCRIPT" "$loc" "$calib_code" \
            > "$LOG_DIR/${loc}_${calib_code}.out" 2>&1
        local rc=$?

        if (( rc != 0 )); then
            echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code (exit $rc) — skipping remaining calibration codes" >&2
            return 1
        fi

        echo "[$(date '+%F %T')] DONE    $loc :: $calib_code"

    done

    echo "[$(date '+%F %T')] COMPLETE $loc (all calibration codes)"
}


# ── job-slot manager ───────────────────────────────────────────────────────────
running_cities=0

for loc in "${CITIES[@]}"; do

    while (( running_cities >= MAX_CITIES )); do
        wait -n -p done_pid
        (( running_cities-- ))
        echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running_cities=$running_cities)"
    done

    run_city "$loc" "${CALIBRATION_CODES[@]}" &
    (( running_cities++ ))
    echo "[$(date '+%F %T')] LAUNCHED $loc (PID $!, running_cities=$running_cities)"

done

wait
echo "[$(date '+%F %T')] ALL CITIES DONE"
