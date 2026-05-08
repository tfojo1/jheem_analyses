#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launch_shield_multiprocess.sh > applications/SHIELD/logs/launcher.out 2>&1 &
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
#   Check a specific city+calibname log:
#       tail -f applications/SHIELD/logs/C.19100_calib.4.29.stage2.pk.out
#
# HOW IT WORKS
#   Each (city, calibname) pair is an independent background job.
#   The outer nested loop keeps at most MAX_JOBS jobs alive at once.
#   When any job finishes (or fails), its slot is freed and the
#   next (city, calibname) combination launches.
#
# ON FAILURE
#   The failed job prints to stderr and releases its slot.
#   All other jobs keep running untouched.
#   Check logs/<loc>_<calibname>.out for the R-level error message.


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

# Each calibname is run independently across all cities in parallel.
# Add or remove entries here to control which calibrations are dispatched.
calibnames=(
    calib.5.7.stg0.gtr.16
    calib.5.7.stg0.gtr.18
    calib.5.7.stg0.gtr.20
    calib.5.7.stg0.gtr.22
    calib.5.7.stg0.gtr.24
    calib.5.7.stg0.gtr.26
)

script="$SCRIPT_DIR/shield_calib_setup_and_run.R"
MAX_JOBS=20


# ── per-job worker ─────────────────────────────────────────────────────────────
# Arguments: $1 = city code, $2 = calibname
# Runs a single Rscript call for one (city, calibname) combination.
run_job() {
    local loc="$1"
    local calibname="$2"

    echo "[$(date '+%F %T')] START   $loc :: $calibname"

    Rscript "$script" "$loc" "$calibname" > "$LOG_DIR/${loc}_${calibname}.out" 2>&1
    local rc=$?

    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calibname (exit $rc)" >&2
        return 1
    fi

    echo "[$(date '+%F %T')] DONE    $loc :: $calibname"
}


# ── job-slot manager ───────────────────────────────────────────────────────────
# Dispatches all (city × calibname) combinations as independent parallel jobs,
# keeping at most MAX_JOBS running at once.
running=0

for loc in "${ten_cities[@]}"; do
    for calibname in "${calibnames[@]}"; do

        while (( running >= MAX_JOBS )); do
            wait -n -p done_pid
            (( running-- ))
            echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running=$running)"
        done

        run_job "$loc" "$calibname" &
        (( running++ ))
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $calibname (PID $!, running=$running)"

    done
done

wait
echo "All jobs finished"