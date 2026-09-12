#!/usr/bin/env bash
#
# STEP 4 of 4: ASSEMBLE
#   Assembles + saves the simset for each city, for each calibration code.
#   Run this LAST, only after you've confirmed ALL chains are complete for the
#   listed cities -- including any chains you had to resume via
#   launch_resume_chain.sh. The R script uses allow.incomplete = T, so if you
#   run this before every chain is done it will silently assemble a PARTIAL
#   simset. Verify first.
#
# USAGE
#   nohup bash applications/SHIELD/launchers/launch_assemble.sh > applications/SHIELD/logs/launcher_assemble.out 2>&1 &
#
# Kill:
#   pkill -u pkasaie1 -x R
#   pkill -u pkasaie1 -f "Rscript"
#
# Monitor:
#   tail -f applications/SHIELD/logs/launcher_assemble.out
#   tail -f applications/SHIELD/logs/<loc>_<calib_code>_assemble.out

# -- resolve paths relative to this script's location --------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts + logs are one level up
LOG_DIR="$PARENT_DIR/logs"
mkdir -p "$LOG_DIR"

SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

# -- thread settings -----------------------------------------------------------
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# -- config: EDIT THESE for each run -------------------------------------------
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
    C.33100 C.35620 C.37980
)
shield3_cities=(
    C.38060 C.42660
)

# ── set active cities here ─────────────────────────────────────────────────────
CITIES=("${shield2_cities[@]}")

CALIBRATION_CODES=(
    calib.7.30.stage2.LA.PA
)

MAX_JOBS=20

# -- preflight -----------------------------------------------------------------
if [[ ! -f "$SCRIPT" ]]; then
    echo "Error: R script not found at $SCRIPT" >&2
    exit 1
fi

# -- per city+calibration code assemble job ------------------------------------
run_assemble() {
    local loc="$1"
    local calib_code="$2"
    echo "[$(date '+%F %T')] START   ASSEMBLE $loc :: $calib_code"
    Rscript "$SCRIPT" "$loc" "$calib_code" assemble \
        > "$LOG_DIR/${loc}_${calib_code}_assemble.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  ASSEMBLE $loc :: $calib_code (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    ASSEMBLE $loc :: $calib_code"
}

# -- job-slot manager ----------------------------------------------------------
running_jobs=0

for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do

        while (( running_jobs >= MAX_JOBS )); do
            wait -n -p done_pid
            (( running_jobs-- ))
            echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running_jobs=$running_jobs)"
        done

        run_assemble "$loc" "$calib_code" &
        (( running_jobs++ ))
        echo "[$(date '+%F %T')] LAUNCHED ASSEMBLE $loc :: $calib_code (PID $!, running_jobs=$running_jobs)"

    done
done

wait
echo "[$(date '+%F %T')] ALL ASSEMBLE JOBS DONE"