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
#   nohup bash applications/SHIELD/launchers/launch_assemble.sh > /dev/null 2>&1 &
#
# Kill:
#   pkill -u pkasaie1 -x R
#   pkill -u pkasaie1 -f "Rscript"
#
# Monitor:
#   tail -f /home/jheem-shared/logs/launcher_assemble_<user>.out
#   tail -f /home/jheem-shared/logs/<loc>_<calib_code>_assemble.out

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed city must not abort the launcher

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
#   2. Watch with:   tail -f $LOG_DIR/launcher_assemble_<user>.out
#   3. Run in the foreground and the output stays on your screen instead; the
#      guard below only redirects when stdout is not a terminal.
#   4. TO REVERT: comment the exec line and put the redirect back on the launch
#      command:  nohup bash <this script> > applications/SHIELD/logs/launcher.out 2>&1 &
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_assemble_${USER:-$(id -un)}.out" 2>&1

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"

SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

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
    C.33100 C.37980 C.38060 C.42660
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
    calib.9.19.stage3
)

# MAX_JOBS = max concurrent Rscript processes on this machine (1 core each).
MAX_JOBS=20

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

# ── per city+calibration code assemble job ─────────────────────────────────────
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

# ── job-slot manager ───────────────────────────────────────────────────────────
for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do

        wait_for_slot "$MAX_JOBS" jobs

        run_assemble "$loc" "$calib_code" &
        echo "[$(date '+%F %T')] LAUNCHED ASSEMBLE $loc :: $calib_code (PID $!, running_jobs=$(running_slots))"

    done
done

wait
echo "[$(date '+%F %T')] ALL ASSEMBLE JOBS DONE"
