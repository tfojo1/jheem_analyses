#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launchers/launch_parallel_stages.sh > /dev/null 2>&1 &
#   Kill Runs:
#       pkill -u pkasaie1 -x R
#       pkill -u pkasaie1 -f "Rscript"
#
#   Connect to server via SSH:
#       ssh username@10.253.170.91  (SHIELD1)
#       ssh username@10.253.170.89  (SHIELD2)
#
#   Monitor overall progress:
#       tail -f /home/jheem-shared/logs/launcher_parallel_<user>.out
#
#   Check a specific city+calibration log:
#       tail -f /home/jheem-shared/logs/C.19100_calib.5.6.stage3.2.p2.out
#
# HOW IT WORKS
#   Each city+calibration code combination gets its own subshell.
#   The outer loop keeps at most MAX_JOBS subshells alive at once.
#   When a city finishes (or fails early), its slot is freed and the
#   next city launches. Memory is fully released between calibration codes
#   because each is a separate Rscript process.
#   the order of calibration code is not guaranteed: if CALIBRATION_CODES had >1 entry, multiple codes for the same city could run concurrently
#
# ON FAILURE
#   The failed city prints to stderr and releases its slot.
#   All other cities keep running.
#   Check /home/jheem-shared/logs/<loc>_<calibration_code>.out for the R-level error message.

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
#   2. Watch with:   tail -f $LOG_DIR/launcher_parallel_<user>.out
#   3. Run in the foreground and the output stays on your screen instead; the
#      guard below only redirects when stdout is not a terminal.
#   4. TO REVERT: comment the exec line and put the redirect back on the launch
#      command:  nohup bash <this script> > applications/SHIELD/logs/launcher.out 2>&1 &
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_parallel_${USER:-$(id -un)}.out" 2>&1

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

all_except_ten_cities=(
    C.19100 C.47900 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820
    C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.12420
    C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
)


two_cities=(
    C.37980
)
# ── set active cities and calibration codes here ───────────────────────────────
# MAX_JOBS = max concurrent Rscript processes on this machine (1 core each).
MAX_JOBS=20

CITIES=("${two_cities[@]}")

CALIBRATION_CODES=(
    calib.7.30.stage2.LA.PA
)


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

# ── per city+calibration code runner ──────────────────────────────────────────
run_calib_code() {
    local loc="$1"
    local calib_code="$2"
    echo "[$(date '+%F %T')] START   $loc :: $calib_code"
    Rscript "$SCRIPT" "$loc" "$calib_code" all \
        > "$LOG_DIR/${loc}_${calib_code}.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  $loc :: $calib_code (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    $loc :: $calib_code"
}

# ── job-slot manager ───────────────────────────────────────────────────────────
for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do

        wait_for_slot "$MAX_JOBS" jobs

        run_calib_code "$loc" "$calib_code" &
        echo "[$(date '+%F %T')] LAUNCHED $loc :: $calib_code (PID $!, running_jobs=$(running_slots))"

    done
done

wait
echo "[$(date '+%F %T')] ALL CITIES DONE"
