#!/usr/bin/env bash
#
# STEP 3 of 4: RESUME FAILED RUNS
#   Reruns ONE specific (city, calibration code, chain) combo that failed
#   during launch_run_chains.sh. All three are passed on the command line.
#
#   run.calibration() picks up from the cached state for that chain (the RUN
#   stage never clears the cache), so this resumes rather than restarting.
#
# USAGE
#   nohup bash applications/SHIELD/launchers/launch_resume_chain.sh <city> <calib_code> <chain> \
#       > /dev/null 2>&1 &
#
#   Example:
#       nohup bash applications/SHIELD/launchers/launch_resume_chain.sh C.37980 calib.7.30.stage2.LA.PA 3 \
#           > /dev/null 2>&1 &
#
#   nohup is what lets the run survive logout. To watch it on your terminal
#   instead, drop the nohup and the redirect - but then it dies with your
#   session, and a resumed chain can take hours.
#
# Kill:
#   pkill -u pkasaie1 -x R
#   pkill -u pkasaie1 -f "Rscript"
#
# Monitor:
#   tail -f /home/jheem-shared/logs/<city>_<calib_code>_chain<chain>.out
#
# NOTE ON LOGGING
#   This appends (>>) to the same log file the original chain run used, so the
#   full history (original failure + resume) stays in one place. Change ">>" to
#   ">" below if you'd rather start a clean log per resume attempt.

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: the exit code is handled explicitly below

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
#   2. Watch with:   tail -f $LOG_DIR/launcher_resume_<user>.out
#   3. Run in the foreground and the output stays on your screen instead; the
#      guard below only redirects when stdout is not a terminal.
#   4. TO REVERT: comment the exec line and put the redirect back on the launch
#      command:  nohup bash <this script> > applications/SHIELD/logs/launcher.out 2>&1 &
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_resume_${USER:-$(id -un)}.out" 2>&1

SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

# ── args ───────────────────────────────────────────────────────────────────────
if (( $# != 3 )); then
    echo "Usage: bash $(basename "$0") <city> <calib_code> <chain>" >&2
    echo "  e.g. bash $(basename "$0") C.37980 calib.7.30.stage2.az 3" >&2
    exit 2
fi

LOC="$1"
CALIB_CODE="$2"
CHAIN="$3"

# chain must be a positive integer
if ! [[ "$CHAIN" =~ ^[1-9][0-9]*$ ]]; then
    echo "Error: chain must be a positive integer, got '$CHAIN'" >&2
    exit 2
fi

# modular R script must exist
if [[ ! -f "$SCRIPT" ]]; then
    echo "Error: R script not found at $SCRIPT" >&2
    exit 1
fi

# ── thread settings ────────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ── run ────────────────────────────────────────────────────────────────────────
LOG_FILE="$LOG_DIR/${LOC}_${CALIB_CODE}_chain${CHAIN}.out"

echo "[$(date '+%F %T')] START   RESUME $LOC :: $CALIB_CODE :: chain $CHAIN"
echo "[$(date '+%F %T')] Logging to $LOG_FILE"

Rscript "$SCRIPT" "$LOC" "$CALIB_CODE" run "$CHAIN" >> "$LOG_FILE" 2>&1
rc=$?

if (( rc != 0 )); then
    echo "[$(date '+%F %T')] FAILED  RESUME $LOC :: $CALIB_CODE :: chain $CHAIN (exit $rc)" >&2
    exit "$rc"
fi

echo "[$(date '+%F %T')] DONE    RESUME $LOC :: $CALIB_CODE :: chain $CHAIN"
