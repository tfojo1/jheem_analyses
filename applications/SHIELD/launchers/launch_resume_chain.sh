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
#   bash applications/SHIELD/launchers/launch_resume_chain.sh <city> <calib_code> <chain>
#
#   Example:
#       bash applications/SHIELD/launchers/launch_resume_chain.sh C.37980 calib.7.30.stage2.LA.PA 3
#
#   To survive logout:
#       nohup bash applications/SHIELD/launchers/launch_resume_chain.sh C.37980 calib.7.30.stage2.LA.PA 3 \
#           > applications/SHIELD/logs/launcher_resume.out 2>&1 &
#
# Kill:
#   pkill -u pkasaie1 -x R
#   pkill -u pkasaie1 -f "Rscript"
#
# Monitor:
#   tail -f applications/SHIELD/logs/<city>_<calib_code>_chain<chain>.out
#
# NOTE ON LOGGING
#   This appends (>>) to the same log file the original chain run used, so the
#   full history (original failure + resume) stays in one place. Change ">>" to
#   ">" below if you'd rather start a clean log per resume attempt.

# -- resolve paths relative to this script's location --------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# logs live in SHIELD/logs, one level up from SHIELD/launchers (where this
# script lives) -- create it first so the subsequent cd/pwd normalization
# below has something to resolve
mkdir -p "$SCRIPT_DIR/../logs"
LOG_DIR="$(cd "$SCRIPT_DIR/../logs" && pwd)"

SCRIPT="$SCRIPT_DIR/../shield_calib_setup_and_run_modular.R"

# -- args ----------------------------------------------------------------------
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

# -- thread settings -----------------------------------------------------------
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# -- run -----------------------------------------------------------------------
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