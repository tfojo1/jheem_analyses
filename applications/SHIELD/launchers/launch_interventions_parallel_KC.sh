#!/usr/bin/env bash
#
# KING COUNTY-ALIGNED DOXY-PEP SCENARIOS — SEATTLE (C.42660) ONLY
#   Runs intervention/intervention_run_KC_parallel.R, which runs the four scenarios in
#   intervention/intervention_definitions_KC.R (doxy.kc.30.flat, doxy.kc.30.to50,
#   doxy.kc.45.flat, doxy.kc.45.to50) on the stage 4 posterior (1,000 sims).
#   noint and doxy.cov.5-50 already exist for Seattle and are not rerun.
#   Seattle's stage 4 interventions ran on shield2 (tag shield-paper1-interventions-stage4-run1):
#   use the same jheem2 version so these runs stay comparable.
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launchers/launch_interventions_parallel_KC.sh > /dev/null 2>&1 &
#   Kill Runs:
#       pkill -u pkasaie1 -x R
#       pkill -u pkasaie1 -f "Rscript"
#
#   Connect to server via SSH:
#       ssh username@10.253.170.91  (SHIELD1)
#       ssh username@10.253.170.89  (SHIELD2)
#
#   Monitor overall progress:
#       ls -lt /home/jheem-shared/logs/launcher_interventions_KC_*.out   # newest first
#       tail -f /home/jheem-shared/logs/<the file you want>
#
#   Check a specific city+calibration log:
#       tail -f /home/jheem-shared/logs/<loc>_<calib_code>_<intervention>_interventions_KC.out
#
# ON FAILURE
#   The failed city prints to stderr and releases its slot.
#   All other cities keep running.
#   Check /home/jheem-shared/logs/<loc>_<calibration_code>_<intervention>_interventions_KC.out for the R-level error message.

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
#   2. Find a run:   ls -lt $LOG_DIR/launcher_interventions_*.out
#      Watch it:     tail -f $LOG_DIR/<the file you want>
#   3. Run in the foreground and the output stays on your screen instead; the
#      guard below only redirects when stdout is not a terminal.
#   4. TO REVERT: comment the exec line and put the redirect back on the launch
#      command:  nohup bash <this script> > applications/SHIELD/logs/launcher.out 2>&1 &
# ── this launch's identity ────────────────────────────────────────────────────
# RUN_ID stamps the user and the launch time onto the launcher-level files, so a
# new launch never overwrites a file that a previous, still-running launch is
# still using. List them newest first to find the run you want:
#   ls -lt $LOG_DIR/launcher_interventions_KC_*.out
# TO REVERT: comment RUN_ID and the exec line below it, then uncomment the
# previous exec line.
RUN_ID="${USER:-$(id -un)}_$(date +%Y%m%d_%H%M%S)"
# [[ -t 1 ]] || exec > "$LOG_DIR/launcher_interventions_${USER:-$(id -un)}.out" 2>&1   # previous: one file per user, truncated by each launch
[[ -t 1 ]] || exec > "$LOG_DIR/launcher_interventions_KC_${RUN_ID}.out" 2>&1

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"
SCRIPT="$PARENT_DIR/intervention/intervention_run_KC_parallel.R"   # King County scenarios, Seattle only

# ── thread settings ────────────────────────────────────────────────────────────
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ── config ─────────────────────────────────────────────────────────────────────
# Seattle only: intervention_run_KC_parallel.R stops for any other location
CITIES=(C.42660)

# One Rscript process per scenario: the four scenarios run in parallel (1 core each).
# Each process loads the Seattle base simset, so memory use is about 4 simsets.
MAX_JOBS=4

# Scenario codes, as defined in intervention/intervention_definitions_KC.R
INTERVENTIONS=(
    doxy.kc.30.flat
    doxy.kc.30.to50
    doxy.kc.45.flat
    doxy.kc.45.to50
)

CALIBRATION_CODES=(
    calib.10.20.stage4
)

N_SIM=1000
FIRST_YEAR=2000
LAST_YEAR=2040


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
if (( ${#INTERVENTIONS[@]} == 0 )); then
    echo "Error: INTERVENTIONS is empty — check the array name in the config block above" >&2
    exit 1
fi
if (( ${#CALIBRATION_CODES[@]} == 0 )); then
    echo "Error: CALIBRATION_CODES is empty — check the array name in the config block above" >&2
    exit 1
fi

# ── per city + calibration + scenario runner ──────────────────────────────────
run_one_scenario() {
    local loc="$1"
    local calib_code="$2"
    local int_code="$3"
    echo "[$(date '+%F %T')] START   INTERV $loc :: $calib_code :: $int_code"
    Rscript "$SCRIPT" "$loc" "$calib_code" "$N_SIM" "$FIRST_YEAR" "$LAST_YEAR" "$int_code" \
        > "$LOG_DIR/${loc}_${calib_code}_${int_code}_interventions_KC.out" 2>&1
    local rc=$?
    if (( rc != 0 )); then
        echo "[$(date '+%F %T')] FAILED  INTERV $loc :: $calib_code :: $int_code (exit $rc)" >&2
        return 1
    fi
    echo "[$(date '+%F %T')] DONE    INTERV $loc :: $calib_code :: $int_code"
}

# ── job-slot manager ───────────────────────────────────────────────────────────
for loc in "${CITIES[@]}"; do
    for calib_code in "${CALIBRATION_CODES[@]}"; do
        for int_code in "${INTERVENTIONS[@]}"; do

            wait_for_slot "$MAX_JOBS" jobs

            run_one_scenario "$loc" "$calib_code" "$int_code" &
            echo "[$(date '+%F %T')] LAUNCHED INTERV $loc :: $calib_code :: $int_code (PID $!, running_jobs=$(running_slots))"

        done
    done
done

wait
echo "[$(date '+%F %T')] SEATTLE KC SCENARIOS DONE"
