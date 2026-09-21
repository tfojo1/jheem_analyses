#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash applications/SHIELD/launchers/launch_sequential_stages.sh > applications/SHIELD/logs/launcher.out 2>&1 &
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
#   All other cities keep running.
#   Check logs/<loc>_<calibration_code>.out for the R-level error message.

# ── shell options ──────────────────────────────────────────────────────────────
set -uo pipefail   # `set -e` deliberately omitted: one failed city must not abort the launcher

# ── resolve paths relative to this script's location ──────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"   # launchers live in a subfolder; R scripts + logs are one level up
LOG_DIR="$PARENT_DIR/logs"
mkdir -p "$LOG_DIR"

# ── where runs are written ─────────────────────────────────────────────────────
# mcmc_runs/ and simulations/ are written into the log folder, on local disk,
# rather than onto the NAS. Export JHEEM_ROOT_DIR before launching to send a run
# somewhere else - e.g. JHEEM_ROOT_DIR=/mnt/jheem_nas_share puts it back on the
# NAS. Every stage of a pipeline must use the same value; a run cannot find the
# output of a setup step that wrote elsewhere.
# export JHEEM_ROOT_DIR="${JHEEM_ROOT_DIR:-$LOG_DIR}"

# ── shared helpers ─────────────────────────────────────────────────────────────
source "$SCRIPT_DIR/_shield_slots.sh"

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
CITIES=(C.35620)

# Calibration codes run sequentially per city — each is a separate Rscript process
# so the OS fully reclaims memory between them
CALIBRATION_CODES=(
    calib.9.19.stage0
    calib.9.19.stage1
    calib.9.19.stage2
    calib.9.19.stage3
)

SCRIPT="$PARENT_DIR/shield_calib_setup_and_run_modular.R"

# MAX_CITIES = max cities in flight at once. Each city runs its calibration codes
# sequentially (1 core each), so peak cores = MAX_CITIES.
MAX_CITIES=20

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


# ── per-city pipeline ──────────────────────────────────────────────────────────
# Arguments: $1 = city code, $2..$N = calibration code names
# Runs all calibration codes in sequence for one city.
# If any calibration code fails, logs the error and skips remaining ones for that city.
run_city() {
    local loc="$1"
    shift   # drop city arg so "$@" contains only calibration codes

    for calib_code in "$@"; do

        echo "[$(date '+%F %T')] START   $loc :: $calib_code"

        Rscript "$SCRIPT" "$loc" "$calib_code" all \
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
for loc in "${CITIES[@]}"; do

    wait_for_slot "$MAX_CITIES" cities

    run_city "$loc" "${CALIBRATION_CODES[@]}" &
    echo "[$(date '+%F %T')] LAUNCHED $loc (PID $!, running_cities=$(running_slots))"

done

wait
echo "[$(date '+%F %T')] ALL CITIES DONE"
