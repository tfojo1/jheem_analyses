#!/usr/bin/env bash
#
# USAGE
#   Launch over SSH (survives logout):
#       nohup bash launch_shield_calib_multiprocess.sh > logs/launcher.out 2>&1 &
# <connect to server via ssh : ssh username@10.253.170.91:8787  (SHIELD1) & ssh username@10.253.170.89:8787 (SHIELD2) 
#
#   Monitor overall progress:
#       tail -f logs/launcher.out
#
#   Check a specific city+stage log:
#       tail -f logs/C.19100_calib.4.24.stage1.az.out
#
# HOW IT WORKS
#   Each city gets its own subshell that runs all stages sequentially.
#   The outer loop keeps at most MAX_JOBS subshells alive at once.
#   When a city finishes (or fails early), its slot is freed and the
#   next city launches. No city ever starts a stage before its own
#   prior stage completes. Memory is fully released between stages
#   because each stage is a separate Rscript process.
#
# ON FAILURE
#   The failed city prints to stderr and releases its slot.
#   All other cities keep running untouched.
#   Check logs/<loc>_<stage>.out for the R-level error message.


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

all_except_selected_cities=(
    C.19100 C.47900 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820
    C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.12420
    C.26900 C.17140 C.18140 C.12940 C.40900 C.17460
)

# Stages run sequentially per city — each is a separate Rscript process
# so the OS fully reclaims memory between them
stages=(
    calib.4.24.stage0.az
    calib.4.24.stage1.az
    calib.4.24.stage2.az
    calib.4.24.stage3.az
)

script="./shield_calib_setup_and_run_multiprocess.R"
MAX_JOBS=15
mkdir -p logs


# ── per-city pipeline ──────────────────────────────────────────────────────────
# Arguments: $1 = city code, $2..$N = stage names
# Runs all stages in sequence for one city.
# If any stage fails, logs the error and skips remaining stages for that city.
run_city() {
    local loc="$1"

    for stage in "$@"; do   # "$@" is now the list of stages: iterate over whatever stages were passed in

        echo "[$(date '+%F %T')] START   $loc :: $stage"

        # Run this stage; redirect both stdout and stderr to the city+stage log file
        Rscript "$script" "$loc" "$stage" > "logs/${loc}_${stage}.out" 2>&1
        local rc=$?          # capture exit code (0 = success, non-zero = failure)

        if (( rc != 0 )); then
            # >&2 sends this message to stderr so it appears in launcher.out
            # separately from the per-city log files
            echo "[$(date '+%F %T')] FAILED  $loc :: $stage (exit $rc) — skipping remaining stages" >&2
            return 1         # exit the function early; remaining stages for this city are skipped
        fi

        echo "[$(date '+%F %T')] DONE    $loc :: $stage"

    done

    echo "[$(date '+%F %T')] COMPLETE $loc (all stages)"
}


# ── job-slot manager ───────────────────────────────────────────────────────────
# Keeps at most MAX_JOBS city-pipelines running at once.
# Uses bash 5.1+ wait -n -p (available on RHEL 9 / bash 5.1.8).
running=0

for loc in "${all_except_selected_cities[@]}"; do

    # Before launching a new city, check if we are already at the limit.
    # If yes, stay in this while-loop and keep waiting.
    while (( running >= MAX_JOBS )); do

        # Block until ANY ONE background job finishes.
        # -n  : wait for the next single job to finish, not all of them
        # -p  : capture the PID of whichever job just finished into done_pid
        wait -n -p done_pid

        # One slot has opened — decrement the counter
        (( running-- ))

        echo "[$(date '+%F %T')] SLOT FREED (PID $done_pid, running=$running)"

        # If still at the limit, loop back and wait for another job.
        # If under the limit, exit the while-loop and launch the next city.
    done

    # A slot is open — launch this city's full pipeline as a background subshell.
    # Passing stages as arguments so run_city does not depend on the global array name.
    # The & sends it to the background so the outer for-loop can continue immediately.
    run_city "$loc" "${stages[@]}" &

    # $! is always the PID of the last backgrounded process
    (( running++ ))
    echo "[$(date '+%F %T')] LAUNCHED $loc (PID $!, running=$running)"

done

# The for-loop is exhausted (all cities launched) but up to MAX_JOBS
# pipelines may still be running. Wait here until every last one finishes.
wait
echo "All jobs finished"