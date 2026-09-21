#!/usr/bin/env bash
#
# Shared slot-manager helpers for the SHIELD launchers.
# SOURCED by launch_*.sh -- not meant to be executed directly.
#
# WHY THIS EXISTS
#   Every launcher needs the same "keep at most N background jobs alive" loop.
#   It used to be copy-pasted into each script with a hand-maintained counter
#   (running_jobs++ / running_jobs--). That counter was only decremented while
#   blocking at the cap, so jobs that finished below the cap were never
#   subtracted and the counter drifted upward -- the cap acted as a floor, not
#   a ceiling. These helpers count live jobs from the shell's own job table
#   instead, so the cap is exact.
#
#   Requires bash >= 4.3 for `wait -n` (SHIELD servers run 5.1.8).
#
# NOTE
#   `jobs` reports the job table of the shell that started the background jobs,
#   so wait_for_slot must be called from the launcher's main body -- not from
#   inside a function that itself runs as a background subshell.

# ── running_slots ──────────────────────────────────────────────────────────────
# Echoes the number of background jobs of this shell that are still running.
running_slots() {
    jobs -rp | wc -l | tr -d ' '
}

# ── wait_for_slot <max_slots> [label] ──────────────────────────────────────────
# Blocks until fewer than <max_slots> background jobs are running, then returns
# so the caller can launch one more. <label> only affects the log wording
# (e.g. "jobs" or "cities").
wait_for_slot() {
    local max_slots="$1"
    local label="${2:-jobs}"
    local running
    running="$(running_slots)"
    while (( running >= max_slots )); do
        wait -n
        running="$(running_slots)"
        echo "[$(date '+%F %T')] SLOT FREED (running_${label}=${running}/${max_slots})"
    done
}
