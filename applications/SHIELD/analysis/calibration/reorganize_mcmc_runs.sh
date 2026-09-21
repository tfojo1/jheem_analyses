#!/usr/bin/env bash
# ****************************************************************************************************
# REORGANIZE MCMC_RUNS TO THE NEW JHEEM2 DIRECTORY LAYOUT
# ****************************************************************************************************
#
# 1. WHAT CHANGED
#    jheem2 commit ccb1f9b (dev branch, 2026-09-21) changed get.calibration.dir()
#    from
#        mcmc_runs/<version>/<location>/<calibration.code>/
#    to
#        mcmc_runs/<version>/<calibration.code>/<location>/
#
#    Calibrations written before that commit sit in the old layout. The updated
#    jheem2 looks in the new one, finds no chain*_control.Rdata, and
#    get.calibration.progress() returns NA for every location. Nothing is
#    corrupt when that happens - the files are just one level out of place.
#
#    Note that mcmc_summaries did NOT change. get.mcmc.summary.file() already
#    used <calibration.code> with the location in the filename, so summaries
#    need no migration.
#
# 2. WHAT THIS DOES
#    Moves each matching <city>/<calibration.code>/ directory to
#    <calibration.code>/<city>/. These are renames within a single filesystem,
#    so no file data is copied - it runs in seconds regardless of size.
#
# 3. SCOPE
#    Only calibration codes matching CALIB_PATTERN are moved. Anything else
#    stays in the old layout and stays invisible to the updated jheem2. Set
#    CALIB_PATTERN='calib.*' to migrate everything.
#
#    A city directory is removed only once it is completely empty, via rmdir,
#    which refuses to delete a directory that still has contents. Calibrations
#    outside CALIB_PATTERN are therefore never at risk.
#
# 4. HOW TO RUN
#    Dry run is the default and changes nothing:
#        bash reorganize_mcmc_runs.sh
#
#    Apply:
#        DRY_RUN=0 bash reorganize_mcmc_runs.sh
#
#    Against a different root, for example a local logs folder:
#        BASE=~/jheem/code/jheem_analyses/applications/SHIELD/logs/mcmc_runs/shield \
#            DRY_RUN=0 bash reorganize_mcmc_runs.sh
#
# 5. BEFORE RUNNING ON THE SHARED DRIVE
#    The NAS is shared. This walks every city directory under BASE, including
#    calibrations belonging to other people. Check with the team before
#    applying it to /mnt/jheem_nas_share.
#
# 6. VERIFY AFTERWARDS
#    In R, with no root.dir argument:
#        for (x in SHIELD.TEN.MSAS)
#            print(get.calibration.progress("shield", x, "calib.9.19.stage0"))
#    Percentages rather than NA means the layout now matches what jheem2 expects.
# ****************************************************************************************************

BASE="${BASE:-/mnt/jheem_nas_share/mcmc_runs/shield}"
CALIB_PATTERN="${CALIB_PATTERN:-calib.8.21.*}"
DRY_RUN="${DRY_RUN:-1}"

log() { printf '%s\n' "$*"; }

if [ ! -d "$BASE" ]; then
    log "ERROR: base directory not found: $BASE"
    exit 1
fi

log "base    : $BASE"
log "pattern : $CALIB_PATTERN"
if [ "$DRY_RUN" = "1" ]; then
    log "mode    : DRY RUN - nothing will be changed"
else
    log "mode    : LIVE - directories will be moved"
fi
log ""

MOVED=0
SKIPPED=0
PROBLEMS=0

# Cities are the C.<code> directories sitting directly under <version>.
for city_dir in "$BASE"/C.*; do

    [ -d "$city_dir" ] || continue
    city=$(basename "$city_dir")

    for calib_dir in "$city_dir"/$CALIB_PATTERN; do

        [ -d "$calib_dir" ] || continue
        calib=$(basename "$calib_dir")

        dest_parent="$BASE/$calib"
        dest="$dest_parent/$city"

        if [ -e "$dest" ]; then
            log "SKIP  $city/$calib  ->  destination already exists: $calib/$city"
            SKIPPED=$((SKIPPED + 1))
            continue
        fi

        log "MOVE  $city/$calib  ->  $calib/$city"

        if [ "$DRY_RUN" != "1" ]; then
            mkdir -p "$dest_parent" || { log "      ERROR: could not create $dest_parent"; PROBLEMS=$((PROBLEMS + 1)); continue; }
            mv "$calib_dir" "$dest"  || { log "      ERROR: move failed";                 PROBLEMS=$((PROBLEMS + 1)); continue; }
        fi

        MOVED=$((MOVED + 1))
    done

    if [ "$DRY_RUN" != "1" ]; then
        rmdir "$city_dir" 2>/dev/null && log "      removed now-empty $city/"
    fi
done

log ""
log "--- Summary ---"
log "  moved    : $MOVED"
log "  skipped  : $SKIPPED"
log "  problems : $PROBLEMS"

if [ "$DRY_RUN" = "1" ]; then
    log ""
    log "Dry run only. Re-run with DRY_RUN=0 to apply."
    exit 0
fi

log ""
log "New layout:"
ls -d "$BASE"/$CALIB_PATTERN 2>/dev/null

[ "$PROBLEMS" -eq 0 ] || exit 1
exit 0
