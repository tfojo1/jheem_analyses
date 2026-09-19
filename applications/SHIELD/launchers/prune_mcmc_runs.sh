#!/usr/bin/env bash
#
# prune_mcmc_runs.sh - delete old calibration folders from the JHEEM NAS.
#
# 1. WHAT IT DELETES
#    Whole calibration folders at <root>/<CITY>/calib.*/
#    Never individual files, never the city folders themselves.
#
# 2. HOW IT PICKS THEM
#    By folder modification time, NOT by the folder name. The names carry a
#    month and day but no year, so "calib.11.17.stage3.1" could be from 2025
#    or 2026. Do not change this to a name match.
#
# 3. HOW TO RUN IT - two steps
#    a) Dry run. Deletes nothing; writes a list for you to read:
#         ./prune_mcmc_runs.sh
#    b) Read the list, then delete exactly what it contains:
#         ./prune_mcmc_runs.sh --execute --from-list ~/prune_mcmc_preview_....txt
#
# 4. OPTIONS
#    --before YYYY-MM-DD  cutoff date              (default 2026-07-01)
#    --root PATH          which tree to scan       (default SHIELD)
#    --log PATH           where to write the list
#    --from-list FILE     delete exactly what this list contains
#    --protect REGEX      never touch folders matching this
#    --measure            add folder sizes to the list (slow over SMB)
#    --yes                skip the typed DELETE confirmation
#    --no-deep-check      skip the scan for folders currently being written to
#
# 5. SAFETY
#    Dry run is the default. The scan spares folders with recent content inside
#    them and folders it could not read. --execute asks you to type DELETE and
#    logs every removal to your home directory. The NAS has no undo.
#
set -euo pipefail

ROOT='/Volumes/jheem$/mcmc_runs/shield'
CUTOFF='2026-07-01'
EXECUTE=false
ASSUME_YES=false
MEASURE=false
DEEP_CHECK=true
PROTECT_RE=''
FROM_LIST=''
LOGFILE=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --root)      ROOT="$2"; shift 2 ;;
        --before)    CUTOFF="$2"; shift 2 ;;
        --protect)   PROTECT_RE="$2"; shift 2 ;;
        --log)       LOGFILE="$2"; shift 2 ;;
        --from-list) FROM_LIST="$2"; shift 2 ;;
        --execute)   EXECUTE=true; shift ;;
        --yes)       ASSUME_YES=true; shift ;;
        --measure)   MEASURE=true; shift ;;
        --no-deep-check) DEEP_CHECK=false; shift ;;
        -h|--help)   sed -n '2,33p' "$0"; exit 0 ;;
        *) echo "unknown option: $1" >&2; exit 2 ;;
    esac
done

[[ -d "$ROOT" ]] || { echo "root does not exist: $ROOT" >&2; exit 1; }
case "$ROOT" in
    */mcmc_runs/*) : ;;
    *) echo "refusing to run: root is not under an mcmc_runs directory: $ROOT" >&2; exit 1 ;;
esac

CANDIDATES="$(mktemp -t prune_mcmc_list)"
REF="$(mktemp -t prune_mcmc_ref)"
FINDERR="$(mktemp -t prune_mcmc_err)"
trap 'rm -f "$REF" "$CANDIDATES" "$CANDIDATES.tmp" "$FINDERR"' EXIT

# =============================================================================
# Path A: --from-list. Delete exactly what a previous dry run wrote.
# =============================================================================
if [[ -n "$FROM_LIST" ]]; then
    [[ "$EXECUTE" == true ]] || { echo "--from-list only makes sense with --execute" >&2; exit 2; }
    [[ -r "$FROM_LIST" ]]    || { echo "cannot read list: $FROM_LIST" >&2; exit 1; }

    # The list records the root it was generated against. Use it, so a list made
    # for one tree is not silently rejected path-by-path when the default root
    # happens to be another tree.
    LIST_ROOT="$(sed -n 's/^# root  *: //p' "$FROM_LIST" | head -1)"
    if [[ -n "$LIST_ROOT" && "$LIST_ROOT" != "$ROOT" ]]; then
        echo "note: list was generated for root $LIST_ROOT; using that."
        ROOT="$LIST_ROOT"
        [[ -d "$ROOT" ]] || { echo "that root does not exist: $ROOT" >&2; exit 1; }
    fi

    # Every path is re-validated. A list file is not a licence to rm -rf.
    bad=0; n=0
    : > "$CANDIDATES.tmp"
    while IFS= read -r line; do
        [[ -z "$line" || "$line" == \#* ]] && continue
        path="${line#*$'\t'}"; path="${path#*$'\t'}"     # strip date and size columns
        [[ -z "$path" ]] && continue
        if [[ "$path" != "$ROOT"/* ]]; then
            echo "  REJECT (outside root): $path" >&2; bad=$((bad+1)); continue
        fi
        rel="${path#"$ROOT"/}"
        if [[ "$(awk -F/ '{print NF}' <<<"$rel")" -ne 2 ]]; then
            echo "  REJECT (not <city>/<calib>): $path" >&2; bad=$((bad+1)); continue
        fi
        if [[ "$(basename "$path")" != calib.* ]]; then
            echo "  REJECT (not a calib folder): $path" >&2; bad=$((bad+1)); continue
        fi
        if [[ ! -d "$path" ]]; then
            echo "  skip (already gone): $path" >&2; continue
        fi
        printf '%s\0' "$path" >> "$CANDIDATES.tmp"; n=$((n+1))
    done < "$FROM_LIST"
    mv "$CANDIDATES.tmp" "$CANDIDATES"
    [[ "$bad" -eq 0 ]] || { echo "refusing to run: $bad rejected entries in $FROM_LIST" >&2; exit 1; }
    COUNT="$n"
    echo "root:     $ROOT"
    echo "from:     $FROM_LIST"
    echo "to delete: $COUNT folder(s)"
    echo
else
# =============================================================================
# Path B: scan.
# =============================================================================
    [[ "$CUTOFF" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || { echo "bad --before, want YYYY-MM-DD: $CUTOFF" >&2; exit 2; }
    if [[ -z "$(find "$ROOT" -mindepth 1 -maxdepth 1 -type d -print -quit 2>/dev/null || true)" ]]; then
        echo "refusing to run: no city folders under $ROOT" >&2; exit 1
    fi

    # Reference file stamped at the cutoff; "older than" == "not newer than" it.
    touch -t "$(echo "$CUTOFF" | tr -d '-')0000" "$REF"

    # mindepth/maxdepth 2 pins us to exactly <root>/<city>/<calib folder>.
    # find exits non-zero if ANY path was unreadable. Over a network share that
    # is routine and must not abort the run, so errors are collected, not fatal.
    find "$ROOT" -mindepth 2 -maxdepth 2 -type d -name 'calib.*' ! -newer "$REF" -print0 \
        > "$CANDIDATES" 2>"$FINDERR" || true

    # A directory's mtime does NOT change when something deep inside it is
    # written - only when an entry is added or removed directly in it. So a
    # folder that looks old can still be receiving writes from a running job.
    # One extra pass finds anything newer than the cutoff and spares its folder.
    if [[ "$DEEP_CHECK" == true ]]; then
        echo "checking for recent content inside old folders..."
        ACTIVE="$(mktemp -t prune_mcmc_active)"
        { find "$ROOT" -mindepth 3 -newer "$REF" -print 2>>"$FINDERR" || true; } \
            | awk -v root="$ROOT" '{
                  rel = substr($0, length(root) + 2)
                  n = index(rel, "/"); if (n == 0) next
                  city = substr(rel, 1, n - 1); rest = substr(rel, n + 1)
                  m = index(rest, "/"); cal = (m ? substr(rest, 1, m - 1) : rest)
                  print root "/" city "/" cal
              }' | sort -u > "$ACTIVE"
        if [[ -s "$ACTIVE" ]]; then
            SPARED="$(tr '\0' '\n' < "$CANDIDATES" | grep -cxFf "$ACTIVE" || true)"
            tr '\0' '\n' < "$CANDIDATES" | grep -vxFf "$ACTIVE" | tr '\n' '\0' > "$CANDIDATES.tmp" || true
            mv "$CANDIDATES.tmp" "$CANDIDATES"
            echo "  spared $SPARED folder(s) containing content newer than $CUTOFF"
        else
            echo "  none found"
        fi
        rm -f "$ACTIVE"

        # If the scan could not read inside a folder, the freshness check above
        # could not run for it. Treat that as "might be active" and spare it,
        # rather than deleting something we were unable to inspect.
        if [[ -s "$FINDERR" ]]; then
            UNREADABLE="$(mktemp -t prune_mcmc_unread)"
            sed -E 's/^find: //; s/: [^:]*$//' "$FINDERR" \
                | awk -v root="$ROOT" '
                      index($0, root "/") != 1 { next }
                      {
                          rel = substr($0, length(root) + 2)
                          n = index(rel, "/"); if (n == 0) next
                          city = substr(rel, 1, n - 1); rest = substr(rel, n + 1)
                          m = index(rest, "/"); cal = (m ? substr(rest, 1, m - 1) : rest)
                          print root "/" city "/" cal
                      }' | sort -u > "$UNREADABLE"
            if [[ -s "$UNREADABLE" ]]; then
                SKIPPED="$(tr '\0' '\n' < "$CANDIDATES" | grep -cxFf "$UNREADABLE" || true)"
                if [[ "$SKIPPED" -gt 0 ]]; then
                    tr '\0' '\n' < "$CANDIDATES" | grep -vxFf "$UNREADABLE" | tr '\n' '\0' > "$CANDIDATES.tmp" || true
                    mv "$CANDIDATES.tmp" "$CANDIDATES"
                    echo "  spared $SKIPPED folder(s) whose contents could not be read"
                fi
            fi
            rm -f "$UNREADABLE"
        fi
    fi

    if [[ -n "$PROTECT_RE" ]]; then
        KEPT="$(tr '\0' '\n' < "$CANDIDATES" | grep -cE "$PROTECT_RE" || true)"
        tr '\0' '\n' < "$CANDIDATES" | grep -vE "$PROTECT_RE" | tr '\n' '\0' > "$CANDIDATES.tmp" || true
        mv "$CANDIDATES.tmp" "$CANDIDATES"
        echo "protected by --protect: $KEPT folder(s)"
    fi

    COUNT="$(tr -dc '\0' < "$CANDIDATES" | wc -c | tr -d ' ')"
    TOTAL="$( { find "$ROOT" -mindepth 2 -maxdepth 2 -type d -name 'calib.*' 2>/dev/null || true; } | wc -l | tr -d ' ')"

    if [[ -s "$FINDERR" ]]; then
        echo "warning: $(wc -l < "$FINDERR" | tr -d ' ') path(s) could not be read while scanning."
        echo "         Any calibration folder they sit under has been spared. First few:"
        sed -n '1,3p' "$FINDERR" | sed 's/^/           /'
        echo
    fi
    echo "root:      $ROOT"
    echo "cutoff:    modified before $CUTOFF (by mtime, not by folder name)"
    echo "selected:  $COUNT of $TOTAL calibration folders"
    echo
    [[ "$COUNT" -eq 0 ]] && { echo "nothing to do."; exit 0; }

    echo "per city:"
    tr '\0' '\n' < "$CANDIDATES" | awk -F/ '{print $(NF-1)}' | sort | uniq -c | sed 's/^/  /'
    echo

    # ---- the reviewable log -------------------------------------------------
    [[ -n "$LOGFILE" ]] || LOGFILE="$HOME/prune_mcmc_preview_$(date +%Y%m%d_%H%M%S).txt"
    {
        echo "# prune_mcmc_runs.sh preview"
        echo "# generated : $(date '+%Y-%m-%d %H:%M:%S')"
        echo "# root      : $ROOT"
        echo "# cutoff    : mtime before $CUTOFF"
        echo "# folders   : $COUNT of $TOTAL"
        [[ "$MEASURE" == true ]] || echo "# (sizes not measured; re-run with --measure for a size column)"
        echo "#"
        echo "# Review this file, then delete exactly what it lists with:"
        echo "#   $0 --execute --from-list $LOGFILE"
        echo "#"
        echo "# mtime<TAB>size<TAB>path"
    } > "$LOGFILE"

    if [[ "$MEASURE" == true ]]; then echo "measuring sizes (slow over SMB)..."; fi
    BYTES=0
    while IFS= read -r -d '' d; do
        when="$(stat -f '%Sm' -t '%Y-%m-%d' "$d" 2>/dev/null || echo '?')"
        if [[ "$MEASURE" == true ]]; then
            kb="$(du -sk "$d" 2>/dev/null | cut -f1)"; kb="${kb:-0}"
            BYTES=$(( BYTES + kb ))
            sz="$(awk -v k="$kb" 'BEGIN{ if(k>1048576) printf "%.1fG", k/1048576; else if(k>1024) printf "%.0fM", k/1024; else printf "%dK", k }')"
        else
            sz="-"
        fi
        printf '%s\t%s\t%s\n' "$when" "$sz" "$d" >> "$LOGFILE"
    done < "$CANDIDATES"

    sort -t$'\t' -k1,1 -o "$LOGFILE.s" <(grep -v '^#' "$LOGFILE") 2>/dev/null || true
    if [[ -s "$LOGFILE.s" ]]; then
        { grep '^#' "$LOGFILE"; cat "$LOGFILE.s"; } > "$LOGFILE.n" && mv "$LOGFILE.n" "$LOGFILE"
        rm -f "$LOGFILE.s"
    fi

    echo "oldest and newest selected:"
    grep -v '^#' "$LOGFILE" | sed -n '1p;$p' | awk -F'\t' '{print "  " $1 "  " $3}'
    echo
    if [[ "$MEASURE" == true ]]; then
        echo "would free roughly: $(( BYTES / 1024 / 1024 )) GiB"
        echo
    fi

    echo "Review log written to:"
    echo "  $LOGFILE"
    echo
    if [[ "$EXECUTE" != true ]]; then
        echo "DRY RUN - nothing deleted."
        echo "When the log looks right:"
        echo "  $0 --execute --from-list $LOGFILE"
        exit 0
    fi
fi

# =============================================================================
# Delete
# =============================================================================
if [[ "$ASSUME_YES" != true ]]; then
    echo "About to permanently delete $COUNT folders under $ROOT."
    echo "This is a shared network share. There is no trash and no undo."
    printf 'Type DELETE to proceed: '
    read -r reply
    [[ "$reply" == "DELETE" ]] || { echo "aborted."; exit 1; }
fi

DELLOG="$HOME/prune_mcmc_deleted_$(date +%Y%m%d_%H%M%S).log"
echo "logging deletions to $DELLOG"
ok=0; fail=0
while IFS= read -r -d '' d; do
    if rm -rf -- "$d" 2>>"$DELLOG"; then
        echo "DELETED $d" >> "$DELLOG"; ok=$((ok+1))
    else
        echo "FAILED  $d" >> "$DELLOG"; fail=$((fail+1))
    fi
    printf '\r  deleted %d/%d' "$ok" "$COUNT"
done < "$CANDIDATES"
printf '\n'
echo "done: $ok deleted, $fail failed. Log: $DELLOG"
df -h "$ROOT" | tail -1
