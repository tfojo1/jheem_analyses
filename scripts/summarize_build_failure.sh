#!/usr/bin/env bash
# Summarize a failed manager build for the run's summary page: the R error that
# stopped the most recently written of the given logs (the failing step's, when that
# step writes one), with the lines just before it; otherwise the end of that log.
#
#   bash scripts/summarize_build_failure.sh _ci_outputs/build_log.txt [other logs ...] >> "$GITHUB_STEP_SUMMARY"
#
# Logs that don't exist are ignored.
set -uo pipefail

newest=""
for log in "$@"; do
  [[ -f "$log" ]] || continue
  if [[ -z "$newest" || "$log" -nt "$newest" ]]; then newest="$log"; fi
done

echo "## Build failed"
echo
if [[ -z "$newest" ]]; then
  echo "The build stopped before writing any of its logs, so the failure is in an earlier step"
  echo "(checkout, package installation, or downloading inputs). Open the failed step in the run log."
  exit 0
fi

name=$(basename "$newest")
total=$(wc -l < "$newest" | tr -d ' ')
# The error that stopped R is the last one before "Execution halted"; without
# that line, take the first error.
halted=$(grep -n -m1 '^Execution halted' "$newest" | cut -d: -f1)
if [[ -n "$halted" ]]; then
  line=$(head -n "$halted" "$newest" | grep -n '^Error' | tail -n 1 | cut -d: -f1)
else
  line=$(grep -n -m1 '^Error' "$newest" | cut -d: -f1)
fi
if [[ -n "$line" ]]; then
  start=$(( line > 8 ? line - 8 : 1 ))
  echo "The R error in \`$name\` is on line $line of $total:"
  echo
  echo '```'
  # The lines before the error show what was being processed; the error runs
  # until R's "Execution halted" (at most 25 lines). Very long lines are cut.
  awk -v s="$start" -v e="$line" 'NR >= s && NR < e' "$newest" | cut -c1-1000
  awk -v e="$line" 'NR >= e { print; if (/^Execution halted/ || NR >= e + 24) exit }' "$newest" | cut -c1-1000
  echo '```'
else
  echo "No R error in \`$name\`, the last log written. Its last lines:"
  echo
  echo '```'
  tail -n 20 "$newest" | cut -c1-1000
  echo '```'
fi
echo
echo "The full logs are in this run's artifact."
