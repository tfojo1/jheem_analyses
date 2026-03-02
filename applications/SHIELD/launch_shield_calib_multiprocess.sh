#!/usr/bin/env bash
locs=(C.12580 C.12060 C.35620 C.33100 C.31080 C.26420 C.38060 C.16980)
script="./shield_calib_setup_and_run_multiprocess.R"
mkdir -p logs
for loc in "${locs[@]}"; do
echo "Launching $loc"
Rscript "$script" "$loc" >"logs/${loc}.out" 2>&1 &
done
wait
echo "All jobs finished"