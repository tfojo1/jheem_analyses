#!/usr/bin/env bash
# setting threads for OpenBLAS, OpenMP and Intel MKL (Math Kernel Library)
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1         
export MKL_NUM_THREADS=1         

allLocs=(C.35620 C.33100 C.31080 C.12060 C.26420 C.19100 C.16980 C.47900 C.37980 C.36740 C.41860 C.38060 C.45300 C.40140 C.19820 C.12580 C.29820 C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.42660 C.12420 C.26900 C.17140 C.18140 C.12940 C.40900 C.17460)
selected_cities=(C.12580 C.12060 C.35620 C.33100 C.31080 C.26420 C.38060 C.16980)
all_except_selected_cities=(C.19100 C.47900 C.37980 C.36740 C.41860 C.45300 C.40140 C.19820 C.29820 C.14460 C.41740 C.16740 C.41700 C.27260 C.35380 C.32820 C.42660 C.12420 C.26900 C.17140 C.18140 C.12940 C.40900 C.17460)
phoenix=(C.38060)

script="./shield_calib_setup_and_run_multiprocess.R"
mkdir -p logs

for loc in "${all_except_selected_cities[@]}"; do
echo "Launching $loc"
Rscript "$script" "$loc" >"logs/${loc}.out" 2>&1 &
done
wait
echo "All jobs finished"

#pkill -u pkasaie1 -f "Rscript"