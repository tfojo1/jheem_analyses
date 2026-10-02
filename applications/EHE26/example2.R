# Minimal example: R crashes (segfault) when running any intervention from 2026
#
# 1. WHAT HAPPENS
#    noint$run() kills R with:
#      *** caught segfault ***
#      address 0xfffffffffffffff9, cause 'invalid permissions'
#      1: populate_outcomes_array(...)
#
# 2. SETUP WHERE IT HAPPENS
#    R 4.5.2 on Mac, jheem2 sourced from ../jheem2 (branch dev; master has the same code)
#    Simset: ehe / final.ehe / Baltimore (C.12580), read from /Volumes/jheem$
#    Run time: about 1 minute. Nothing is saved to disk.
#
# 3. WHERE THE BUG IS: jheem2/src/outcomes.cpp, populate_outcomes_array()
#    1. 'aids.diagnoses' is only tracked for 1970-2005.
#    2. A run starting in 2026 calculates no new years for it, so new_times is empty.
#    3. This line runs even when new_times is empty, and reads new_times[-1]:
#         if (new_times.length()>0)
#             first_new_time = new_times[0];
#         last_new_time = new_times[new_times.length()-1];
#    4. On R 4.5 an empty vector's data pointer is 1, so reading one slot before it
#       hits address 1 - 8 = 0xffffffffquit()fffffff9, which is the crash address.
#
# 4. SUGGESTED FIX: add braces so both lines are inside the if
#         if (new_times.length()>0)
#         {
#             first_new_time = new_times[0];
#             last_new_time = new_times[new_times.length()-1];
#         }

# Working directory must be jheem_analyses/
source('../jheem_analyses/applications/EHE/ehe_specification.R')

# 1. LOAD A CALIBRATED SIMSET AND KEEP 2 SIMULATIONS ----
simset = retrieve.simulation.set(version = 'ehe',
                                 location = 'C.12580',
                                 calibration.code = 'final.ehe',
                                 n.sim = 100)
simset = simset$thin(n = 2) # 50 sims

# 2. RUN NO INTERVENTION FROM 2026 -> R CRASHES HERE ----
noint = get.null.intervention()
sim.noint = noint$run(simset, start.year = 2026, end.year = 2035)

print("No crash: the bug is fixed")
