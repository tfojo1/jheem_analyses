# CROI 2026 abstract: frozen analysis

This folder holds the CROI abstract analysis exactly as it was run: the code from commit `0845c7e1`, before the manuscript workflow in `../intervention/` was reorganised. Keep it unchanged so the abstract's numbers can be re-created.

The only edits are the `source()` paths, which point at the copies in this folder instead of their old locations. Everything else is line for line the committed code.

## What was used

1. Calibration `calib.9.23.stage3.pk`, 400 simulations.
2. Interventions: no intervention, and doxy-PEP coverage 5% to 50% in steps of 5.
3. Outcome names from before the renaming: `pct_incidence_averted`, `rate_incidence_averted_per_pop`, `num_cum_incidence_averted`, and so on.

## Files

| File | What it does | Was |
|---|---|---|
| `01_generate_results_arrays.R` | simsets → `total_raw_results`, `sex_raw_results` | `results/generate_results_arrays.R` |
| `02_generate_custom_outcomes.R` | raw arrays → `total_calc_results`, `sex_calc_results` | `results/generate_custom_outcomes.R` |
| `03_croi26_outputs.R` | the CROI heatmap and Table S1 | `intervention/croi26.R` |
| `intervention_helper_functions.R` | table and figure functions used by step 03 | `intervention/intervention_helper_functions.R` |
| `shield_output_paths.R` | output folders, used by steps 01 and 02 | `shield_output_paths.R` |

Shared files that are not copied, because they are part of the model and were not changed: `commoncode/locations_of_interest.R` and `applications/SHIELD/shield_specification.R`.

## Re-creating the abstract outputs

The arrays are already on the drive, in `<root>/shield/outputs/calib.9.23.stage3.pk/` (dated 29 Sep 2026). To re-make the figure and table from them:

1. Start a fresh R session in the `jheem_analyses` folder. These files define functions with the same names as the ones in `../intervention/`, so do not mix the two in one session.
2. Source `applications/SHIELD/shield_specification.R`. Step 03 needs it but does not source it itself (its `source()` line is commented out, as in the original).
3. Run `03_croi26_outputs.R`. It writes `figures/fig_CROI.png` and `tables/S1-impact-cov<c>-<yr>.csv` into the calibration's output folder.

To rebuild the arrays from the simsets as well, run `01_generate_results_arrays.R`, then `02_generate_custom_outcomes.R`, then step 03. Step 01 skips extracts that already exist in `extracts/`. Both steps write over the arrays on the drive, so only do this if you need to.
