# SHIELD sensitivity analyses (doxy-PEP)

Two analyses of which calibrated parameters drive the 2030 % reduction in incidence with doxy-PEP at
20% coverage vs. no doxy-PEP (same simulation), for the total population, MSM and women, in each MSA.
The original PRCC code (v1) is in `analysis/sensitivity/` and is not changed.

| Folder | Analysis | Run |
|---|---|---|
| `prcc/` | Top vs. bottom 20% of simulations by each parameter (deviation from the baseline median), partial rank correlation coefficients (PRCC), tornado plots per MSA | `run_prcc_all.R` (steps 1-4) |
| `gam/` | Share of each outcome's posterior variance explained by each parameter group (GAM / MARS, `voi`) | scripts `00`-`02`; see `gam/README.md` |
| `shared/` | `sensitivity_setup.R`: package checks, SHIELD base files loaded once per session, `get_param_matrix()` | sourced by each settings file |

## Before the first run

1. Save the calibrated parameters (once per calibration): run `analysis/intervention/01a_generate_raw_results_arrays.R`
   with `RUN.STRATA <- character(0)`. It loads only the 10 no-intervention simsets and writes
   `<ROOT.DIR>/shield/outputs/<calibration>/extracts/params/<MSA code>.Rdata`. No arrays are re-saved
   (the paper 1 outputs are frozen). Both analyses stop with a message if a parameter file is missing.
2. Open R in the `jheem_analyses` folder (every `source()` path is relative to it).

## Running

- Each script sources its settings file (`prcc/00_prcc_settings.R`, `gam/gam_settings.R`), which sources
  `shared/sensitivity_setup.R`. The setup checks every package the analysis needs, then loads
  `locations_of_interest.R` and `shield_specification.R` only if they are not already in the session (or
  changed since loaded). Running the next script in the same session skips them.
- `FORCE.RELOAD <- TRUE` in the console reloads everything once.
- `TEST.MSA` in each settings file: one MSA for a test run, `NULL` for all 10.

## Outputs (`<ROOT.DIR>/shield/outputs/calib.10.20.stage4/`)

| Analysis | Folder |
|---|---|
| PRCC | `sensitivity_prcc2/` (test runs: `sensitivity_prcc2/test/`) |
| GAM | `sensitivity_analysis_GAM/` (test runs: `_test` file suffix) |
