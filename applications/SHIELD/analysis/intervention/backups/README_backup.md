# SHIELD Doxy-PEP analysis: results and intervention scripts

This folder takes the intervention simsets and turns them into the manuscript's tables and figures. The scripts are numbered in the order you run them.

## How to run

1. Open R in the `jheem_analyses` folder. Every `source()` path is relative to it.
2. Check the settings in `00_settings.R`, mainly `CALIB.NAME` and `N.SIM`.
3. Run `01a_generate_raw_results_arrays.R` from Terminal. It takes hours (see "Step 01a" below).
4. Run `01b_generate_custom_outcomes.R`.
5. Run `03_manuscript_main.R`, then `04_women_to_msm_impact_ratio.R`.

`02_pretty_calibration_plots.R` reads the calibration simsets, not the arrays, so it can run at any point.

Steps 01a and 01b only need to run again when the simsets change. Steps 02 to 04 can be re-run as often as you like.

## What each script does

| Step | File | Reads | Writes |
|---|---|---|---|
| 00 | `00_settings.R` | nothing | nothing; sets the shared settings |
| 01a | `01a_generate_raw_results_arrays.R` | intervention simsets | `total_raw_results`, `sex_raw_results` (and `race_raw_results`, `age_raw_results` if turned on) |
| 01b | `01b_generate_custom_outcomes.R` | the raw arrays listed in `CALC.STRATA` | `total_calc_results`, `sex_calc_results` (and race or age if listed) |
| 02 | `02_pretty_calibration_plots.R` | baseline calibration simsets | calibration-fit figures for the manuscript and supplement |
| 03 | `03_manuscript_main.R` | the raw and calc arrays (total, sex, race) | Table 1, Figure 3, Figure 4, Table 2 |
| 04 | `04_women_to_msm_impact_ratio.R` | `total_raw`, `sex_raw` | women-to-MSM relative impact ratio: values table, associations table, scatter figure |

### 00_settings.R
The one place to set what every other script uses:

1. `CALIB.NAME`, `N.SIM`: which calibration run to read and write.
2. `LOCATIONS`, `MSAS`: the ten MSAs, with codes and city names.
3. `INTERVENTION.CODES`: every simset that goes into the arrays (`noint` and 5% to 50% coverage).
4. `COVERAGE.LEVELS`: the coverage levels the tables and figures show (10% to 50%). Each one must be in `INTERVENTION.CODES`.
5. `YEARS`, `YEAR0` (2022: the reference year for the `_vs_year0` outcomes and the start of the cumulative sums), `EVAL.YEAR` (2030).

It also sources `locations_of_interest.R`, `shield_specification.R` and `../shield_output_paths.R`, so the scripts that source it get all three.

### Step 01a: 01a_generate_raw_results_arrays.R
Pulls the raw model outcomes (diagnoses, incidence, population, prevalence, screening, doxy coverage) out of every simset and saves them as arrays, one array per stratification:

1. Loads one simset at a time and saves a small "extract" file for each stratification.
2. Checks every extract file. It stops before saving anything if a file is damaged or out of date, and prints the `rm` commands that clear the bad files.
3. Stacks the extracts into one array per stratification: year x [stratum] x sim x outcome x intervention x location.

You set two things at the top of the file, under SETTINGS FOR THIS STEP:

1. **Stratification levels** (`RUN.STRATA`): any of `"total"`, `"sex"`, `"race"`, `"age"`. The default is total and sex. The outcomes pulled for each level are listed in `STRATA` just below it. Age has not been run yet.
2. **Number of cores** (`N.CORES`): how many simsets are loaded at the same time, one per worker. Memory use is roughly `N.CORES` simsets, so start with 3-4 and watch memory. `1` runs one file at a time.

Extracts that already exist are skipped, so a re-run only loads the new simsets, and adding a stratification later only builds the new extracts. Set `OVERWRITE = TRUE` to redo everything. Run it from Terminal with `nohup`, not RStudio (parallel runs use forking); the exact commands are in the file's header.

### Step 01b: 01b_generate_custom_outcomes.R
Takes each raw array from step 01a and builds a set of custom outcomes from it: rates, cumulative cases, and the change against two references. The result is saved as `<strat>_calc_results`, with the same dimensions as the raw array from `YEAR0` onward.

`CALC.STRATA` at the top of the file sets which raw arrays to process (default: total and sex). Only those calc files are written, so adding race later leaves the total and sex files alone.

**Naming rule:** `<summary>_<quantity>_vs_<reference>`

1. **Quantity:** `incidence`, `diagnosis_total`, `diagnosis_ps`. Add `_rate` for per 100,000 population, or put `cum_` in front for cumulative from `YEAR0`.
2. **Summary:** no prefix for the value itself, `abs_change_` for x − reference, `pct_change_` for 100 × (x − reference) / reference.
3. **Reference:** `vs_year0` for the same intervention in `YEAR0` (2022), and `vs_noint` for no intervention in the same year.

Change is always x minus the reference, so **a negative change means fewer cases**.

**The four sets of outcomes** (`<q>` = incidence, diagnosis_total or diagnosis_ps):

| Set | What it tells you | Outcomes |
|---|---|---|
| 1. Levels | the epidemic in one scenario, no comparison | `<q>_rate`, `cum_<q>` |
| 2. Change vs YEAR0 | how the epidemic has moved since 2022, within one scenario | `abs_change_<q>_vs_year0`, `pct_change_<q>_vs_year0`, `abs_change_<q>_rate_vs_year0`, `pct_change_<q>_rate_vs_year0` |
| 3. Change vs no intervention | the effect of doxy-PEP, in the same year or cumulative from 2022 | `abs_change_<q>_vs_noint`, `pct_change_<q>_vs_noint`, `abs_change_<q>_rate_vs_noint`, `abs_change_cum_<q>_vs_noint`, `pct_change_cum_<q>_vs_noint` |
| 4. Other | detection, doxy-PEP use, and who is getting infected | `diagnosis_ps_per_incidence`, `diagnosis_el_per_incidence` (if the raw array has EL diagnoses), `cum_doxy_coverage` (total only), `share_incidence` and `share_incidence_among_male` (sex only) |

Worked example (Atlanta, incidence, 2030): no intervention 9,117 cases, 30% coverage 6,117 cases, 2022 value 6,098.

1. `pct_change_incidence_vs_year0`, no intervention: 100 × (9,117 − 6,098) / 6,098 = +49.5%
2. `abs_change_incidence_vs_noint`, 30% coverage: 6,117 − 9,117 = −3,000 cases
3. `pct_change_incidence_vs_noint`, 30% coverage: 100 × −3,000 / 9,117 = −32.9%

Every outcome is calculated inside each simulation. Report the median of the outcome itself; don't rebuild it from the medians of its parts. The comment block at the top of the file has more detail and examples.

### Step 02: 02_pretty_calibration_plots.R
Plots the calibration fit (model against surveillance data) for the manuscript, using `shield.plot()` from `pretty_plots_helper_functions.R`. It saves the main-text figure `figures/fig2-calibration.png` and the supplement figures `figures/supplement-calibration-*.png`.

1. It has its own settings, `CALIB.NAME` and `N.SIM`, at the top of the file. It does not use `00_settings.R`, because the calibration figures need different settings from the intervention analysis.
2. It reads the baseline calibration simsets directly, not the result arrays, so it can run at any point.
3. It saves to the same `figures/` folder as the analysis, as long as its `CALIB.NAME` matches the one in `00_settings.R`.

### Step 03: 03_manuscript_main.R
The main-text tables and figures. Run SETUP first; after that each section runs on its own, except Table 2, which needs Figure 4.

1. **Table 1** (`tables/table1_no_intervention.csv`): the epidemic without Doxy-PEP, one pair of rows per MSA (median, then the 95% interval) and a Total row for the ten MSAs combined. Incidence (count, rate in 2022 and 2030, fold change, rate by sex), total and P&S diagnoses (count, rate, P&S share), P&S diagnoses and rates by sex, and the MSM share of incidence. Each column is one line in `table1_rows()`. Counts are summed over the MSAs within each simulation, and rates are rebuilt from those sums, so the Total row is computed the same way as the MSA rows.
2. **Figure 3** (`figures/fig3_heatmap_vs_noint.png`): two heatmaps, MSM and total population, of the % change in incidence in 2030 with vs without Doxy-PEP, for every MSA and coverage level. `FIG3.COLOURS` sets which value gets which colour. Cells at `TARGET` (−50%) or lower get a black outline.
3. **Figure 4** (`figures/fig4_impact_boxplots.png`): nine panels at `CCRIT`, with columns for the total population (cyan), MSM (blue) and women (red). Row 1: box plots of the relative change in incidence vs no Doxy-PEP in 2030. Row 2: bars of infections averted 2022–2030 with 95% intervals. Row 3: incidence rate per 100,000 in 2030 without (open circle) and with (filled circle) Doxy-PEP. Cities run down the side in the same order in every panel (smallest reduction at the top).
4. **Table 2** (`tables/table2_fig4_values_<CCRIT>.csv`): every value in Figure 4 as median and 95% interval, in the figure's city order, plus a Total row for the ten MSAs combined.

`TARGET` is set in the Figure 3 section; `CCRIT` at the top of the Figure 4 section. `FULL.NAMES.IN.TABLE` and `FULL.NAMES.IN.FIGURES` in SETUP choose full or short MSA names.

### Step 04: 04_women_to_msm_impact_ratio.R
The Women-to-MSM Relative Impact Ratio and what it is associated with. Reads only the raw arrays; if `03_manuscript_main.R` ran first in the same session, it reuses its arrays instead of reading them again.

1. **The ratio**: relative reduction in incidence among women / among MSM, in 2030 at `COVERAGE`. NA when MSM incidence did not fall.
2. **Baseline characteristics** (2022, no Doxy-PEP): incidence rates among women and MSM, population size (whole MSA and MSM), total syphilis diagnoses (count and rate, total and among MSM), and projected incidence growth among women relative to MSM, 2022–2030. They are listed in `DRIVERS`.
3. **Outputs** (`<cov>` = e.g. `20pct`):
   - `tables/4-impact_ratio_values_at_<cov>_doxy.csv`: the ratio and each characteristic, median and 95% interval per MSA.
   - `tables/4-impact_ratio_associations_at_<cov>_doxy.csv`: Spearman rho of the ratio with each characteristic, between the MSA medians and within each simulation.
   - `figures/4-impact_ratio_vs_drivers_at_<cov>_doxy.png`: the ratio against each characteristic with |rho| above `RHO.CUTOFF`, one panel each.

Everything is computed within each simulation, then summarised across simulations.

### CROI 2026 abstract
Not part of this workflow. The abstract's analysis is frozen, with all its dependencies, in `../croi26/`; its README explains how to re-create it.

### Helpers (never run on their own)
1. `intervention_helper_functions.R`: everything steps 03 and 04 use:
   - the colour palette (`SHIELD.PAL`, `PAL.*`, the heatmap colours)
   - table builders: `make_multi_location_table()` (a wide table) and `table_to_long()` (the tidy form the figures take)
   - figures: `plot_trend_by_location()`, `plot_coverage_heatmap()`, `plot_coverage_needed()`, `plot_trend_with_ci()`, `plot_impact_over_time()`, `plot_dose_response()`
2. `int_simset_helper_functions.R`: loads and plots the raw intervention simsets (`load.int.simsets()`, `plot.int.location()`, `plot.int.comparison()`). Used by the archived QC script.
3. `pretty_plots_helper_functions.R`: the functions behind step 02's figures: `shield.prepare()` pulls the model and observed data, and `shield.plot()` draws them. Examples are at the bottom of the file. It sources the style file below.
4. `pretty_plots_style.R`: how those figures look: years and projection line, colours, theme, outcome titles and labels, data-source names, figure sizes, and `save.shield.fig()`, which saves to `FIG.DIR` unless you give it another folder. Edit this file to change a figure's look.

Step 02 also sources `../calibration/calibration_helper_functions.R` for `load.calib.simsets()`. That file stays in the calibration folder because the calibration diagnostics use it too.

Two shared files sit one folder up, in `analysis/`:

1. `shield_output_paths.R`: `shield.output.path()`, `shield.fig.path()`, `shield.table.path()`, and `load.shield.results()`, which loads the result arrays. Assign them as `intervention.results <- load.shield.results(CALIB.NAME)`. If `intervention.results` is already in memory for the same calibration and its files haven't changed on disk, it is reused instead of read again. `force.reload = TRUE` always reads from disk. `which = c(..., "race_raw", "race_calc")` loads the race arrays too.
2. `shield_plot_core.R`: plotting utilities for simsets.

## How the files depend on each other

```
                       00_settings.R
        (locations_of_interest, shield_specification, shield_output_paths)
                             │  sourced by every numbered script except 02
   ┌──────────────┬──────────┴───┬──────────────────┐
   01a            01b            03                 04
   │              │              │                  │
simsets ──► raw arrays ──► calc arrays              │
                 │               ▲                  │
                 └───────────────┼──────────────────┘   (04 reads the raw arrays only)
                                 │   03 and 04 also source intervention_helper_functions.R
                                 │   (which sources ../shield_output_paths.R)

02_pretty_calibration_plots.R ──► pretty_plots_helper_functions.R ──► pretty_plots_style.R
                              └─► ../calibration/calibration_helper_functions.R ──► ../shield_plot_core.R ──► ../shield_output_paths.R
     (own settings, not 00_settings.R; reads the baseline calibration simsets)
```

Data flow, step by step:

1. Simsets: `<root>/simulations/shield/<CALIB.NAME>-<N.SIM>/<location>/`
2. Step 01a writes extracts to `<root>/shield/outputs/<CALIB.NAME>/extracts/`, and the raw arrays to `<root>/shield/outputs/<CALIB.NAME>/`.
3. Step 01b writes the calc arrays to the same folder.
4. Steps 02 to 04 write to `figures/` and `tables/` inside that folder.

`<root>` is `get.jheem.root.directory()`: `Q:` on the desktop, `/Volumes/jheem$` on the Mac.

## Archive

`archive/` holds scripts that are no longer part of the workflow: the old results builder, the older full manuscript script (`03_manuscript_outputs.R`), the efficiency tables, the array comparison tool, the QC trajectory plots, earlier drafts, exploratory figures, example calls and dated backups. Nothing above sources them. `archive/README.md` lists why each file was archived.

Do not source `archive/doxy_figures.R` after the helpers. It carries old copies of the `plot_*` functions, which would replace the current ones.
