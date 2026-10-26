# SHIELD Doxy-PEP analysis: results and intervention scripts

This folder takes the intervention simsets and turns them into the manuscript's tables and figures. The scripts are numbered in the order you run them.

## How to run

1. Open R in the `jheem_analyses` folder. Every `source()` path is relative to it.
2. Check the settings in `00_settings.R`, mainly `CALIB.NAME` and `N.SIM`.
3. Run `01_generate_raw_results_arrays.R` from Terminal. It takes hours (see "Step 01" below).
4. Run `02_generate_custom_outcomes.R`.
5. Run `03_manuscript_outputs.R`, then `04_table_efficiency.R`.

Steps 01 and 02 only need to run again when the simsets change. Steps 03 and 04 can be re-run as often as you like.

## What each script does

| Step | File | Reads | Writes |
|---|---|---|---|
| 00 | `00_settings.R` | nothing | nothing; sets the shared settings |
| 01 | `01_generate_raw_results_arrays.R` | intervention simsets | `total_raw_results`, `sex_raw_results` (and `race_raw_results`, `age_raw_results` if turned on) |
| 02 | `02_generate_custom_outcomes.R` | the raw arrays listed in `CALC.STRATA` | `total_calc_results`, `sex_calc_results` (and race or age if listed) |
| 02b | `02b_compare_results.R` | two arrays you pass in | console only |
| 03 | `03_manuscript_outputs.R` | the four arrays | manuscript tables and figures (older, full version) |
| 03b | `03b_manuscript_main.R` | the four arrays | main-text Table 1, Figure 3 and Figure 4 |
| 04 | `04_table_efficiency.R` | the four arrays | the two efficiency tables |
| 06 | `06_pretty_plots.R` | baseline calibration simsets | calibration-fit figure for the manuscript |
| QC | `qc_intervention_plots.R` | intervention simsets | trajectory plots |

### 00_settings.R
The one place to set what every other script uses:

1. `CALIB.NAME`, `N.SIM`: which calibration run to read and write.
2. `LOCATIONS`, `MSAS`: the ten MSAs, with codes and city names.
3. `INTERVENTION.CODES`: every simset that goes into the arrays (`noint` and 5% to 50% coverage).
4. `COVERAGE.LEVELS`: the coverage levels the tables and figures show (10% to 50%). Each one must be in `INTERVENTION.CODES`.
5. `YEARS`, `YEAR0` (2022: the reference year for the `_vs_year0` outcomes and the start of the cumulative sums), `EVAL.YEAR` (2030).

It also sources `locations_of_interest.R`, `shield_specification.R` and `../shield_output_paths.R`, so the scripts that source it get all three.

### Step 01: 01_generate_raw_results_arrays.R
Pulls the raw model outcomes (diagnoses, incidence, population, prevalence, screening, doxy coverage) out of every simset and saves them as arrays, one array per stratification:

1. Loads one simset at a time and saves a small "extract" file for each stratification.
2. Checks every extract file. It stops before saving anything if a file is damaged or out of date, and prints the `rm` commands that clear the bad files.
3. Stacks the extracts into one array per stratification: year x [stratum] x sim x outcome x intervention x location.

You set two things at the top of the file, under SETTINGS FOR THIS STEP:

1. **Stratification levels** (`RUN.STRATA`): any of `"total"`, `"sex"`, `"race"`, `"age"`. The default is total and sex. The outcomes pulled for each level are listed in `STRATA` just below it. Race and age have not been run yet. Steps 02 to 04 currently read only the total and sex arrays.
2. **Number of cores** (`N.CORES`): how many simsets are loaded at the same time, one per worker. Memory use is roughly `N.CORES` simsets, so start with 3-4 and watch memory. `1` runs one file at a time.

Extracts that already exist are skipped, so a re-run only loads the new simsets, and adding a stratification later only builds the new extracts. Set `OVERWRITE = TRUE` to redo everything. Run it from Terminal with `nohup`, not RStudio (parallel runs use forking); the exact commands are in the file's header.

### Step 02: 02_generate_custom_outcomes.R
Takes each raw array from step 01 and builds a set of custom outcomes from it: rates, cumulative cases, and the change against two references. The result is saved as `<strat>_calc_results`, with the same dimensions as the raw array from `YEAR0` onward.

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

### Step 02b: 02b_compare_results.R (optional)
`compare_calc_results(a, b)` reports whether two arrays hold the same numbers. It shows differences in dimensions, levels, attributes and values. Use it after a re-run to see what changed.

### Step 03: 03_manuscript_outputs.R
Builds every manuscript table and figure, in this order:

1. Table 1: baseline epidemiology, no intervention (`tables/1-notInt.csv`)
2. Figure 2: no-intervention trends (`figures/fig2_noint.png`)
3. Figure 3: impact heatmaps (`figures/fig3_heatmaps.png`)
4. Figure 3a: critical coverage (`figures/fig3a_crit_coverage.png`)
5. Figure 4: impact box plots at one coverage level (`figures/fig5_impact_boxplots.png`)
6. Table 3: Doxy-PEP efficiency (`tables/3-efficiency.csv`)
7. Table S1: impact at one coverage level, by population (`tables/S1-impact-cov<c>-<yr>.csv`)
8. Table S2: spillover and its candidate drivers (`tables/S2-spillover-cov<c>.csv`)
9. Table S2a: associations with the drivers (`tables/5-spillover-associations-cov<c>.csv`)
10. Figure 5: spillover against four drivers (`figures/fig4_spillover_drivers.png`)
11. Check 1: does the decomposition hold? (console only)

Table S2 has to run before Table S2a, Figure 5 and Check 1, because they use the objects it builds. The other sections stand on their own. Some file names are one number off from their section; the file's header explains why.

The CONFIGURATION block at the top sources the settings and helpers and loads the arrays. To re-run single sections without reloading, change its `if(1==1)` to `if(1==2)`.

### Step 03b: 03b_manuscript_main.R
The main-text outputs only, written as a short standalone script. It does not use or change `03_manuscript_outputs.R`, and its file names don't clash with it.

1. **Table 1** (`tables/table1_no_intervention.csv`): the epidemic without doxy-PEP in 2022 and 2030: incident infections, incidence rate and its % change since 2022, P&S diagnoses, total diagnosis rate, P&S diagnoses per incident infection, and the MSM share of incidence. The columns are listed in `T1.COLUMNS`; add or drop a row there to change the table.
2. **Figure 3** (`figures/fig3_heatmap_vs_noint.png`): two heatmaps, MSM and total population, of the % change in incidence in 2030 with vs without doxy-PEP, for every MSA and coverage level. `FIG3.COLOURS` sets which value gets which colour, e.g. `c("-100" = "#35978F", "0" = "#F2F2F2")` shades one colour from light at 0 to dark at −100; add a third entry for a two-colour scale.
3. **Figure 4** (`figures/fig4_impact_boxplots.png`): nine panels at `CCRIT`, with columns for the total population (cyan), MSM (blue) and women (red). Row 1: box plots of the relative change in incidence vs no Doxy-PEP in 2030. Row 2: bars of infections averted 2022–2030 with 95% intervals. Row 3: incidence rate per 100,000 in 2030 without (open circle) and with (filled circle) Doxy-PEP. Each row has one title; cities run down the side in the same order in every panel (smallest reduction at the top), with grey stripes to follow a city across the columns.

`TARGET` (−50%: the Figure 3 outline and the Figure 4 dashed line) is set in the Figure 3 section; `CCRIT` (the coverage level in Figure 4) is set at the top of the Figure 4 section.

### Step 04: 04_table_efficiency.R
Person-years of doxycycline per infection averted, for each step up the coverage ladder, at 2030 and 2035:

1. `tables/table_efficiency_by_msa.csv`: one row per MSA and coverage level.
2. `tables/table_efficiency_pooled.csv`: the ten MSAs combined.

It then runs five checks and prints them to the console. Run it after step 03 in the same session, or on its own; it loads the arrays if they are not already loaded.

**This is a second version of Table 3.** Step 03 builds the other. Keep one of the two.

### CROI 2026 abstract
Not part of this workflow. The abstract's analysis is frozen, with all its dependencies, in `../croi26/`; its README explains how to re-create it.

### Step 06: 06_pretty_plots.R
Plots the calibration fit (model against surveillance data) for the manuscript, using `shield.plot()` from `pretty_plots_helper_functions.R`. It currently saves `figures/fig2-Epi_impact1.png`.

1. It has its own settings, `CALIB.NAME` and `N.SIM`, at the top of the file. It does not use `00_settings.R`, because the calibration figures need different settings from the intervention analysis.
2. It reads the baseline calibration simsets directly, not the result arrays, so it can run at any point.
3. It saves to the same `figures/` folder as the analysis, as long as its `CALIB.NAME` matches the one in `00_settings.R`.

### QC: qc_intervention_plots.R (optional)
Plots trajectories straight from the simsets, without the arrays: one city with interventions overlaid, and a comparison across cities. Run it any time after the simsets exist.

### Helpers (never run on their own)
1. `intervention_helper_functions.R`: everything steps 03 and 04 use:
   - the colour palette (`SHIELD.PAL`, `PAL.*`, the heatmap colours)
   - table builders: `make_multi_location_table()` (a wide table) and `table_to_long()` (the tidy form the figures take)
   - figures: `plot_trend_by_location()`, `plot_coverage_heatmap()`, `plot_coverage_needed()`, `plot_trend_with_ci()`, `plot_impact_over_time()`, `plot_dose_response()`
2. `int_simset_helper_functions.R`: loads and plots the raw intervention simsets (`load.int.simsets()`, `plot.int.location()`, `plot.int.comparison()`). Used by the QC script.
3. `pretty_plots_helper_functions.R`: the functions behind step 06's figures: `shield.prepare()` pulls the model and observed data, and `shield.plot()` draws them. Examples are at the bottom of the file. It sources the style file below.
4. `pretty_plots_style.R`: how those figures look: years and projection line, colours, theme, outcome titles and labels, data-source names, figure sizes, and `save.shield.fig()`, which saves to `FIG.DIR` unless you give it another folder. Edit this file to change a figure's look.

Step 06 also sources `../calibration/calibration_helper_functions.R` for `load.calib.simsets()`. That file stays in the calibration folder because the calibration diagnostics use it too.

Two shared files sit one folder up, in `analysis/`:

1. `shield_output_paths.R`: `shield.output.path()`, `shield.fig.path()`, `shield.table.path()`, and `load.shield.results()`, which loads the result arrays. Assign them as `intervention.results <- load.shield.results(CALIB.NAME)`. If `intervention.results` is already in memory for the same calibration and its files haven't changed on disk, it is reused instead of read again. `force.reload = TRUE` always reads from disk. `which = c(..., "race_raw", "race_calc")` loads the race arrays too.
2. `shield_plot_core.R`: plotting utilities for simsets.

## How the files depend on each other

```
                       00_settings.R
        (locations_of_interest, shield_specification, shield_output_paths)
                             │  sourced by every numbered script
   ┌──────────────┬──────────┴───┬──────────────────┐
   01             02             03                 04
   │              │              │                  │
simsets ──► raw arrays ──► calc arrays ─────────────┘
                                 ▲   both also source intervention_helper_functions.R
                                 │   (which sources ../shield_output_paths.R)

qc_intervention_plots.R ──► int_simset_helper_functions.R ──► ../shield_plot_core.R ──► ../shield_output_paths.R
     (also sources 00_settings.R; reads the simsets, not the arrays)

06_pretty_plots.R ──► pretty_plots_helper_functions.R ──► pretty_plots_style.R
                  └─► ../calibration/calibration_helper_functions.R ──► ../shield_plot_core.R ──► ../shield_output_paths.R
     (own settings, not 00_settings.R; reads the baseline calibration simsets)
```

Data flow, step by step:

1. Simsets: `<root>/simulations/shield/<CALIB.NAME>-<N.SIM>/<location>/`
2. Step 01 writes extracts to `<root>/shield/outputs/<CALIB.NAME>/extracts/`, and the raw arrays to `<root>/shield/outputs/<CALIB.NAME>/`.
3. Step 02 writes the calc arrays to the same folder.
4. Steps 03 and 04 write to `figures/` and `tables/` inside that folder.

`<root>` is `get.jheem.root.directory()`: `Q:` on the desktop, `/Volumes/jheem$` on the Mac.

## Archive

`archive/` holds scripts that are no longer part of the workflow: the old results builder, earlier drafts of the manuscript script, exploratory figures, example calls and dated backups. Nothing above sources them. `archive/README.md` lists why each file was archived.

Do not source `archive/doxy_figures.R` after the helpers. It carries old copies of the `plot_*` functions, which would replace the current ones.
