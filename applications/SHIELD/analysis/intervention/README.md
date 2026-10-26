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
| 02 | `02_generate_custom_outcomes.R` | the total and sex raw arrays | `total_calc_results`, `sex_calc_results` |
| 02b | `02b_compare_results.R` | two arrays you pass in | console only |
| 03 | `03_manuscript_outputs.R` | the four arrays | manuscript tables and figures |
| 04 | `04_table_efficiency.R` | the four arrays | the two efficiency tables |
| 05 | `05_croi26.R` | the four arrays (CROI calibration) | CROI figure and table |
| QC | `qc_intervention_plots.R` | intervention simsets | trajectory plots |

### 00_settings.R
The one place to set what every other script uses:

1. `CALIB.NAME`, `N.SIM`: which calibration run to read and write.
2. `LOCATIONS`, `MSAS`: the ten MSAs, with codes and city names.
3. `INTERVENTION.CODES`: every simset that goes into the arrays (`noint` and 5% to 50% coverage).
4. `COVERAGE.LEVELS`: the coverage levels the tables and figures show (10% to 50%). Each one must be in `INTERVENTION.CODES`.
5. `YEARS`, `BASE.YEAR` (2022), `EVAL.YEAR` (2030).

It also sources `locations_of_interest.R`, `shield_specification.R` and `../shield_output_paths.R`, so the scripts that source it get all three.

### Step 01: 01_generate_raw_results_arrays.R
Pulls the raw model outcomes (diagnoses, incidence, population, prevalence, screening, doxy coverage) out of every simset and saves them as arrays, one array per stratification:

1. Loads one simset at a time and saves a small "extract" file for each stratification.
2. Checks every extract file. It stops before saving anything if a file is damaged or out of date, and prints the `rm` commands that clear the bad files.
3. Stacks the extracts into one array per stratification: year x [stratum] x sim x outcome x intervention x location.

You set two things at the top of the file, under SETTINGS FOR THIS STEP:

1. **Stratification levels** (`RUN.STRATA`): any of `"total"`, `"sex"`, `"race"`, `"age"`. The default is total and sex. The outcomes pulled for each level are listed in `STRATA` just below it. Race and age have not been run yet. Steps 02 to 05 currently read only the total and sex arrays.
2. **Number of cores** (`N.CORES`): how many simsets are loaded at the same time, one per worker. Memory use is roughly `N.CORES` simsets, so start with 3-4 and watch memory. `1` runs one file at a time.

Extracts that already exist are skipped, so a re-run only loads the new simsets, and adding a stratification later only builds the new extracts. Set `OVERWRITE = TRUE` to redo everything. Run it from Terminal with `nohup`, not RStudio (parallel runs use forking); the exact commands are in the file's header.

### Step 02: 02_generate_custom_outcomes.R
Takes the raw arrays from step 01 and generates a set of custom outcomes from them, saved as `total_calc_results` and `sex_calc_results`. The arrays keep the same dimensions, but the `outcome` dimension holds the custom outcomes instead of the raw ones. They are built for incidence, total diagnoses and P&S diagnoses, from `BASE.YEAR` onward:

1. **Number of cases:** cumulative cases from `BASE.YEAR` (`num_cum_*`), and the doxy-PEP person-years delivered (`num_cum_doxy_coverage`).
2. **Cases averted:** annual and cumulative, against no intervention (`num_*_averted`, `num_cum_*_averted`).
3. **Percent averted:** annual and cumulative (`pct_*_averted`, `pct_cum_*_averted`).
4. **Rates per 100,000:** per population (`rate_*_per_pop`), cases averted per population (`rate_*_averted_per_pop`), and cumulative infections averted per 100,000 doxy person-years (`rate_cum_*_ppy_doxy`).
5. **Change since baseline:** fold-change (`ratio_*_vs_baseline`) and percent reduction (`pct_*_reduction_vs_baseline`) against `BASE.YEAR`.
6. **Proportions:** P&S diagnoses per incident infection (`ratio_ps_diagnosis_to_incidence`). At the sex level only, each group's share of incidence (`pct_incidence_share`) and each male group's share of male incidence (`pct_male_incidence_share`).

Every outcome is calculated inside each simulation, so each has a full posterior. Summarise it as a median of that outcome, never by combining the medians of its parts. The outcomes compare against one of three things:

1. `*_averted`: no intervention, in the same year.
2. `*_vs_baseline`, `*_reduction_vs_baseline`: the same arm in `BASE.YEAR`.
3. `rate_*`, `ratio_*_to_*`: nothing; they are levels.

The doxy-PEP outcomes exist only at the total level, because doxy coverage is not split by sex. The naming convention, and why there is no single-year efficiency outcome, are explained at the bottom of the file.

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

### Step 04: 04_table_efficiency.R
Person-years of doxycycline per infection averted, for each step up the coverage ladder, at 2030 and 2035:

1. `tables/table_efficiency_by_msa.csv`: one row per MSA and coverage level.
2. `tables/table_efficiency_pooled.csv`: the ten MSAs combined.

It then runs five checks and prints them to the console. Run it after step 03 in the same session, or on its own; it loads the arrays if they are not already loaded.

**This is a second version of Table 3.** Step 03 builds the other. Keep one of the two.

### Step 05: 05_croi26.R (optional)
A cut-down copy of step 03 for the CROI abstract: a two-panel heatmap (`figures/fig_CROI.png`) and Table S1. It is pinned to `calib.9.23.stage3.pk`, so re-running it reproduces the submitted numbers. Delete that line to follow `00_settings.R` instead.

### QC: qc_intervention_plots.R (optional)
Plots trajectories straight from the simsets, without the arrays: one city with interventions overlaid, and a comparison across cities. Run it any time after the simsets exist.

### Helpers (never run on their own)
1. `intervention_helper_functions.R`: everything steps 03 to 05 use:
   - the colour palette (`SHIELD.PAL`, `PAL.*`, the heatmap colours)
   - table builders: `make_multi_location_table()` (a wide table) and `table_to_long()` (the tidy form the figures take)
   - figures: `plot_trend_by_location()`, `plot_coverage_heatmap()`, `plot_coverage_needed()`, `plot_trend_with_ci()`, `plot_impact_over_time()`, `plot_dose_response()`
2. `int_simset_helper_functions.R`: loads and plots the raw intervention simsets (`load.int.simsets()`, `plot.int.location()`, `plot.int.comparison()`). Used by the QC script.

Two shared files sit one folder up, in `analysis/`:

1. `shield_output_paths.R`: `shield.output.path()`, `shield.fig.path()`, `shield.table.path()`, and `load.shield.results()`, which loads the four arrays.
2. `shield_plot_core.R`: plotting utilities for simsets.

## How the files depend on each other

```
                       00_settings.R
        (locations_of_interest, shield_specification, shield_output_paths)
                             │  sourced by every numbered script
   ┌──────────────┬──────────┴───┬──────────────────┬──────────────────┐
   01             02             03                 04                 05
   │              │              │                  │                  │
simsets ──► raw arrays ──► calc arrays ─────────────┴──────────────────┘
                                 ▲   all three also source intervention_helper_functions.R
                                 │   (which sources ../shield_output_paths.R)

qc_intervention_plots.R ──► int_simset_helper_functions.R ──► ../shield_plot_core.R ──► ../shield_output_paths.R
     (also sources 00_settings.R; reads the simsets, not the arrays)
```

Data flow, step by step:

1. Simsets: `<root>/simulations/shield/<CALIB.NAME>-<N.SIM>/<location>/`
2. Step 01 writes extracts to `<root>/shield/outputs/<CALIB.NAME>/extracts/`, and the raw arrays to `<root>/shield/outputs/<CALIB.NAME>/`.
3. Step 02 writes the calc arrays to the same folder.
4. Steps 03 to 05 write to `figures/` and `tables/` inside that folder.

`<root>` is `get.jheem.root.directory()`: `Q:` on the desktop, `/Volumes/jheem$` on the Mac.

## Archive

`archive/` holds scripts that are no longer part of the workflow: the old results builder, earlier drafts of the manuscript script, exploratory figures, example calls and dated backups. Nothing above sources them. `archive/README.md` lists why each file was archived.

Do not source `archive/doxy_figures.R` after the helpers. It carries old copies of the `plot_*` functions, which would replace the current ones.
