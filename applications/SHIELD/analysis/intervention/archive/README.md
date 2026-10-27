# Archive

Scripts that are no longer part of the workflow. Nothing in the numbered scripts sources them. The workflow itself is listed at the top of `../00_settings.R`.

1. `retired-generate_total_results_array.R`: the old results builder, which loaded every simset into memory at once. Replaced by `01a_generate_raw_results_arrays.R`.
2. `doxy_summary_aug.R`: an earlier draft of the manuscript script. It does not parse (`t2.outcomes <- c(` is never closed). Its figures and tables moved to `03_manuscript_outputs.R` (item 8).
3. `doxy_figures.R`: exploratory figures. The file contains itself twice. It also holds stale copies of the `plot_*` helpers, so sourcing it after `intervention_helper_functions.R` replaces the current versions. The spillover analysis that grew out of it is Table S2, S2a and Figure 5 in `03_manuscript_outputs.R` (item 8), and now `../04_women_to_msm_impact_ratio.R`.
4. `eg_tables.R`: example calls to `make_multi_location_table()`, all switched off.
5. `generate_correlation_plots.R`: an early scatter of impact against 2021 features. Replaced by Table S2a and Figure 5.
6. `1.R`: drafts of Figure 2 and Table 3. Both were copied into `03_manuscript_outputs.R` (item 8).
7. `backups/`: dated hand backups of the manuscript and helper scripts.
8. `03_manuscript_outputs.R`: the older, full manuscript script (every table and figure, including the spillover analysis). Replaced by `../03_manuscript_main.R` and `../04_women_to_msm_impact_ratio.R`.
9. `04_table_efficiency.R`: doxycycline person-years per infection averted, by MSA and pooled. Not used in the current manuscript.
10. `02b_compare_results.R`: `compare_calc_results(a, b)`, which reports whether two result arrays hold the same numbers. Useful after a re-run.
11. `qc_intervention_plots.R`: trajectories read straight from the simsets. Uses `../int_simset_helper_functions.R`.

Items 8 to 11 source the settings and helpers from the main folder, so they still run from here. The usage example at the top of `02b_compare_results.R` gives its old location; source it from `archive/`.
