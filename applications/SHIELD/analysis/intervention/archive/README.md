# Archive

Scripts that are no longer part of the workflow. Nothing in the numbered scripts sources them. The workflow itself is listed at the top of `../00_settings.R`.

1. `retired-generate_total_results_array.R`: the old results builder, which loaded every simset into memory at once. Replaced by `01_generate_raw_results_arrays.R`.
2. `doxy_summary_aug.R`: an earlier draft of the manuscript script. It does not parse (`t2.outcomes <- c(` is never closed). Its figures and tables now live in `03_manuscript_outputs.R`.
3. `doxy_figures.R`: exploratory figures. The file contains itself twice. It also holds stale copies of the `plot_*` helpers, so sourcing it after `intervention_helper_functions.R` replaces the current versions. The spillover analysis that grew out of it is Table S2, S2a and Figure 5 in `03_manuscript_outputs.R`.
4. `eg_tables.R`: example calls to `make_multi_location_table()`, all switched off.
5. `generate_correlation_plots.R`: an early scatter of impact against 2021 features. Replaced by Table S2a and Figure 5.
6. `1.R`: drafts of Figure 2 and Table 3. Both were copied into `03_manuscript_outputs.R`.
7. `backups/`: dated hand backups of the manuscript and helper scripts.
