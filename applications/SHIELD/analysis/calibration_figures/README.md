# calibration_figures

Supplement calibration figures for the SHIELD paper (calib.10.20.stage4).
Built on top of the pretty-plots helpers in `analysis/intervention/`, which are **sourced, not edited**.

| Script | What it does | Output |
|---|---|---|
| `00_calib_fig_settings.R` | Dependencies, sources, CALIB.NAME/N.SIM, output folder, session-only style tweaks | — |
| `01_supplement_timeseries.R` | S1–S8: key targets, 10 MSAs in a 5x2 grid | `<root>/shield/outputs/<CALIB.NAME>/figures/calibration_supplement/calib-supp-S*.png` |
| `02_*` (planned) | Fit-quality heatmap: all targets x strata x MSAs | same folder |

Run from the same working directory as the intervention scripts.
