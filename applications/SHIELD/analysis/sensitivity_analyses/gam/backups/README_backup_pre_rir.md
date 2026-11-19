# GAM-based sensitivity analysis (SHIELD, doxy-PEP)

**Question:** which groups of calibrated parameters explain the posterior uncertainty in the
2030 % reduction in incidence (total, MSM, women, and women minus MSM), in each MSA?
Script 01 also gives single-parameter shares (`gam_single_param_shares.csv`).

**Method:** first-order variance share of each parameter group,
`Var(E[Y | group]) / Var(Y)`, estimated by regressing the outcome on the group's parameters
across the 1,000 posterior simulations (`voi::evppivar()`; GAM for 1–2 parameters, MARS/`earth`
for larger groups). No new simulations are needed. The fit is repeated on shuffled outcomes
to estimate the share found by chance.

| Script | What it does | Output (in `<outputs>/calib.10.20.stage4/sensitivity_analysis_GAM/`) |
|---|---|---|
| `00_gam_settings.R` | Shared settings, packages (`SENS.PACKAGES`), parameter groups (`PARAM_GROUPS`), `TEST.MSA` | — |
| `run_gam_all.R` | Runs steps 1-3 in order | — |
| `sensitivity_step1_param_correlations.R` | Spearman correlations between parameters in the posterior sims; flags correlated pairs split across groups | `param_correlations.csv` |
| `sensitivity_step2_variance_shares.R` | Loads parameters and results arrays, computes group and single-parameter shares | `gam_variance_shares.csv` / `.Rdata`, `gam_single_param_shares.csv` / `.Rdata` |
| `sensitivity_step3_heatmap.R` | Heatmap: groups × MSAs, one panel per outcome | `gam_variance_shares_heatmap.png` |

**Before the first run:** the calibrated parameters are read from `extracts/params/<MSA>.Rdata`, saved by `analysis/intervention/01a_generate_raw_results_arrays.R` (`RUN.STRATA <- character(0)` = parameters only). Scripts stop with a message if a file is missing. Packages and the SHIELD base files are loaded once per R session by `../shared/sensitivity_setup.R`.

**Running:** `source("applications/SHIELD/analysis/sensitivity_analyses/gam/run_gam_all.R")` from `jheem_analyses` runs steps 1-3; each step can also be run on its own.

**Testing:** set `TEST.MSA <- names(SHIELD.TEN.MSAS)[1]` in `00_gam_settings.R`; all steps follow it and write to the `test/` subfolder.

**Reading the results**
- A share of 30% means: if the group's parameters were known exactly, the outcome's variance
  would be expected to drop by 30%.
- Shares overlap (correlated groups) and exclude interactions between groups, so they do not
  sum to 100%. The "All parameters" row gives the total the regression can explain.
- Analyses are within MSA; simulations are not paired across MSAs.

**References**
- Jackson C et al. A guide to value of information methods for prioritising research in health
  impact modelling. *Epidemiol Methods* 2021;10(1):20210012. PMID 35127249 (tutorial; `voi` package)
- Strong M, Oakley JE, Brennan A. Estimating multiparameter partial EVPI from a PSA sample:
  a nonparametric regression approach. *Med Decis Making* 2014;34:311–26. PMID 24246566 (method)
