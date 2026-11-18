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
| `gam_settings.R` | Shared settings, parameter groups (`PARAM_GROUPS`), `TEST.MSA`, parameter loader/cache | — |
| `00_param_correlations.R` | Spearman correlations between parameters in the posterior sims; flags correlated pairs split across groups | `param_correlations.csv` |
| `01_gam_sensitivity.R` | Loads parameters and results arrays, computes the shares | `gam_variance_shares.csv` / `.Rdata`, cached `param_cache/<MSA>.Rdata` |
| `02_gam_sensitivity_plots.R` | Heatmap: groups × MSAs, one panel per outcome | `gam_variance_shares_heatmap.png` |

**Testing:** set `TEST.MSA <- names(SHIELD.TEN.MSAS)[1]` in `gam_settings.R` and `FILE.SUFFIX <- "_test"` in script 02. The parameter cache is per MSA, so the full run reuses the test MSA's cache.

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
