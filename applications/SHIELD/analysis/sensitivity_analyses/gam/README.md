# GAM-based sensitivity analysis (SHIELD, doxy-PEP)

**Question:** which groups of calibrated parameters explain the posterior uncertainty in the
2030 % reduction in incidence (total, MSM, women, the women-to-MSM ratio, and women minus MSM), in each MSA?
Step 2 also gives single-parameter shares (`gam_single_param_shares.csv`).

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
| `sensitivity_step3_heatmap.R` | Heatmaps (MSAs × rows, one panel per outcome): parameter groups; top `TOP_N_SINGLE` single parameters | `gam_variance_shares_heatmap.png`, `gam_single_param_shares_heatmap.png` |

**Before the first run:** the calibrated parameters are read from `extracts/params/<MSA>.Rdata`, saved by `analysis/intervention/01a_generate_raw_results_arrays.R` (`RUN.STRATA <- character(0)` = parameters only). Scripts stop with a message if a file is missing. Packages and the SHIELD base files are loaded once per R session by `../shared/sensitivity_setup.R`.

**Running:** `source("applications/SHIELD/analysis/sensitivity_analyses/gam/run_gam_all.R")` from `jheem_analyses` runs steps 1-3; each step can also be run on its own.

**Testing:** set `TEST.MSA <- names(SHIELD.TEN.MSAS)[1]` in `00_gam_settings.R`; all steps follow it and write to the `test/` subfolder.

## Parameter groups: how they are built and checked

### Why groups
Parameters that are correlated in the posterior share information: knowing one tells you much of the
other. Scored one at a time, each would get credit for the same variance (shares overlap and can add
up to well over 100%). Scored together as a group, the shared variance is counted once. Groups also
make the result readable ("which parts of the model drive the uncertainty").

### How the initial groups were built
All 173 calibrated parameters (every parameter with a prior in `shield_calib_parameters.R`) are
assigned to one of 21 groups in `PARAM_GROUPS` (`00_gam_settings.R`), using four rules:

1. **Mechanism:** transmission, sexual mixing, screening, care seeking, natural history, testing,
   demographics, etc.
2. **Population:** split into MSM vs. heterosexual wherever the model does (transmission level and
   trend, transmission by age, screening).
3. **One curve = one group:** parameters that together define one function stay together, e.g. the
   global transmission rate and its time-spline knots (1970-2022), or the age-specific multipliers.
4. **Nuisance blocks lumped:** fertility, mortality, immigration and aging (80 parameters) form one
   "Demographics" group; future-change multipliers get their own groups because they set the
   2023-2030 trajectory.

The groups agree with the calibration's own sampling blocks (`SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS`):
every block falls inside one group, except three blocks that are deliberately split by population
(see exceptions).

**Implementation:** one regular expression per group; each parameter goes to the FIRST group it
matches, so order matters. A parameter that matches no group goes to "Unassigned", with a warning.

### Exceptions and judgment calls
| Decision | Why |
|---|---|
| `global.transmission.rate.msm` / `.het` are in the MSM / heterosexual "level and trend" groups, although they share one sampling block | They act on different populations; posterior link between them is moderate (r = -0.58 in Atlanta) |
| `covid.screening.red.sex` and `age.transmission.young` sampling blocks are split into MSM vs. heterosexual groups | Same reason: different populations |
| HIV testing is separate from stage misclassification | Different mechanisms |
| `el.rel.secondary.transmissibility` is with `prop.early.latent.to.secondary` ("Infectiousness by stage"), not with natural history | It is about infectiousness; the two trade off (r = -0.76 in Atlanta) |
| Care-seeking parameters (`or.careseeking.symptomatic.*`) also contain "symptomatic" | Listed before "Natural history and symptoms", so they go to "Symptomatic care seeking" |
| `transmission.rate.multiplier.msm1970` / `.heterosexual1970` also end in "1970" | Listed before "Initial conditions (1970)", so they stay with their transmission curves |

Merges made after the first correlation check (Atlanta, 2026-11-17):

| Merged | Why |
|---|---|
| Global transmission rate + its time knots ("level and trend") | Global rate vs. all 6 heterosexual knots: r = -0.77 to -0.83 (only rate x knot is identified) |
| Race transmission multipliers + race-assortative mixing ("Race: transmission and mixing") | r up to 0.77; also strong in 3 MSAs in the within-chain MCMC check |
| Relapse to secondary + secondary-stage infectiousness ("Infectiousness by stage") | r = -0.76 |

### Checking the groups (step 1) and how to read it
Step 1 computes the Spearman correlation of every pair of parameters across the posterior simulations,
in each MSA (same sims as step 2, pooled across chains, raw scale).

- **Correlation INSIDE a group is fine** (often expected): the shared variance is counted once.
- **Correlation BETWEEN groups is the problem**: both groups claim the same variance, so both look
  important even if only one is. The sign does not matter (negative = trade-off, positive = move together).

| | Good grouping | Bad grouping |
|---|---|---|
| Strong correlations (\|r\| >= 0.7) | all within groups | some between groups |
| Across MSAs | any strong cross-group link is city-specific (1-2 MSAs) | the same link in many MSAs (structural) |

**Rule (settings `GROUP.CHECK.R`, `GROUP.CHECK.N.MSAS`):** two groups should be merged if a parameter in
one and a parameter in the other have |r| >= 0.7 in at least 3 MSAs (in every MSA, if fewer are run).
Cross-group links of 0.5-0.7 are expected in a calibrated posterior and are tolerated: merging at that
level would collapse the model into 2-3 groups.

**Automatic check:** step 1 ends with a printed verdict and saves it (`group_check.Rdata`,
`group_check_links.csv`):
- `PASS`: no cross-group link meets the rule. Step 2 can run.
- `FAIL` (full run): it lists the groups to merge (linked groups are combined: A-B and B-C -> A+B+C)
  and the parameter pairs behind them. **Step 2 stops** with the same message until `PARAM_GROUPS` is
  fixed and step 1 is rerun.
- `WARN` (one-MSA test run): reported, step 2 not blocked; run all MSAs to confirm.

Step 2 also stops if `PARAM_GROUPS` or the MSAs changed since step 1 was run. After reviewing a flagged
link and deciding to keep the groups, set `GROUP.CHECK.OVERRIDE <- TRUE` (and say why in the methods).

**How merging changes the results:** two merged groups get ONE share. In theory it is at least as large
as the larger of the two separate shares, and when the groups overlap because of correlation it is
smaller than their sum (the double-counted variance is removed; it can exceed the sum only if the two
groups interact). The ranking of the other groups and the single-parameter shares do not change.

### Review status
| Date | MSAs checked | Result |
|---|---|---|
| 2026-11-17 | Atlanta only | 3 merges above; remaining strongest cross-group link 0.71 (MSM knots - `transmission.rate.multiplier.black`), single MSA |
| — | all 10 | **pending**: rerun step 1 with `TEST.MSA <- NULL` and act on the verdict before using step 2 results |

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
