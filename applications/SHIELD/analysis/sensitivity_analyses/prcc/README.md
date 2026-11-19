# PRCC sensitivity analysis (SHIELD, doxy-PEP)

**Question:** within each MSA, which calibrated parameters most influence the projected impact of
doxy-PEP? No new simulations: the analysis uses the 1,000 posterior simulations per MSA, with all
parameters varying jointly as sampled by the calibration (a global, posterior-based analysis, not
one-way).

## Outcomes

All in 2030, doxy-PEP at 20% coverage (`doxy.cov.20`) vs. no doxy-PEP, within the same simulation
(simulations are paired across scenarios within an MSA, not across MSAs, so everything is done within MSA).

| Code | Outcome | Units |
|---|---|---|
| `total`, `msm`, `female` | Relative reduction in incidence: total population, MSM, women | % (positive = fewer cases) |
| `rir` | Women-to-MSM relative impact ratio = women % reduction / MSM % reduction; missing when MSM incidence did not fall (as in `intervention/04_women_to_msm_impact_ratio.R`) | ratio |
| `women_minus_msm` | Women % reduction - MSM % reduction (as in the GAM analysis); `INCLUDE.WOMEN.MINUS.MSM <- FALSE` drops it | percentage points |

## Steps

Run `run_prcc_all.R` from the `jheem_analyses` folder (or each step on its own). Settings are in
`00_prcc_settings.R`; packages and SHIELD base files load once per session (`../shared/sensitivity_setup.R`).

| Step | Script | What it does | Logic |
|---|---|---|---|
| — | `intervention/01a_generate_raw_results_arrays.R` (run once) | Saves each MSA's calibrated parameters from the no-intervention simset to `extracts/params/` | Parameters come from the same simsets, in the same simulation order, as the outcome arrays. Step 1 stops if a file is missing |
| 1 | `sensitivity_step1_sort_sims.R` | Loads the parameter matrices; sorts simulations by each parameter's value | — |
| 2 | `sensitivity_step2_top_bottom_ranking.R` | For each MSA × outcome × parameter: outcome distribution in simulations in the highest vs. lowest quintile (`TOP_BOTTOM_FRAC = 0.2`) of the parameter's values; standardized difference; ranking | Measures the size of each parameter's **marginal** effect, in units comparable across outcomes |
| 3 | `sensitivity_step3_find_prccs.R` | PRCC (estimate, 95% CI) of every parameter with every outcome, adjusting for **all** other parameters; applies the filters; summary tables | Adds the **independent** association and its direction. PRCC is computed with all parameters (a partial correlation must adjust for all of them), never only the top ones |
| 4 | `sensitivity_step4_tornado_plots.R` | One figure per MSA, one panel per outcome: paired boxplots (top vs. bottom quintile) for the selected parameters, labelled with the PRCC; dashed line = baseline | — |

## Measures

**Baseline projection:** median outcome across all simulations in the MSA (the statistic reported in
the primary analysis, `intervention/03_manuscript_main.R`).

**Standardized difference (`std_diff`)** = |median outcome, top quintile - median outcome, bottom quintile| /
SD of the outcome across all simulations in that MSA.
- Unitless, so it applies equally to % reductions, the RIR and percentage points.
- Relative to **that MSA's** posterior spread: the same `std_diff` can mean different absolute effects
  in different MSAs. The absolute difference (`top_minus_bottom`) is in the ranking table for reporting.
- It is marginal: a parameter can show a large difference just because it is correlated with other
  parameters that drive the outcome.

**PRCC** (Marino et al. 2008, PMID 18572196): rank-transform all parameters and the outcome, remove the
part explained by all other parameters, and correlate what is left.
- Sign = direction: positive means higher parameter values give higher outcome values.
- PRCC² = share of the outcome's **remaining** rank variance (after the other parameters) that the
  parameter explains: 0.2 -> 4%, 0.3 -> 9%, 0.5 -> 25%. It is a measure of independent association,
  not a share of total outcome variance (for that, see the GAM analysis).
- Not tested against 0: with 1,000 simulations, any |PRCC| above about 0.07 would be "significant",
  so p-values say little about importance. The 95% CI (Fisher z) is reported.
- Computed in base R (`calc_prcc_base()` in the settings); same method as `epiR::epi.prcc`.

## Filtering (influential parameters)

A parameter **stands out** for an MSA × outcome when both hold:

| Setting | Current | Criterion | Why |
|---|---|---|---|
| `MIN_STD_DIFF` | 1 | `std_diff >= 1`: the top and bottom quintiles differ by at least 1 SD of the outcome | The effect is large |
| `MIN_ABS_PRCC` | 0.2 | `abs(PRCC) >= 0.2` (`0` turns this off) | The effect is the parameter's own, not a correlated partner's |

Why both: when the posterior is clustered (e.g., chains settling in different regions, as appears to
happen in some MSAs), many parameters differ between clusters and pass the first criterion with a PRCC
near 0. The PRCC criterion removes them.

**Plots (`PLOT.FILTER`):** `"sd"` shows every parameter that stands out (the number varies by panel);
`"top_n"` shows the `TOP_N_PARAMS` parameters with the largest `std_diff`. Both are ordered by `std_diff`;
file names end in `__sd<…>_prcc<…>` or `__top<N>`.

## Interpreting the results

| Pattern | Meaning |
|---|---|
| Large `std_diff`, \|PRCC\| >= 0.2, same sign (`direction_agrees_prcc = TRUE`) | Influential: a large effect with an independent association |
| Large `std_diff`, PRCC near 0 | Marginal difference driven by correlated parameters (or clustering), not by this parameter |
| Small `std_diff`, \|PRCC\| >= 0.2 | Independent association, but a small effect on the outcome |
| Many parameters with large `std_diff` and tight, separated boxes in one MSA | Check for a bimodal outcome / clustering by chain before interpreting |

Across MSAs, use `stand_out_by_parameter.csv`: in how many MSAs a parameter stands out, and whether
its direction is consistent.

## Outputs (`<ROOT.DIR>/shield/outputs/calib.10.20.stage4/sensitivity_prcc2/`, test runs in `test/`)

| File | Contents |
|---|---|
| `parameter_ranking_top_bottom.csv` | Every MSA × outcome × parameter: baseline, quintile medians, differences, `std_diff`, PRCC with CI, both criteria, `stands_out`, rank |
| `prcc_all_params.csv` | PRCC and 95% CI for every MSA × outcome × parameter |
| `stand_out_counts.csv` | Per MSA (name and code) × outcome: parameters meeting the first criterion and both criteria |
| `stand_out_by_parameter.csv` | Manuscript-ready: per outcome, each influential parameter (readable name), number of MSAs where it is influential, by PRCC sign, PRCC and standardized difference as median (range) across those MSAs, and the MSA names |
| `figures/tornado/<MSA>__tornado__….png` | Tornado plots |
| `*.Rdata` | Intermediate objects (`param_matrices`, `sorted_arrs`, `top_bottom_ranking`, `prccs`, `rank_df_with_prcc`, `stand_out_summary`, `ALL_PARAM_TITLES`) |

Every CSV starts with rows describing its columns.
