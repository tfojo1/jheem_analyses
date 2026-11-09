# SHIELD calibration diagnostics

## How to run

1. Set `CALIBRATION.CODE` and `LOCATIONS` in `diagnostics_setup.R`. This is the only place they are set.
2. Optional: source `diagnostics_setup.R` once ahead of time, to load the specification and helpers.
3. Run any script below from `jheem_analyses`. Each one sources `diagnostics_setup.R` itself, and
   the heavy files load only once per R session.
4. Or run several at once: switch them on in `run_diagnostics.R` and source that file.
5. Review: `run_diagnostics.R` ends by knitting `reports/calibration_review.Rmd` into
   `calibration_review_<code>.html` in the calibration folder: one page with every table (coloured) and plot.

Outputs:

1. `fit/` and `prior/` write to `<jheem root>/shield/calibration_diagnostics/<calibration code>/`
   (data-only scripts write to `calibration_diagnostics/data_only/`)
2. `mcmc/` writes to `calibration_diagnostics/<calibration code>/mixing/` (plots in `mixing/plots/`);
   only its MCMC cache stays in `<jheem root>/shield/mcmc_diagnostics/mcmc_cache/`
3. `likelihood/` writes to `calibration_diagnostics/<calibration code>/likelihood/`

Two sets of calibration diagnostics, each with its own runner, run in this order:

1. MCMC (`mcmc/`), runner `run_mcmc_diagnostics.R`: do the chains mix? Reads the raw MCMC (parameter draws,
   log-likelihood, acceptance), no simsets. Uses the last `MIXING.BURN.KEEP` (0.5, set in `diagnostics_setup.R`)
   of each chain; `mixing_check.R` explains which draws and why not the whole chain.
   Stops if a city has no MCMC cache: build it first with `mcmc/build_mcmc_cache.R`.
2. Likelihoods (`likelihood/`), runner `run_likelihood_diagnostics.R`: what each likelihood contributes. Reads the
   `N.SIM.NO.BURN` simset and the likelihoods. Shows the newest mixing verdict first, and stops if there is none.

Both runners work in R (`source(...)`) or unattended (`nohup Rscript ... &`), from `jheem_analyses`.

Simsets read: set `N.SIM` (fit and prior scripts) and `N.SIM.NO.BURN` (likelihood scripts) in
`diagnostics_setup.R`, along with `CALIBRATION.CODE`. Neither should be the full simset, which is 85 GB per city in stage 4.

## Files

| Folder | File | What it does | Needs simsets? |
|---|---|---|---|
| | `diagnostics_setup.R` | calibration code, locations, simset sizes, `MIXING.BURN.KEEP`, sources, output folder | – |
| | `run_mcmc_diagnostics.R` | runner, step 1: checks the MCMC cache, runs `mcmc/`, prints the verdict by city | raw MCMC |
| | `run_likelihood_diagnostics.R` | runner, step 2: shows the mixing verdict, checks the simsets, runs `likelihood/` | `N.SIM.NO.BURN` |
| | `run_diagnostics.R` | runner for the fit, prior and report scripts | yes |
| helpers | `run_helpers.R` | `run.diagnostic.scripts()`, `newest.file()` (used by the runners) | – |
| helpers | `fit_helpers.R` | `observed.by.sex()`, `check.session()` | – |
| mcmc | `mcmc_helpers.R` | functions for the mixing checks (R-hat, ESS, acceptance, burn, report) | – |
| mcmc | `build_mcmc_cache.R` | reads each city's raw MCMC once and saves the slimmed copy to `mcmc_cache/`; run it in the background first for long calibrations (stage 4: about 84 GB of chunks per city) | raw MCMC |
| mcmc | `mixing_check.R` | MCMC mixing and acceptance, per city; HTML report. Uses the last `MIXING.BURN.KEEP` (0.5) of each chain, the same simulations as `N.SIM` | raw MCMC |
| mcmc | `mixing_correlated_parameter_pairs.R` | strongly correlated parameter pairs, and whether they share a sampling block | raw MCMC |
| likelihood | `likelihood_helpers.R` | functions for the likelihood breakdown | – |
| likelihood | `likelihood_breakdown.R` | per likelihood: change from the start to the end of each chain, spread across kept sims, likelihoods that pull against each other; CSVs | `N.SIM.NO.BURN` |
| fit | `check_fit_by_sex_over_time.R` | diagnoses by sex, model / data by period; plots; transmission multipliers; incidence and share diagnosed | yes |
| fit | `check_fit_by_age.R` | diagnoses by age, model / data, 2019-2021; plots | yes |
| fit | `check_fit_by_race.R` | diagnoses by race (black, hispanic, other), model / data, 2019-2021 and 2022-2023; each race relative to other; plots | yes |
| fit | `check_ps_el_split.R` | P&S:EL ratio, model vs data, 2015-2021 | yes |
| fit | `check_msm_proportion.R` | model MSM share of men vs BRFSS and Emory; MSM multipliers | yes |
| prior | `check_prior_position.R` | where each posterior median sits in its prior; transmission changes between spline years | yes |
| prior | `check_spline_prior_against_data.R` | tests the transmission spline prior's assumptions against P&S by sex across MSAs | no |
| prior | `check_diagnosis_shifts_by_sex.R` | male and female change per period; where the spline points should go | no |
| reports | `msm_share_emory_vs_brfss.Rmd` | Emory vs BRFSS, and where the model sits (knit it) | section 6 only |
| reports | `calibration_review.Rmd` | one page per calibration: flags summary, then every saved table and plot | no (reads the saved files) |
| reports | `msm_het_transmission_prior_review.Rmd` | note on the MSM/het transmission prior and candidate revisions | – |
| | `test_ehe_msm_likelihood.R` | EHE, not SHIELD: tests EHE's proportion-MSM likelihood. Run in a fresh session; does not use the setup | EHE simset |
