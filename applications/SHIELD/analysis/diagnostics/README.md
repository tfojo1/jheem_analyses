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

## Files

| Folder | File | What it does | Needs simsets? |
|---|---|---|---|
| | `diagnostics_setup.R` | calibration code, locations, sources, output folder | – |
| helpers | `mcmc_helpers.R` | functions for the mixing checks (R-hat, ESS, acceptance, report) | – |
| helpers | `fit_helpers.R` | `observed.by.sex()`, `check.session()` | – |
| mcmc | `mixing_check.R` | MCMC mixing and acceptance, per city; HTML report | raw MCMC |
| mcmc | `mixing_correlated_parameter_pairs.R` | strongly correlated parameter pairs, and whether they share a sampling block | raw MCMC |
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
