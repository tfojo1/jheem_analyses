# Seattle (SHIELD) vs King County doxy-PEP study

Compares SHIELD's Seattle MSA (C.42660) projections with the population-level effect of doxy-PEP
reported for King County, WA: Menza et al., *Clin Infect Dis* 2026;83:267-73 (PMID 41885610,
doi:10.1093/cid/ciag209). Kept separate from the main manuscript analysis: nothing here reads or
writes the main result arrays (`total_raw`, `sex_raw`) or the main `tables/` and `figures/` folders.

## What we did

1. **King County-aligned scenarios.** The main paper scales doxy-PEP up linearly from 2023 to a 2030
   target, which reaches only ~11% coverage in 2024 even at the 50% target. King County reported
   12-15% of MSM using doxy-PEP in 2023 and 31-45% by mid-2024. We ran two Seattle scenarios on the
   stage 4 posterior (`calib.10.20.stage4`, 1,000 simulations): coverage among MSM aged 15-64 from 0%
   in Jan 2023, 15% by mid-2023, then **30% or 45% by May 2024**, held constant.
   Codes: `doxy.kc.30.flat`, `doxy.kc.45.flat`. Two variants rising to 50% by end-2025 (`*.to50`)
   were also run; they differed by <1.5 points in the comparison window and are not analysed.
2. **Outcome and window.** Total (all-stage) syphilis diagnoses among all men (MSM + heterosexual men),
   women and the total population, cumulative over the paper's primary post period (Mar 2023-Jun 2025).
   Annual model outputs are weighted by the fraction of each year in the window.
3. **Two counterfactuals**, each computed within every simulation (median and 95% interval reported):
   - **A. Model effect:** scenario vs SHIELD with no doxy-PEP.
   - **C. ITS method applied to SHIELD:** scenario vs SHIELD's 2021-2022 linear trend extended forward,
     mirroring the paper's interrupted time series counterfactual. The difference between A and C is
     the **trend artifact**: how much a trend-based method adds when the epidemic departs from a straight line.

## What we found (primary window, 30% / 45% scenarios)

| Group | Model effect (A) | ITS method on SHIELD (C) | King County |
|---|---|---|---|
| Total | -24% / -29% | -33% / -37% | -52% [-74, -31] |
| All men | -26% / -31% | -36% / -40% | -53% [-79, -28] |
| Women | -12% / -14% | -12% / -14% | -47% [-58, -36] |

- **Men:** after applying the same method, SHIELD agrees with the lower end of the King County estimate.
  About a third of the apparent gap comes from the ITS assuming the 2021-22 rise would continue linearly,
  whereas MSM growth in SHIELD slows after 2022 even without doxy-PEP.
- **Women:** SHIELD projects a much smaller reduction, and the method does not explain the gap. Possible
  reasons: SHIELD's assumptions about bridging between MSM and women, and/or factors other than doxy-PEP
  in King County (national syphilis declines 2022-24; the authors could not attribute the decline in
  women to doxy-PEP with certainty).
- **Epidemic growth:** SHIELD projects continued growth after 2022 (diagnosis calibration targets end in
  2021): +75% in monthly diagnoses pre vs post even at 45% coverage, vs about +4% observed in King County.
  Post-2021 projections should be validated against Seattle surveillance data.
- **Coverage check:** the `doxy.coverage` outcome counts MSM aged 15-64 on doxy-PEP, so coverage
  shares must use MSM aged 15-64 as the denominator, not `population.msm` (all ages, ~30% too low).
  The main manuscript scripts were checked and are not affected.

## How to run (from `jheem_analyses`)

| Step | Script | Notes |
|---|---|---|
| 00 | `00_settings_seattle_kc.R` | Shared settings and package check; sourced by 01 and 02 |
| 01 | `01_extract_seattle_kc.R` | Loads Seattle `noint` + scenario simsets one at a time; saves `seattle_kc_raw_results.Rdata` |
| 02 | `02_seattle_vs_king_county.R` | Tables and figures. Clear `seattle.kc.results` from the session (or use `Rscript`) after re-running 01 |

Simsets were run with `intervention/intervention_definitions_KC.R`, `intervention/intervention_run_KC_parallel.R`
and `launchers/launch_interventions_parallel_KC.sh` (Seattle only, 4 scenarios in parallel).

## Outputs (`<root>/shield/outputs/calib.10.20.stage4/seattle_vs_king_county/`)

| File | Contents |
|---|---|
| `tables/kc-1_counterfactual.csv` | Estimand A vs paper, all windows and groups |
| `tables/kc-2_pre_post.csv` | Pre-post change in monthly diagnoses, model vs crude observed |
| `tables/kc-3_doxy_coverage.csv` | Coverage reached among MSM aged 15-64, by year and window |
| `tables/kc-4_its_on_model.csv` | Estimand C, estimand A, trend artifact, paper estimate |
| `figures/kc-4_combined_trajectories_and_effects.png` | Main figure: (A) projected diagnoses with/without doxy-PEP and the extended trend; (B) relative reductions vs King County |
| `figures/kc-1` to `kc-3` | Supporting figures |

## Open items

- Validate SHIELD's 2022-2024 Seattle projections against observed MSA surveillance data (row A of the
  main figure is titled "Observed vs projected" but currently shows projections only).
- Check the share of women's infections linked to MSM in Seattle (`diagnostics/deepDive/07_women_exposure_share_from_msm.R`).

`backups/` holds earlier versions of these scripts, including the superseded `03_seattle_vs_king_county_old06.R`.
