# SHIELD Code Review — Fix Status as of 2026-09-18

Re-verification of every item in `code_review_findings_2026-09-17.md` against the current
`jheem_analyses` checkout (HEAD `40e32de2`). Each item was checked by reading the current code;
the numerical items (#2) were re-run.

## Fixed and verified (7)

| # | Item | Verification |
|---|---|---|
| 1 | Heterosexual-male age counts were MSM counts | `R/shield_specification_helpers.R:871-872` now returns male minus MSM. |
| 2 | Joint spline prior past-year covariance built backwards | Re-ran the numerical test on `make.joint.mv.spline.prior()`: past-side SDs are now 10.10 / 1.418 / 1.005 for 1970 / 1990 / 1995 (baseline 1.00), matching the future side's structure. Also correct for two parameters. **This is the function used by the live calibration.** |
| 3 | `status_check.R` would not parse | Bash arrays are commented out; the file parses. |
| 4 | Top-level `return()` in historical-penalty helper | File deleted; `source()` line removed from `shield_likelihoods.R`. |
| 5 | `mean(a, b)` in pairing manager | Now `mean(c(a, b))`. |
| 7 | Fitted `sdlog` computed but ignored | Historical penalty now reads `data$sdlog`, computed from the national 1970–1993 series in the surveillance manager. Verified the slice exists in the cached manager: min ratio 0.486, max 1.0, sd(log) = 0.215. |
| 11 | Stray zero-argument `create.natural.spline.functional.form()` | Line removed. |

## Not fixed (31)

All of the following are unchanged from the 2026-09-17 findings. Line references still hold.

**Live model / statistical:** 6 (age-mixing CSVs lose first row, `header=TRUE`), 8 / 38n (`duration.tertiary`
and `duration.cns` scale labels), 33 (`prp.infections.among.msm.1970` uses `log(.5)` as a logit),
34 (×4 assortativity prior median), 37 (three values for MSM-with-female fraction; base-parameter CI still
malformed), 9, 10, 38a–o.

**Infrastructure / production:** 17 (41 files still hardcode `../jheem_analyses/`), 18 (`launch_run_chains.sh`
still runs `calib.9.10.stage3.az`, which is not registered; the register's latest is `calib.9.11.stage3.az`),
19 and 20 (forced git reset on `jheem2` on every source, `FETCH.JHEEM2.UPDATES <- T`, up to 20 racing
chains), 21 (silent local fallback for `ROOT.DIR`), 22 (dead manager-pin config), 23 (edit-in-place
launcher config, hardcoded username), 25 (retry loops hide the error; "5 minutes" message vs 15-second sleep),
26 (broken test reprex path, no CI for the model), 27, 28 (`.RDataTmp` 67 MB, `analysis/backups/`,
`intervention/untitled folder/`, `CROI copy` all still tracked), 29 (pre-signed S3 URL still in a comment).

**Intervention pipeline:** 24 (`stop.for.errors = FALSE`, no completeness check), 30 (runner asks for
`doxy.cov.60`–`100`, definitions build `doxy.cov.5`–`50`), 31 (unseeded `rlnorm`, mis-centred lognormal),
32 (`doxy.uptake` never driven).

**Analysis:** 36 (chain 1 only; `analysis/calibration/calibration_plots.R` defaults `sim.subset = "last20"`
in four places).

**Cleanup:** 12, 13 (seven backup launchers still resolve `LOG_DIR` inside `launchers/`), 14, 15, 16.

## New issues found in this pass

### N1. The fix to `make.mv.spline.prior()` pasted diff markers into the code (regression)
`R/shield_multivariate_spline_prior.R:241-243`. Three lines begin with a literal `+` copied from a diff view.
R parses them as unary-plus operators, so the past-year assignment becomes `+++M[i, ...] <- 1` and the
function now throws `could not find function "+<-"` whenever there is at least one past year. Confirmed by
running it. This function has no live callers (only `make.joint.mv.spline.prior()` is used, and that one is
correct), so calibration is unaffected, but the "corrected" function is now unusable. Remove the three `+`
characters. The stale "BUG:" comment in the test block at ~line 489 can also be removed.

### N2. `analysis/doxy_summary_aug.R` cannot be parsed
Line 293 opens `t2.outcomes <- c(` and every element **and the closing parenthesis** were commented out, so
the file is syntactically incomplete from line 293 to the end. Everything after it (Table 2 checks) cannot
run. Found by parsing all 102 SHIELD `.R` files; this and the dormant
`intervention/untitled folder/doxy_interventions_april.R:29` are the only two that fail to parse.

### N3. Joint spline prior returns all-NaN with `correlation = 0`
`R/shield_multivariate_spline_prior.R:~395` divides the diagonal by `correlation` to undo the off-diagonal
scaling. Input validation accepts any value in [-1, 1], but 0 yields a NaN covariance. The live call uses
0.7, so this is latent. Guard the zero case or tighten validation.

### N4. Historical penalty: ineffective guard, and the sharpness changed
`shield_likelihoods.R:218-221`. The `tryCatch` returns a character string on failure, and the very next line
divides by it, so sourcing still errors if the national series is missing. The guard adds nothing. Separately,
the penalty sharpness moved from the placeholder 0.347 to the data-derived 0.215, which is the spread of the
national trajectory rather than a measurement-error scale. That is a defensible design choice, but any stage-3
calibration launched before commit `09a5f5d6` used the old value, so old and new runs are not directly
comparable and the choice should be documented.

### N5. Growth-penalty divide-by-zero fix biases small strata
`shield_likelihoods.R:543-545` (commit `55bd55c6`, pre-dates the review but was not flagged). Adding 1 to the
2020 denominator shifts the 10-year ratio downward in small race/sex strata. A `pmax(x, eps)` floor or
skipping empty strata would avoid the bias.

### N6. Clarification on #22 (manager pin vs cache)
The pinned `syphilis-manager-v2026.07.27` is what actually loads, from
`cached/data-managers/syphilis.manager.rdata/syphilis-manager-v2026.07.27/`. The root-level
`cached/syphilis.manager.rdata.version` (`v2026.08.26`) is a separate legacy cache file, not the one in use.
The env-var override path is still dead code.

### N7. Clarification on #17 (which clone runs)
This checkout is `/Users/Trinity/JHEEM/jheem_analyses`, so the hardcoded `../jheem_analyses/` paths resolve
to itself when run from here. The `jheem_analyses copy` clone still exists at commit `d23ef506`, twelve
commits behind, and is the only place the two review documents
(`Documentation/code_review_findings_2026-09-17.md`, `spline_prior_bug_explainer.md`) exist. They are
untracked there.

### N8. No run since the fixes
`applications/SHIELD/logs/` is empty and no commit after the fixes touches run outputs. The #1 fix
(array subtraction of male minus MSM single-year counts) is correct by inspection but has not been exercised
end to end. A single specification build for one city would confirm the two arrays are conformable.
