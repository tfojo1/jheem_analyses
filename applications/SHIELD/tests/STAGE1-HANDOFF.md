# SHIELD stage-1 handoff check

Checked October 4, 2026. A short container run successfully used the actual
stage-0 and stage-1 calibration definitions, including stage 1's MSM diagnosis
likelihood. Stage 1 started with the exact 173 model-parameter values saved by
its predecessor, and both stages completed through summary and simset assembly.

| Check | Observed result |
|---|---|
| Actual sampled parameter sets | Stage 0: 88 variables; stage 1: 87 variables. |
| Actual likelihoods | Six registered terms in stage 0; twelve in stage 1, including `prop.male.ps.diag.among.msm`. Stored total log likelihoods and priors were finite. |
| Predecessor handoff | All 173 starting model parameters exactly matched the saved stage-0 summary; the stage-1 record identified that predecessor's output receipt by digest. |
| Completed outputs | Each stage produced a summary and two simulations with 173 finite parameters and finite selected outcome totals. Recorded output digests passed verification. |
| Repeated pipeline | Both completed stages were verified and skipped, not rerun. |

The test copies `calib.10.1.stage0` and `calib.10.1.stage1` into test-only names,
with two iterations, no burn-in, and no thinning. Scientific registration fields
remain unchanged. Production registrations, likelihood formulas, engine and
sampler behavior, installed server images, and active calibrations were not
changed.

This is a prerequisite check for an operator trial, **not** a scientifically
meaningful calibration, convergence assessment, full-stage performance test,
stage-2/3 test, or demonstration of identical sampling across restarts. The
separate [replay finding](REPLAY-COMPARISON.md) remains open. The container still
does not support multi-chain stage 3; full calibrations should use the usual
workflow for now.

## Evidence and rerun

The [hosted run and reports](https://github.com/ncsizemore/jheem-containers/actions/runs/37203984715)
passed with analyses `d23deca51b33f18bcbf3c726bf7b9264141caab0`, container source
`979c00930ddcd8523d3503a5d6e5c18a44498024`, engine
`9578726b012a2ee380b380ef0203733d1bd81163`, and sampler
`4e0d13e85857396bb0e6e2ac1d244775b2145f75`. The tested image was
`sha256:4eae3c47f8a2a1920aacd84d5ac463529abc3f2bf7581b63ea255c16899764b6`.
Inputs were the September 9 syphilis and August 26 census releases, seed 0,
with one BLAS thread and no network during execution. Their exact digests and
the full source selection are in `handoff.json`. Reports were downloaded and
the parameter transfer independently checked, not inferred only from job status.

Use the container workflow's **Check stage-1 handoff** input with
`september-2026`. The `shield-stage1-handoff` artifact contains `handoff.json`,
numeric reports, logs, and run records; retention is 14 days. Cache/simset bytes
are not uploaded. This run did not export an installable image or change the
server installation. See the [technical procedure](../RECORDED-RUN-PILOT.md#actual-stage-1-predecessor-handoff-diagnostic).
