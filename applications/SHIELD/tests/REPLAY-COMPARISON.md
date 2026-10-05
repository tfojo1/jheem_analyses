# Does SHIELD repeat the same calibration under the same seed?

Checked October 3, 2026. In a small container test, two fresh runs matched
exactly. Restarting from a saved checkpoint did **not** reproduce the
uninterrupted run's remaining samples, even though its checkpoint seeds and
saved state matched.

| Check | Result |
|---|---|
| Two fresh runs, seed 0 | Initial parameters, checkpoint seeds, samples, likelihoods, and adaptive state matched exactly. |
| Same setup, seed 1 | Checkpoint seeds and sampled values changed, showing that the configured seed affects this workflow. |
| Uninterrupted versus stopped/resumed, seed 0 | Completed checkpoints survived unchanged, and the resumed run completed. Its sampled trajectory first differed at iteration 4. |

At that first differing iteration, the uninterrupted run kept the MSM and
heterosexual transmission rates at `1.6, 1.6`. The resumed run accepted
`1.5435453720102092, 1.4921837799750717`. Their log likelihoods were
`-16484.35126810809` and `-16475.77615942093`. This is a different accepted
proposal, not a tiny floating-point discrepancy.

## Why a restart can change the samples

The sampler keeps its current simulation in memory between checkpoint chunks.
A new process does not have that simulation, so it rebuilds it before sampling
the next chunk. That happens **after** setting the chunk's seed, and before
drawing its proposals:

```r
set.seed(seed)
# ...
if (is.null(initial.sim))
    current.sim = control@simulation.function(chain.state@current.parameters)
else
    current.sim = initial.sim
```

See the pinned sampler's [chunk loop](https://github.com/tfojo1/bayesian.simulations/blob/4e0d13e85857396bb0e6e2ac1d244775b2145f75/R/cache.R#L350-L413)
and [starting simulation](https://github.com/tfojo1/bayesian.simulations/blob/4e0d13e85857396bb0e6e2ac1d244775b2145f75/R/adaptive_blockwise_metropolis.R#L296-L348).

In the tested engine, creating a SHIELD simulation also draws a random number
for its metadata seed. Its [seed-selection code](https://github.com/CIPHER-Epi/jheem2/blob/9578726b012a2ee380b380ef0203733d1bd81163/R/JHEEM_simulation.R#L1620-L1624)
reads `jheem.kernel$calibrated.parameters`, whereas the kernel exposes
[`calibrated.parameter.names`](https://github.com/CIPHER-Epi/jheem2/blob/9578726b012a2ee380b380ef0203733d1bd81163/R/JHEEM_kernel.R#L567-L573).
An instrumented local check of three real SHIELD fixed-parameter simulations
confirmed that each constructor advanced R's RNG by that metadata seed draw.
No package files or scientific formulas were changed for this check.

This is a concrete mechanism for shifting the proposal sequence on restart.
The [small sampler-only reproducer](reproduce-resume-rng.R) isolates it: the
same checkpoint seeds and saved adaptive state reproduce exactly when its toy
simulation consumes no random numbers, but diverge when simulation construction
consumes one draw. Preserving RNG state around only the toy's extra starting
simulation restores agreement. That last check isolates the mechanism; it is
**not** a general fix for stochastic simulations or a proposed production patch.

## What was tested

The [hosted experiment and downloadable reports](https://github.com/ncsizemore/jheem-containers/actions/runs/37161484559)
used the real Baltimore stage-0 model and likelihood: one chain, two sampled
transmission rates, eight iterations, and four two-iteration checkpoints. There
was no burn-in or thinning. The resumed run was interrupted after checkpoints
one and two; the harness checked complete checkpoint state while paused and
verified that earlier chunk files remained byte-identical afterward.

All four processes used the same image, offline manager bytes, initial model
parameters, and single-threaded BLAS setting. The reports retain full double
precision, excluding timestamps and durations from numerical comparisons.

- Analyses: `cd53da8e9613bb11f3fdefe868ccc90cee1749ed`.
- Container source: `221ab6f8e28f250e0dbf056fe0b808b7a23e6f99`.
- Image: `sha256:c2101eb594e4160a83c6f7113ba16fdb7690ea69be7428e2e5f98f1c1d5b3648`.
- Engine: `9578726b012a2ee380b380ef0203733d1bd81163`;
  sampler: `4e0d13e85857396bb0e6e2ac1d244775b2145f75`.
- Runtime: Linux x86-64, R 4.4.2, OpenBLAS; `OPENBLAS_NUM_THREADS=1`.
- Syphilis: `syphilis-manager-v2026.09.09`, SHA-256
  `c3e3c983d6b4e9c961f735f9c59d45483bd63cfa715cf75c1fae874da2d129e6`.
- Census: `data-managers-v2026.08.26`, SHA-256
  `f8c710684ccfdfb3a5c8e6f4b54caf0417ab67afb82b89ab3259c4b8151e1053`.

To rerun the real-model comparison, use the container workflow's optional
**Compare calibration traces** input with `september-2026`. It currently exits
unsuccessfully on the resumed-trace difference; reports are still uploaded.
The eight-iteration result does not establish convergence, full-stage or
multi-chain behavior, equality across servers, or the sampler API's explicit
seed-argument behavior. It does not establish that an existing calibration is
scientifically invalid.

The next engineering step is an owner-reviewed proposal for preserving
starting-simulation and RNG behavior across checkpoints, tested against this
comparison. Fixing the separate explicit seed-argument issue alone would not
address the extra simulation performed on restart. Neither package was patched,
and no installed server image or active calibration was changed.
