# Finding #34 revisited — the race-assortativity multiplier prior

Follow-up analysis of item 34 in `code_review_findings_2026-09-17.md`. **The original finding's central
arithmetic is wrong, but a real and smaller problem remains.** Everything below was computed from the
actual code path and real 2020 county census data for the ten SHIELD cities.

## What the original finding got wrong

It claimed the prior-median within-race O/E is `3.76 × 4 ≈ 15` for black-black, "far above any published
estimate." That treats the product as a realized observed/expected ratio. It is not. The product is an
unnormalized preference weight, and it passes through **two** normalizations before it becomes mixing:

1. `get.geographically.aggregated.race.oes()` divides projected partner counts by their row sums
   (`shield_specification_helpers.R:1002`) before dividing by the expected race proportions.
2. `oes.to.proportions()` renormalizes again when the O/E matrix is converted to contact proportions
   (`shield_specification_helpers.R:1034-1037`).

Both steps shrink the value. The realized within-race O/E at multiplier 4 is **2.6 to 8.3** for black-black
across the ten cities, not 15.

I verified this by reproducing the aggregation in closed form. For within-race entries it reduces to

```
aggregated O/E[r,r] = w * g / ( p * (1 + (w-1) * g) )
```

where `w` is the literature weight, `p` the MSA race proportion, and `g` the population-weighted mean local
race share (a county-level segregation index, `g >= p`). This matches the code to machine precision.

## The realized O/E has a hard ceiling of 1/p

Because the O/E is a ratio of proportions, a race with population share `p` cannot exceed `1/p` no matter how
large the multiplier. That ceiling is binding in the SHIELD cities:

| City | black share | ceiling on black-black O/E | literature value |
|---|---|---|---|
| Atlanta | 0.352 | 2.84 | 3.76 |
| Baltimore | 0.299 | 3.34 | 3.76 |
| Chicago | 0.166 | 6.0 | 3.76 |
| Los Angeles | 0.068 | 14.7 | 3.76 |

In Atlanta and Baltimore the published value of 3.76 is **mathematically unattainable**, whatever the
multiplier. This is the strongest argument in favour of calibrating a multiplier at all: a within-race O/E is
a property of a specific population, so one literature number cannot be imposed across MSAs.

## The direction of the bias is the opposite of what was reported

The finding said the calibration "can only inflate assortativity relative to the literature, never reduce
it." At multiplier 1 the model already sits **below** the literature in every city:

| Race | realized O/E at m = 1 | literature |
|---|---|---|
| black | 2.1 – 3.6 | 3.76 |
| hispanic | 1.4 – 2.3 | 2.19 |
| other | 1.1 – 1.6 | 1.55 |

So the multiplier floor of 1 does not pin the model above the literature. It pins it above a baseline that is
already conservative. The multiplier needed to reproduce the literature exactly is roughly 1.5 to 2.3 for
black, 1.2 to 2.1 for hispanic, and 2 to 3.5 for other, with no solution where the ceiling binds.

## What is genuinely wrong

The prior is `Lognormal(meanlog = log(4), sdlog = log(2))`, which is median 4 with a 95% interval of
[1.03, 15.6]. Two real problems:

- **The median is about twice too high.** A multiplier near 2 reproduces the literature; 4 overshoots it in
  every city where the ceiling is not binding, reaching a realized black-black O/E of 8.3 in Los Angeles,
  8.2 in Phoenix and 8.0 in Seattle against a literature value of 3.76.
- **The 2.5th percentile of 1.03 excludes the whole region below the literature-derived baseline.** At the
  upper end, a multiplier of 15.6 drives Los Angeles black-black assortativity to 13.1 against a ceiling of
  14.7, meaning near-total racial separation of partnerships. Assortativity is a direct driver of modelled
  racial disparity in incidence, so neither tail should be unconstrained by accident.

## Recommendation

Keep the base literature-derived aggregation and recentre the multiplier as a perturbation of it. The
aggregation is doing real work and should not be discarded: feeding it an all-ones matrix still returns
O/E values of 0.74 to 1.42 purely from county-level segregation, which is information the model should keep.

```r
# shield_calib_parameters.R:199-201
black.black.sexual.multi       = Lognormal.Distribution(meanlog = log(2), sdlog = log(2)),
hispanic.hispanic.sexual.multi = Lognormal.Distribution(meanlog = log(2), sdlog = log(2)),
other.other.sexual.multi       = Lognormal.Distribution(meanlog = log(2), sdlog = log(2)),
```

That gives median 2 and a 95% interval of [0.51, 7.8]: centred on the value that reproduces the published
O/E ratios, able to move below the literature baseline, and with an upper tail that no longer reaches the
mathematical ceiling. This is a prior change, so it invalidates comparison with calibrations run under the
current prior. It should not be applied silently to in-flight runs.

Whatever is chosen, the comment "Mu and SD are chosen empirically" should be replaced with a note that the
multiplier compensates for the normalization deflation, since the deflation is invisible at the call site and
is what made this look like a ×4-on-top-of-×3.76 error in the first place.

## Secondary observation — needs confirmation

The mixing matrix is not symmetric and the implied partnership counts do not balance. With real Chicago data
the aggregated matrix has black-to-hispanic 0.675 against hispanic-to-black 0.824, and the maximum relative
partnership imbalance is 18% at multiplier 1, rising to 32% at 4 and 41% at 16. That is, the number of
black-hispanic partnerships counted from the black side differs from the count from the hispanic side, and
raising the multiplier makes it worse.

This is a pre-existing property of the normalization, not something the multiplier introduced, and I have not
traced whether the jheem2 transmission engine rebalances downstream. Worth confirming before it is treated as
a defect.

## Reproducing this

The analysis needs only `jheem2`, `locations` and `cached/census.manager.rdata`. It deliberately does not
source `shield_source_code.R`, which would force-reset the sibling jheem2 checkout (finding #19).
