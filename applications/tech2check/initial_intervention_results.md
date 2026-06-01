# Tech2Check — initial intervention results


## What this shows

Initial result from running the calibrated **Tech2Check intervention on
Maryland state-level baselines** (1000-sim posterior, sustained
recruitment over 2026–2030). Pipeline runs end-to-end; structural and
directional checks pass. Maryland state is the modeled geography because
current-spec MSA calibration is not yet available. Calibration fits ~82%
of CDC surveillance for 13–24 at 2023 with some recent-year drift — fine
for the relative effects reported here; worth noting for absolute
counts.

The simulated scenario is sustained recruitment of diagnosed-chronic
youth (13–24) into the four-state lifecycle
(`on_intervention → recently_intervened → distantly_intervened`), with
the trial OR (default 2.0) applied to suppression in `on_intervention`
and `recently_intervened` and OR = 1 in `distantly_intervened`.
Recruitment is held at 0.5/yr — a placeholder for `recruitment.rate`,
not a trial-derived value. Effects below are compared against a
no-intervention re-run of the same posterior at year 2030.

## Population-level effects (2030)

| Outcome       | Δ at 2030 (median) | CI low (2.5%) | CI high (97.5%) | % of base |
|:--------------|-------------------:|--------------:|----------------:|----------:|
| incidence     |              -0.56 |         -1.36 |           -0.08 |    -0.158 |
| new           |              -0.58 |         -1.91 |           -0.03 |    -0.147 |
| hiv.mortality |              -0.09 |         -0.13 |           -0.02 |    -0.013 |

Maryland, 1000-sim posterior, sustained 0.5/yr recruitment. Intervention
vs no-intervention at year 2030.

## The context — why the effect is small

![](initial_intervention_results_files/figure-commonmark/age-share-1.png)

A youth-only intervention is acting on roughly **1% of the diagnosed
prevalence** — even a strong per-person effect can only do so much
against that denominator. The smallness is a population-share story, not
a recruitment-volume story.

## The eligible pool depletes under the intervention

![](initial_intervention_results_files/figure-commonmark/pool-drain-1.png)

Cumulative reach over the 4-year window is **~310 enrollees** (median;
95% CI 208–388). The intervention is mechanically doing its job —
recruiting youth into the lifecycle and depleting the eligible pool
faster than it replenishes. A natural next question: what would pushing
recruitment harder do?

## Recruitment sensitivity

| Recruitment rate (/yr) | Cum. enrollments by 2030 | Δ incidence at 2030 | Δ mortality at 2030 |
|:---|---:|---:|---:|
| 0.5 | 309 | -0.56 | -0.086 |
| 2 | 389 | -0.58 | -0.085 |
| 10 | 973 | -0.62 | -0.086 |

Maryland, 1000-sim posterior; sustained recruitment at three rates.
Intervention vs no-intervention at year 2030.

Pushing recruitment from 0.5/yr toward saturation (10/yr) roughly
triples cumulative reach and drains the eligible pool to ~6 by 2030, but
the median effects at 2030 barely move. The reach→impact curve is
effectively flat from the base case onward — the conclusion is bounded
by the size of the eligible pool, not by recruitment intensity.

## Initial read

Looks like a structurally bounded result — the per-person effect is
real, but the population it acts on is small. Curious how you’d want to
frame this, whether the natural framing is still scale-up impact or
whether the small denominator wants a rethink.
