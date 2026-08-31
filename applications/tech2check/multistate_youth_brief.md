# Youth-only Tech2Check scale-up across 31 jurisdictions


- [What this looks at](#what-this-looks-at)
- [What happens across
  jurisdictions](#what-happens-across-jurisdictions)
- [How sensitive is that conclusion to the trial
  effect?](#how-sensitive-is-that-conclusion-to-the-trial-effect)
- [How consistent is the result across
  jurisdictions?](#how-consistent-is-the-result-across-jurisdictions)
  - [Absolute counts mainly reflect program
    scale](#absolute-counts-mainly-reflect-program-scale)
  - [Age composition is only a partial
    correlate](#age-composition-is-only-a-partial-correlate)
- [What the program delivers
  directly](#what-the-program-delivers-directly)
- [Does the model’s youth underallocation change the
  story?](#does-the-models-youth-underallocation-change-the-story)
- [How this connects to broader
  eligibility](#how-this-connects-to-broader-eligibility)
- [Bottom line](#bottom-line)
- [Jurisdiction-level results](#jurisdiction-level-results)

## What this looks at

We asked what would happen if the same youth-only Tech2Check scenario
were implemented in every calibrated jurisdiction available to the
model.

- **Population:** all diagnosed people ages 13–24.
- **Program:** one course, with recruitment at 0.5/year during
  2026–2030.
- **Effect while on or recently off the intervention:** suppression odds
  ratio of 2, using the trial point estimate. Fixed ORs of 0.90 and 4.47
  represent the trial confidence interval’s lower and upper anchors.
- **Comparison:** a matched no-program counterfactual for each of 1,000
  calibrated posterior draws in each jurisdiction.

These are conditional model projections. Within each OR scenario,
uncertainty intervals describe variation in the calibrated epidemic
draws while holding the program design and trial effect fixed. The three
ORs are separate scenario anchors, not a propagated probability
distribution for intervention efficacy.

## What happens across jurisdictions

At the OR 2.00 point estimate, the answer is consistent in kind but
variable in degree. The median jurisdiction averts 3.07 infections over
five years. Jurisdictional medians range from 0.79 to 43.46, while the
2030 incidence reduction remains small everywhere: 0.04% to 0.23%.

![Cumulative infections averted during 2026–2030 under the same
youth-only scenario. Points are jurisdictional posterior medians; lines
are conditional 95% posterior uncertainty
intervals.](figures/multistate_youth_brief-state-impact-1.svg)

The larger counts in some jurisdictions should not be read as large
epidemic effects: even the largest proportional reduction is less than
one quarter of one percent by 2030.

## How sensitive is that conclusion to the trial effect?

The trial estimate is imprecise enough that its lower anchor (OR 0.90)
implies slightly worse suppression, while its point and upper anchors
imply benefit. The modeled direction changes accordingly in every
jurisdiction. What does not change is the population-scale conclusion:
even at OR 4.47, the largest 2030 incidence reduction is less than one
half of one percent.

| Suppression OR anchor | Median infections averted | Range of jurisdictional medians | 2030 incidence change |
|:---|---:|---:|---:|
| 0.90 lower | -0.59 | -8.37 to -0.15 | 0.01%–0.05% increase |
| 2.00 point | 3.07 | 0.79 to 43.46 | 0.04%–0.23% reduction |
| 4.47 upper | 5.09 | 1.36 to 72.66 | 0.06%–0.42% reduction |

Trial-effect sensitivity across 31 jurisdictions. Each row is a fixed
conditional scenario, not an uncertainty distribution.

![Projected 2030 incidence change under the trial lower, point, and
upper suppression-effect anchors. Gray lines are jurisdictions; the teal
line is the cross-jurisdiction median. Negative values indicate reduced
incidence.](figures/multistate_youth_brief-effect-sensitivity-1.svg)

At OR 0.90, the cross-jurisdiction median is -0.59 infections
averted—that is, a small net increase in infections. At OR 4.47, the
median is 5.09 infections averted. All 31 conditional posterior
intervals lie on the harmful side at OR 0.90 and on the beneficial side
at OR 2.00 and 4.47. This is strong scenario consistency, but it should
not be mistaken for a frequentist claim about the probability that
Tech2Check is beneficial.

## How consistent is the result across jurisdictions?

### Absolute counts mainly reflect program scale

Absolute infections averted and cumulative enrollment are very closely
associated across jurisdictions (Pearson 0.968; Spearman 0.943). On the
logarithmic scale shown below, cumulative enrollment accounts for 93.7%
of the between-state variation in absolute infections averted. This is
partly mechanical and partly a reflection of epidemic and jurisdiction
size: an intervention can only act through people reached. The useful
conclusion is therefore not a newly identified state-level mechanism,
but that Maryland was not exceptional—the same scale constraint appears
throughout the calibrated jurisdictions.

![Absolute impact closely follows the number of people enrolled. Axes
are logarithmic; each point is a jurisdictional posterior
median.](figures/multistate_youth_brief-reach-impact-1.svg)

Impact per 1,000 enrollments still ranges from 8.82 to 23.38. Thus reach
does not explain everything: there is about a 2.6-fold range in
enrollment-normalized impact. The present analysis establishes that
residual variation, but it does not yet establish its epidemiologic
mechanism.

### Age composition is only a partial correlate

We also examined whether the intervention has greater impact in
epidemics with a relatively larger youth component. The observed 2023
share of diagnosed prevalence among ages 13–24 is the most directly
aligned surveillance measure because the modeled program recruits
diagnosed youth.

The youth share is weakly associated with absolute infections averted
(Pearson 0.121) and moderately associated with percentage reduction in
endpoint incidence (0.478) and infections averted per 1,000 enrollments
(0.426). This is directionally compatible with greater proportional
benefit in younger epidemics, but it is not a strong or complete
explanation. These are descriptive associations across 31 epidemics, not
causal estimates, a mechanistic explanation, or a state-allocation
model.

## What the program delivers directly

At the OR 2.00 point estimate, statewide infections averted are not the
only modeled benefit. Across jurisdictions, the program generates a
median of 21.3 additional suppressed person-years during 2026–2030, with
jurisdictional medians ranging from 7.9 to 257.6. This is the model’s
analytic suppression gain under the assumed odds-ratio effect, not an
observed clinical outcome.

Under that same point-estimate scenario, the one-course program requires
about 42.8 to 113.3 enrollments per infection averted across
jurisdictions. This is a transparent program-burden metric, not a
cost-effectiveness analysis. Together, these measures clarify that a
small statewide transmission effect does not mean zero direct benefit
for enrolled youth; it means that the direct benefit reaches too small a
share of the statewide epidemic to produce a large population-level
change.

## Does the model’s youth underallocation change the story?

The calibrated baselines understate 2023 diagnosed youth prevalence in
every jurisdiction. As a transparent sensitivity, we scaled each
jurisdiction’s paired intervention effect by its observed-to-modeled
youth stock ratio. This raises the median infections averted from 3.07
to 4.31, while preserving the cross-jurisdiction ordering closely
(Spearman 0.987).

That calculation is a reach sensitivity, not a recalibrated forecast. It
does not alter age mixing, epidemic feedback, depletion, or transmission
leverage. It supports the qualitative conclusion that youth-only reach
remains too small to generate a large statewide effect, but it does not
support resource-allocation rankings.

## How this connects to broader eligibility

The multistate analysis above deliberately holds eligibility to ages
13–24. In the prior Maryland analysis, expanding recruitment through age
34 produced a wide conditional bracket because the youth trial does not
establish an adult effect. The same adults were offered the program in
each broadened scenario; only the assumed adult suppression effect
changed.

![Maryland eligibility sensitivity. Broadening through age 34 increases
impact only to the extent that the youth intervention effect transports
to adults. These are conditional Maryland scenarios, not multistate
adult-effect
evidence.](figures/multistate_youth_brief-maryland-broadening-bridge-1.svg)

The Maryland medians are 2.76 infections averted for youth-only
eligibility and 2.23, 27.25, 40.90 under no, partial, and full
adult-effect transport, respectively.

This Maryland bracket illustrates the decision raised by the multistate
result: broader reach can change population impact, but only under
additional uptake and effect-transport assumptions. It should not be
generalized across jurisdictions without running and clearly labeling an
all-state adult-transport analysis.

## Bottom line

Across all available calibrated jurisdictions, one-course youth-only
Tech2Check scale-up produces a small statewide incidence effect under
the trial point and upper effect anchors, and slight harm under the
lower anchor. Effect uncertainty therefore changes direction and
magnitude, but not the population-scale conclusion: even the optimistic
anchor reduces 2030 incidence by less than 0.5% in every jurisdiction.
At the point estimate, variation in absolute infections averted mainly
reflects how many youth are enrolled, and the Maryland result
generalizes in direction across all 31 jurisdictions. Age composition is
only a partial descriptive correlate and does not explain the remaining
variation.

Substantially larger population effects would therefore require broader
reach and evidence that uptake and efficacy transport beyond the trial
population. The current analysis does not establish that adult
transport.

## Jurisdiction-level results

| Jurisdiction | Infections averted \[95% UI\] | Cumulative enrollments | Additional suppressed PY | Averted per 1,000 enrollments | Enrollments per infection averted | 2030 incidence change |
|:---|---:|---:|---:|---:|---:|---:|
| AL | 3.71 \[2.65, 6.26\] | 213 | 21.3 | 18.20 | 54.9 | -0.15% |
| AR | 2.64 \[1.42, 4.35\] | 134 | 17.1 | 19.79 | 50.5 | -0.16% |
| AZ | 3.07 \[1.88, 5.92\] | 225 | 28.2 | 13.77 | 72.6 | -0.10% |
| CA | 16.11 \[11.63, 21.56\] | 1317 | 126.4 | 12.34 | 81.1 | -0.13% |
| CO | 1.96 \[0.81, 3.72\] | 192 | 16.7 | 10.14 | 98.6 | -0.19% |
| DC | 0.79 \[0.42, 1.31\] | 64 | 9.9 | 12.17 | 82.2 | -0.14% |
| FL | 18.72 \[13.39, 25.88\] | 1062 | 128.6 | 18.12 | 55.2 | -0.13% |
| GA | 18.29 \[13.01, 23.51\] | 1391 | 145.2 | 13.29 | 75.3 | -0.19% |
| IL | 5.73 \[3.86, 8.68\] | 384 | 44.1 | 14.66 | 68.2 | -0.14% |
| IN | 2.07 \[1.39, 3.34\] | 146 | 17.8 | 14.94 | 66.9 | -0.11% |
| KY | 0.93 \[0.39, 2.03\] | 104 | 12.2 | 8.82 | 113.3 | -0.08% |
| LA | 8.58 \[5.21, 12.54\] | 363 | 33.8 | 23.38 | 42.8 | -0.23% |
| MA | 0.89 \[0.46, 1.58\] | 74 | 8.5 | 12.06 | 82.9 | -0.04% |
| MD | 2.76 \[2.07, 4.02\] | 308 | 27.3 | 9.60 | 104.2 | -0.13% |
| MI | 2.43 \[1.37, 4.88\] | 177 | 15.9 | 14.26 | 70.1 | -0.21% |
| MN | 1.31 \[0.73, 2.29\] | 140 | 13.6 | 9.31 | 107.4 | -0.13% |
| MO | 4.18 \[2.58, 6.48\] | 190 | 22.4 | 21.90 | 45.7 | -0.22% |
| MS | 2.69 \[1.74, 5.30\] | 150 | 14.1 | 18.64 | 53.6 | -0.14% |
| NC | 15.08 \[10.32, 21.79\] | 785 | 96.0 | 19.19 | 52.1 | -0.22% |
| NJ | 3.16 \[1.85, 4.82\] | 212 | 20.8 | 14.92 | 67.0 | -0.07% |
| NV | 1.38 \[0.74, 2.26\] | 105 | 13.8 | 12.85 | 77.8 | -0.06% |
| NY | 11.11 \[7.49, 16.32\] | 595 | 56.9 | 18.53 | 54.0 | -0.15% |
| OH | 5.46 \[3.40, 8.72\] | 357 | 36.5 | 15.58 | 64.2 | -0.19% |
| OK | 2.46 \[1.03, 4.42\] | 157 | 21.0 | 14.87 | 67.2 | -0.22% |
| PA | 4.63 \[2.92, 6.82\] | 350 | 45.5 | 13.26 | 75.4 | -0.19% |
| SC | 3.11 \[2.32, 7.53\] | 268 | 27.0 | 12.22 | 81.8 | -0.11% |
| TN | 8.75 \[6.30, 16.74\] | 480 | 69.9 | 18.52 | 54.0 | -0.20% |
| TX | 43.46 \[31.47, 56.25\] | 2144 | 257.6 | 20.28 | 49.3 | -0.22% |
| VA | 1.91 \[1.17, 3.05\] | 160 | 14.6 | 12.08 | 82.8 | -0.10% |
| WA | 1.74 \[0.97, 2.92\] | 196 | 15.7 | 8.99 | 111.3 | -0.07% |
| WI | 1.24 \[0.59, 2.43\] | 105 | 7.9 | 11.75 | 85.1 | -0.13% |

Youth-only OR 2.00 point-estimate results by jurisdiction. Intervals are
conditional on the fixed program and effect assumptions.
