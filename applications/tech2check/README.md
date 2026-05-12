# Tech2Check

JHEEM extension assessing the population-level impact of scaling up the Tech2Check intervention for viral suppression in youth with HIV. Inherits from EHE; adds a 4-state intervention lifecycle (`diagnosed_chronic` → `on_intervention` → `recently_intervened` → `distantly_intervened`) on the `continuum` dimension. Recruitment is age-restricted to JHEEM's youth band (13-24 years); the suppression OR is applied per intervention compartment via dispatcher composition on `suppression.of.diagnosed`.

## Files

| File | Purpose |
|---|---|
| `tech2check_specification.R` | Canonical Tech2Check spec. Run via `source('applications/tech2check/tech2check_specification.R')` from jheem_analyses root. |
| `tech2check_engine_test.R` | Engine test: confirms spec sources, engine builds, sim produces output at Baltimore. Exits 0/1; CI/testthat-promotable. |
| `tech2check_specification_guided.r` | Earlier sketch variant. Not the production spec. |
| `design.md` | Model design reference: compartment structure, parameters, rationale. |

## Quick start

```sh
# From jheem_analyses root:
Rscript applications/tech2check/tech2check_engine_test.R
```

Expected output: `OK: sim produced (class=jheem.simulation.set/...)`, exit code 0.

## Design overview

- **Parent spec:** EHE (`parent.version = 'ehe'`).
- **Compartment extension:** `continuum` dimension gains three intervention-lifecycle states beyond `diagnosed_chronic`. `diagnosed_chronic` is retained as "never intervened."
- **Recruitment:** `diagnosed_chronic → on_intervention` transition, age-restricted to `'13-24 years'` via `applies.to = list(age = TECH2CHECK.ELIGIBLE.AGES)`. Rate is the primary policy lever (`tech2check.recruitment.rate`).
- **Other lifecycle flows:** completion (`on → recently`, 6-month default), dropout (`on → distantly`), waning (`recently → distantly`).
- **Suppression effect:** OR-on-odds applied per intervention compartment via a per-compartment OR dispatcher (`tech2check.suppression.OR`) and a single-expression redefinition of `suppression.of.diagnosed` referencing `super.suppression.of.diagnosed`. Inherited tracked outcomes (notably `suppression`) pick up the per-compartment values automatically.

## Tracked outcomes (Tech2Check-specific)

| Outcome | Type | Use |
|---|---|---|
| `tech2check.enrollments` | transition | Annual program enrollments per stratum |
| `intervention.population` | point | Stock in each intervention lifecycle compartment |
| `person.years.on.intervention` | integrated | Person-time in active intervention (cost-analysis input) |
| `diagnosed.prevalence.by.intervention.state` | integrated | Per-state denominator for the suppression breakout |
| `suppression.by.intervention.state` | integrated | Per-state proportion suppressed; load-bearing for verifying OR dispatch |

## Additional working notes

The engine test is the minimum useful CI seed. A fuller verification suite (multi-location intervention-off match against EHE, OR=1 cancellation, etc.) is maintained in the tech2check working repo and will be mirrored here when stable.
