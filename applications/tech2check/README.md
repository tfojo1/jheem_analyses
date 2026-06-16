# Tech2Check

JHEEM extension assessing the population-level impact of scaling up the Tech2Check intervention for viral suppression in youth with HIV. Inherits from EHE; adds a 4-state intervention lifecycle (`diagnosed_chronic` → `on_intervention` → `recently_intervened` → `distantly_intervened`) on the `continuum` dimension. Recruitment is age-restricted to JHEEM's youth band (13-24 years); the suppression OR is applied per intervention compartment via dispatcher composition on `suppression.of.diagnosed`.

## Files

| File | Purpose |
|---|---|
| `tech2check_specification.R` | Canonical Tech2Check spec. Run via `source('applications/tech2check/tech2check_specification.R')` from jheem_analyses root. |
| `tech2check_engine_test.R` | Engine test: confirms spec sources, engine builds, sim produces output at Baltimore. Exits 0/1; CI/testthat-promotable. |
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
- **Recruitment:** one `diagnosed_chronic → on_intervention` transition whose rate is an age dispatcher (`tech2check.recruitment.rate`): youth (`13-24`) and adult (`25-34`) bands route to band-specific underlying rate elements (`tech2check.recruitment.rate.youth/.adult`), default 0 elsewhere. A single transition with a dispatched rate is required — jheem2 rejects two transitions on the same compartments regardless of `applies.to`. Youth-only base (adult rate 0) reproduces the original single-band model; the adult band is the broaden-the-pool sensitivity (#35). Rate is the primary policy lever.
- **Other lifecycle flows:** completion (`on → recently`, 6-month default), dropout (`on → distantly`), waning (`recently → distantly`).
- **Suppression effect:** OR-on-odds applied per intervention compartment via a per-compartment OR dispatcher (`tech2check.suppression.OR`) and a single-expression redefinition of `suppression.of.diagnosed` referencing `super.suppression.of.diagnosed`. Inherited tracked outcomes (notably `suppression`) pick up the per-compartment values automatically.

## Tracked outcomes (Tech2Check-specific)

| Outcome | Type | Use |
|---|---|---|
| `tech2check.enrollments` | integrated | Annual program enrollments, total (dispatcher-multiplied over 13-34) |
| `tech2check.enrollments.youth` / `.adult` | integrated | Annual enrollments split by recruitment band (13-24 / 25-34); sum to the total |
| `intervention.population` | point | Stock in each intervention lifecycle compartment (all three) |
| `person.years.on.intervention` | integrated | Person-time in `on_intervention` only (cost-analysis denominator) |
| `person.years.on.or.recently.intervened` | integrated | "Active intervention" aggregate (`on_intervention + recently_intervened`); correct denominator for headline policy claims, excludes `distantly_intervened` where OR = 1.0 in base case |
| `diagnosed.prevalence.by.intervention.state` | integrated | Per-state denominator for the suppression breakout |
| `suppression.by.intervention.state` | integrated | Per-state proportion suppressed; load-bearing for verifying OR dispatch |

## Additional working notes

The engine test is the minimum useful CI seed. A fuller verification suite (multi-location intervention-off match against EHE, OR=1 cancellation, etc.) is maintained in the tech2check working repo and will be mirrored here when stable.
