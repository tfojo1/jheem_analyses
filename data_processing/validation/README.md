# Data Manager Validation

Spec-based structural validation for jheem data managers. Catches build errors — missing outcomes, missing sources, ontology changes — before they reach calibration.

## How It Works

1. A **spec** captures the structural fingerprint of a known-good data manager (what outcomes, sources, ontologies, stratifications, and dimension values exist).
2. After each build, **validation** checks the new manager against the spec and reports any mismatches.
3. When the structure changes intentionally, **regenerate** the spec and commit it — the git diff shows exactly what changed.

## Quick Start

### Run validation (syphilis manager)

```bash
Rscript data_processing/syphilis.manager/validation/run_validation.R path/to/syphilis.manager.rdata
```

### Regenerate the spec after a structural change

```bash
Rscript data_processing/syphilis.manager/validation/generate_spec.R path/to/known-good-manager.rdata
git diff data_processing/syphilis.manager/validation/syphilis_manager_spec.json
# Review the diff, then commit
```

## File Layout

```
data_processing/
  validation/                           # Generic tools (work with any data manager)
    manager_spec_tools.R                #   extract_manager_spec()
                                        #   validate_manager_structure()
                                        #   print_validation_results()
    data_quality_report.R               #   report_data_quality()
                                        #   (NA analysis, component consistency,
                                        #    marginal consistency)

  syphilis.manager/validation/          # Syphilis manager specific
    syphilis_manager_spec.json          #   Structural spec (committed, diffable)
    generate_spec.R                     #   Regenerate spec from a manager
    run_validation.R                    #   Entry point for validation

  surveillance.manager/validation/      # (future — same pattern)
```

## What the Spec Captures

For each outcome/source/ontology/stratification:
- **Fixed dimensions** (race, sex, age): exact values — catches ontology changes
- **Variable dimensions** (year, location): exact values — lost values fail, new values warn

## Interpreting Results

The output has two distinct layers. Layer 1 (structural validation) is a pass/fail gate. Layer 2 (data quality report) is informational — it surfaces things worth reviewing but does not affect the pass/fail result.

### Layer 1: Structural Validation

The structural validation header looks like:

```
Checked 47 outcomes, 82 sources, 96 ontologies, 354 stratifications (579 total)
Result: 579 passed, 0 failed
```

Each number is a count of checks at that level of the data manager hierarchy. The data manager is organized as: outcome > source > ontology > stratification > array. Each check verifies that something present in the spec is also present in the manager with the correct structure.

**Outcomes (47):** Each outcome is a data variable the model uses — things like `ps.syphilis.diagnoses`, `population`, `hiv.diagnoses`. The check confirms the outcome exists in the manager.

**Sources (82):** Each outcome can have data from multiple sources. For example, `ps.syphilis.diagnoses` has four sources: `cdc.aggregated.county`, `cdc.sti`, `cdc.sti.surveillance.reports`, and `lhd`. The 82 is the total number of outcome-source pairs across all outcomes, not 82 distinct sources. The check confirms each expected source exists under its outcome. A missing source means a build step was skipped or broke — this is what happened when `total.syphilis.diagnoses` lost its `cdc.aggregated.county` source.

**Ontologies (96):** Each source stores data under an ontology, which defines the categorical scheme — what race categories, age groups, etc. are used. The 96 is the total count of outcome-source-ontology paths. The check confirms the expected ontology exists. If an ontology was swapped (e.g., `cdc.sti.two` with 7 age groups replaced by `cdc.sti` with 10 age groups), the old ontology would show as missing.

**Stratifications (354):** Each ontology contains one or more stratification arrays. A stratification like `year__location__race` is a numeric array with those three dimensions. The 354 is the total count of outcome-source-ontology-stratification paths. For each stratification, the check verifies:

- The array exists
- It has the expected dimensions
- **Fixed dimensions** (race, sex, age) have exactly the expected category values — if the categories changed, this catches it
- **Variable dimensions** (year, location) haven't lost any values that were previously present. New values (e.g., a new year of data) produce a warning but pass. Lost values (e.g., a year that was present before is now missing) are a failure.

### Layer 2: Data Quality Report

The data quality report has up to three sections:

**NA Analysis:** For each stratification array in the manager, reports the percentage of cells that are NA. Results are grouped by severity (high >=50%, moderate 10-50%, low <10%) with the top 10 worst shown. Each line reads as:

```
95.9% NA: prep > cdc.prep > cdc > year__location__sex
          ^^^^   ^^^^^^^^   ^^^   ^^^^^^^^^^^^^^^^^^
          outcome source    ontology stratification
```

This means: in the PrEP data from the CDC PrEP source, under the `cdc` ontology, the array broken down by year, location, and sex has 95.9% empty cells. High NA rates are normal for sparse county-level demographic data — not every county reports every variable for every year by every demographic breakdown. The value of this section is tracking *changes* over time: if a source that was 60% NA suddenly jumps to 95%, something may have gone wrong upstream.

**Component Consistency:** Checks that totals are at least as large as the sum of their components. For example, `total.syphilis.diagnoses` should be >= `ps.syphilis.diagnoses` + `early.syphilis.diagnoses` + `unknown.duration.or.late.syphilis.diagnoses`. This is checked cell by cell across matching year-location pairs.

**Marginal Consistency:** When available, uses `inspect_marginals()` to check that demographic breakdowns sum to their aggregates (e.g., the race-stratified values for a given year and location should sum to the unstratified value for that same year and location). Currently may be skipped due to a technical limitation with deserialized managers — see the note in `data_quality_report.R`.

## Updating the Spec

When a structural change is intentional (e.g., new outcome, ontology switch):

1. Make the code change and rebuild the manager
2. Run `generate_spec.R` against the new manager
3. Review the JSON diff — it should reflect your intended change and nothing else
4. Commit both the code change and the updated spec

The spec should only change when the structure changes. If it drifts without a corresponding code change, something unexpected happened.
