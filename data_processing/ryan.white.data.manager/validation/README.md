# Ryan White Data Manager Compatibility

This tooling performs a selective target comparison between a historical web/display manager and a
newer full Ryan White manager without modifying either input. It verifies input SHA-256 hashes
before deserialization, aligns arrays by
named dimensions, compares shared cells exactly, classifies additive and structural differences,
enforces required outcome-status contracts, and validates the documented total-level ADAP-derived
likelihood formulas in the candidate manager. The derived checks cover total, sex, age, and race
ADAP proportions of diagnosed prevalence, plus the total ADAP-suppressed proportion of diagnosed
prevalence; they reproduce the owner processing code's age aggregation, race mapping, and ratio
filter.

## Run the comparison

From the `jheem_analyses` repository root:

```sh
Rscript data_processing/ryan.white.data.manager/validation/compare_managers.R \
  --baseline /path/to/ryan.white.web.data.manager.rdata \
  --baseline-sha256 4f1b5063ae6f6e9ffa4b254d4cad71fdf088903295339fb59a17e71819f99989 \
  --candidate /path/to/ryan.white.data.manager.2026-03-16.rdata \
  --candidate-sha256 cc227cb9bdf43d9948f97db54d9c2652f034c4b780a8515cb57c99ea6f735188 \
  --output-json /tmp/ryan-white-manager-compatibility.json \
  --output-markdown /tmp/ryan-white-manager-compatibility.md
```

The reports contain basenames, hashes, embedded manager metadata, structural/value comparison
results, and derived-target checks. They do not contain local input paths or raw observations.

## Status meanings

- `identical`: paths, dimensions, missingness, and values are unchanged.
- `additive`: every baseline path/value is retained; the candidate only adds dimension values or
  fills previously missing cells.
- `compatible_on_overlap`: shared values are unchanged, but one manager has paths or dimension
  values absent from the other.
- `incompatible`: a shared value changed, a baseline value became missing, or dimensions cannot be
  aligned.

Compatibility does not establish historical artifact identity or whole-manager interchangeability.
In particular, passing this check does not claim that the March 2026 manager was used to fit a
posterior created earlier. Candidate-only outcomes such as `adap.clients` have no historical numeric
compatibility evidence in the April 2025 display manager. The derived checks establish internal
formula consistency in the candidate, not historical identity of their inputs or results.

### Legacy `adap.clients` overlay caveat

Historical Ryan White simulation metadata mapped the simulated `adap.clients` outcome to observed
`non.adap.clients`, so plot exporters using `corresponding.observed.outcome` can duplicate the
non-ADAP observations under the ADAP label. Model source corrected that mapping in commit
`e986be4f42f48ee8045f3530d1eeaa279052c022` on January 9, 2026. A duplicated overlay from an older
deployed simset is therefore evidence of the legacy metadata defect, not evidence that the two
observed series are equivalent. Keep `adap.clients` outside the historical compatibility contract
unless a separately identified historical full manager can be compared directly.

## Test the comparison logic

```sh
Rscript data_processing/ryan.white.data.manager/validation/test_manager_compatibility_tools.R
```
