# Ryan White Data Manager Compatibility Report

**Schema:** `jheem-manager-compatibility/v1`

## Inputs

| Role | File | SHA-256 | Embedded creation | Embedded modification |
|---|---|---|---|---|
| Baseline | `ryan.white.web.data.manager.rdata` | `4f1b5063ae6f6e9ffa4b254d4cad71fdf088903295339fb59a17e71819f99989` | 2025-04-08 16:21:06 CDT | 2025-04-08 16:25:53 CDT |
| Candidate | `ryan.white.data.manager.2026-03-16.rdata` | `cc227cb9bdf43d9948f97db54d9c2652f034c4b780a8515cb57c99ea6f735188` | 2026-03-16 11:42:34 CDT | 2026-03-16 11:46:08 CDT |

## Outcome comparison

| Outcome | Status | Shared cells | Changed values | Candidate fills | Candidate gaps |
|---|---:|---:|---:|---:|---:|
| `adap.clients` | `candidate_only` | 0 | 0 | 0 | 0 |
| `adap.clients.service.distribution` | `candidate_only` | 0 | 0 | 0 | 0 |
| `adap.income.distribution` | `candidate_only` | 0 | 0 | 0 | 0 |
| `adap.proportion` | `identical` | 3060 | 0 | 0 | 0 |
| `adap.proportion.of.diagnosed` | `candidate_only` | 0 | 0 | 0 | 0 |
| `adap.suppressed.proportion.of.diagnosed` | `candidate_only` | 0 | 0 | 0 | 0 |
| `adap.suppression` | `additive` | 208 | 0 | 0 | 0 |
| `awareness` | `baseline_only` | 0 | 0 | 0 | 0 |
| `diagnosed.prevalence` | `compatible_on_overlap` | 754936 | 0 | 2998 | 0 |
| `diagnoses` | `baseline_only` | 0 | 0 | 0 | 0 |
| `non.adap.clients` | `identical` | 19902 | 0 | 0 | 0 |
| `oahs.clients` | `identical` | 7719 | 0 | 0 | 0 |
| `oahs.suppression` | `identical` | 7719 | 0 | 0 | 0 |
| `prep` | `baseline_only` | 0 | 0 | 0 | 0 |
| `proportion.tested` | `baseline_only` | 0 | 0 | 0 | 0 |
| `proportion.tested.n` | `baseline_only` | 0 | 0 | 0 | 0 |
| `suppression` | `baseline_only` | 0 | 0 | 0 | 0 |
| `total.prevalence` | `baseline_only` | 0 | 0 | 0 | 0 |

## Required outcome contract

| Outcome | Check | Actual status | Allowed statuses |
|---|---:|---:|---|
| `non.adap.clients` | `passed` | `identical` | `identical` |
| `oahs.clients` | `passed` | `identical` | `identical` |
| `oahs.suppression` | `passed` | `identical` | `identical` |
| `adap.proportion` | `passed` | `identical` | `identical` |
| `adap.suppression` | `passed` | `additive` | `identical`, `additive` |
| `diagnosed.prevalence` | `passed` | `compatible_on_overlap` | `identical`, `additive`, `compatible_on_overlap` |

## Candidate derived-target checks

| Check | Status | Comparable cells | Mismatches | Maximum absolute difference |
|---|---:|---:|---:|---:|
| `adap_proportion_of_diagnosed_total` | `passed` | 305 | 0 | 0 |
| `adap_proportion_of_diagnosed_by_sex` | `passed` | 200 | 0 | 0 |
| `adap_proportion_of_diagnosed_by_age` | `passed` | 505 | 0 | 0 |
| `adap_proportion_of_diagnosed_by_race` | `passed` | 548 | 0 | 0 |
| `adap_suppressed_proportion_of_diagnosed_total` | `passed` | 127 | 0 | 0 |

## Scope and provenance boundary

- This is a selective target-compatibility report, not a whole-manager compatibility or interchangeability claim.
- Compatibility on shared target cells does not establish that the candidate manager was used to fit the historical posterior.
- Candidate-only outcomes, including adap.clients, have no historical numeric compatibility evidence in the baseline display manager.
- Derived-target checks establish formula consistency within the candidate manager; they do not establish historical identity of the inputs or derived values.

## Interpretation contract

- `identical`: paths, dimensions, missingness, and values are unchanged.
- `additive`: every baseline path/value is retained; the candidate only adds dimension values or fills.
- `compatible_on_overlap`: shared values are unchanged, but one manager has paths or dimension values absent from the other.
- `incompatible`: a shared value changed, a baseline value became missing, or dimensions cannot be aligned.
