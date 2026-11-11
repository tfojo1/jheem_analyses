# SHIELD paper 1 (doxy-PEP): frozen model and results

This page records what produced the results in the first SHIELD paper and where each piece is kept. Everything below
was checked against the saved outputs on the NAS in November 2026.

## What ran

Every step used the data releases `census-manager-v2026.10.08` and `syphilis-manager-v2026.10.09`.

| Step | When (2026) | Server | `jheem_analyses` | `jheem2` |
|---|---|---|---|---|
| Calibration stage 1, all 10 cities | Oct 20-21 | shield3 | `shield-paper1-stage1` | `shield-paper1-calibration` |
| Stages 2 and 3, all 10 cities | Oct 21-23 | shield3 (6 cities), shield2 (4) | `shield-paper1-stages2-3` | `shield-paper1-calibration` |
| Stage 4, all 10 cities | Oct 23 - Nov 4 | shield3 (6), shield2 (4) | `shield-paper1-stage4`; shield2: `shield-paper1-stage4-shield2` (same model code) | `shield-paper1-calibration` |
| Stage 3 result set (800 simulations) | Oct 23 | | built from the stage 3 chains (below) | |
| Stage 3 interventions | Oct 23 | shield1 | intervention code as in `shield-paper1-interventions-stage4` | `shield-paper1-calibration` |
| Stage 4 result set (1,000 simulations) | Nov 3-4 | | built from the stage 4 chains (below) | |
| Stage 4 interventions: no intervention and doxy-PEP 5-15% (all cities), 20% (3 cities) | Nov 5 | shield2 | `shield-paper1-interventions-stage4-run1` | `shield-paper1-interventions-stage4-run1` |
| Stage 4 interventions: doxy-PEP 20% (7 cities), 25-50% (all cities) | Nov 5 | shield3 | `shield-paper1-interventions-stage4` | `shield-paper1-interventions-stage4` |

The six shield3 cities are Atlanta (C.12060), Baltimore (C.12580), Chicago (C.16980), Houston (C.26420), Los Angeles
(C.31080) and Miami (C.33100); the four shield2 cities are New York (C.35620), Philadelphia (C.37980), Phoenix
(C.38060) and Seattle (C.42660). shield3 runs R 4.6.0 with deSolve 1.42; shield1 and shield2 run R 4.5.2 with
deSolve 1.40. All three have bayesian.simulations 0.2.2, distributions 1.0.0 and locations 0.4.0.

The result sets come straight from the chains: the 800-simulation set is the last half of each stage 3 chain, and
the 1,000-simulation set is the last half of each stage 4 chain, every 10th sample.

## Where everything is

- **Code:** the tags above, on GitHub.
- **Data:** the two GitHub releases above, which never change. Each has a SHA-256 checksum that loading verifies.
- **Results:** on the NAS, under `mcmc_runs/shield/calib.10.20.stage1` to `stage4`, `mcmc_summaries/shield/`,
  `simulations/shield/calib.10.20.stage3-800` and `calib.10.20.stage4-1000`, and `shield/outputs/`. A checksum list,
  `shield/paper1-freeze/MANIFEST.sha256`, gives every file's SHA-256, so any later change can be detected.
- **Run records:** beside each output, in a `run_records/` folder (below).
- **Software:** each server's R version and full package list, captured in November 2026, are in
  `shield/paper1-freeze/env/`. That folder also holds a copy of this page and the list of tags.

## Run records

Since November 7, each step of the modular SHIELD launcher writes a short run record: the code commits, package
versions, data releases with their checksums, the server, and the seed. The paper's runs happened before that, so
their records were written afterwards from the shared logs, git history and the saved outputs. There are 230: one
for each setup, chain and assembly of each city and stage, and one for each result set and intervention run.

Each record is a small text file in the `run_records/` folder of the calibration or result set it describes, for
example `mcmc_runs/shield/calib.10.20.stage4/C.12060/run_records/run-chain1-...-reconstructed.dcf`. Read one in R:

```r
record <- read.dcf("run-chain1-20261023T154849Z-shield3-reconstructed.dcf", all = TRUE)
```

Each row is one entry: the step itself, then its code (`source`), its data (`data manager`), and for steps that
saved results, its outputs. The paper's records are marked `reconstructed: yes`, and each fact in them says where it
came from (`basis`): seen directly in a log, git history or file; worked out from those; or confirmed by rerunning
the step.
