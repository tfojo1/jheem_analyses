# SHIELD test suite

Before this existed, nothing in CI ran the SHIELD model. The three workflows in
`.github/workflows/` build and validate the syphilis **data manager**; none of
them builds the specification, runs the engine, scores a likelihood, or runs an
intervention. Every numerical bug in the September 2026 code review would have
passed CI untouched.

## Start here

**New to testing? Read [HOW-TO-RUN.md](HOW-TO-RUN.md) first.** It explains what
a test is, how to run these, how to read the output, and what to do when
something fails. This file is the reference: what the suite covers and how it is
put together.

## Running it

From the repo root:

```bash
Rscript applications/SHIELD/tests/run_tests.R              # everything
Rscript applications/SHIELD/tests/run_tests.R static       # static tier only
Rscript applications/SHIELD/tests/run_tests.R unit         # static + unit
Rscript applications/SHIELD/tests/run_tests.R integration  # everything
Rscript applications/SHIELD/tests/run_tests.R --filter=spline
```

Exit status is 0 when nothing failed. **Skips are not failures**: a tier whose
inputs are missing says why and is not counted against you.

| Environment variable | Effect |
|---|---|
| `JHEEM_SYPHILIS_MANAGER_TAG` | Manager release tag to test against. `latest` follows the promoted manager. Default: the pinned tag in the bootstrap. |
| `SHIELD_TEST_SKIP_SLOW=true` | Skip the engine and intervention runs. |

## The tiers

| Tier | Needs | Time | Files |
|---|---|---|---|
| **static** | R only | ~7 s | `parse` (every file parses, every `source()` target exists, no diff markers, no deleted-but-tracked files) · `wiring` (launcher ↔ register, runner ↔ intervention definitions, sampling blocks) · `hygiene` (secrets, absolute paths, seeding, scratch files, mutating git) · `penalties` (the two custom penalty likelihoods' known problems) · `bootstrap-drift` |
| **unit** | jheem2, `distributions` | ~3 s | `spline-prior` (all three builders) · `doxy-effectiveness` · `pairing` · `base-parameters` · `restratify` (the data-ingest funnel) · `error-terms` (the likelihood CV estimator) |
| **integration** | cached data managers | ~22 s | `manager-contract` · `jheem2-api` · `specification` · `spec-helpers` · `ontology-mappings` · `parameters` (prior, apply-fn, register) · `engine` · `likelihoods` · `interventions` |

## Files

```
tests/
  HOW-TO-RUN.md               start here if you are new to tests
  README.md                   this file
  run_tests.R                 CLI runner: tier selection, skip explanations
  testthat.R                  standard testthat entry point
  shield_test_bootstrap.R     loads a SHIELD environment, without side effects
  testthat/
    helper-shield.R           skip helpers, fixtures, memoised simulation
    test-static-*.R           tier 1
    test-unit-*.R             tier 2
    test-integration-*.R      tier 3
```

### `shield_test_bootstrap.R` is not `shield_source_code.R`

The production loader cannot be used from a test, because sourcing it

* runs `git pull` on `jheem_analyses`,
* runs `git reset --hard` and `git checkout -f dev` on `../jheem2`, discarding
  uncommitted work there, and
* downloads the currently promoted surveillance manager over the network.

A test must not mutate the working tree, and a test whose input changes whenever
someone promotes a manager is not a regression test. So the bootstrap performs
the same *loading* steps with the side effects removed, and pins a manager tag
by default.

That duplication is checked, not assumed: `test-static-bootstrap-drift.R`
asserts that the set of files the bootstrap sources still matches the set
`shield_source_code.R` sources, so the bootstrap cannot silently fall out of
date and leave the integration tier testing a different model than production
runs.

### Three things the bootstrap has to work around

These are properties of the codebase, not of the tests, and each one is also
asserted somewhere in the suite.

1. **The model only runs from the repo root.**
   `commoncode/cache_object_for_version_functions.R` resolves its cache with the
   literal path `../jheem_analyses/commoncode/object_for_version_cache`, and
   about forty SHIELD files use the same `../jheem_analyses/` prefix. testthat
   resets the working directory to the test directory before every file, so any
   test that touches the specification calls `use_repo_root()`, which is scoped
   to that test.

2. **`ROOT.DIR` is relative.** `commoncode/file_paths.R` leaves it as
   `../../files` on a laptop, so everything the jheem root points at moves with
   the working directory. The bootstrap resolves it once against the repo root,
   falling back to a session temp directory when the real archive is absent.

3. **The installed jheem2 and the `dev` clone are not interchangeable.**
   `use_jheem2_package_setting.R` sets `USE.JHEEM2.PACKAGE <- F`, so production
   runs the clone. The installed jheem2 1.12.0 has
   `create.custom.likelihood.instructions(name, compute.function,
   get.data.function, verbose)`; the clone adds `weights`, and
   `shield_likelihoods.R` passes `weights = 1`. Against the package, sourcing
   the likelihoods dies with `unused argument (weights = 1)`.

   The bootstrap prefers the clone and falls back to the package if the clone
   will not source (it compiles C++ via `Rcpp::sourceCpp`, which needs a
   toolchain). `test-integration-jheem2-api.R` then reports the mismatch by
   name, and the likelihood and intervention tests skip with that reason instead
   of erroring.

### Tests that are meant to fail today

Part of the suite's job is to hold the open findings from the code review so
they cannot be lost. Those tests fail on purpose, and each failure message
states the problem and what the fix looks like. A failing assertion here is a
to-do item, not a broken test.

If you fix one, the test turns green and stays green. If you decide a finding is
not worth fixing, delete the test and say why in the commit message — do not
loosen it until it passes.

## Adding a test

* **A new pure function** goes in the unit tier. Add the file to
  `SHIELD.TEST.STANDALONE.FILES` in the bootstrap if it is not already sourced.
* **Anything needing the specification** goes in the integration tier, starts
  with `skip_unless_stage("has.shield.helpers")`, and calls `use_repo_root()`
  inside any test that builds an engine.
* **Start every file with `local_edition(3)`.** Without it `test_dir()` falls
  back to testthat edition 2, where `tolerance` is not the strict relative
  difference you expect — an 8% error passes a 5% tolerance.
* Prefer asserting a **contract** (columns sum to 1, uncertainty grows away from
  the baseline, diagnoses cannot exceed prevalence) over a frozen number.
  Contract tests survive re-parameterisation; golden numbers do not.
* Write the failure message so it argues its own case. Several tests here
  compute the size of the effect they are complaining about and put it in the
  message.
