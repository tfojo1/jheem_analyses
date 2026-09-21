# How to run the SHIELD tests

Written for someone who has not used an automated test suite before. It assumes
you know R and you know SHIELD; it assumes nothing about testing.

If you only read one line: open a terminal, go to the `jheem_analyses` folder,
and run

```bash
Rscript applications/SHIELD/tests/run_tests.R
```

Everything below explains what that does and how to read what comes back.

---

## 1. What a test suite is, in this context

A test is a small piece of R code that runs part of SHIELD and then states
something that ought to be true — "the population is never negative", "the
mixing matrix columns sum to 1", "every calibration code the launchers run is
one the register defines". If the statement holds, the test passes silently. If
it does not, you get a message naming the file, the line, and what was expected
versus what happened.

The point is not to prove the model is right. It is to notice, in seconds, the
specific class of mistake that otherwise surfaces days later as a strange plot
or a cluster job that produced nothing.

Three things follow from that, and they are worth internalising early:

- **A test that passes tells you nothing new.** That is fine. Silence is the
  product.
- **A failing test is information, not an emergency.** Read the message. It was
  written to explain itself.
- **You do not have to run all of them all the time.** See the tiers below.

---

## 2. Running them

All commands are run **from the repo root** (`/Users/Trinity/JHEEM/jheem_analyses`),
not from inside the tests folder. This matters: SHIELD resolves paths like
`../jheem_analyses/...`, so the working directory is not arbitrary.

```bash
cd /Users/Trinity/JHEEM/jheem_analyses
```

### Everything

```bash
Rscript applications/SHIELD/tests/run_tests.R
```

About 30 seconds. Builds the specification, runs the model once, and checks
everything.

### Just the fast checks

```bash
Rscript applications/SHIELD/tests/run_tests.R static
```

About 7 seconds. Reads the source files without running the model. This is the
one to run after editing anything — it catches unparseable files, broken
`source()` paths, and launcher/register mismatches.

### Fast checks plus the maths

```bash
Rscript applications/SHIELD/tests/run_tests.R unit
```

About 10 seconds. Adds the spline priors, doxy-PEP efficacy, pairing maths and
base parameters.

### One area only

```bash
Rscript applications/SHIELD/tests/run_tests.R --filter=spline
Rscript applications/SHIELD/tests/run_tests.R --filter=doxy
Rscript applications/SHIELD/tests/run_tests.R --filter=integration-engine
```

`--filter` matches against test file names. Every test file's header block tells
you the exact filter to use for it.

### From inside an R or RStudio session

```r
setwd("/Users/Trinity/JHEEM/jheem_analyses")
source("applications/SHIELD/tests/run_tests.R")
```

Note that `run_tests.R` calls `quit()` at the end, which will close your R
session. In RStudio, prefer the terminal, or run:

```r
setwd("/Users/Trinity/JHEEM/jheem_analyses")
library(testthat)
test_dir("applications/SHIELD/tests/testthat", filter = "spline")
```

---

## 3. Reading the output

A run ends with a line like:

```
passed 356 | failed 22 | errors 0 | skipped 22 | 32s
```

### passed

Assertions that held. Not "tests" — one test usually makes several assertions.

### failed

An assertion that did not hold. You get a block like this:

```
── 21. Failure ('test-unit-doxy-effectiveness.R:34:5'): the fitted lognormal ...
Expected `fitted.lo` to equal `RR.LO`.
Differences:
  `actual`: 0.074
`expected`: 0.080

fitted 2.5% limit is 0.07355, published limit is 0.08
```

Read it as:

- `test-unit-doxy-effectiveness.R:34:5` — file and line. Open it; the comment
  above the assertion explains why the check exists.
- `Expected ... to equal ...` — the assertion, in testthat's words.
- The paragraph at the bottom is the message *I* wrote for that specific check.
  That is the part that tells you what to do.

### errors

Different from a failure. A failure means the check ran and the answer was
wrong. An **error** means the test code itself blew up before it could check
anything — a missing function, a bad argument. Errors usually mean the test
needs fixing, or the environment is not set up.

### skipped

The test did not run, because something it needs is not available. Skips are
**not failures** and do not affect the exit status. The runner prints the reason
for each one. Typical reasons on this machine:

```
Reason: shield_likelihoods.R could not be sourced: unused argument (weights = 1)
        (jheem2 in use: package 1.12.0)
```

That is the suite telling you the environment is wrong, not the model. See §6.

### Exit status

`0` if nothing failed, `1` otherwise. That is what CI uses. You can check it
yourself with `echo $?` right after a run.

---

## 4. Some tests are *supposed* to fail right now

This is the part that is unusual, so it is worth being explicit.

Part of this suite's job is to hold the open findings from the September 2026
code review so they cannot be lost track of. Those tests fail on purpose. Each
one names a real problem and says what fixing it would look like. For example:

```
the growth penalty still adds 1 to the 10-year denominator:
  total_vals[as.character(end_year)] / (total_vals[as.character(start_year)] + 1),
At a 2020 count of 2 this understates the ratio by 33%, at 5 by 17%, and at
2000 by ~0%. Use pmax(denominator, eps) or skip empty strata instead.
```

**A failing assertion here is a to-do item, not a broken test.**

If you fix the underlying problem, the test turns green and stays green — that
is how you know the fix worked. If you decide a finding is not worth fixing,
delete the test and say why in the commit message. Do not loosen a test until it
passes; that throws away the only record that the problem exists.

The currently-failing set covers: the doxy-PEP efficacy centring, the growth
penalty's denominator, the historical penalty's ineffective guard, a reversed
confidence interval in the base parameters, the `correlation = 0` NaN in the
spline prior, unregistered calibration codes in the launchers, interventions the
runner asks for but nothing builds, a committed pre-signed S3 URL, hardcoded
`Q:/` paths, unseeded random draws, and tracked scratch files.

---

## 5. What to do when something fails

1. **Read the message at the bottom of the block.** It was written to be read.
2. **Open the file and line it names.** The comment above the assertion explains
   the reasoning.
3. **Decide which of three things happened:**
   - *The model changed and the test is now wrong.* Update the test, and say in
     the commit message why the new expectation is right.
   - *The model changed and broke something.* Fix the model.
   - *The test was always wrong.* It happens. Fix the test.
4. **Re-run just that file** with `--filter=` until it is green.

If you are unsure which of the three it is, that uncertainty is itself the
useful output — it means the invariant was never written down anywhere before.

---

## 6. When a lot of things skip

Skips almost always mean the environment, not the code. The runner prints the
cause at the end of the run.

### "jheem2 in use: package 1.12.0" / "unused argument (weights = 1)"

`use_jheem2_package_setting.R` sets `USE.JHEEM2.PACKAGE <- F`, so SHIELD is
meant to run against the **`dev` clone** at `../jheem2`, not the installed
package. The clone compiles C++ at load time via `Rcpp::sourceCpp()`.

If that compile fails, the bootstrap falls back to the installed package, which
is older and lacks things SHIELD calls (`weights`, `get.null.intervention`,
`collapse.with.and`), so the likelihood, intervention, ontology and restratify
tests skip.

On this machine the compile fails because the **Xcode licence has not been
accepted**:

```bash
sudo xcodebuild -license accept
```

Then re-run. The skips should go to zero. Note this affects *running SHIELD at
all* on this machine, not just the tests — `shield_source_code.R` loads jheem2
the same way.

### "bootstrap stage 'has.surveillance.manager' unavailable"

The cached data manager is missing. The integration tier skips; the static and
unit tiers still run. To test against a different manager:

```bash
JHEEM_SYPHILIS_MANAGER_TAG=syphilis-manager-v2026.07.27 \
  Rscript applications/SHIELD/tests/run_tests.R

JHEEM_SYPHILIS_MANAGER_TAG=latest \
  Rscript applications/SHIELD/tests/run_tests.R
```

`latest` follows whatever manager is currently promoted — worth running before
promoting a new one, because the suite checks that the manager still provides
every stratification the model pulls on.

### Runs are slow and you only want the cheap checks

```bash
SHIELD_TEST_SKIP_SLOW=true Rscript applications/SHIELD/tests/run_tests.R
```

Skips the engine and intervention runs.

---

## 7. Writing a new test

The shortest useful version. Open any existing file for a fuller example.

```r
test_that("a plain-English statement of what must be true", {
    ## A comment saying WHY this matters - what breaks, silently, if it is false.
    result <- some.shield.function(arguments)

    expect_true(all(result >= 0),
                info = "a message that explains the failure to whoever reads it")
})
```

Rules of thumb for this suite:

- **Put it in the right tier.** Needs no data manager → a `test-unit-*.R` file.
  Needs the specification → `test-integration-*.R`, starting with
  `skip_unless_stage("has.shield.helpers")`.
- **Start every file with `local_edition(3)`.** Without it, testthat falls back
  to edition 2, where `tolerance` is not the strict relative difference you
  expect — an 8% error silently passes a 5% tolerance. This bit me while writing
  the suite.
- **Call `use_repo_root()`** inside any test that builds a specification or an
  engine. testthat resets the working directory before every file, and SHIELD
  only works from the repo root.
- **Assert a contract, not a number.** "columns sum to 1" survives
  re-parameterisation; "equals 0.4275" does not.
- **Write the `info =` message for a reader who has not seen the code.** Several
  tests here compute the size of the effect they are complaining about and put
  it in the message. That is the standard to aim for.

### The helpers available to you

| Helper | What it does |
|---|---|
| `skip_unless_stage("has.shield.helpers")` | Skip unless the bootstrap loaded that far |
| `skip_unless_sim()` | Returns the memoised engine/params/sim, or skips |
| `skip_unless_likelihoods()` | Skip unless `shield_likelihoods.R` sourced |
| `skip_unless_slow()` | Honour `SHIELD_TEST_SKIP_SLOW` |
| `use_repo_root()` | Set the working directory for this test only |
| `SHIELD.DIR`, `REPO.ROOT` | Paths |
| `SHIELD.TEST.LOCATION` | The MSA the integration tier uses (Baltimore) |

---

## 8. It also runs in CI

`.github/workflows/test-shield.yml` runs the static tier on every push touching
`applications/SHIELD/`, then the unit tier, then the model tier against a pinned
manager. You do not have to do anything for that to happen.

You can also trigger it by hand from the Actions tab, choosing a jheem2 branch
and a manager release tag — which is the cheapest way to answer "would this new
manager break SHIELD?" before promoting it.

---

## 9. Where things are

```
applications/SHIELD/tests/
  HOW-TO-RUN.md            this file
  README.md                what the suite covers, and how it is built
  run_tests.R              the runner
  shield_test_bootstrap.R  loads SHIELD without git pulls or downloads
  testthat/
    helper-shield.R        the helpers in the table above
    test-static-*.R        read the source, run nothing
    test-unit-*.R          self-contained maths
    test-integration-*.R   build the specification and run the model
```

Every test file starts with a header saying what it covers, why it matters, and
the exact `--filter=` to run just that file.
