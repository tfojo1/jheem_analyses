# SHIELD calibration provenance

The supported SHIELD calibration launchers capture a shadow provenance record
without changing their command line or calibration arguments. Capture is enabled
by default and does not alter the sampler, its cache format, or the simulation-set
serialization.

The record answers four practical questions:

1. Which calibration and location were requested?
2. Which code, installed packages, and data managers were visible to the run?
3. Which cache configuration, starting parameters, and stored chunk seeds were
   created at setup?
4. Which simulation-set artifact was saved, and what is its SHA-256 digest?

## Files written

Calibration setup creates a unique run directory below the existing calibration
directory:

```text
<root>/mcmc_runs/<version>/<location>/<calibration>/provenance/
├── current.json
└── runs/<run-id>/
    ├── context.json
    ├── RUN_INFO.txt
    ├── events/
    └── receipt.json
```

`context.json` is the immutable setup-time record. Separate setup, chain, and
assembly processes share its run ID through `current.json`. Each chain writes a
distinct append-only event file, avoiding a shared status file during parallel
execution.

After `save.simulation.set()` succeeds, assembly writes two files beside the
simset:

```text
<simset>.Rdata.provenance.json
<simset>.Rdata.RUN_INFO.txt
```

The JSON sidecar contains the setup context and completed artifact receipt. The
text file is the concise human-readable view. The artifact itself is unchanged.

## Local spool and durability boundary

Each record is also copied to a machine-local spool. Its default location follows
R's per-user data-directory convention and can be overridden explicitly:

```sh
export JHEEM_PROVENANCE_SPOOL_DIR=/path/to/jheem-provenance
```

Completed artifacts are indexed in the spool by SHA-256 digest and run ID. This
permits a simset that has been separated from its sidecar to be matched to its
record on the machine that produced it without discarding distinct provenance
records for byte-identical artifacts:

```r
inspect.jheem.artifact.provenance("/path/to/simset.Rdata")
```

The local spool is an outbox, not yet a durable shared archive. A later archiver
can copy its immutable per-run files and digest index to a backed-up NAS location,
a serialized Git repository, or object storage without changing the record
schema or scientific run.

## Identity interpretation

The collector reports identity quality rather than filling gaps with guesses:

- `exact`: a Git commit or release tag plus digest is available;
- `modified`: the repository commit is known but the worktree has local changes;
- `version_only`: an installed package version is known but its source commit is
  unavailable; and
- `floating` or `unknown`: the loaded input cannot be resolved to an immutable
  artifact from the available information.

This distinction is important under the current source-mode workflow. The
executing `jheem2` code comes from the synchronized checkout, while
`bayesian.simulations`, `distributions`, and `locations` execute as installed
packages. A default syphilis-manager load remains floating; a run that uses an
explicit release tag records the resolved tag and SHA-256 digest as exact.

## Failure behavior and opt-out

Provenance capture is observational. A write or inspection failure emits a
warning but does not terminate the calibration or change its retry behavior.
Repository remotes are recorded without URL userinfo so credentials are not
copied into receipts. The collector checks that it does not advance
`.Random.seed`, and its focused test covers split-process events, cache seed
capture, adjacent receipts, digest-led recovery, opt-out, and an unavailable
local spool.

Capture can be disabled for troubleshooting:

```sh
export JHEEM_PROVENANCE_ENABLED=false
```

An opt-out run has no receipt and should therefore be treated as having unknown
provenance unless its inputs are reconstructed separately.
