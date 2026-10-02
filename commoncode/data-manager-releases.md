# Selecting a data-manager release

`load.data.manager.from.cache()` continues to load the currently promoted data
manager by default:

```r
SURVEILLANCE.MANAGER <- load.data.manager.from.cache(
  "syphilis.manager.rdata",
  set.as.default = TRUE
)
```

To reproduce an earlier run or continue work while a newer manager is being
investigated, select an immutable GitHub Release tag for that run:

```r
SURVEILLANCE.MANAGER <- load.data.manager.from.cache(
  "syphilis.manager.rdata",
  set.as.default = TRUE,
  release.tag = "syphilis-manager-v2026.07.27"
)
```

The explicit-release path downloads the release asset to a version-specific
directory, verifies its published SHA-256 digest, and records its release
identity. It does not replace `cached/syphilis.manager.rdata` or change the
promoted default. Multiple releases can therefore coexist locally.

```text
cached/data-managers/
└── syphilis.manager.rdata/
    └── syphilis-manager-v2026.07.27/
        ├── resolution.json
        └── syphilis.manager.rdata
```

The resolved identity is available from the loaded manager:

```r
get.data.manager.resolution(SURVEILLANCE.MANAGER)
```

This includes the repository, requested and resolved tags, asset name, SHA-256
digest, publication time, and local cache path. Both default GitHub loading and
explicit release selection expose this identity. The OneDrive path and an
unverified legacy-only offline copy return `NULL`.

## SHIELD

The ordinary SHIELD bootstrap sets `SYPHILIS.MANAGER.RELEASE.TAG` to `NULL`
inside `shield_source_code.R`, selecting the promoted manager. Setting the same
variable before sourcing that file does not override it. Recorded runs instead
use their explicitly configured release. If `SURVEILLANCE.MANAGER` already
exists, the ordinary bootstrap retains that object and warns that its selector
was ignored. The generic loader examples above do not override these
application-specific choices.

## Public downloads and authentication

The managers in `tfojo1/jheem_analyses` are public; credentials are optional.
The loader uses `GITHUB_TOKEN`, or otherwise `GH_TOKEN`, for higher API rate
limits. If GitHub rejects that credential with HTTP 401, a request to this
repository's release API or public release-download path is retried once without
authentication, with a warning. The requested resource and digest verification
do not change. No anonymous retry is made for other repositories or endpoints,
HTTP 403/429 rate or permission failures, or a second failed request. Credentials
are not sent to download hosts other than `github.com` or `api.github.com`.

An online lookup that still fails stops rather than substituting a cached input.
This applies even when a previously verified version is available. Use explicit
offline mode below when loading cached data is intended; the error message names
that option. Updating authentication does not change an already running
calibration's loaded manager.

## Offline use

An immutable tag can be loaded with `offline = TRUE` after it has been cached.
The load succeeds only if the saved release metadata and artifact digest both
validate. A missing, incomplete, or modified cache is an error; the loader does
not fall back to a different release.

Passing a mutable `*-latest` alias as `release.tag` requires network access to
resolve it to its promoted immutable tag. Prefer the immutable tag for recorded
runs and offline work.

Omitting `release.tag` remains the supported way to follow the current promoted
manager. That route resolves the alias the same way, downloads and verifies the
promoted version into the same per-version cache, and records it in
`cached/data-managers/<manager>/current.json`. With `offline = TRUE`, it loads
that last verified version without checking for updates. It also keeps the
older `cached/<manager>` copy, with a `.version` file naming its release, up to
date for scripts that load that path directly. `get.data.manager.resolution()`
returns the resolved release for managers loaded either way.

Offline reads do not create locks or update the compatibility copy, so a verified
cache can be read-only. A broken current-version record or a corrupt verified
artifact fails explicitly. Only a legacy-only cache, with no current-version
record and an explicit `offline = TRUE` request, may use the old file with an
unverified-copy warning. This preserves deliberate offline access during
migration without assigning it a release identity; it is not automatic recovery
from an online lookup failure. Prefer an exact release with verified metadata
for a new analysis run.

The synthetic regression check needs neither real data nor the NAS:

```sh
Rscript commoncode/tests/test_data_manager_release_selection.R
```

It runs in the **Test manager cache** workflow independently of SHIELD's
scientific/model-test tiers.
