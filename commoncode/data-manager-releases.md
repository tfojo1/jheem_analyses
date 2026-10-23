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

After successfully loading a verified release, the loader prints the resolved
tag and SHA-256 digest. This applies to exact selection, following the promoted
alias, and verified offline reads. The message describes the loaded file, not
just the requested version; it is useful in launch logs but is not a complete
run record.

## SHIELD

The ordinary SHIELD bootstrap loads the releases declared in
`applications/SHIELD/data_managers.csv` (columns `name,file,release`; lines
starting with `#` are comments). Syphilis must name a dated release. Census may
too (`census-manager-v2026.10.08`), or be `unpinned`, which keeps it on the
release named by the shared cached-metadata file. A named census release is
loaded from GitHub Releases and verified like syphilis; loads that name no census
release, in SHIELD or elsewhere, keep the OneDrive path. To change a release, edit
the file and commit. Keep the releases fixed
across a calibration's stages; selecting a different manager does not update an
earlier stage's results or make its checkpoints safe to resume. Setting
`SYPHILIS.MANAGER.RELEASE.TAG` before sourcing the bootstrap does not override the
file. Recorded runs always use their configured releases instead, and the
container wrapper reads the same file from the commit it captures.

After loading, the bootstrap registers the managers as `census` and `syphilis`,
so code can refer to them as `get.data.manager("census")` (with a jheem2 that has
the data-manager registry). If `CENSUS.MANAGER` or `SURVEILLANCE.MANAGER` already
exists, the ordinary bootstrap keeps that object, and warns when it is not the
declared release or has changed since it was loaded. The generic loader still
follows the promoted release when given no tag; SHIELD's declaration does not.

## Public downloads and authentication

The managers in `tfojo1/jheem_analyses` are public; credentials are optional.
The loader uses `GITHUB_TOKEN`, or otherwise `GH_TOKEN`, for higher API rate
limits. If GitHub rejects that credential with HTTP 401, a request to this
repository's release API or public release-download path is retried once without
authentication, with a warning. The requested resource and digest verification
do not change. No anonymous retry is made for other repositories or endpoints,
HTTP 403/429 rate or permission failures, or a second failed request. Credentials
are not sent to download hosts other than `github.com` or `api.github.com`.

A named immutable release (`release.tag = "syphilis-manager-v2026.10.09"`) that is
already cached, and whose file still matches the SHA-256 recorded when it was
downloaded and verified, loads without contacting GitHub: it is exactly the file
requested. A `*-latest` alias, a release not yet cached, or a cached copy that fails
verification is resolved online. An online lookup that fails stops rather than
substituting a cached input; for the promoted alias this applies even when an older
verified version is cached. Use explicit offline mode below when loading cached data
is intended; the error message names that option. Updating authentication does not
change an already running calibration's loaded manager.

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
