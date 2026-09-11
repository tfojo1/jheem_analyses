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
digest, publication time, and local cache path. Managers loaded through the
default or legacy paths return `NULL` because those paths do not yet provide the
same immutable identity contract.

## SHIELD

SHIELD uses the promoted manager unless a selector is provided before
`shield_source_code.R` is sourced:

```r
SYPHILIS.MANAGER.RELEASE.TAG <- "syphilis-manager-v2026.07.27"
source("../jheem_analyses/applications/SHIELD/shield_source_code.R")
```

The same selection can be made for a batch job without changing an R script:

```sh
JHEEM_SYPHILIS_MANAGER_TAG=syphilis-manager-v2026.07.27 Rscript my_run.R
```

Set the selector before the manager is loaded. If `SURVEILLANCE.MANAGER`
already exists, `shield_source_code.R` retains that object and warns that the
selector was ignored.

## Offline use

An immutable tag can be loaded with `offline = TRUE` after it has been cached.
The load succeeds only if the saved release metadata and artifact digest both
validate. A missing, incomplete, or modified cache is an error; the loader does
not fall back to a different release.

The mutable `syphilis-manager-latest` alias requires network access to resolve
to its promoted immutable tag. Prefer the immutable tag for recorded runs and
offline work. Omitting `release.tag` remains the supported way to follow the
current promoted manager.
