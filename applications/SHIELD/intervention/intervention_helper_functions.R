# =============================================================================
# Path Builder
# Constructs the full .Rdata file path for one location × intervention pair.
# Mirrors the hardcoded pattern from the original code.
# =============================================================================

build.simset.path <- function(base.path,
                              calibration.code,
                              n.sim,
                              location.code,
                              intervention.code) {
    run.tag  <- paste0(calibration.code, "-", n.sim)
    filename <- paste0("shield_", run.tag, "_", location.code, "_", intervention.code, ".Rdata")
    file.path(base.path, run.tag, location.code, filename)
}
# =============================================================================
# Core Loader — with cache resolution
#
# Cache lookup priority (when force.reload = FALSE):
#   1. Explicit `cache` argument, if provided
#   2. Object named `cache.name` in globalenv(), if it exists
#   3. Nothing found → load everything from file
#
# For each location × intervention, the function:
#   - Uses the cached simset if found (no file I/O)
#   - Loads from file only if missing from cache
#   - Merges newly loaded simsets into the returned list
#
# Arguments:
#   locations           Named character vector (names = city, values = codes)
#   intervention.codes  Character vector of intervention codes
#   calibration.code    Calibration run identifier
#   n.sim               Number of simulations per set
#   base.path           Root path to simulation folders
#   intervention.labels Named vector mapping code -> display label
#   cache               (optional) An already-loaded named simset list to reuse
#   cache.name          Name of the object to look for in globalenv() as fallback
#                       cache. Defaults to "all.simsets".
#   force.reload        If TRUE, ignore all caches and reload everything from file
#   verbose             Print progress and summary messages
# append = TRUE  → keep everything already in cache, add missing keys on top.
#                  The returned list grows with each call.
# append = FALSE → return only the keys matching the requested
#                  locations × interventions (cache still consulted to avoid
#                  redundant file reads, but unrelated simsets are dropped).
# =============================================================================
# =============================================================================
# load.all.simsets()
#
# Loads JHEEM simsets across locations × interventions, with caching to avoid
# re-reading from disk and an `append` mode to accumulate across calls.
#
# Arguments:
#   locations           Named char vector — names = city display names,
#                       values = location codes (e.g. SHIELD.TEN.MSAS).
#   intervention.codes  Char vector of intervention codes to load.
#   calibration.code    Calibration run ID string.
#   n.sim               Number of simulations per set.
#   base.path           Root path to simulation folders.
#   intervention.labels Named char vector mapping code -> display label.
#                       If NULL, codes are used as labels.
#   cache               (optional) A previously returned simset list.
#   cache.name          Name to look up in globalenv() if `cache` is NULL.
#                       Defaults to "all.simsets".
#   force.reload        TRUE → ignore all caches, reload everything.
#   append              TRUE → keep all cached simsets in the output, even
#                              ones unrelated to the current request.
#                       FALSE → return only the requested keys (cache still
#                               consulted to skip redundant file reads).
#   verbose             Print progress and summary.
# =============================================================================

load.all.simsets <- function(locations,
                             intervention.codes,
                             calibration.code,
                             n.sim,
                             base.path,
                             intervention.labels = NULL,
                             cache              = NULL,
                             cache.name         = "all.simsets",
                             force.reload       = FALSE,
                             append             = TRUE,
                             verbose            = TRUE) {
    
    # --- Resolve city names and codes ---
    city.names     <- if (!is.null(names(locations))) names(locations) else unname(locations)
    location.codes <- unname(locations)
    
    if (is.null(intervention.labels))
        intervention.labels <- setNames(intervention.codes, intervention.codes)
    
    # --- Resolve cache: explicit arg → globalenv() lookup → none ---
    resolved.cache <- NULL
    
    if (!force.reload) {
        if (!is.null(cache)) {
            resolved.cache <- cache
            if (verbose) message("[Cache] Using provided cache (",
                                 length(cache), " simset(s) available)")
            
        } else if (exists(cache.name, envir = globalenv(), inherits = FALSE)) {
            resolved.cache <- get(cache.name, envir = globalenv())
            if (verbose) message("[Cache] Found '", cache.name,
                                 "' in global environment (",
                                 length(resolved.cache), " simset(s) available)")
            
        } else {
            if (verbose) message("[Cache] No cache found — loading all from file")
        }
    } else {
        if (verbose) message("[Cache] force.reload = TRUE — ignoring cache")
    }
    
    # --- Build key map for requested locations × interventions ---
    key.map <- list()
    
    for (i in seq_along(location.codes)) {
        city.name <- city.names[i]
        loc.code  <- location.codes[i]
        
        for (int.code in intervention.codes) {
            int.label <- if (int.code %in% names(intervention.labels))
                intervention.labels[[int.code]] else int.code
            
            simset.key <- paste0(city.name, " \u2013 ", int.label)
            
            key.map[[simset.key]] <- list(
                loc.code  = loc.code,
                int.code  = int.code,
                city.name = city.name,
                int.label = int.label
            )
        }
    }
    
    expected.keys <- names(key.map)
    
    # --- Cache hits/misses among the requested keys ---
    if (!force.reload && !is.null(resolved.cache)) {
        cached.keys  <- intersect(expected.keys, names(resolved.cache))
        missing.keys <- setdiff(expected.keys,   names(resolved.cache))
        
        if (verbose) {
            # When appending, the count "served from cache" reflects the entire
            # cache being carried forward — not just the requested intersection.
            n.cache.msg <- if (append) length(resolved.cache) else length(cached.keys)
            message("[Cache] ", n.cache.msg,          " simset(s) served from cache")
            message("[Cache] ", length(missing.keys), " simset(s) not in cache — will load from file")
        }
    } else {
        cached.keys  <- character(0)
        missing.keys <- expected.keys
    }
    
    # --- Seed the output list ---
    if (append && !is.null(resolved.cache)) {
        # Keep everything in the cache; new loads are added on top.
        all.simsets <- resolved.cache
        if (verbose) message("[Append] Mode ON — preserving all ",
                             length(resolved.cache), " cached simset(s)")
        
    } else if (!is.null(resolved.cache)) {
        # Drop unrelated simsets, but reuse the cache for any requested keys
        # that are already loaded (avoids redundant file reads).
        all.simsets <- resolved.cache[cached.keys]
        if (verbose) message("[Append] Mode OFF — returning requested keys only")
        
    } else {
        all.simsets <- list()
    }
    
    # --- Load missing keys from file ---
    n.loaded  <- 0
    n.skipped <- 0
    
    for (simset.key in missing.keys) {
        info <- key.map[[simset.key]]
        
        path <- build.simset.path(base.path, calibration.code, n.sim,
                                  info$loc.code, info$int.code)
        
        if (!file.exists(path)) {
            warning("File not found, skipping: ", path)
            n.skipped <- n.skipped + 1
            next
        }
        
        if (verbose) message("  Loading: ", simset.key)
        all.simsets[[simset.key]] <- load.simulation.set(path)
        n.loaded <- n.loaded + 1
    }
    
    # --- Summary ---
    if (verbose) {
        n.preserved <- if (append && !is.null(resolved.cache))
            length(setdiff(names(resolved.cache), expected.keys)) else 0
        
        message("\n--- Load Summary ---")
        message("  Requested from cache : ", length(cached.keys),
                "  (of ", length(expected.keys), " requested)")
        message("  Preserved via append : ", n.preserved,
                "  (unrelated, carried forward)")
        message("  Loaded from file     : ", n.loaded)
        message("  Not found            : ", n.skipped)
        message("  Total                : ", length(all.simsets))
    }
    
    return(all.simsets)
}
# =============================================================================
# Simset Subsetting Helpers
# These make it easy to pull a slice of the combined list for simplot.
# Both return named sublists, ready to be passed to do.call(simplot, ...).
# =============================================================================

# Returns all interventions for a given city name (partial match supported)
get.simsets.for.city <- function(simsets, city.name, exact = FALSE) {
    if (exact)
        simsets[startsWith(names(simsets), paste0(city.name, " \u2013 "))]
    else
        simsets[grepl(city.name, names(simsets), fixed = TRUE)]
}

# Returns one intervention across all cities
get.simsets.for.intervention <- function(simsets, intervention.label, exact = FALSE) {
    if (exact)
        simsets[endsWith(names(simsets), paste0(" \u2013 ", intervention.label))]
    else
        simsets[grepl(intervention.label, names(simsets), fixed = TRUE)]
}

# Returns a single specific simset by city + intervention label
get.simset <- function(simsets, city.name, intervention.label) {
    key <- paste0(city.name, " \u2013 ", intervention.label)
    if (!key %in% names(simsets))
        stop("Simset not found: '", key, "'")
    simsets[[key]]
}
