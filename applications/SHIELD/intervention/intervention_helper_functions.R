# ****************************************************************************************************
# JHEEM SIMULATION SET LOADER AND UTILITIES ----
# ****************************************************************************************************
# This module provides functions for loading, caching, and subsetting JHEEM
# simulation sets across multiple locations and interventions. It includes:
#   - Path construction for .Rdata files
#   - Smart caching to avoid redundant disk I/O
#   - Flexible subsetting helpers for analysis and plotting
#   - Intervention comparison utilities
#   - Visualization/plotting functions
# ****************************************************************************************************


# build.simset.path ----
# 
# Path Builder: Constructs the full .Rdata file path for one location × 
# intervention pair.
#
# The resulting path follows the structure:
#   {base.path}/{calibration.code}-{n.sim}/{location.code}/
#       shield_{run.tag}_{location}_{intervention}.Rdata
#
# Arguments:
#   base.path         - Root directory where simulation folders are stored
#   calibration.code  - Identifier for the calibration run (e.g., "calib_v1")
#   n.sim             - Number of simulations per set
#   location.code     - Location identifier code (e.g., "C.12580")
#   intervention.code - Intervention identifier code (e.g., "noint", "full")
#
# Returns:
#   Character string containing the full file path to the .Rdata file
# ******************************************************************************************************
build.simset.path <- function(base.path,
                              calibration.code,
                              n.sim,
                              location.code,
                              intervention.code) {
    run.tag  <- paste0(calibration.code, "-", n.sim)
    filename <- paste0("shield_", run.tag, "_", location.code, "_", intervention.code, ".Rdata")
    file.path(base.path, run.tag, location.code, filename)
}


# load.all.simsets ----
# 
# Core Loader with Cache Resolution: Loads JHEEM simsets across locations × 
# interventions, with intelligent caching to avoid re-reading from disk.
#
# Cache lookup priority (when force.reload = FALSE):
#   1. Explicit `cache` argument, if provided
#   2. Object named `cache.name` in globalenv(), if it exists
#   3. Nothing found → load everything from file
#
# Arguments:
#   locations           - Named char vector (names = city names, values = codes)
#   intervention.codes  - Character vector of intervention codes to load
#   calibration.code    - Calibration run ID string
#   n.sim               - Number of simulations per set
#   base.path           - Root path to simulation folders
#   intervention.labels - Named char vector mapping code -> display label
#   cache               - (optional) A previously returned simset list to reuse
#   cache.name          - Name to look up in globalenv() if `cache` is NULL
#   force.reload        - TRUE → ignore all caches, reload everything
#   append              - TRUE → keep all cached simsets in output
#                         FALSE → return only requested keys
#   verbose             - Print progress and summary messages
#
# Returns:
#   Named list of simsets, keyed by "CityName – InterventionLabel"
# ****************************************************************************************************
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
    
    # Resolve city names and codes
    city.names     <- if (!is.null(names(locations))) names(locations) else unname(locations)
    location.codes <- unname(locations)
    
    if (is.null(intervention.labels))
        intervention.labels <- setNames(intervention.codes, intervention.codes)
    
    # Resolve cache: explicit arg → globalenv() lookup → none
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
    
    # Build key map for requested locations × interventions
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
    
    # Determine cache hits/misses
    if (!force.reload && !is.null(resolved.cache)) {
        cached.keys  <- intersect(expected.keys, names(resolved.cache))
        missing.keys <- setdiff(expected.keys,   names(resolved.cache))
        
        if (verbose) {
            n.cache.msg <- if (append) length(resolved.cache) else length(cached.keys)
            message("[Cache] ", n.cache.msg,          " simset(s) served from cache")
            message("[Cache] ", length(missing.keys), " simset(s) not in cache — will load from file")
        }
    } else {
        cached.keys  <- character(0)
        missing.keys <- expected.keys
    }
    
    # Initialize output list based on append mode
    if (append && !is.null(resolved.cache)) {
        all.simsets <- resolved.cache
        if (verbose) message("[Append] Mode ON — preserving all ",
                             length(resolved.cache), " cached simset(s)")
        
    } else if (!is.null(resolved.cache)) {
        all.simsets <- resolved.cache[cached.keys]
        if (verbose) message("[Append] Mode OFF — returning requested keys only")
        
    } else {
        all.simsets <- list()
    }
    
    # Load missing simsets from file
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
    
    # Print summary
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


# ****************************************************************************************************
# SIMSET SUBSETTING HELPERS ----
# ****************************************************************************************************
# Utility functions for extracting subsets of the combined simset list.
# All return named sublists ready for do.call(simplot, ...).
# ****************************************************************************************************


# get.simsets.for.city ----
#
# Returns all interventions for a given city name.
#
# Arguments:
#   simsets   - Named list of simsets from load.all.simsets()
#   city.name - City name to filter by
#   exact     - If TRUE, requires exact prefix match; if FALSE, uses partial match
#
# Returns:
#   Named sublist of matching simsets
# ****************************************************************************************************
get.simsets.for.city <- function(simsets, city.name, exact = FALSE) {
    if (exact)
        simsets[startsWith(names(simsets), paste0(city.name, " \u2013 "))]
    else
        simsets[grepl(city.name, names(simsets), fixed = TRUE)]
}


# get.simsets.for.intervention ----
#
# Returns one intervention across all cities.
#
# Arguments:
#   simsets            - Named list of simsets from load.all.simsets()
#   intervention.label - Intervention label to filter by
#   exact              - If TRUE, requires exact suffix match; if FALSE, uses partial match
#
# Returns:
#   Named sublist of matching simsets
# ****************************************************************************************************
get.simsets.for.intervention <- function(simsets, intervention.label, exact = FALSE) {
    if (exact)
        simsets[endsWith(names(simsets), paste0(" \u2013 ", intervention.label))]
    else
        simsets[grepl(intervention.label, names(simsets), fixed = TRUE)]
}


# get.simset ----
#
# Returns a single specific simset by city + intervention label.
#
# Arguments:
#   simsets            - Named list of simsets from load.all.simsets()
#   city.name          - Exact city name
#   intervention.label - Exact intervention label
#
# Returns:
#   Single simset object (not a list)
#
# Errors:
#   Stops with error if the specified simset is not found
# ****************************************************************************************************
get.simset <- function(simsets, city.name, intervention.label) {
    key <- paste0(city.name, " \u2013 ", intervention.label)
    if (!key %in% names(simsets))
        stop("Simset not found: '", key, "'")
    simsets[[key]]
}


# get_intervention_simsets ----
#
# Runs an intervention and null intervention across all cities, returning
# paired results for comparison.
#
# Arguments:
#   simset.data      - List of simset data by city (with $last20_sims and $full_simset)
#   intervention     - Intervention object with $run() method
#   just.last.twenty - If TRUE, uses last 20 sims (faster); if FALSE, uses full simset
#
# Returns:
#   Named list by city, each containing:
#     $int_simset  - Results from running the specified intervention
#     $null_simset - Results from running the null intervention
# ****************************************************************************************************
get_intervention_simsets <- function(simset.data, intervention, just.last.twenty = TRUE) {
    setNames(lapply(cities, function(city) {
        browser()
        
        if (is.null(simset.data[[city]])) {
            print(paste0("No simset found for '", city, "': Skipping"))
            return(NULL)
        }
        
        used_simset <- if (just.last.twenty) simset.data[[city]]$last20_sims else simset.data[[city]]$full_simset
        
        print(paste0("Running intervention for '", city, "'..."))
        int_simset <- intervention$run(used_simset, start.year = 2020, end.year = 2035)
        
        print(paste0("Running null intervention for '", city, "'..."))
        null_simset <- get.null.intervention()$run(used_simset, start.year = 2020, end.year = 2035)
        
        print(paste0("Done with '", city, "'"))
        list(int_simset = int_simset,
             null_simset = null_simset)
        
    }), cities)
}


# create_intervention_plots ----
#
# Generates and saves comparison plots for intervention vs null across cities.
#
# Arguments:
#   int.sim.data - Output from get_intervention_simsets()
#   create.dirs  - If TRUE, creates output directories if they don't exist
#
# Side Effects:
#   Saves PNG plot files to:
#     {jheem.root}/shield/interventionPlots/{CALIB}/{city}/diagnosis_ps.png
#
# Errors:
#   Stops if directory doesn't exist and create.dirs = FALSE
# ****************************************************************************************************
create_intervention_plots <- function(int.sim.data, create.dirs = FALSE) {
    
    for (city in names(int.sim.data)) {
        
        if (is.null(int.sim.data[[city]]$int_simset)) next
        
        plotting_path <- paste0(get.jheem.root.directory(), "/shield/interventionPlots/", CALIB, "/", city, "/")
        
        if (!dir.exists(plotting_path)) {
            if (!create.dirs)
                stop(paste0("Error: directory for '", city, "' and '", CALIB, 
                            "' does not exist. Check that get.jheem.root.directory() ",
                            "shows the right place, then try again with 'create.dirs' set to TRUE."))
            dir.create(plotting_path, recursive = TRUE, showWarnings = FALSE)
            print(paste0("Generating directories for '", city, "' and '", CALIB, "'"))
        }
        
        plot <- simplot(int.sim.data[[city]]$int_simset,
                        int.sim.data[[city]]$null_simset,
                        "diagnosis.ps",
                        summary.type = "median.and.interval",
                        style.manager = create.style.manager(color.sim.by = "simset"))
        
        file_png <- file.path(paste0(plotting_path, "diagnosis_ps.png"))
        ggsave(file_png, plot = plot, width = 12, height = 7, dpi = 300)
    }
    
    print("Done creating intervention plots")
}