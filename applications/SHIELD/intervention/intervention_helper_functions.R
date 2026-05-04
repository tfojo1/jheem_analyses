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

# SIMSET LOADING FUNCTIONS ----
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




# PLOTTING FUNCTIONS ----
# plot_int_single_city ----
#' Plot multiple interventions for a single city
#' Creates one plot with different panels for each outcome
#'
#' @param all.simsets Named list of all loaded simsets
#' @param city City name to plot
#' @param interventions Character vector of intervention labels to include (NULL = all)
#' @param outcomes Character vector of outcomes (each becomes a panel)
#' @param years Year range for x-axis
#' @param style.manager Optional custom style manager
#' @param save Logical, whether to save the plot
#' @param save.dir Directory to save plot (default: PLOT.BASE.DIR/city/)
#' @param filename Custom filename (default: auto-generated from city and outcomes)
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Plot resolution
#' @param create.dirs Create directories if they don't exist
#' @return ggplot object (invisibly if saved)
plot_int_single_city <- function(all.simsets,
                                 city,
                                 interventions = NULL,
                                 outcomes = c("diagnosis.ps", "doxy.uptake"),
                                 split.by=NULL,
                                 facet.by=NULL,
                                 years = 2020:2035,
                                 plot.which="sim.and.data",
                                 style.manager = NULL,
                                 save = FALSE,
                                 save.dir = NULL,
                                 filename = NULL,
                                 width = 12,
                                 height = 7,
                                 dpi = 300,
                                 create.dirs = FALSE) {
    
    # Get simsets for this city
    city.simsets <- get.simsets.for.city(all.simsets, city)
    
    if (length(city.simsets) == 0) {
        stop(paste0("No simsets found for city: ", city))
    }
    
    # Filter to requested interventions
    if (!is.null(interventions)) {
        missing <- setdiff(interventions, names(city.simsets))
        if (length(missing) > 0) {
            warning(paste0("Interventions not found for ", city, ": ", 
                           paste(missing, collapse = ", ")))
        }
        city.simsets <- city.simsets[intersect(interventions, names(city.simsets))]
    }
    
    # Default style manager
    if (is.null(style.manager)) {
        style.manager <- create.style.manager(color.sim.by = "simset")
    }
    
    # Create plot
    p <- do.call(simplot, c(
        city.simsets,
        list(
            outcomes         = outcomes,
            dimension.values = list(year = years),
            style.manager    = style.manager,
            summary.type     = "median.and.interval",
            split.by=split.by,
            facet.by=facet.by,
            plot.which=plot.which
        )
    ))
    
    # Add title
    p <- p + ggtitle(city)
    
    # Save if requested
    if (save) {
        # Default save directory: ROOT.DIR/shield/interventionPlots/CALIBRATION.CODE/city/
        if (is.null(save.dir)) {
            save.dir <- file.path(PLOT.BASE.DIR, city)
        }
        
        # Create directory if needed
        if (!dir.exists(save.dir)) {
            if (!create.dirs) {
                stop(paste0("Directory does not exist: ", save.dir,
                            "\nSet create.dirs = TRUE to create it."))
            }
            dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)
            message(paste0("Created directory: ", save.dir))
        }
        
        # Default filename
        if (is.null(filename)) {
            outcomes.str <- paste(outcomes, collapse = "_")
            filename <- paste0(city, "_", outcomes.str, ".png")
        }
        
        # Ensure .png extension
        if (!grepl("\\.png$", filename)) {
            filename <- paste0(filename, ".png")
        }
        
        filepath <- file.path(save.dir, filename)
        ggsave(filepath, plot = p, width = width, height = height, dpi = dpi)
        message(paste0("Saved: ", filepath))
        
        return(invisible(p))
    }
    
    p
}

# plot_int_mult_cities ----
#' Compare interventions across multiple cities (side-by-side)
#' Creates a grid: rows = outcomes, columns = cities
#'
#' @param all.simsets Named list of all loaded simsets
#' @param cities Character vector of city names to compare
#' @param interventions Character vector of intervention labels to include (NULL = all)
#' @param outcomes Character vector of outcomes
#' @param years Year range for x-axis
#' @param ncol Number of columns in grid (NULL = one per city)
#' @param style.manager Optional custom style manager
#' @param save Logical, whether to save the plot
#' @param save.dir Directory to save plot (default: PLOT.BASE.DIR/comparisons/)
#' @param filename Custom filename (default: auto-generated)
#' @param width Plot width in inches (default: 4 per city)
#' @param height Plot height in inches
#' @param dpi Plot resolution
#' @param create.dirs Create directories if they don't exist
#' @return patchwork object (invisibly if saved)
plot_int_mult_cities <- function(all.simsets,
                                 cities,
                                 interventions = NULL,
                                 outcomes = c("diagnosis.ps", "doxy.uptake"),
                                 years = 2020:2035,
                                 ncol = NULL,
                                 style.manager = NULL,
                                 save = FALSE,
                                 save.dir = NULL,
                                 filename = NULL,
                                 width = NULL,
                                 height = 7,
                                 dpi = 300,
                                 create.dirs = FALSE) {
    
    # Default style manager
    if (is.null(style.manager)) {
        style.manager <- create.style.manager(color.sim.by = "simset")
    }
    
    # Generate one plot per city
    plots <- lapply(cities, function(city) {
        
        city.simsets <- get.simsets.for.city(all.simsets, city)
        
        if (length(city.simsets) == 0) {
            warning(paste0("No simsets found for city: ", city))
            return(NULL)
        }
        
        # Filter to requested interventions
        if (!is.null(interventions)) {
            city.simsets <- city.simsets[intersect(interventions, names(city.simsets))]
        }
        
        if (length(city.simsets) == 0) {
            warning(paste0("No matching interventions for city: ", city))
            return(NULL)
        }
        
        # Create plot for this city
        p <- do.call(simplot, c(
            city.simsets,
            list(
                outcomes         = outcomes,
                dimension.values = list(year = years),
                style.manager    = style.manager,
                summary.type     = "median.and.interval"
            )
        ))
        
        # Add city title
        p + ggtitle(city)
    })
    
    # Remove NULLs
    valid.idx <- !sapply(plots, is.null)
    plots <- plots[valid.idx]
    valid.cities <- cities[valid.idx]
    names(plots) <- valid.cities
    
    if (length(plots) == 0) {
        stop("No valid plots generated for any city")
    }
    
    # Combine into grid
    if (is.null(ncol)) ncol <- length(plots)
    
    combined <- wrap_plots(plots, ncol = ncol) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
    
    # Save if requested
    if (save) {
        # Default save directory: ROOT.DIR/shield/interventionPlots/CALIBRATION.CODE/comparisons/
        if (is.null(save.dir)) {
            save.dir <- PLOT.COMPARISON.DIR
        }
        
        # Create directory if needed
        if (!dir.exists(save.dir)) {
            if (!create.dirs) {
                stop(paste0("Directory does not exist: ", save.dir,
                            "\nSet create.dirs = TRUE to create it."))
            }
            dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)
            message(paste0("Created directory: ", save.dir))
        }
        
        # Default filename
        if (is.null(filename)) {
            cities.str <- paste(valid.cities, collapse = "_")
            outcomes.str <- paste(outcomes, collapse = "_")
            filename <- paste0("comparison_", cities.str, "_", outcomes.str, ".png")
            
            # Truncate if too long
            if (nchar(filename) > 100) {
                filename <- paste0("comparison_", length(valid.cities), 
                                   "cities_", outcomes.str, ".png")
            }
        }
        
        # Ensure .png extension
        if (!grepl("\\.png$", filename)) {
            filename <- paste0(filename, ".png")
        }
        
        # Default width based on number of cities
        if (is.null(width)) {
            width <- 4 * min(ncol, length(plots))
        }
        
        filepath <- file.path(save.dir, filename)
        ggsave(filepath, plot = combined, width = width, height = height, dpi = dpi)
        message(paste0("Saved: ", filepath))
        
        return(invisible(combined))
    }
    
    combined
}

# plot_int_all_cities ----
#' Generate and save plots for all cities
#'
#' @param all.simsets Named list of all loaded simsets
#' @param cities Character vector of city names (NULL = all available)
#' @param interventions Character vector of intervention labels (NULL = all)
#' @param outcomes Character vector of outcomes
#' @param years Year range
#' @param save.dir Base directory for saving (default: PLOT.BASE.DIR)
#' @param create.dirs Create directories if they don't exist
plot_int_all_cities <- function(all.simsets,
                                cities = NULL,
                                interventions = NULL,
                                outcomes = c("diagnosis.ps", "doxy.uptake"),
                                years = 2020:2035,
                                save.dir = NULL,
                                create.dirs = TRUE) {
    
    # Default save directory
    if (is.null(save.dir)) {
        save.dir <- PLOT.BASE.DIR
    }
    
    # Get all cities if not specified
    if (is.null(cities)) {
        all.names <- names(all.simsets)
        cities <- unique(sapply(strsplit(all.names, " – "), `[`, 1))
    }
    
    message(paste0("Generating plots for ", length(cities), " cities..."))
    message(paste0("Saving to: ", save.dir))
    
    for (city in cities) {
        tryCatch({
            plot_int_single_city(
                all.simsets,
                city          = city,
                interventions = interventions,
                outcomes      = outcomes,
                years         = years,
                save          = TRUE,
                save.dir      = file.path(save.dir, city),
                create.dirs   = create.dirs
            )
        }, error = function(e) {
            warning(paste0("Failed to generate plot for ", city, ": ", e$message))
        })
    }
    
    message("Done generating all city plots")
}
