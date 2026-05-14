# ****************************************************************************************************
# **** SHIELD CALIBRATION HELPERS  **** 
# ****************************************************************************************************
# Single helper module for all calibration loading, subsetting, and plotting.
#
# LOCATION INPUT CONVENTION (consistent across all functions):
#   All functions accept locations as a named character vector where:
#     - names  = display names (e.g., "Baltimore")
#     - values = MSA codes (e.g., "C.12580")
#
#   Examples using SHIELD.TEN.MSAS:
#     SHIELD.TEN.MSAS            → all 10 cities
#     SHIELD.TEN.MSAS[1:3]       → first 3 cities
#     SHIELD.TEN.MSAS["Baltimore"] → Baltimore only
#     SHIELD.TEN.MSAS[c("Baltimore", "Atlanta")] → Baltimore and Atlanta
#     NULL                       → all available in calibration.simsets
#
# LOADING:
#   load.calibration.simsets()            — 3-tier cached loader
#
# SUBSETTING:
#   extract.calibration.simsets()         — flexible subsetter (location, code, or both)
#
# PUBLIC PLOT FUNCTIONS:
#   create_plots_for_calibration()        — per-location stage plots
#   create_multipanel_comparison()        — multi-panel grid comparing locations/calibrations
#
# KEY CONVENTION:
#   All simsets are keyed as "LocationName – CalibrationCode" (en dash U+2013)
# ****************************************************************************************************

# ****  SIMSET LOADING  **** ----
# ****************************************************************************************************

## load.calibration.simsets ----
#
# Core loader with 3-tier cache resolution.
#
# Arguments:
#   locations           - Named character vector (names = display, values = MSA codes)
#                         e.g., SHIELD.TEN.MSAS or SHIELD.TEN.MSAS[1:3]
#   calibration.codes   - Character vector of calibration code strings
#   assemble.incomplete - If TRUE, assembles partial calibrations; if FALSE, skips them
#   cache               - (optional) Pass a previously loaded simset list directly
#   cache.name          - Name of globalenv variable to use as cache (default: "calibration.simsets")
#   force.reload        - TRUE → ignore all caches; reload everything from disk
#   append              - TRUE → keep all cached entries in output (default)
#   verbose             - Print progress and summary messages
#   version             - JHEEM version string (default: "shield")
#
# Returns:
#   Named list of simset sub-lists, keyed by "LocationName – CalibrationCode"
# ****************************************************************************************************
load.calibration.simsets <- function(locations,
                                     calibration.codes,
                                     assemble.incomplete = FALSE,
                                     cache               = NULL,
                                     cache.name          = "calibration.simsets",
                                     force.reload        = FALSE,
                                     append              = TRUE,
                                     verbose             = TRUE,
                                     version             = "shield") {
    
    # Extract names and codes from named vector
    location.codes <- unname(locations)
    location.names <- names(locations)
    
    # Fallback if unnamed (shouldn't happen with SHIELD.TEN.MSAS, but just in case)
    if (is.null(location.names)) {
        location.names <- sapply(location.codes, function(lc) {
            tryCatch(get.location.name(lc), error = function(e) lc)
        })
    }
    
    # Resolve cache
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
            if (verbose) message("[Cache] No cache found — loading all from disk")
        }
    } else {
        if (verbose) message("[Cache] force.reload = TRUE — ignoring cache")
    }
    
    # Build key map
    key.map <- list()
    for (i in seq_along(location.codes)) {
        for (calib.code in calibration.codes) {
            simset.key <- paste0(location.names[i], " – ", calib.code)
            key.map[[simset.key]] <- list(
                loc.code   = location.codes[i],
                loc.name   = location.names[i],
                calib.code = calib.code
            )
        }
    }
    
    expected.keys <- names(key.map)
    
    # Determine cache hits and misses
    if (!force.reload && !is.null(resolved.cache)) {
        cached.keys  <- intersect(expected.keys, names(resolved.cache))
        missing.keys <- setdiff(expected.keys, names(resolved.cache))
        
        if (verbose) {
            n.cache.msg <- if (append) length(resolved.cache) else length(cached.keys)
            message("[Cache] ", n.cache.msg, " simset(s) served from cache")
            message("[Cache] ", length(missing.keys), " simset(s) not in cache — will load from disk")
        }
    } else {
        cached.keys  <- character(0)
        missing.keys <- expected.keys
    }
    
    # Initialize output
    if (append && !is.null(resolved.cache)) {
        calibration.simsets <- resolved.cache
        if (verbose) message("[Append] Mode ON — preserving all ",
                             length(resolved.cache), " cached simset(s)")
    } else if (!is.null(resolved.cache)) {
        calibration.simsets <- resolved.cache[cached.keys]
        if (verbose) message("[Append] Mode OFF — returning requested keys only")
    } else {
        calibration.simsets <- list()
    }
    
    # Group missing keys by calibration code
    missing.by.code <- list()
    for (k in missing.keys) {
        cc <- key.map[[k]]$calib.code
        missing.by.code[[cc]] <- c(missing.by.code[[cc]], k)
    }
    
    n.loaded  <- 0
    n.skipped <- 0
    
    for (calib.code in names(missing.by.code)) {
        
        keys.for.code <- missing.by.code[[calib.code]]
        locs.for.code <- sapply(keys.for.code, function(k) key.map[[k]]$loc.code)
        
        calib.info <- tryCatch(get.calibration.info(calib.code), error = function(e) NULL)
        
        if (is.null(calib.info)) {
            warning("Could not retrieve calibration info for '", calib.code, "' — skipping")
            n.skipped <- n.skipped + length(keys.for.code)
            next
        }
        
        if (calib.info$n.burn != 0)
            warning("Calibration '", calib.code, "' has n.burn != 0; derived n.sim may be incorrect")
        
        n.sim <- calib.info$n.iter / calib.info$thin
        
        calib.progress <- tryCatch(
            get.calibration.progress(version = version, locations = locs.for.code,
                                     calibration.code = calib.code),
            error = function(e) NULL
        )
        
        if (is.null(calib.progress)) {
            warning("Could not retrieve calibration progress for '", calib.code, "' — skipping")
            n.skipped <- n.skipped + length(keys.for.code)
            next
        }
        
        if (ncol(calib.progress) > 1)
            warning("Calibration '", calib.code, "' is multi-chain; only chain 1 used")
        
        for (simset.key in keys.for.code) {
            
            info     <- key.map[[simset.key]]
            loc.code <- info$loc.code
            pct      <- calib.progress[loc.code, 1]
            
            if (is.na(pct)) {
                if (verbose) message("  Skipping (no data): ", simset.key)
                n.skipped <- n.skipped + 1
                next
            }
            
            if (pct < 100 && !assemble.incomplete) {
                if (verbose) message("  Skipping (incomplete, ", pct, "%): ", simset.key)
                n.skipped <- n.skipped + 1
                next
            }
            
            load.label <- if (pct < 100) paste0(" (", pct, "% — assembling)") else ""
            if (verbose) message("  Loading: ", simset.key, load.label)
            
            full.simset <- if (pct < 100) {
                tryCatch(
                    assemble.simulations.from.calibration(
                        version = version, location = loc.code,
                        calibration.code = calib.code, allow.incomplete = TRUE
                    ),
                    error = function(e) { warning("Error assembling '", simset.key, "': ", e$message); NULL }
                )
            } else {
                tryCatch(
                    retrieve.simulation.set(
                        version = version, location = loc.code,
                        calibration.code = calib.code, n.sim = n.sim
                    ),
                    error = function(e) { warning("Error retrieving '", simset.key, "': ", e$message); NULL }
                )
            }
            
            if (is.null(full.simset)) { n.skipped <- n.skipped + 1; next }
            
            n.sim.eff    <- full.simset$n.sim
            title.suffix <- paste0(": ", calib.code, if (pct < 100) paste0(" (", pct, "% complete)") else "")
            
            calibration.simsets[[simset.key]] <- list(
                full_simset   = full.simset,
                last20_sims   = full.simset$subset((n.sim.eff - 19):n.sim.eff),
                last_sim      = full.simset$last.sim(),
                title_suffix  = title.suffix,
                location.code = loc.code,
                location.name = info$loc.name,
                calib.code    = calib.code
            )
            
            n.loaded <- n.loaded + 1
        }
    }
    
    if (verbose) {
        n.preserved <- if (append && !is.null(resolved.cache))
            length(setdiff(names(resolved.cache), expected.keys)) else 0
        
        message("\n--- Load Summary ---")
        message("  Requested from cache : ", length(cached.keys))
        message("  Preserved via append : ", n.preserved)
        message("  Loaded from disk     : ", n.loaded)
        message("  Skipped              : ", n.skipped)
        message("  Total                : ", length(calibration.simsets))
    }
    
    return(calibration.simsets)
}


# ****************************************************************************************************
# **** SIMSET SUBSETTING  **** ----
# ****************************************************************************************************

## extract.calibration.simsets ----
#
# Flexible subsetter:
#   location.name only    → all codes for that location
#   calibration.code only → all locations for that code
#   both                  → single entry
extract.calibration.simsets <- function(calibration.simsets,
                                        location.name    = NULL,
                                        calibration.code = NULL,
                                        exact            = FALSE) {
    
    if (!is.null(location.name) && !is.null(calibration.code)) {
        key <- paste0(location.name, " – ", calibration.code)
        if (!key %in% names(calibration.simsets))
            stop("Calibration simset not found: '", key, "'")
        return(calibration.simsets[[key]])
    }
    
    if (!is.null(location.name)) {
        if (exact)
            return(calibration.simsets[startsWith(names(calibration.simsets), paste0(location.name, " – "))])
        else
            return(calibration.simsets[grepl(location.name, names(calibration.simsets), fixed = TRUE)])
    }
    
    if (!is.null(calibration.code)) {
        if (exact)
            return(calibration.simsets[endsWith(names(calibration.simsets), paste0(" – ", calibration.code))])
        else
            return(calibration.simsets[grepl(calibration.code, names(calibration.simsets), fixed = TRUE)])
    }
    
    stop("Provide at least one of: location.name or calibration.code")
}


# ****************************************************************************************************
# ****  PUBLIC PLOT FUNCTIONS  **** ----
# ****************************************************************************************************

## create_plots_for_calibration ----
#
# Generates per-location diagnostic plots for a calibration code.
#
# Arguments:
#   calibration.code    - Calibration code to plot
#   stage               - Calibration stage: 0, 1, or 2
#   locations           - Named vector like SHIELD.TEN.MSAS or subset thereof.
#                         NULL = all locations available for this calibration.code
#   calibration.simsets - Named list from load.calibration.simsets(); auto-resolved if NULL
#   style.manager       - Style manager (default: source + stratum)
#   create.dirs         - Create output directories automatically
#   use.full.simset     - If TRUE, uses full_simset; if FALSE (default), uses last20_sims
#
# Returns:
#   Character vector of location codes successfully plotted (invisibly)
# ****************************************************************************************************
create_plots_for_calibration <- function(calibration.code,
                                         stage,
                                         locations           = NULL,
                                         calibration.simsets = NULL,
                                         style.manager       = NULL,
                                         create.dirs         = TRUE,
                                         use.full.simset     = FALSE) {
    
    calibration.simsets <- .resolve.calibration.simsets(calibration.simsets,
                                                        "create_plots_for_calibration")
    
    if (is.null(style.manager))
        style.manager <- create.style.manager(shape.data.by = "source", color.data.by = "stratum")
    
    # Get available locations for this calibration code
    available.locs <- .get.available.locations(calibration.simsets, calibration.code)
    
    if (length(available.locs) == 0) {
        warning("No simsets found for calibration code: '", calibration.code, "'")
        return(invisible(character(0)))
    }
    
    # Filter to requested locations
    target.locs <- .filter.to.requested.locations(locations, available.locs,
                                                  "create_plots_for_calibration")
    
    if (length(target.locs) == 0) {
        return(invisible(character(0)))
    }
    
    message("Plotting ", length(target.locs), " location(s) for '", calibration.code, "'")
    
    successful_locations <- character(0)
    
    for (i in seq_along(target.locs)) {
        loc.name <- names(target.locs)[i]
        loc.code <- target.locs[i]
        simset.key <- paste0(loc.name, " – ", calibration.code)
        
        entry <- calibration.simsets[[simset.key]]
        if (is.null(entry)) {
            message("  Skipping '", loc.name, "' — entry not found")
            next
        }
        
        title_suffix <- entry$title_suffix
        plotting_path <- file.path(get.jheem.root.directory(), "shield", "calibrationPlots",
                                   calibration.code, loc.code, "")
        
        tryCatch(ensure.plot.dir(plotting_path, create.dirs),
                 error = function(e) stop(e$message))
        
        use_sims <- if (use.full.simset) entry$full_simset else entry$last20_sims
        last_sim <- entry$last_sim
        
        if (is.null(use_sims)) {
            message("  Skipping '", loc.name, "' — no simset data")
            next
        }
        
        message("  [", i, "/", length(target.locs), "] Generating stage ", stage,
                " plots for '", loc.name, "'")
        
        tryCatch({
            if (stage == 0) make_stage0_plots_for_location(use_sims, last_sim, plotting_path,
                                                           title.suffix = title_suffix, style.manager = style.manager)
            if (stage == 1) make_stage1_plots_for_location(use_sims, last_sim, plotting_path,
                                                           title.suffix = title_suffix, style.manager = style.manager)
            if (stage == 2) make_stage2_plots_for_location(use_sims, last_sim, plotting_path,
                                                           title.suffix = title_suffix, style.manager = style.manager)
            successful_locations <- c(successful_locations, loc.code)
        }, error = function(e) {
            warning("Error generating plots for '", loc.name, "': ", e$message)
        })
    }
    
    message("\nDone. Generated stage ", stage, " plots for ",
            length(successful_locations), " location(s).")
    invisible(successful_locations)
}


## create_multipanel_comparison ----
#
# Flexible multi-panel comparison plotting.
#
# Arguments:
#   calibration.codes   - Character vector of calibration codes to compare
#   locations           - Named vector like SHIELD.TEN.MSAS or subset; NULL = all available
#   outcomes            - Character vector of outcome names
#   calibration.simsets - Named list; auto-resolved if NULL
#   separate.by         - "outcome" | "location" | "calibration"
#   facet.by            - Dimension to facet by within each panel (e.g., "sex", "race", "age")
#   split.by            - Dimension to split by within each panel (e.g., "sex", "race")
#   years               - Year range for x-axis
#   nrow / ncol         - Panel grid dimensions (NULL = auto)
#   width / height      - Output dimensions in inches
#   dpi                 - Output resolution
#   create.dirs         - Create output directories automatically
#   use.full.simset     - Use full_simset vs last20_sims
#   style.manager       - Custom style manager
#   summary.type        - Summary type for simplot
#   plot.which          - "sim.and.data" (default) or "sim.only"
#   verbose             - Print progress messages
#
# Filename convention:
#   - Default (sim.and.data): outcome_diagnosis-total_by_location.png
#   - With sim.only:          outcome_diagnosis-total_by_location_simOnly.png
#   - With stratification:    outcome_diagnosis-total_by_location_split-sex_facet-race.png
#   - Combined:               outcome_diagnosis-total_by_location_simOnly_split-sex_facet-race.png
#
# Returns:
#   Invisible list of saved file paths
# ****************************************************************************************************
create_multipanel_comparison <- function(calibration.codes,
                                         locations           = NULL,
                                         outcomes,
                                         calibration.simsets = NULL,
                                         separate.by         = c("outcome", "location", "calibration"),
                                         facet.by            = NULL,
                                         split.by            = NULL,
                                         years               = 2000:2030,
                                         nrow                = NULL,
                                         ncol                = NULL,
                                         width               = 20,
                                         height              = 10,
                                         dpi                 = 300,
                                         create.dirs         = TRUE,
                                         use.full.simset     = FALSE,
                                         style.manager       = NULL,
                                         summary.type        = "median.and.interval",
                                         plot.which          = "sim.and.data",
                                         verbose             = TRUE) {
    
    separate.by <- match.arg(separate.by)
    
    calibration.simsets <- .resolve.calibration.simsets(calibration.simsets,
                                                        "create_multipanel_comparison")
    
    if (is.null(style.manager))
        style.manager <- create.style.manager(shape.data.by = "source", color.data.by = "stratum")
    
    # Get available locations (across all requested calibration codes)
    available.locs <- .get.available.locations(calibration.simsets, calibration.codes[1])
    for (cc in calibration.codes[-1]) {
        cc.locs <- .get.available.locations(calibration.simsets, cc)
        available.locs <- available.locs[names(available.locs) %in% names(cc.locs)]
    }
    
    if (length(available.locs) == 0) {
        warning("No locations found with all requested calibration codes")
        return(invisible(character(0)))
    }
    
    target.locs <- .filter.to.requested.locations(locations, available.locs,
                                                  "create_multipanel_comparison")
    
    if (length(target.locs) == 0) return(invisible(character(0)))
    
    location.names <- names(target.locs)
    
    # Build filename suffix components
    suffix_parts <- character(0)
    
    # Add simOnly suffix only if not default
    if (plot.which == "sim.only") {
        suffix_parts <- c(suffix_parts, "simOnly")
    }
    
    # Add stratification suffixes
    if (!is.null(split.by)) suffix_parts <- c(suffix_parts, paste0("split-", paste(split.by, collapse = "-")))
    if (!is.null(facet.by)) suffix_parts <- c(suffix_parts, paste0("facet-", paste(facet.by, collapse = "-")))
    
    # Combine into single suffix string
    file_suffix <- if (length(suffix_parts) > 0) paste0("_", paste(suffix_parts, collapse = "_")) else ""
    
    if (verbose) {
        message("\n=== Multi-panel comparison ===")
        message("  Calibrations: ", paste(calibration.codes, collapse = ", "))
        message("  Locations: ", length(target.locs), " (", paste(location.names, collapse = ", "), ")")
        message("  Outcomes: ", length(outcomes))
        message("  Separate by: ", separate.by)
        message("  Plot which: ", plot.which)
        if (!is.null(facet.by)) message("  Facet by: ", paste(facet.by, collapse = ", "))
        if (!is.null(split.by)) message("  Split by: ", paste(split.by, collapse = ", "))
        if (nchar(file_suffix) > 0) message("  Filename suffix: ", file_suffix)
    }
    
    # Helper functions
    get_entry <- function(loc.name, calib.code) {
        key <- paste0(loc.name, " – ", calib.code)
        calibration.simsets[[key]]
    }
    
    get_sims <- function(entry) {
        if (is.null(entry)) return(NULL)
        if (use.full.simset) entry$full_simset else entry$last20_sims
    }
    
    auto_grid <- function(n) {
        if (!is.null(nrow) && !is.null(ncol)) return(list(nrow = nrow, ncol = ncol))
        if (!is.null(ncol)) return(list(nrow = ceiling(n / ncol), ncol = ncol))
        if (!is.null(nrow)) return(list(nrow = nrow, ncol = ceiling(n / nrow)))
        nc <- ceiling(sqrt(n * 1.5))
        list(nrow = ceiling(n / nc), ncol = nc)
    }
    
    sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "_", gsub("\\.", "-", x))
    
    # Overlay multiple calibrations on one panel using do.call
    overlay_calibrations <- function(loc.name, outcome) {
        sims_list <- list()
        labels <- character(0)
        
        for (cc in calibration.codes) {
            entry <- get_entry(loc.name, cc)
            if (is.null(entry)) next
            sims <- get_sims(entry)
            if (is.null(sims)) next
            sims_list[[length(sims_list) + 1]] <- sims
            labels <- c(labels, cc)
        }
        
        if (length(sims_list) == 0) return(NULL)
        
        tryCatch({
            simplot_args <- list(
                outcomes         = outcome,
                facet.by         = facet.by,
                split.by         = split.by,
                style.manager    = style.manager,
                summary.type     = summary.type,
                plot.which       = plot.which,
                dimension.values = list(year = years)
            )
            
            if (length(sims_list) > 1) {
                simplot_args$simset.names <- labels
            }
            
            all_args <- c(sims_list, simplot_args)
            do.call(simplot, all_args)
            
        }, error = function(e) { 
            if (verbose) warning("Plot error for '", loc.name, "': ", e$message)
            NULL 
        })
    }
    
    # Single calibration plot using do.call
    single_calib_plot <- function(loc.name, outcome, calib.code) {
        entry <- get_entry(loc.name, calib.code)
        sims <- get_sims(entry)
        if (is.null(sims)) return(NULL)
        
        tryCatch({
            simplot_args <- list(
                outcomes         = outcome,
                facet.by         = facet.by,
                split.by         = split.by,
                style.manager    = style.manager,
                summary.type     = summary.type,
                plot.which       = plot.which,
                dimension.values = list(year = years)
            )
            
            all_args <- c(list(sims), simplot_args)
            do.call(simplot, all_args)
            
        }, error = function(e) NULL)
    }
    
    # Build subtitle for plots
    build_subtitle <- function() {
        subtitle_parts <- character(0)
        if (plot.which == "sim.only") subtitle_parts <- c(subtitle_parts, "Simulation only")
        if (!is.null(split.by)) subtitle_parts <- c(subtitle_parts, paste0("Split: ", paste(split.by, collapse = ", ")))
        if (!is.null(facet.by)) subtitle_parts <- c(subtitle_parts, paste0("Facet: ", paste(facet.by, collapse = ", ")))
        if (length(subtitle_parts) > 0) paste(subtitle_parts, collapse = " | ") else NULL
    }
    
    subtitle <- build_subtitle()
    
    # Set up output directory
    calib_label <- if (length(calibration.codes) == 1) calibration.codes[1] else
        paste0(calibration.codes[1], "_vs_", length(calibration.codes) - 1, "_others")
    
    root_dir <- file.path(get.jheem.root.directory(), "shield", "calibrationPlots",
                          "comparison", sanitize(calib_label), paste0("by_", separate.by), "")
    
    tryCatch(ensure.plot.dir(root_dir, create.dirs),
             error = function(e) stop("Cannot create output directory: ", e$message))
    
    saved_files <- character(0)
    
    # --- SEPARATE BY OUTCOME ---
    if (separate.by == "outcome") {
        for (oi in seq_along(outcomes)) {
            outcome <- outcomes[oi]
            if (verbose) message(sprintf("  [%d/%d] Outcome: %s", oi, length(outcomes), outcome))
            
            panels <- list()
            for (loc in location.names) {
                p <- overlay_calibrations(loc, outcome)
                if (!is.null(p)) {
                    panels[[loc]] <- p + ggtitle(loc) +
                        theme(plot.title = element_text(size = 10, hjust = 0.5))
                }
            }
            
            if (length(panels) == 0) {
                if (verbose) message("    No panels generated — skipping")
                next
            }
            
            grid <- auto_grid(length(panels))
            
            combined <- wrap_plots(panels, nrow = grid$nrow, ncol = grid$ncol) +
                plot_annotation(
                    title    = paste0("Outcome: ", outcome),
                    subtitle = subtitle,
                    theme    = theme(
                        plot.title    = element_text(size = 14, hjust = 0.5, face = "bold"),
                        plot.subtitle = element_text(size = 11, hjust = 0.5)
                    )
                )
            
            fname <- paste0("outcome_", sanitize(outcome), "_by_location", file_suffix, ".png")
            out_path <- file.path(root_dir, fname)
            tryCatch({
                ggsave(out_path, plot = combined, width = width, height = height, dpi = dpi)
                saved_files <- c(saved_files, out_path)
                if (verbose) message("    Saved: ", out_path)
            }, error = function(e) warning("Failed to save: ", e$message))
        }
    }
    
    # --- SEPARATE BY LOCATION ---
    if (separate.by == "location") {
        for (li in seq_along(location.names)) {
            loc <- location.names[li]
            if (verbose) message(sprintf("  [%d/%d] Location: %s", li, length(location.names), loc))
            
            panels <- list()
            for (outcome in outcomes) {
                p <- overlay_calibrations(loc, outcome)
                if (!is.null(p)) {
                    panels[[outcome]] <- p + ggtitle(outcome) +
                        theme(plot.title = element_text(size = 10, hjust = 0.5))
                }
            }
            
            if (length(panels) == 0) {
                if (verbose) message("    No panels generated — skipping")
                next
            }
            
            grid <- auto_grid(length(panels))
            
            combined <- wrap_plots(panels, nrow = grid$nrow, ncol = grid$ncol) +
                plot_annotation(
                    title    = paste0("Location: ", loc),
                    subtitle = subtitle,
                    theme    = theme(
                        plot.title    = element_text(size = 14, hjust = 0.5, face = "bold"),
                        plot.subtitle = element_text(size = 11, hjust = 0.5)
                    )
                )
            
            fname <- paste0("location_", sanitize(loc), "_by_outcome", file_suffix, ".png")
            out_path <- file.path(root_dir, fname)
            tryCatch({
                ggsave(out_path, plot = combined, width = width, height = height, dpi = dpi)
                saved_files <- c(saved_files, out_path)
                if (verbose) message("    Saved: ", out_path)
            }, error = function(e) warning("Failed to save: ", e$message))
        }
    }
    
    # --- SEPARATE BY CALIBRATION ---
    if (separate.by == "calibration") {
        for (cc in calibration.codes) {
            for (outcome in outcomes) {
                if (verbose) message(sprintf("  Calibration: %s | Outcome: %s", cc, outcome))
                
                panels <- list()
                for (loc in location.names) {
                    p <- single_calib_plot(loc, outcome, cc)
                    if (!is.null(p)) {
                        panels[[loc]] <- p + ggtitle(loc) +
                            theme(plot.title = element_text(size = 10, hjust = 0.5))
                    }
                }
                
                if (length(panels) == 0) {
                    if (verbose) message("    No panels generated — skipping")
                    next
                }
                
                grid <- auto_grid(length(panels))
                
                combined <- wrap_plots(panels, nrow = grid$nrow, ncol = grid$ncol) +
                    plot_annotation(
                        title    = paste0("Calibration: ", cc, " — Outcome: ", outcome),
                        subtitle = subtitle,
                        theme    = theme(
                            plot.title    = element_text(size = 14, hjust = 0.5, face = "bold"),
                            plot.subtitle = element_text(size = 11, hjust = 0.5)
                        )
                    )
                
                fname <- paste0("calib_", sanitize(cc), "_outcome_", sanitize(outcome), file_suffix, ".png")
                out_path <- file.path(root_dir, fname)
                tryCatch({
                    ggsave(out_path, plot = combined, width = width, height = height, dpi = dpi)
                    saved_files <- c(saved_files, out_path)
                    if (verbose) message("    Saved: ", out_path)
                }, error = function(e) warning("Failed to save: ", e$message))
            }
        }
    }
    
    if (verbose) message("\nDone. Saved ", length(saved_files), " file(s) to: ", root_dir)
    invisible(saved_files)
}


# ****************************************************************************************************
# ****  INTERNAL HELPERS  **** ----
# ****************************************************************************************************

## .resolve.calibration.simsets ----
# Auto-resolution from globalenv.
.resolve.calibration.simsets <- function(calibration.simsets, caller.name) {
    if (!is.null(calibration.simsets)) return(calibration.simsets)
    
    if (exists("calibration.simsets", envir = globalenv(), inherits = FALSE)) {
        cs <- get("calibration.simsets", envir = globalenv())
        message("[", caller.name, "] Using 'calibration.simsets' from global environment (",
                length(cs), " simset(s))")
        return(cs)
    }
    
    stop("calibration.simsets not found.\n",
         "Either pass it explicitly or load it into the global environment ",
         "as 'calibration.simsets' via load.calibration.simsets() first.")
}


## .get.available.locations ----
# Extracts available locations from calibration.simsets as a named vector.
# Optionally filters by calibration.code.
.get.available.locations <- function(calibration.simsets, calibration.code = NULL) {
    if (is.null(calibration.simsets) || length(calibration.simsets) == 0)
        return(character(0))
    
    if (!is.null(calibration.code)) {
        relevant <- extract.calibration.simsets(calibration.simsets,
                                                calibration.code = calibration.code,
                                                exact = TRUE)
    } else {
        relevant <- calibration.simsets
    }
    
    if (length(relevant) == 0) return(character(0))
    
    loc.codes <- sapply(relevant, `[[`, "location.code")
    loc.names <- sapply(relevant, `[[`, "location.name")
    
    # Deduplicate and return named vector
    unique.codes <- unique(loc.codes)
    unique.names <- loc.names[match(unique.codes, loc.codes)]
    names(unique.codes) <- unique.names
    return(unique.codes)
}


## .filter.to.requested.locations ----
# Filters available locations to only those requested.
# Returns named vector of matching locations.
.filter.to.requested.locations <- function(requested.locations,
                                           available.locations,
                                           caller.name = "unknown") {
    
    # NULL means use all available
    if (is.null(requested.locations)) return(available.locations)
    
    if (length(requested.locations) == 0) {
        warning("[", caller.name, "] Empty locations vector provided")
        return(character(0))
    }
    
    req.names <- names(requested.locations)
    req.codes <- unname(requested.locations)
    avail.names <- names(available.locations)
    avail.codes <- unname(available.locations)
    
    # Match by name or code
    matched.by.name <- req.names %in% avail.names
    matched.by.code <- req.codes %in% avail.codes
    matched <- matched.by.name | matched.by.code
    
    if (!all(matched)) {
        invalid <- if (!is.null(req.names)) req.names[!matched] else req.codes[!matched]
        warning("[", caller.name, "] Location(s) not available: ",
                paste(invalid, collapse = ", "))
    }
    
    if (!any(matched)) {
        warning("[", caller.name, "] None of the requested locations are available")
        return(character(0))
    }
    
    # Return matching subset of available
    keep.names <- req.names[matched]
    available.locations[avail.names %in% keep.names]
}


# ****************************************************************************************************
## msa var lookup ----
msa_var_names <- c(
    N  = "C.35620",
    M  = "C.33100",
    L  = "C.31080",
    A  = "C.12060",
    H  = "C.26420",
    D  = "C.19100",
    C  = "C.16980",
    DC = "C.47900",
    PH = "C.37980",
    O  = "C.36740",
    SF = "C.41860",
    P  = "C.38060",
    TM = "C.45300",
    R  = "C.40140",
    DT = "C.19820",
    B  = "C.12580",
    V  = "C.29820",
    BO = "C.14460",
    SD = "C.41740",
    CT = "C.16740",
    SA = "C.41700",
    J  = "C.27260",
    NO = "C.35380",
    ME = "C.32820",
    S  = "C.42660",
    AU = "C.12420",
    IN = "C.26900",
    CI = "C.17140",
    CO = "C.18140",
    BR = "C.12940",
    SC = "C.40900",
    CD = "C.17460",
    SL = "C.41180",
    DV = "C.19740",
    PD = "C.38900",
    BI = "C.13820",
    MO = "C.33660",
    JN = "C.27140"
)
msa_var_names <- setNames(names(msa_var_names), msa_var_names)   # flip: code → affix


# ****************************************************************************************************
## ensure.plot.dir ----
#
# Checks that a directory exists; creates it recursively if create.dirs = TRUE.
# Centralised here to eliminate the duplicate definitions that previously existed
# in both shield_calibration_inspection_helpers.R and intervention_helper_functions.R.
#
# Arguments:
#   path        - Directory path to check / create
#   create.dirs - If FALSE (default), stops with an informative error when missing;
#                 if TRUE, creates the directory silently
#
# Returns:
#   path (invisibly), for optional pipe-style use
# ****************************************************************************************************
ensure.plot.dir <- function(path, create.dirs = FALSE) {
    if (!dir.exists(path)) {
        if (!create.dirs)
            stop("Directory does not exist: ", path,
                 "\nSet create.dirs = TRUE to create it automatically.")
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        message("Created directory: ", path)
    }
    invisible(path)
}

# ****  PLOT PRIMITIVE & STAGE WRAPPERS (INTERNAL)  **** ----
# ****************************************************************************************************
## make_simplot ----
make_simplot <- function(outcome, last20, lastsim, style.manager, plotting.path, title.suffix,
                         facet.by = NULL, split.by = NULL) {
    
    if (!is.null(split.by) && length(split.by) != length(facet.by))
        stop("split.by and facet.by must be the same length when both are supplied")
    
    iterations <- if (is.null(facet.by) && is.null(split.by)) {
        list(list(split = NULL, facet = NULL))
    } else if (is.null(split.by)) {
        lapply(facet.by, function(f) list(split = NULL, facet = f))
    } else {
        mapply(function(s, f) list(split = s, facet = f), split.by, facet.by, SIMPLIFY = FALSE)
    }
    
    for (it in iterations) {
        p <- simplot(last20, lastsim, outcomes = outcome, split.by = it$split, facet.by = it$facet,
                     style.manager = style.manager, title.suffix = title.suffix,
                     dimension.values = list(year = 2000:2030))
        
        strat_suffix <- paste(c(it$split, it$facet), collapse = "_")
        strat_suffix <- if (nchar(strat_suffix) > 0) paste0("_", strat_suffix) else ""
        filename <- paste0(gsub("\\.", "-", outcome), strat_suffix, ".png")
        
        ggsave(file.path(plotting.path, filename), plot = p, width = 12, height = 7, dpi = 300)
    }
}

## make_stage0_plots_for_location ----
make_stage0_plots_for_location <- function(last20, lastsim, plotting.path, title.suffix, style.manager) {
    make_simplot("population", last20, lastsim, style.manager, plotting.path, title.suffix)
    make_simplot("population", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("sex", "race", "age"))
    make_simplot("population", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("age", "age", "sex"), split.by = c("sex", "race", "race"))
    
    make_simplot("deaths", last20, lastsim, style.manager, plotting.path, title.suffix)
    make_simplot("deaths", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("sex", "race", "age"))
    make_simplot("deaths", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("age", "age", "sex"), split.by = c("sex", "race", "race"))
    
    make_simplot("fertility.rate", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("age"), split.by = c("race"))
    
    make_simplot("immigration", last20, lastsim, style.manager, plotting.path, title.suffix)
    make_simplot("immigration", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("sex", "race", "age"))
    
    make_simplot("emigration", last20, lastsim, style.manager, plotting.path, title.suffix)
    make_simplot("emigration", last20, lastsim, style.manager, plotting.path, title.suffix,
                 facet.by = c("sex", "race", "age"))
    
    make_simplot("diagnosis.ps", last20, lastsim, style.manager, plotting.path, title.suffix)
}

## make_stage1_plots_for_location ----
make_stage1_plots_for_location <- function(last20, lastsim, plotting.path, title.suffix, style.manager) {
    for (outcome in c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
                      "diagnosis.late.misclassified", "hiv.testing")) {
        make_simplot(outcome, last20, lastsim, style.manager, plotting.path, title.suffix)
        make_simplot(outcome, last20, lastsim, style.manager, plotting.path, title.suffix,
                     facet.by = c("sex", "race"))
        if (outcome != "hiv.testing") {
            make_simplot(outcome, last20, lastsim, style.manager, plotting.path, title.suffix,
                         facet.by = c("race"), split.by = c("sex"))
        }
    }
}

## make_stage2_plots_for_location ----
make_stage2_plots_for_location <- function(last20, lastsim, plotting.path, title.suffix, style.manager) {
    for (outcome in c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
                      "diagnosis.late.misclassified", "hiv.testing")) {
        make_simplot(outcome, last20, lastsim, style.manager, plotting.path, title.suffix)
        make_simplot(outcome, last20, lastsim, style.manager, plotting.path, title.suffix,
                     facet.by = c("sex", "race", "age"))
        if (outcome != "hiv.testing") {
            make_simplot(outcome, last20, lastsim, style.manager, plotting.path, title.suffix,
                         facet.by = c("age", "age", "sex"), split.by = c("sex", "race", "race"))
        }
    }
}


# ****************************************************************************************************
