# ****************************************************************************************************
# SHIELD SIMSET LIBRARY ----
# ****************************************************************************************************
# Unified functions for loading, subsetting, and plotting calibration and intervention simsets.
#
# SIMSET TYPES:
#   Calibration  — 2D: City × CalibrationCode
#                  Key: "City – CalibCode"
#                  Stores: full_simset, last20_sims, last_sim
#
#   Intervention — 3D: City × CalibrationCode × Intervention
#                  Key: "City – CalibCode – InterventionLabel"
#                  Stores: full_simset
#
# LOCATION INPUT CONVENTION (all functions):
#   Named character vector — names = display names, values = MSA codes
#     SHIELD.TEN.MSAS                         → all 10 cities
#     SHIELD.TEN.MSAS[1:3]                    → first 3 cities
#     SHIELD.TEN.MSAS["Baltimore"]            → one city
#     NULL                                    → all available
#
# PUBLIC API:
#   SHARED:       ensure.plot.dir()
#   CALIBRATION:  load.calib.simsets()  |  extract.calib.simsets()
#                 plot.calib.stages()   |  plot.calib.location()  |  plot.calib.comparison()
#   INTERVENTION: load.int.simsets()    |  extract.int.simsets()
#                 plot.int.location()   |  plot.int.comparison()
#
# ****************************************************************************************************


# ****  REQUIRED LIBRARIES  **** ----
# ****************************************************************************************************
library(ggplot2)    # plotting primitives, themes, guides
library(cowplot)    # panel layout, legend extraction, assembly: plot_grid, get_legend, save_plot


# ****  PATH CONSTANTS  **** ----
# ****************************************************************************************************
# Set these once after sourcing this file. All functions use them as defaults.
#
# NAS path structure:
#   smb://cloud.nas.jh.edu/jheem$/simulations/shield/
#     {calibration.code}-{n.sim}/
#       {location.code}/
#         shield_{calibration.code}-{n.sim}_{location.code}_{intervention.code}.Rdata
#
# SHIELD.BASE.PATH  — root of all simulation files (contains {calib.code}-{n.sim}/ folders)
# SHIELD.PLOT.PATH  — root for all saved plots
#
# Example (adjust mount point for your system):
#   SHIELD.BASE.PATH <- file.path(get.jheem.root.directory(), "simulations", "shield")
#   SHIELD.PLOT.PATH <- file.path(get.jheem.root.directory(), "shield")
# ****************************************************************************************************
SHIELD.BASE.PATH <- file.path(get.jheem.root.directory(), "simulations", "shield")
SHIELD.PLOT.PATH <- file.path(get.jheem.root.directory(), "shield")


# ****  SECTION 1: SHARED INTERNAL UTILITIES  **** ----
# ****************************************************************************************************

.build.calib.key <- function(loc.name, calib.code)
    paste0(loc.name, " \u2013 ", calib.code)

.build.int.key <- function(loc.name, calib.code, int.label)
    paste0(loc.name, " \u2013 ", calib.code, " \u2013 ", int.label)

.resolve.cache <- function(cache, cache.name, force.reload, verbose) {
    if (force.reload) {
        if (verbose) message("[Cache] force.reload = TRUE — ignoring cache")
        return(NULL)
    }
    if (!is.null(cache)) {
        if (verbose) message("[Cache] Using provided cache (", length(cache), " simset(s))")
        return(cache)
    }
    if (exists(cache.name, envir = globalenv(), inherits = FALSE)) {
        cs <- get(cache.name, envir = globalenv())
        if (verbose) message("[Cache] Found '", cache.name, "' in global environment (",
                             length(cs), " simset(s))")
        return(cs)
    }
    if (verbose) message("[Cache] No cache found — loading all from file")
    NULL
}

.filter.to.requested.locations <- function(requested, available, caller = "unknown") {
    if (is.null(requested)) return(available)
    if (length(requested) == 0) { warning("[", caller, "] Empty locations vector"); return(character(0)) }
    req.names   <- if (!is.null(names(requested))) names(requested) else unname(requested)
    req.codes   <- unname(requested)
    avail.names <- names(available)
    avail.codes <- unname(available)
    matched     <- req.names %in% avail.names | req.codes %in% avail.codes
    if (!all(matched)) warning("[", caller, "] Not available: ", paste(req.names[!matched], collapse = ", "))
    if (!any(matched)) { warning("[", caller, "] None of the requested locations are available"); return(character(0)) }
    available[avail.names %in% req.names[matched]]
}

.get.plot.simset <- function(entry, sim.subset = "full") {
    if (sim.subset == "last20" && !is.null(entry$last20_sims)) return(entry$last20_sims)
    if (sim.subset == "last1"  && !is.null(entry$last_sim))   return(entry$last_sim)
    entry$full_simset
}

.auto.grid <- function(n, nrow = NULL, ncol = NULL) {
    if (!is.null(nrow) && !is.null(ncol)) return(list(nrow = nrow, ncol = ncol))
    if (!is.null(ncol)) return(list(nrow = ceiling(n / ncol), ncol = ncol))
    if (!is.null(nrow)) return(list(nrow = nrow, ncol = ceiling(n / nrow)))
    nc <- ceiling(sqrt(n * 1.5))
    list(nrow = ceiling(n / nc), ncol = nc)
}

.build.file.suffix <- function(plot.which, split.by, facet.by) {
    parts <- character(0)
    if (!is.null(plot.which) && plot.which == "sim.only") parts <- c(parts, "simOnly")
    if (!is.null(split.by)) parts <- c(parts, paste0("split-", paste(split.by, collapse = "-")))
    if (!is.null(facet.by)) parts <- c(parts, paste0("facet-",  paste(facet.by, collapse = "-")))
    if (length(parts) > 0) paste0("_", paste(parts, collapse = "_")) else ""
}

.sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "_", gsub("\\.", "-", x))

# Builds a default style manager based on split.by and facet.by context:
#   No stratification → color by simset; split.by set → color by stratum, linetype by simset
# Can always be overridden by passing style.manager explicitly.
.auto.style.manager <- function(split.by, facet.by, n.simsets = NULL) {
    if (!is.null(split.by)) {
        create.style.manager(color.sim.by    = "stratum",
                             linetype.sim.by = "simset")
    } else {
        create.style.manager(color.sim.by = "simset")
    }
}


## int.style.manager ----
#
# Builds a style manager for intervention comparisons where:
#   Color    = intervention (same color for same intervention across calibrations)
#   Linetype = calibration code (solid for calib 1, dashed for calib 2, etc.)
#
# This requires knowing the ordered intervention labels and calibration codes
# so that the color/linetype vectors align with how simsets are passed to simplot.
# Simsets must be ordered: all interventions for calib1, then all for calib2, etc.
#
# Arguments:
#   intervention.labels - Character vector of intervention display labels (in order)
#   calibration.codes   - Character vector of calibration codes (in order)
#   palette             - Base color function for interventions (default: ggsci::pal_jama())
#                         Must support at least length(intervention.labels) colors
#
# Returns:
#   A style manager object from create.style.manager()
#
# Usage:
#   plot.int.comparison(...,
#       style.manager = int.style.manager(
#           intervention.labels = c("baseline","int.1","int.2","int.3","int.4"),
#           calibration.codes   = calibration.codes
#       )
#   )
# ****************************************************************************************************
int.style.manager <- function(intervention.labels,
                              calibration.codes,
                              palette         = ggsci::pal_jama(),
                              linewidth.slope = 0) {
    n.int   <- length(intervention.labels)
    n.calib <- length(calibration.codes)
    n.total <- n.int * n.calib
    
    # Each calibration block repeats the same n.int colors
    int.colors  <- palette(n.int)
    rep.colors  <- rep(int.colors, times = n.calib)
    
    # Each calibration block gets one linetype applied to all its interventions
    base.linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash")
    rep.linetypes  <- rep(base.linetypes[seq_len(n.calib)], each = n.int)
    
    create.style.manager(
        color.sim.by    = "simset",
        linetype.sim.by = "simset",
        sim.palette     = scales::manual_pal(values = rep.colors),
        linetypes       = rep.linetypes,
        linewidth.slope = linewidth.slope
    )
}
# the number. If multiple exist (e.g. two n.sim runs), returns the largest.
.detect.n.sim <- function(calibration.code) {
    if (!dir.exists(SHIELD.BASE.PATH)) return(NULL)
    dirs    <- list.dirs(SHIELD.BASE.PATH, recursive = FALSE, full.names = FALSE)
    escaped <- gsub("\\.", "\\\\.", calibration.code)
    matches <- grep(paste0("^", escaped, "-([0-9]+)$"), dirs, value = TRUE)
    if (length(matches) == 0) return(NULL)
    n.sims  <- as.integer(sub(paste0(".*-"), "", matches))
    max(n.sims)
}

# Core simplot call from a named list of simset objects + display labels
.make.panel <- function(simset.list, labels, outcomes, split.by, facet.by,
                        style.manager, summary.type, plot.which, years) {
    if (length(simset.list) == 0) return(NULL)
    args <- list(outcomes = outcomes, dimension.values = list(year = years),
                 style.manager = style.manager, summary.type = summary.type,
                 plot.which = plot.which)
    if (!is.null(split.by)) args$split.by <- split.by
    if (!is.null(facet.by)) args$facet.by  <- facet.by
    if (length(simset.list) > 1 && !is.null(labels)) args$simset.names <- unname(labels)
    do.call(simplot, c(unname(simset.list), args))
}

# Patchwork grid from a named list of ggplot panels
# Uses cowplot to extract and place the legend separately — avoids patchwork
# legend clipping and overlap issues with nested grids.
#
# legend.position: "bottom" (default) or "right"
# legend.nrow:     number of rows in the legend when position = "bottom"
# Builds a multi-panel grid with a vertical legend in a dedicated right-side column.
# The legend column is allocated upfront as a fixed fraction of total width,
# so panels never overlap with it regardless of how many legend items there are.
#
# Arguments:
#   nrow / ncol    - Panel grid dimensions (NULL = auto)
#   legend.width   - Fraction of total figure width reserved for the legend column
#                    (default 0.2 = 20%). Increase if legend text is long.
#   title          - Optional overall title drawn above the figure
.make.patchwork <- function(panels, title = NULL, subtitle = NULL,
                            nrow = NULL, ncol = NULL,
                            legend.nrow   = NULL,   # kept for API compatibility, not used
                            legend.position = "right",  # kept for API compatibility
                            legend.height   = 0.06,     # kept for API compatibility
                            legend.width    = 0.2) {
    
    panels <- Filter(Negate(is.null), panels)
    if (length(panels) == 0) return(NULL)
    grid <- .auto.grid(length(panels), nrow, ncol)
    
    panel.theme <- theme(
        legend.text       = element_text(size = 9),
        legend.key.width  = unit(1.2, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(fill = "white", colour = NA),
        plot.margin       = margin(t = 5, r = 5, b = 5, l = 5)
    )
    
    # Extract vertical legend from first panel
    legend.grob <- cowplot::get_legend(
        panels[[1]] +
            panel.theme +
            theme(legend.position  = "right",
                  legend.box       = "vertical",
                  legend.direction = "vertical")
    )
    
    # Strip legends from all panels
    panels.no.legend <- lapply(panels, function(p)
        p + panel.theme + theme(legend.position = "none"))
    
    # Build panel grid (left column)
    panel.grid <- cowplot::plot_grid(
        plotlist = panels.no.legend,
        nrow     = grid$nrow,
        ncol     = grid$ncol
    )
    
    # Combine: panels (left) + legend (right) with pre-allocated widths
    p <- cowplot::plot_grid(
        panel.grid, legend.grob,
        nrow       = 1,
        rel_widths = c(1 - legend.width, legend.width)
    )
    
    # Add title if requested
    if (!is.null(title)) {
        title.row <- cowplot::ggdraw() +
            cowplot::draw_label(title, fontface = "bold", size = 14, x = 0.5, hjust = 0.5)
        heights <- c(0.05, 1)
        rows    <- list(title.row, p)
        if (!is.null(subtitle)) {
            sub.row <- cowplot::ggdraw() +
                cowplot::draw_label(subtitle, size = 11, x = 0.5, hjust = 0.5)
            heights <- c(0.04, 0.03, 1)
            rows    <- list(title.row, sub.row, p)
        }
        p <- cowplot::plot_grid(plotlist = rows, ncol = 1, rel_heights = heights)
    }
    p
}

# Save a combined plot to disk — handles both ggplot and cowplot objects
.save.plot <- function(combined, save.dir, filename, width, height, dpi, create.dirs, verbose) {
    ensure.plot.dir(save.dir, create.dirs)
    if (!grepl("\\.png$", filename)) filename <- paste0(filename, ".png")
    fp <- file.path(save.dir, filename)
    if (inherits(combined, "gg")) {
        ggsave(fp, plot = combined, width = width, height = height, dpi = dpi)
    } else {
        cowplot::save_plot(fp, combined, base_width = width, base_height = height, dpi = dpi)
    }
    if (verbose) message("  Saved: ", fp)
    invisible(fp)
}

# Auto-scales figure height based on number of panel rows and panel.height per row.
# panel.height: inches per row of panels (default 3.5)
# legend.extra: additional inches for bottom legend area
.auto.height <- function(n.panels, ncol, nrow = NULL, panel.height = 3.5, legend.extra = 0.8) {
    n.rows <- if (!is.null(nrow)) nrow else ceiling(n.panels / max(ncol, 1))
    n.rows * panel.height + legend.extra
}
# Checks a directory exists; creates it if create.dirs = TRUE.
ensure.plot.dir <- function(path, create.dirs = FALSE) {
    if (!dir.exists(path)) {
        if (!create.dirs) stop("Directory does not exist: ", path,
                               "\nSet create.dirs = TRUE to create it automatically.")
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        message("Created directory: ", path)
    }
    invisible(path)
}


# ****  SECTION 2: CALIBRATION — LOADING  **** ----
# ****************************************************************************************************

## load.calib.simsets ----
#
# Loads JHEEM calibration simsets across locations × calibration codes.
# Each entry stores full_simset, last20_sims, last_sim, and metadata.
#
# Arguments:
#   locations           - Named character vector (names = display, values = MSA codes)
#   calibration.codes   - Character vector of calibration code strings
#   n.sim               - Number of simulations per set. Only used for unregistered calibrations
#                         (registered ones get n.sim from the JHEEM registry automatically).
#                         Three accepted forms:
#                           NULL (default) → auto-detect from directory name on disk
#                           300            → use 300 for all unregistered calibrations
#                           c("calib.A"=300, "calib.B"=500) → per-code values
#   calib.file.code     - Filename code used for calibration .Rdata files on disk (default: "baseline")
#                         Calibration files follow the same naming as interventions:
#                         shield_{calib.code}-{n.sim}_{loc.code}_{calib.file.code}.Rdata
#   assemble.incomplete - If TRUE, assembles partial calibrations; if FALSE, skips them
#   cache               - (optional) Previously returned simset list to reuse
#   cache.name          - globalenv() variable name used as fallback cache (default: "calib.simsets")
#   force.reload        - TRUE → ignore all caches, reload everything
#   append              - TRUE → keep all cached entries in the output list
#   verbose             - Print progress and summary messages
#   version             - JHEEM version string (default: "shield")
#
# Returns:
#   Named list keyed as "City – CalibCode"
# ****************************************************************************************************
load.calib.simsets <- function(locations,
                               calibration.codes,
                               n.sim               = NULL,
                               calib.file.code     = "baseline",
                               assemble.incomplete = FALSE,
                               cache               = NULL,
                               cache.name          = "calib.simsets",
                               force.reload        = FALSE,
                               append              = TRUE,
                               verbose             = TRUE,
                               version             = "shield") {
    
    location.codes <- unname(locations)
    location.names <- if (!is.null(names(locations))) names(locations) else
        sapply(location.codes, function(lc) tryCatch(get.location.name(lc), error = function(e) lc))
    
    resolved.cache <- .resolve.cache(cache, cache.name, force.reload, verbose)
    
    # Build full key map
    key.map <- list()
    for (i in seq_along(location.codes))
        for (cc in calibration.codes) {
            key <- .build.calib.key(location.names[i], cc)
            key.map[[key]] <- list(loc.code = location.codes[i], loc.name = location.names[i], calib.code = cc)
        }
    expected.keys <- names(key.map)
    
    # Cache hit / miss
    if (!force.reload && !is.null(resolved.cache)) {
        cached.keys  <- intersect(expected.keys, names(resolved.cache))
        missing.keys <- setdiff(expected.keys, names(resolved.cache))
        if (verbose) {
            message("[Cache] ", if (append) length(resolved.cache) else length(cached.keys), " simset(s) served from cache")
            message("[Cache] ", length(missing.keys), " simset(s) to load from file")
        }
    } else {
        cached.keys  <- character(0)
        missing.keys <- expected.keys
    }
    
    all.simsets <- if (append && !is.null(resolved.cache)) {
        if (verbose) message("[Append] Mode ON — preserving ", length(resolved.cache), " cached simset(s)")
        resolved.cache
    } else if (!is.null(resolved.cache)) {
        if (verbose) message("[Append] Mode OFF — returning requested keys only")
        resolved.cache[cached.keys]
    } else { list() }
    
    # Group missing keys by calibration code for efficiency
    missing.by.code <- list()
    for (k in missing.keys) missing.by.code[[key.map[[k]]$calib.code]] <-
        c(missing.by.code[[key.map[[k]]$calib.code]], k)
    
    n.loaded <- n.skipped <- 0
    
    for (cc in names(missing.by.code)) {
        keys.for.code <- missing.by.code[[cc]]
        locs.for.code <- sapply(keys.for.code, function(k) key.map[[k]]$loc.code)
        
        # --- PATH 1: JHEEM registry (registered calibrations) ---
        calib.info <- tryCatch(get.calibration.info(cc), error = function(e) NULL)
        
        if (!is.null(calib.info)) {
            if (calib.info$n.burn != 0) warning("Calibration '", cc, "' has n.burn != 0")
            n.sim.reg <- calib.info$n.iter / calib.info$thin
            
            calib.progress <- tryCatch(
                get.calibration.progress(version = version, locations = locs.for.code, calibration.code = cc),
                error = function(e) NULL)
            
            if (is.null(calib.progress)) {
                warning("Could not retrieve calibration progress for '", cc, "' — skipping")
                n.skipped <- n.skipped + length(keys.for.code); next
            }
            if (ncol(calib.progress) > 1) warning("'", cc, "' is multi-chain; only chain 1 used")
            
            for (simset.key in keys.for.code) {
                info     <- key.map[[simset.key]]
                loc.code <- info$loc.code
                pct      <- calib.progress[loc.code, 1]
                
                if (is.na(pct)) {
                    if (verbose) message("  Skipping (no data): ", simset.key)
                    n.skipped <- n.skipped + 1; next
                }
                if (pct < 100 && !assemble.incomplete) {
                    if (verbose) message("  Skipping (incomplete, ", pct, "%): ", simset.key)
                    n.skipped <- n.skipped + 1; next
                }
                if (verbose) message("  Loading: ", simset.key,
                                     if (pct < 100) paste0(" (", pct, "% — assembling)") else "")
                
                full.simset <- if (pct < 100) {
                    tryCatch(assemble.simulations.from.calibration(
                        version = version, location = loc.code,
                        calibration.code = cc, allow.incomplete = TRUE),
                        error = function(e) { warning("Error assembling '", simset.key, "': ", e$message); NULL })
                } else {
                    tryCatch(retrieve.simulation.set(
                        version = version, location = loc.code,
                        calibration.code = cc, n.sim = n.sim.reg),
                        error = function(e) { warning("Error retrieving '", simset.key, "': ", e$message); NULL })
                }
                if (is.null(full.simset)) { n.skipped <- n.skipped + 1; next }
                
                n.sim.eff <- full.simset$n.sim
                all.simsets[[simset.key]] <- list(
                    full_simset   = full.simset,
                    last20_sims   = full.simset$subset((n.sim.eff - 19):n.sim.eff),
                    last_sim      = full.simset$last.sim(),
                    location.name = info$loc.name,
                    location.code = loc.code,
                    calib.code    = cc,
                    title.suffix  = paste0(": ", cc, if (pct < 100) paste0(" (", pct, "% complete)") else ""),
                    pct.complete  = pct
                )
                n.loaded <- n.loaded + 1
            }
            
        } else {
            # --- PATH 2: Disk fallback (unregistered calibrations) ---
            # Resolve n.sim for this calibration code from the n.sim argument or auto-detect
            n.sim.use <- if (is.null(n.sim)) {
                detected <- .detect.n.sim(cc)
                if (is.null(detected)) {
                    warning("Could not auto-detect n.sim for '", cc,
                            "' — no directory matching '", cc, "-<number>' found in SHIELD.BASE.PATH.",
                            "\nPass n.sim explicitly to load.calib.simsets().")
                    n.skipped <- n.skipped + length(keys.for.code); next
                }
                if (verbose) message("[Fallback] Auto-detected n.sim = ", detected, " for '", cc, "'")
                detected
            } else if (length(n.sim) == 1 && is.null(names(n.sim))) {
                n.sim                          # single value — use for all codes
            } else if (!is.null(names(n.sim)) && cc %in% names(n.sim)) {
                n.sim[[cc]]                    # named vector — look up this code
            } else {
                warning("n.sim not found for '", cc, "' — skipping. ",
                        "Provide n.sim as NULL (auto-detect) or a named vector with an entry for '", cc, "'.")
                n.skipped <- n.skipped + length(keys.for.code); next
            }
            
            if (verbose) message("[Fallback] '", cc,
                                 "' not in JHEEM registry — reading from disk (n.sim = ", n.sim.use, ")")
            
            for (simset.key in keys.for.code) {
                info     <- key.map[[simset.key]]
                loc.code <- info$loc.code
                run.tag  <- paste0(cc, "-", n.sim.use)
                dir.path <- file.path(SHIELD.BASE.PATH, run.tag, loc.code)
                
                if (!dir.exists(dir.path)) {
                    warning("Directory not found, skipping: ", dir.path)
                    n.skipped <- n.skipped + 1; next
                }
                
                # Calibration files follow the same pattern as intervention files:
                # shield_{run.tag}_{loc.code}_{calib.file.code}.Rdata
                # Default calib.file.code is "baseline"
                expected.file <- file.path(dir.path,
                                           paste0("shield_", run.tag, "_", loc.code, "_", calib.file.code, ".Rdata"))
                
                path <- if (file.exists(expected.file)) {
                    expected.file
                } else {
                    warning("Calibration file not found: ", expected.file,
                            "\nIf the file uses a different code than '", calib.file.code,
                            "', pass calib.file.code = '<code>' to load.calib.simsets().")
                    n.skipped <- n.skipped + 1; next
                }
                
                if (verbose) message("  Loading: ", simset.key, " [disk]")
                
                full.simset <- tryCatch(
                    load.simulation.set(path),
                    error = function(e) { warning("Error loading '", simset.key, "': ", e$message); NULL })
                if (is.null(full.simset)) { n.skipped <- n.skipped + 1; next }
                
                n.sim.eff <- full.simset$n.sim
                all.simsets[[simset.key]] <- list(
                    full_simset   = full.simset,
                    last20_sims   = full.simset$subset((n.sim.eff - 19):n.sim.eff),
                    last_sim      = full.simset$last.sim(),
                    location.name = info$loc.name,
                    location.code = loc.code,
                    calib.code    = cc,
                    title.suffix  = paste0(": ", cc),
                    pct.complete  = 100
                )
                n.loaded <- n.loaded + 1
            }
        }
    }
    
    if (verbose) {
        n.preserved <- if (append && !is.null(resolved.cache)) length(setdiff(names(resolved.cache), expected.keys)) else 0
        message("\n--- Calibration Load Summary ---")
        message("  From cache   : ", length(cached.keys))
        message("  Preserved    : ", n.preserved)
        message("  Loaded       : ", n.loaded)
        message("  Skipped      : ", n.skipped)
        message("  Total        : ", length(all.simsets))
    }
    all.simsets
}


# ****  SECTION 3: CALIBRATION — SUBSETTING  **** ----
# ****************************************************************************************************

## extract.calib.simsets ----
#
# Flexible subsetter for calibration simsets.
#   location.name only    → all calibration codes for that location
#   calibration.code only → all locations for that calibration code
#   both                  → single entry (returned unwrapped)
#
# Arguments:
#   calib.simsets    - Named list from load.calib.simsets()
#   location.name    - Location display name (NULL = no filter)
#   calibration.code - Calibration code string (NULL = no filter)
#   exact            - If TRUE, requires exact match; if FALSE, uses partial match
#
# Returns:
#   Named sublist, or single entry when both arguments are provided
# ****************************************************************************************************
extract.calib.simsets <- function(calib.simsets,
                                  location.name    = NULL,
                                  calibration.code = NULL,
                                  exact            = FALSE) {
    if (is.null(location.name) && is.null(calibration.code))
        stop("Provide at least one of: location.name or calibration.code")
    
    if (!is.null(location.name) && !is.null(calibration.code)) {
        key <- .build.calib.key(location.name, calibration.code)
        if (!key %in% names(calib.simsets)) stop("Calibration simset not found: '", key, "'")
        return(calib.simsets[[key]])
    }
    
    if (!is.null(location.name)) {
        if (exact) calib.simsets[startsWith(names(calib.simsets), paste0(location.name, " \u2013 "))]
        else       calib.simsets[grepl(location.name, names(calib.simsets), fixed = TRUE)]
    } else {
        if (exact) calib.simsets[endsWith(names(calib.simsets), paste0(" \u2013 ", calibration.code))]
        else       calib.simsets[grepl(calibration.code, names(calib.simsets), fixed = TRUE)]
    }
}


# ****  SECTION 4: CALIBRATION — PLOTTING  **** ----
# ****************************************************************************************************

## .make.stage.plots ----
# Internal: generates all diagnostic plots for one location at a given calibration stage.
.make.stage.plots <- function(entry, stage, plotting.path, style.manager) {
    last20   <- if (!is.null(entry$last20_sims)) entry$last20_sims else entry$full_simset
    last_sim <- entry$last_sim
    suffix   <- entry$title.suffix
    
    make_one <- function(outcome, fb = NULL, sb = NULL) {
        p <- simplot(last20, last_sim, outcomes = outcome, facet.by = fb, split.by = sb,
                     style.manager = style.manager, title.suffix = suffix,
                     dimension.values = list(year = 2000:2030))
        strat <- paste(c(sb, fb), collapse = "_")
        strat <- if (nchar(strat) > 0) paste0("_", strat) else ""
        ggsave(file.path(plotting.path, paste0(.sanitize(outcome), strat, ".png")),
               plot = p, width = 12, height = 7, dpi = 300)
    }
    
    if (stage == 0) {
        for (out in c("population", "deaths", "immigration", "emigration")) {
            make_one(out)
            make_one(out, fb = c("sex", "race", "age"))
            if (out %in% c("population", "deaths"))
                make_one(out, fb = c("age","age","sex"), sb = c("sex","race","race"))
        }
        make_one("fertility.rate", fb = "age", sb = "race")
        make_one("diagnosis.ps")
    }
    
    if (stage %in% c(1, 2)) {
        stage.outcomes <- c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
                            "diagnosis.late.misclassified", "hiv.testing")
        for (out in stage.outcomes) {
            make_one(out)
            make_one(out, fb = if (stage == 1) c("sex","race") else c("sex","race","age"))
            if (out != "hiv.testing") {
                if (stage == 1) make_one(out, fb = "race", sb = "sex")
                if (stage == 2) make_one(out, fb = c("age","age","sex"), sb = c("sex","race","race"))
            }
        }
    }
}


## plot.calib.stages ----
#
# Generates diagnostic plots (stage 0, 1, or 2) for calibration simsets.
# Uses last20_sims + last_sim — calibration-specific.
# Output saved to: <jheem.root>/shield/calibrationPlots/<calib.code>/<loc.code>/
#
# Arguments:
#   calib.simsets    - Named list from load.calib.simsets()
#   calibration.code - Calibration code to plot
#   stage            - 0, 1, or 2
#   locations        - Named vector of locations (NULL = all available for this code)
#   style.manager    - Custom style manager (default: source + stratum)
#   create.dirs      - Create output directories automatically
#   verbose          - Print progress messages
# ****************************************************************************************************
plot.calib.stages <- function(calib.simsets,
                              calibration.code,
                              stage,
                              locations     = NULL,
                              style.manager = NULL,
                              create.dirs   = TRUE,
                              verbose       = TRUE) {
    
    if (is.null(style.manager))
        style.manager <- create.style.manager(shape.data.by = "source", color.data.by = "stratum")
    
    available <- unique(sapply(
        extract.calib.simsets(calib.simsets, calibration.code = calibration.code),
        function(e) setNames(e$location.code, e$location.name)))
    if (length(available) == 0) { warning("No simsets for '", calibration.code, "'"); return(invisible(character(0))) }
    
    target      <- .filter.to.requested.locations(locations, available, "plot.calib.stages")
    if (length(target) == 0) return(invisible(character(0)))
    
    if (verbose) message("Stage ", stage, " plots for ", length(target), " location(s) [", calibration.code, "]")
    successful  <- character(0)
    
    for (i in seq_along(target)) {
        loc.name <- names(target)[i]; loc.code <- target[i]
        entry    <- calib.simsets[[.build.calib.key(loc.name, calibration.code)]]
        if (is.null(entry)) { if (verbose) message("  Skipping '", loc.name, "' — not found"); next }
        
        out.path <- file.path(SHIELD.PLOT.PATH, "calibrationPlots",
                              calibration.code, loc.code, "")
        tryCatch(ensure.plot.dir(out.path, create.dirs), error = function(e) stop(e$message))
        if (verbose) message(sprintf("  [%d/%d] '%s'", i, length(target), loc.name))
        
        tryCatch({ .make.stage.plots(entry, stage, out.path, style.manager); successful <- c(successful, loc.code) },
                 error = function(e) warning("Error for '", loc.name, "': ", e$message))
    }
    
    if (verbose) message("Done. Stage ", stage, " plots for ", length(successful), "/", length(target), " location(s).")
    invisible(successful)
}


## plot.calib.location ----
#
# Plots multiple calibration codes overlaid for one location.
#
# Arguments:
#   calib.simsets     - Named list from load.calib.simsets()
#   location          - Location display name
#   calibration.codes - Codes to include (NULL = all available for this location)
#   outcomes          - Character vector of outcomes
#   sim.subset        - "full" | "last20" | "last1"
#   split.by / facet.by / years / plot.which / style.manager / summary.type - passed to simplot
#   save / save.dir / filename / width / height / dpi / create.dirs - save options
# ****************************************************************************************************
plot.calib.location <- function(calib.simsets,
                                location,
                                calibration.codes = NULL,
                                outcomes,
                                sim.subset        = "full",
                                split.by          = NULL,
                                facet.by          = NULL,
                                years             = 2000:2030,
                                plot.which        = "sim.and.data",
                                style.manager     = NULL,
                                summary.type      = "median.and.interval",
                                save              = FALSE,
                                save.dir          = NULL,
                                filename          = NULL,
                                width             = 12,
                                height            = 7,
                                dpi               = 300,
                                create.dirs       = FALSE) {
    
    loc.entries <- extract.calib.simsets(calib.simsets, location.name = location)
    if (!is.null(calibration.codes))
        loc.entries <- loc.entries[sapply(loc.entries, function(e) e$calib.code %in% calibration.codes)]
    if (length(loc.entries) == 0) stop("No simsets found for location: '", location, "'")
    
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by, n.simsets = length(filtered))
    
    simset.list <- lapply(loc.entries, function(e) .get.plot.simset(e, sim.subset))
    labels      <- sapply(loc.entries, `[[`, "calib.code")
    p           <- .make.panel(simset.list, labels, outcomes, split.by, facet.by,
                               style.manager, summary.type, plot.which, years)
    if (is.null(p)) stop("Failed to generate plot for '", location, "'")
    p <- p + ggtitle(location)
    if (!save) return(p)
    
    if (is.null(save.dir)) save.dir <- file.path(SHIELD.PLOT.PATH, "calibrationPlots", location)
    if (is.null(filename))  filename <- paste0(.sanitize(location), "_",
                                               paste(.sanitize(outcomes), collapse = "_"),
                                               .build.file.suffix(plot.which, split.by, facet.by))
    .save.plot(p, save.dir, filename, width, height, dpi, create.dirs, verbose = TRUE)
    invisible(p)
}


## plot.calib.comparison ----
#
# Multi-panel calibration comparison across locations and/or calibration codes.
#
# separate.by = "outcome"     : one file per outcome;          panels = locations;  colors = calib codes
# separate.by = "location"    : one file per location;         panels = outcomes;   colors = calib codes
# separate.by = "calibration" : one file per calibration code; panels = locations;  colors = (single calib)
#
# Arguments:
#   calib.simsets     - Named list from load.calib.simsets()
#   calibration.codes - Calibration codes to include (NULL = all available)
#   locations         - Named location vector to include (NULL = all available)
#   outcomes          - Character vector of outcomes
#   separate.by       - "outcome" | "location" | "calibration"
#   folder.name          - Optional label inserted into the save path:
#                       calibrationPlots/comparison/{folder.name}/by_{separate.by}/
#                       Useful for distinguishing comparisons between different calibration sets
#                       (default: NULL → calibrationPlots/comparison/by_{separate.by}/)
#   split.by / facet.by / years / plot.which / style.manager / summary.type - passed to simplot
#   nrow / ncol       - Panel grid dimensions (NULL = auto)
#   save / save.dir / width / height / dpi / create.dirs / verbose - save options
#
# Returns:
#   Named list of patchwork plots (invisibly if save = TRUE)
# ****************************************************************************************************
plot.calib.comparison <- function(calib.simsets,
                                  calibration.codes = NULL,
                                  locations         = NULL,
                                  outcomes,
                                  separate.by       = c("outcome", "location", "calibration"),
                                  folder.name          = NULL,
                                  sim.subset        = "full",
                                  split.by          = NULL,
                                  facet.by          = NULL,
                                  years             = 2000:2030,
                                  nrow              = NULL,
                                  ncol              = NULL,
                                  legend.nrow       = NULL,
                                  legend.position   = "bottom",
                                  legend.height     = 0.06,
                                  legend.width      = 0.2,
                                  plot.which        = "sim.and.data",
                                  style.manager     = NULL,
                                  summary.type      = "median.and.interval",
                                  save              = FALSE,
                                  save.dir          = NULL,
                                  width             = 20,
                                  height            = NULL,
                                  dpi               = 300,
                                  create.dirs       = TRUE,
                                  verbose           = TRUE) {
    
    separate.by <- match.arg(separate.by)
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by, n.simsets = length(filtered))
    suffix      <- .build.file.suffix(plot.which, split.by, facet.by)
    
    # Resolve calibration codes and locations from the simset list
    all.calibs <- if (!is.null(calibration.codes)) calibration.codes else
        unique(sapply(calib.simsets, `[[`, "calib.code"))
    all.loc.names <- if (!is.null(locations)) {
        names(.filter.to.requested.locations(locations,
                                             setNames(sapply(calib.simsets, `[[`, "location.code"),
                                                      sapply(calib.simsets, `[[`, "location.name")), "plot.calib.comparison"))
    } else unique(sapply(calib.simsets, `[[`, "location.name"))
    
    if (is.null(save.dir)) save.dir <- file.path(SHIELD.PLOT.PATH, "calibrationPlots",
                                                 "comparison", if (!is.null(folder.name)) folder.name else NULL,
                                                 paste0("by_", separate.by))
    
    # Helper: build one panel for a location (all calibrations overlaid)
    loc.panel <- function(loc, outs) {
        entries <- extract.calib.simsets(calib.simsets, location.name = loc, exact = TRUE)
        entries <- entries[sapply(entries, function(e) e$calib.code %in% all.calibs)]
        if (length(entries) == 0) return(NULL)
        simsets <- lapply(entries, function(e) .get.plot.simset(e, sim.subset))
        labels  <- sapply(entries, `[[`, "calib.code")
        p <- .make.panel(simsets, labels, outs, split.by, facet.by, style.manager, summary.type, plot.which, years)
        if (!is.null(p)) p + ggtitle(loc) else NULL
    }
    
    output <- list()
    
    # --- SEPARATE BY OUTCOME ---
    if (separate.by == "outcome") {
        for (oi in seq_along(outcomes)) {
            outcome <- outcomes[oi]
            if (verbose) message(sprintf("[%d/%d] Outcome: %s", oi, length(outcomes), outcome))
            panels   <- setNames(lapply(all.loc.names, loc.panel, outs = outcome), all.loc.names)
            combined <- .make.patchwork(panels, title = paste0("Outcome: ", outcome), nrow = nrow, ncol = ncol, legend.nrow = legend.nrow, legend.position = legend.position, legend.height = legend.height, legend.width = legend.width)
            if (is.null(combined)) { if (verbose) message("  No panels — skipping"); next }
            output[[outcome]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir,
                                 paste0("outcome_", .sanitize(outcome), "_by_location", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    # --- SEPARATE BY LOCATION ---
    if (separate.by == "location") {
        for (li in seq_along(all.loc.names)) {
            loc <- all.loc.names[li]
            if (verbose) message(sprintf("[%d/%d] Location: %s", li, length(all.loc.names), loc))
            panels <- setNames(lapply(outcomes, function(out) {
                p <- loc.panel(loc, out); if (!is.null(p)) p + ggtitle(out) else NULL
            }), outcomes)
            combined <- .make.patchwork(panels, title = loc, nrow = nrow, ncol = ncol, legend.nrow = legend.nrow, legend.position = legend.position, legend.height = legend.height, legend.width = legend.width)
            if (is.null(combined)) { if (verbose) message("  No panels — skipping"); next }
            output[[loc]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir,
                                 paste0("location_", .sanitize(loc), "_by_outcome", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    # --- SEPARATE BY CALIBRATION ---
    if (separate.by == "calibration") {
        for (cc in all.calibs) {
            if (verbose) message("Calibration: ", cc)
            panels <- setNames(lapply(all.loc.names, function(loc) {
                entry <- tryCatch(extract.calib.simsets(calib.simsets, loc, cc), error = function(e) NULL)
                if (is.null(entry)) return(NULL)
                simsets <- list(.get.plot.simset(entry, sim.subset))
                p <- .make.panel(simsets, NULL, outcomes, split.by, facet.by,
                                 style.manager, summary.type, plot.which, years)
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.loc.names)
            combined <- .make.patchwork(panels, title = paste0("Calibration: ", cc), nrow = nrow, ncol = ncol, legend.nrow = legend.nrow, legend.position = legend.position, legend.height = legend.height, legend.width = legend.width)
            if (is.null(combined)) { if (verbose) message("  No panels — skipping"); next }
            output[[cc]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir, paste0("calib_", .sanitize(cc), suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    if (verbose && save) message("\nDone. Saved ", length(output), " file(s) to: ", save.dir)
    if (save) invisible(output) else output
}


# ****  SECTION 5: INTERVENTION — LOADING  **** ----
# ****************************************************************************************************

.build.int.path <- function(base.path, calibration.code, n.sim, location.code, intervention.code) {
    run.tag  <- paste0(calibration.code, "-", n.sim)
    filename <- paste0("shield_", run.tag, "_", location.code, "_", intervention.code, ".Rdata")
    file.path(base.path, run.tag, location.code, filename)
}


## load.int.simsets ----
#
# Loads JHEEM intervention simsets across locations × calibration codes × interventions.
# Interventions are run on calibrated models; files live alongside calibration outputs.
#
# Arguments:
#   locations           - Named character vector (names = display, values = MSA codes)
#   intervention.codes  - Character vector of short intervention codes (used in filenames)
#   calibration.codes   - Character vector of calibration codes these interventions run on
#   n.sim               - Number of simulations per set
#   base.path           - Root directory where simulation folders are stored
#   intervention.labels - Named vector mapping short code → display label (default: label = code)
#   cache               - (optional) Previously returned simset list to reuse
#   cache.name          - globalenv() variable name used as fallback cache (default: "int.simsets")
#   force.reload        - TRUE → ignore all caches, reload everything
#   append              - TRUE → keep all cached entries in the output list
#   verbose             - Print progress and summary messages
#
# Returns:
#   Named list keyed as "City – CalibCode – InterventionLabel"
# ****************************************************************************************************
load.int.simsets <- function(locations,
                             intervention.codes,
                             calibration.codes,
                             n.sim,
                             base.path           = SHIELD.BASE.PATH,
                             intervention.labels = NULL,
                             cache               = NULL,
                             cache.name          = "int.simsets",
                             force.reload        = FALSE,
                             append              = TRUE,
                             verbose             = TRUE) {
    
    city.names     <- if (!is.null(names(locations))) names(locations) else unname(locations)
    location.codes <- unname(locations)
    
    if (is.null(intervention.labels))
        intervention.labels <- setNames(intervention.codes, intervention.codes)
    
    resolved.cache <- .resolve.cache(cache, cache.name, force.reload, verbose)
    
    # Build full key map: City × CalibCode × Intervention
    key.map <- list()
    for (i in seq_along(location.codes))
        for (cc in calibration.codes)
            for (int.code in intervention.codes) {
                int.label <- if (int.code %in% names(intervention.labels))
                    intervention.labels[[int.code]] else int.code
                key <- .build.int.key(city.names[i], cc, int.label)
                key.map[[key]] <- list(loc.code  = location.codes[i], city.name = city.names[i],
                                       calib.code = cc, int.code = int.code, int.label = int.label)
            }
    expected.keys <- names(key.map)
    
    # Cache hit / miss
    if (!force.reload && !is.null(resolved.cache)) {
        cached.keys  <- intersect(expected.keys, names(resolved.cache))
        missing.keys <- setdiff(expected.keys, names(resolved.cache))
        if (verbose) {
            message("[Cache] ", if (append) length(resolved.cache) else length(cached.keys), " simset(s) served from cache")
            message("[Cache] ", length(missing.keys), " simset(s) to load from file")
        }
    } else {
        cached.keys  <- character(0)
        missing.keys <- expected.keys
    }
    
    all.simsets <- if (append && !is.null(resolved.cache)) {
        if (verbose) message("[Append] Mode ON — preserving ", length(resolved.cache), " cached simset(s)")
        resolved.cache
    } else if (!is.null(resolved.cache)) {
        if (verbose) message("[Append] Mode OFF — returning requested keys only")
        resolved.cache[cached.keys]
    } else { list() }
    
    n.loaded <- n.skipped <- 0
    
    for (simset.key in missing.keys) {
        info <- key.map[[simset.key]]
        path <- .build.int.path(base.path, info$calib.code, n.sim, info$loc.code, info$int.code)
        
        if (!file.exists(path)) {
            warning("File not found, skipping: ", path)
            n.skipped <- n.skipped + 1; next
        }
        if (verbose) message("  Loading: ", simset.key)
        
        simset <- tryCatch(load.simulation.set(path),
                           error = function(e) { warning("Error loading '", simset.key, "': ", e$message); NULL })
        if (is.null(simset)) { n.skipped <- n.skipped + 1; next }
        
        all.simsets[[simset.key]] <- list(
            full_simset   = simset,
            location.name = info$city.name,
            location.code = info$loc.code,
            calib.code    = info$calib.code,
            int.label     = info$int.label,
            int.code      = info$int.code,
            title.suffix  = paste0(": ", info$calib.code, " \u2013 ", info$int.label)
        )
        n.loaded <- n.loaded + 1
    }
    
    if (verbose) {
        n.preserved <- if (append && !is.null(resolved.cache)) length(setdiff(names(resolved.cache), expected.keys)) else 0
        message("\n--- Intervention Load Summary ---")
        message("  From cache   : ", length(cached.keys))
        message("  Preserved    : ", n.preserved)
        message("  Loaded       : ", n.loaded)
        message("  Skipped      : ", n.skipped)
        message("  Total        : ", length(all.simsets))
    }
    all.simsets
}


# ****  SECTION 6: INTERVENTION — SUBSETTING  **** ----
# ****************************************************************************************************

## extract.int.simsets ----
#
# Flexible 3D subsetter for intervention simsets.
# Any combination of the three filter arguments is valid:
#
#   location only               → all CalibCodes × Interventions for that city
#   calib.code only             → all Cities × Interventions for that calibration
#   intervention only           → all Cities × CalibCodes for that intervention
#   location + calib.code       → all Interventions for that City × Calibration
#   location + intervention     → all CalibCodes for that City × Intervention
#   all three                   → single entry (returned unwrapped)
#
# Arguments:
#   int.simsets  - Named list from load.int.simsets()
#   location     - Location display name (NULL = no filter)
#   calib.code   - Calibration code string (NULL = no filter)
#   intervention - Intervention display label OR short code (NULL = no filter)
#   exact        - If TRUE, exact match; if FALSE, partial match
#
# Returns:
#   Named sublist, or single entry when all three arguments are provided and one match exists
# ****************************************************************************************************
extract.int.simsets <- function(int.simsets,
                                location     = NULL,
                                calib.code   = NULL,
                                intervention = NULL,
                                exact        = FALSE) {
    
    if (is.null(location) && is.null(calib.code) && is.null(intervention))
        stop("Provide at least one filter argument: location, calib.code, or intervention")
    
    result <- int.simsets
    
    if (!is.null(location)) {
        if (exact) result <- result[sapply(result, function(e) e$location.name == location)]
        else       result <- result[grepl(location, sapply(result, `[[`, "location.name"), fixed = TRUE)]
    }
    
    if (!is.null(calib.code)) {
        if (exact) result <- result[sapply(result, function(e) e$calib.code == calib.code)]
        else       result <- result[grepl(calib.code, sapply(result, `[[`, "calib.code"), fixed = TRUE)]
    }
    
    if (!is.null(intervention)) {
        if (exact) result <- result[sapply(result, function(e)
            e$int.label == intervention || e$int.code == intervention)]
        else result <- result[sapply(result, function(e)
            grepl(intervention, e$int.label, fixed = TRUE) ||
                grepl(intervention, e$int.code,  fixed = TRUE))]
    }
    
    if (length(result) == 0) stop("No intervention simsets match the specified criteria")
    
    # Unwrap to single entry when all three dimensions specified
    if (!is.null(location) && !is.null(calib.code) && !is.null(intervention) && length(result) == 1)
        return(result[[1]])
    
    result
}


# ****  SECTION 7: INTERVENTION — PLOTTING  **** ----
# ****************************************************************************************************

## plot.int.location ----
#
# Plots all interventions overlaid for one city on one calibration code.
#
# Arguments:
#   int.simsets   - Named list from load.int.simsets()
#   location      - Location display name
#   calib.code    - Single calibration code to use as base
#   interventions - Intervention labels or codes to include (NULL = all for this location × calib)
#   outcomes      - Character vector of outcomes
#   split.by / facet.by / years / plot.which / style.manager / summary.type - passed to simplot
#   save / save.dir / filename / width / height / dpi / create.dirs - save options
# ****************************************************************************************************
plot.int.location <- function(int.simsets,
                              location,
                              calib.code,
                              interventions = NULL,
                              outcomes,
                              split.by      = NULL,
                              facet.by      = NULL,
                              years         = 2000:2030,
                              plot.which    = "sim.and.data",
                              style.manager = NULL,
                              summary.type  = "median.and.interval",
                              save          = FALSE,
                              save.dir      = NULL,
                              filename      = NULL,
                              width         = 12,
                              height        = 7,
                              dpi           = 300,
                              create.dirs   = FALSE) {
    
    entries <- extract.int.simsets(int.simsets, location = location, calib.code = calib.code, exact = TRUE)
    
    if (!is.null(interventions))
        entries <- entries[sapply(entries, function(e)
            e$int.label %in% interventions || e$int.code %in% interventions)]
    if (length(entries) == 0) stop("No intervention simsets found for '", location, "' / '", calib.code, "'")
    
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by, n.simsets = length(filtered))
    
    simset.list <- lapply(entries, function(e) e$full_simset)
    labels      <- sapply(entries, `[[`, "int.label")
    p           <- .make.panel(simset.list, labels, outcomes, split.by, facet.by,
                               style.manager, summary.type, plot.which, years)
    if (is.null(p)) stop("Failed to generate plot for '", location, "' / '", calib.code, "'")
    p <- p + ggtitle(paste0(location, " \u2013 ", calib.code))
    
    if (!save) return(p)
    
    if (is.null(save.dir)) save.dir <- file.path(SHIELD.PLOT.PATH, "interventionPlots",
                                                 calib.code, location)
    if (is.null(filename)) filename <- paste0(.sanitize(location), "_", .sanitize(calib.code), "_",
                                              paste(.sanitize(outcomes), collapse = "_"), .build.file.suffix(plot.which, split.by, facet.by))
    .save.plot(p, save.dir, filename, width, height, dpi, create.dirs, verbose = TRUE)
    invisible(p)
}


## plot.int.comparison ----
#
# Multi-panel intervention comparison with four modes.
#
# separate.by = "outcome"     : one file per outcome;          panels = locations;  colors = interventions
# separate.by = "location"    : one file per location;         single plot;         colors = interventions
# separate.by = "calibration" : one file per calib code;       panels = locations;  colors = interventions
# separate.by = "intervention": one file per intervention;     panels = locations;  colors = calib codes
#
# The "intervention" mode is the key comparison: how does one intervention perform
# across multiple calibration codes at each location?
#
# Arguments:
#   int.simsets       - Named list from load.int.simsets()
#   calibration.codes - Calibration codes to include (NULL = all available)
#   locations         - Location names to include (NULL = all available)
#   interventions     - Intervention labels or codes to include (NULL = all available)
#   outcomes          - Character vector of outcomes
#   separate.by       - "outcome" | "location" | "calibration" | "intervention"
#   folder.name          - Optional label inserted into the save path:
#                       interventionPlots/comparison/{folder.name}/by_{separate.by}/
#                       Useful for distinguishing comparisons between different intervention sets
#                       (default: NULL → interventionPlots/comparison/by_{separate.by}/)
#   split.by / facet.by / years / plot.which / style.manager / summary.type - passed to simplot
#   nrow / ncol       - Panel grid dimensions (NULL = auto)
#   save / save.dir / width / height / dpi / create.dirs / verbose - save options
#
# Returns:
#   Named list of patchwork plots (invisibly if save = TRUE)
# ****************************************************************************************************
plot.int.comparison <- function(int.simsets,
                                calibration.codes = NULL,
                                locations         = NULL,
                                interventions     = NULL,
                                outcomes,
                                separate.by       = c("outcome", "location", "calibration", "intervention"),
                                folder.name          = NULL,
                                split.by          = NULL,
                                facet.by          = NULL,
                                years             = 2000:2030,
                                nrow              = NULL,
                                ncol              = NULL,
                                legend.nrow       = NULL,
                                legend.position   = "bottom",
                                legend.height     = 0.06,
                                legend.width      = 0.2,
                                plot.which        = "sim.and.data",
                                style.manager     = NULL,
                                summary.type      = "median.and.interval",
                                save              = FALSE,
                                save.dir          = NULL,
                                width             = 20,
                                height            = NULL,
                                dpi               = 300,
                                create.dirs       = TRUE,
                                verbose           = TRUE) {
    
    separate.by <- match.arg(separate.by)
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by, n.simsets = length(filtered))
    suffix      <- .build.file.suffix(plot.which, split.by, facet.by)
    
    # Apply dimension filters
    filtered <- int.simsets
    if (!is.null(calibration.codes))
        filtered <- filtered[sapply(filtered, function(e) e$calib.code  %in% calibration.codes)]
    if (!is.null(locations))
        filtered <- filtered[sapply(filtered, function(e) e$location.name %in% locations)]
    if (!is.null(interventions))
        filtered <- filtered[sapply(filtered, function(e)
            e$int.label %in% interventions || e$int.code %in% interventions)]
    if (length(filtered) == 0) stop("No intervention simsets match the specified filters")
    
    # Unique values of each dimension after filtering
    all.locs  <- unique(sapply(filtered, `[[`, "location.name"))
    all.calibs <- unique(sapply(filtered, `[[`, "calib.code"))
    all.ints  <- unique(sapply(filtered, `[[`, "int.label"))
    
    # Determine legend color.by
    color.by <- if (separate.by == "intervention") "calibration" else
        if (separate.by == "calibration")  "intervention" else
            if (length(all.calibs) > 1)        "both" else "intervention"
    
    if (is.null(save.dir)) save.dir <- file.path(SHIELD.PLOT.PATH, "interventionPlots",
                                                 "comparison", if (!is.null(folder.name)) folder.name else NULL,
                                                 paste0("by_", separate.by))
    
    # Build a labeled simset list for one panel
    make.entries.panel <- function(entries, cur.outcomes = outcomes) {
        if (length(entries) == 0) return(NULL)
        simset.list <- lapply(entries, function(e) e$full_simset)
        labels <- switch(color.by,
                         intervention = sapply(entries, `[[`, "int.label"),
                         calibration  = sapply(entries, `[[`, "calib.code"),
                         both         = paste0(sapply(entries, `[[`, "calib.code"), " \u2013 ",
                                               sapply(entries, `[[`, "int.label")))
        .make.panel(simset.list, labels, cur.outcomes, split.by, facet.by,
                    style.manager, summary.type, plot.which, years)
    }
    
    # Filter entries to a specific loc/calib/intervention combination
    filter.entries <- function(loc = NULL, cc = NULL, int = NULL) {
        e <- filtered
        if (!is.null(loc)) e <- e[sapply(e, function(x) x$location.name == loc)]
        if (!is.null(cc))  e <- e[sapply(e, function(x) x$calib.code   == cc)]
        if (!is.null(int)) e <- e[sapply(e, function(x) x$int.label    == int || x$int.code == int)]
        e
    }
    
    output <- list()
    
    # --- SEPARATE BY OUTCOME ---
    if (separate.by == "outcome") {
        for (oi in seq_along(outcomes)) {
            outcome <- outcomes[oi]
            if (verbose) message(sprintf("[%d/%d] Outcome: %s", oi, length(outcomes), outcome))
            panels <- setNames(lapply(all.locs, function(loc) {
                p <- make.entries.panel(filter.entries(loc = loc), cur.outcomes = outcome)
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.locs)
            combined <- .make.patchwork(panels, title = paste0("Outcome: ", outcome), nrow = nrow, ncol = ncol, legend.nrow = legend.nrow, legend.position = legend.position, legend.height = legend.height, legend.width = legend.width)
            if (is.null(combined)) next
            output[[outcome]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir,
                                 paste0("outcome_", .sanitize(outcome), "_by_location", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    # --- SEPARATE BY LOCATION ---
    if (separate.by == "location") {
        for (li in seq_along(all.locs)) {
            loc <- all.locs[li]
            if (verbose) message(sprintf("[%d/%d] Location: %s", li, length(all.locs), loc))
            p <- make.entries.panel(filter.entries(loc = loc))
            if (is.null(p)) next
            p <- p + ggtitle(loc)
            output[[loc]] <- p
            h <- if (is.null(height)) .auto.height(1, ncol = 1, nrow = 1) else height
            if (save) .save.plot(p, save.dir,
                                 paste0("location_", .sanitize(loc), suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    # --- SEPARATE BY CALIBRATION (panels = locations, colors = interventions) ---
    if (separate.by == "calibration") {
        for (cc in all.calibs) {
            if (verbose) message("Calibration: ", cc)
            panels <- setNames(lapply(all.locs, function(loc) {
                p <- make.entries.panel(filter.entries(loc = loc, cc = cc))
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.locs)
            combined <- .make.patchwork(panels, title = paste0("Calibration: ", cc), nrow = nrow, ncol = ncol, legend.nrow = legend.nrow, legend.position = legend.position, legend.height = legend.height, legend.width = legend.width)
            if (is.null(combined)) next
            output[[cc]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir, paste0("calib_", .sanitize(cc), suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    # --- SEPARATE BY INTERVENTION (panels = locations, colors = calibration codes) ---
    if (separate.by == "intervention") {
        for (int in all.ints) {
            if (verbose) message("Intervention: ", int)
            panels <- setNames(lapply(all.locs, function(loc) {
                p <- make.entries.panel(filter.entries(loc = loc, int = int))
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.locs)
            combined <- .make.patchwork(panels, title = paste0("Intervention: ", int), nrow = nrow, ncol = ncol, legend.nrow = legend.nrow, legend.position = legend.position, legend.height = legend.height, legend.width = legend.width)
            if (is.null(combined)) next
            output[[int]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir, paste0("intervention_", .sanitize(int), suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    if (verbose && save) message("\nDone. Saved ", length(output), " file(s) to: ", save.dir)
    if (save) invisible(output) else output
}