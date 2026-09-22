# ****************************************************************************************************
# SHIELD INTERVENTION SIMSET HELPERS ----
# ****************************************************************************************************
# Loading, subsetting and plotting of INTERVENTION simsets.
# Calibration simsets are handled by analysis/calibration/calibration_helper_functions.R.
#
# NOT THE SAME FILE as analysis/intervention/intervention_helper_functions.R, which builds the
# manuscript tables and figures. This one only loads and plots simsets.
#
# SIMSET SHAPE
#   3D: City x CalibrationCode x Intervention. Key: "City – CalibCode – InterventionLabel".
#   Each entry stores full_simset.
#
# PUBLIC API
#   load.int.simsets()     read intervention simsets from disk, with caching
#   extract.int.simsets()  filter by location / calibration code / intervention
#   plot.int.location()    one city, interventions overlaid
#   plot.int.comparison()  multi-panel comparison
#   int.style.manager()    style manager for intervention overlays
#
# EXTRA simplot() ARGUMENTS
#   plot.int.location() and plot.int.comparison() take '...' and pass it to simplot().
#   Any simplot() argument that is not already an argument here works, with nothing to define:
#     plot.int.location(int.simsets = int.simsets, location = "Baltimore",
#                       calib.code = "calib.9.19.stage3", outcomes = "diagnosis.ps",
#                       omit.data.years = 2020:2021)
#   dimension.values is merged with 'years', not replaced.
#   A misspelled name stops with the list of arguments simplot() accepts.
# ****************************************************************************************************


source('../jheem_analyses/applications/SHIELD/analysis/shield_plot_core.R')


## .build.int.key ----
.build.int.key <- function(loc.name, calib.code, int.label)
    paste0(loc.name, " \u2013 ", calib.code, " \u2013 ", int.label)


## .build.int.path ----
.build.int.path <- function(base.path, calibration.code, n.sim, location.code, intervention.code) {
    run.tag  <- paste0(calibration.code, "-", n.sim)
    filename <- paste0("shield_", run.tag, "_", location.code, "_", intervention.code, ".Rdata")
    file.path(base.path, run.tag, location.code, filename)
}


## int.style.manager ----
## int.style.manager ----
#
# Builds a style manager for intervention comparisons where:
#   Color    = calibration code (same color for same calibration across interventions)
#   Linetype = intervention (solid for int 1, dashed for int 2, etc.)
#
# Simsets must be ordered: all interventions for calib1, then all for calib2, etc.
#
# Arguments:
#   intervention.labels - Character vector of intervention display labels (in order)
#   calibration.codes   - Character vector of calibration codes (in order)
#   palette             - Base color function for calibrations (default: ggsci::pal_jama())
#                         Must support at least length(calibration.codes) colors
#   linewidth.slope     - Passed to create.style.manager()
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

## int.style.manager ----
int.style.manager <- function(intervention.labels,
                              calibration.codes,
                              palette         = ggsci::pal_jama(),
                              linewidth.slope = 0) {
    n.int   <- length(intervention.labels)
    n.calib <- length(calibration.codes)
    n.total <- n.int * n.calib
    
    # Each calibration gets one color applied to all its interventions
    calib.colors <- palette(n.calib)
    rep.colors   <- rep(calib.colors, each = n.int)
    
    # Each intervention gets one linetype repeated across calibrations
    base.linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    int.linetypes  <- base.linetypes[seq_len(n.int)]
    rep.linetypes  <- rep(int.linetypes, times = n.calib)
    
    create.style.manager(
        color.sim.by    = "simset",
        linetype.sim.by = "simset",
        sim.palette     = scales::manual_pal(values = rep.colors),
        linetypes       = rep.linetypes,
        linewidth.slope = linewidth.slope
    )
}


## load.int.simsets ----
load.int.simsets <- function(locations,
                             intervention.codes,
                             calibration.codes,
                             n.sim,
                             base.path           = NULL,
                             root.dir            = NULL,
                             intervention.labels = NULL,
                             cache               = NULL,
                             cache.name          = "int.simsets",
                             force.reload        = FALSE,
                             append              = TRUE,
                             verbose             = TRUE,
                             debug               = FALSE) {
    if (debug) browser()
    if (is.null(base.path)) base.path <- .shield.base.path(root.dir)
    city.names     <- if (!is.null(names(locations))) names(locations) else unname(locations)
    location.codes <- unname(locations)
    
    if (is.null(intervention.labels))
        intervention.labels <- setNames(intervention.codes, intervention.codes)
    
    resolved.cache <- .resolve.cache(cache, cache.name, force.reload, verbose)
    
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


## extract.int.simsets ----
extract.int.simsets <- function(int.simsets,
                                location     = NULL,
                                calib.code   = NULL,
                                intervention = NULL,
                                exact        = FALSE,
                                debug        = FALSE) {
    if (debug) browser()
    if (is.null(location) && is.null(calib.code) && is.null(intervention))
        stop("Provide at least one filter argument: location, calib.code, or intervention")
    
    result <- int.simsets
    
    if (!is.null(location)) {
        if (exact) result <- result[sapply(result, function(e) e$location.name == location)]
        else       result <- result[grepl(location, sapply(result, `[[`, "location.name"), fixed = TRUE)]
    }
    
    if (length(result) == 0) stop("No intervention simsets match the specified criteria")
    
    if (!is.null(calib.code)) {
        if (exact) result <- result[sapply(result, function(e) e$calib.code == calib.code)]
        else       result <- result[grepl(calib.code, sapply(result, `[[`, "calib.code"), fixed = TRUE)]
    }
    
    if (length(result) == 0) stop("No intervention simsets match the specified criteria")
    
    if (!is.null(intervention)) {
        if (exact) result <- result[sapply(result, function(e)
            e$int.label == intervention || e$int.code == intervention)]
        else result <- result[sapply(result, function(e)
            grepl(intervention, e$int.label, fixed = TRUE) ||
                grepl(intervention, e$int.code,  fixed = TRUE))]
    }
    
    if (length(result) == 0) stop("No intervention simsets match the specified criteria")
    
    if (!is.null(location) && !is.null(calib.code) && !is.null(intervention) && length(result) == 1)
        return(result[[1]])
    
    result
}


## plot.int.location ----
plot.int.location <- function(int.simsets,
                              location,
                              calib.code,
                              interventions = NULL,
                              outcomes,
                              split.by      = NULL,
                              facet.by      = NULL,
                              years         = 1970:2030,
                              plot.which    = "sim.and.data",
                              style.manager = NULL,
                              summary.type  = "median.and.interval",
                              save          = FALSE,
                              save.dir      = NULL,
                              filename      = NULL,
                              width         = 12,
                              height        = 7,
                              dpi           = 300,
                              create.dirs   = FALSE,
                              root.dir      = NULL,
                              debug         = FALSE,
                              ...) {
    if (debug) browser()
    
    # Anything else you pass goes straight to simplot(), e.g. omit.data.years = 2020:2021
    extra.args <- .check.simplot.args(list(...), "plot.int.location")
    
    entries <- extract.int.simsets(int.simsets, location = location, calib.code = calib.code, exact = TRUE)
    
    if (!is.null(interventions))
        entries <- entries[sapply(entries, function(e)
            e$int.label %in% interventions || e$int.code %in% interventions)]
    if (length(entries) == 0) stop("No intervention simsets found for '", location, "' / '", calib.code, "'")
    
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    
    simset.list <- lapply(entries, function(e) e$full_simset)
    labels      <- sapply(entries, `[[`, "int.label")
    p           <- .make.panel(simset.list, labels, outcomes, split.by, facet.by,
                               style.manager, summary.type, plot.which, years, extra.args)
    if (is.null(p)) stop("Failed to generate plot for '", location, "' / '", calib.code, "'")
    p <- p + ggtitle(paste0(location, " \u2013 ", calib.code))
    
    if (!save) return(p)
    
    if (is.null(save.dir)) save.dir <- file.path(.shield.plot.path(root.dir), "interventionPlots",
                                                 calib.code, location)
    if (is.null(filename)) filename <- paste0(.sanitize(location), "_", .sanitize(calib.code), "_",
                                              paste(.sanitize(outcomes), collapse = "_"), .build.file.suffix( split.by, facet.by,plot.which))
    .save.plot(p, save.dir, filename, width, height, dpi, create.dirs, verbose = TRUE)
    invisible(p)
}


## plot.int.comparison ----
plot.int.comparison <- function(int.simsets,
                                calibration.codes = NULL,
                                locations         = NULL,
                                interventions     = NULL,
                                outcomes,
                                separate.by       = c("outcome", "location", "calibration", "intervention"),
                                folder.name       = NULL,
                                split.by          = NULL,
                                facet.by          = NULL,
                                years             = 1970:2030,
                                nrow              = NULL,
                                ncol              = NULL,
                                plot.which        = "sim.and.data",
                                style.manager     = NULL,
                                summary.type      = "median.and.interval",
                                save              = FALSE,
                                save.dir          = NULL,
                                width             = 20,
                                height            = NULL,
                                dpi               = 300,
                                create.dirs       = TRUE,
                                verbose           = TRUE,
                                root.dir          = NULL,
                                debug             = FALSE,
                                ...) {
    
    if (debug) browser()
    
    # Anything else you pass goes straight to simplot(), e.g. omit.data.years = 2020:2021
    extra.args <- .check.simplot.args(list(...), "plot.int.comparison")
    
    separate.by <- match.arg(separate.by)
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    suffix      <- .build.file.suffix( split.by, facet.by,plot.which)
    
    filtered <- int.simsets
    if (!is.null(calibration.codes))
        filtered <- filtered[sapply(filtered, function(e) e$calib.code  %in% calibration.codes)]
    
    if (length(filtered) == 0) stop("No intervention simsets match the specified filters")
    if (!is.null(locations))
        filtered <- filtered[sapply(filtered, function(e) e$location.name %in% locations)]
    
    if (length(filtered) == 0) stop("No intervention simsets match the specified filters")
    if (!is.null(interventions))
        filtered <- filtered[sapply(filtered, function(e)
            e$int.label %in% interventions || e$int.code %in% interventions)]
    if (length(filtered) == 0) stop("No intervention simsets match the specified filters")
    
    all.locs  <- unique(sapply(filtered, `[[`, "location.name"))
    all.calibs <- unique(sapply(filtered, `[[`, "calib.code"))
    all.ints  <- unique(sapply(filtered, `[[`, "int.label"))
    
    color.by <- if (separate.by == "intervention") "calibration" else
        if (separate.by == "calibration")  "intervention" else
            if (length(all.calibs) > 1)        "both" else "intervention"
    
    if (is.null(folder.name)) folder.name<-paste0(calibration.codes[1],".vs.others")
    if (is.null(save.dir)) save.dir <- file.path(.shield.plot.path(root.dir), "interventionPlots",
                                                 "comparison",folder.name,
                                                 paste0("by_", separate.by))
    
    make.entries.panel <- function(entries, cur.outcomes = outcomes) {
        if (length(entries) == 0) return(NULL)
        simset.list <- lapply(entries, function(e) e$full_simset)
        labels <- switch(color.by,
                         intervention = sapply(entries, `[[`, "int.label"),
                         calibration  = sapply(entries, `[[`, "calib.code"),
                         both         = paste0(sapply(entries, `[[`, "calib.code"), " \u2013 ",
                                               sapply(entries, `[[`, "int.label")))
        .make.panel(simset.list, labels, cur.outcomes, split.by, facet.by,
                    style.manager, summary.type, plot.which, years, extra.args)
    }
    
    filter.entries <- function(loc = NULL, cc = NULL, int = NULL) {
        e <- filtered
        if (!is.null(loc)) e <- e[sapply(e, function(x) x$location.name == loc)]
        if (!is.null(cc))  e <- e[sapply(e, function(x) x$calib.code   == cc)]
        if (!is.null(int)) e <- e[sapply(e, function(x) x$int.label    == int || x$int.code == int)]
        e
    }
    
    output <- list()
    
    if (separate.by == "outcome") {
        for (oi in seq_along(outcomes)) {
            outcome <- outcomes[oi]
            if (verbose) message(sprintf("[%d/%d] Outcome: %s", oi, length(outcomes), outcome))
            panels <- setNames(lapply(all.locs, function(loc) {
                p <- make.entries.panel(filter.entries(loc = loc), cur.outcomes = outcome)
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.locs)
            combined <- .make.patchwork(panels, title = paste0("Outcome: ", outcome), nrow = nrow, ncol = ncol)
            if (is.null(combined)) next
            output[[outcome]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir,
                                 paste0("outcome_", .sanitize(outcome), "_by_location", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
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
    
    if (separate.by == "calibration") {
        for (cc in all.calibs) {
            if (verbose) message("Calibration: ", cc)
            panels <- setNames(lapply(all.locs, function(loc) {
                p <- make.entries.panel(filter.entries(loc = loc, cc = cc))
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.locs)
            combined <- .make.patchwork(panels, title = paste0("Calibration: ", cc), nrow = nrow, ncol = ncol)
            if (is.null(combined)) next
            output[[cc]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir, paste0("calib_", .sanitize(cc), suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    if (separate.by == "intervention") {
        for (int in all.ints) {
            if (verbose) message("Intervention: ", int)
            panels <- setNames(lapply(all.locs, function(loc) {
                p <- make.entries.panel(filter.entries(loc = loc, int = int))
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.locs)
            combined <- .make.patchwork(panels, title = paste0("Intervention: ", int), nrow = nrow, ncol = ncol)
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
