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
library(patchwork)  # panel layout with wrap_plots and plot_annotation


# ****  PATH CONSTANTS  **** ----
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

.build.file.suffix <- function(split.by, facet.by,plot.which) {
    parts <- character(0)
    if (!is.null(split.by)) parts <- c(parts, paste0("split-", paste(split.by, collapse = "-")))
    if (!is.null(facet.by)) parts <- c(parts, paste0("facet-",  paste(facet.by, collapse = "-")))
    if (!is.null(plot.which) && plot.which == "sim.only") parts <- c(parts, "simOnly")
    if (length(parts) > 0) paste0("_", paste(parts, collapse = "_")) else ""
}

.sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "_", gsub("\\.", "-", x))

.auto.style.manager <- function(split.by, facet.by, n.simsets = NULL) {
    if (!is.null(split.by)) {
        create.style.manager(color.sim.by    = "stratum",
                             linetype.sim.by = "simset")
    } else {
        create.style.manager(color.sim.by = "simset")
    }
}

.extract.location.info <- function(location) {
    
    val <- unname(as.character(location))[1] #Takes whatever was passed in, strips off any name attribute (so a named vector like c(Atlanta = "C.12060") reduces to just "C.12060"), converts to character, and takes the first element. This normalizes all three possible input types into a single plain string val.
    
    if (grepl("^C\\.", val)) { #Checks whether val starts with the literal characters "C.". This is the heuristic for "is this a location code?" since all codes in SHIELD.MSAS.OF.INTEREST follow the "C.#####" pattern.
        # input is a code
        match <- SHIELD.MSAS.OF.INTEREST[SHIELD.MSAS.OF.INTEREST == val]
        if (length(match) == 0) {
            stop("code '", val, "' not found in SHIELD.MSAS.OF.INTEREST")
        }
        return(match)
        
    } else {
        # input is a name
        if (!val %in% names(SHIELD.MSAS.OF.INTEREST)) {
            stop("name '", val, "' not found in SHIELD.MSAS.OF.INTEREST")
        }
        return(SHIELD.MSAS.OF.INTEREST[val])
    }
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
    if (length(simset.list) >= 1 && !is.null(labels)) args$simset.names <- unname(labels)
    do.call(simplot, c(unname(simset.list), args))
    # browser()
    
    }

# Patchwork grid - each panel retains its own legend
# Simply arranges panels in a grid without collecting/sharing legends
# Patchwork grid with single shared legend on the right
# Removes legends from individual panels and shows one collected legend
.make.patchwork <- function(panels, title = NULL, subtitle = NULL,
                            nrow = NULL, ncol = NULL) {
    
    panels <- Filter(Negate(is.null), panels)
    if (length(panels) == 0) return(NULL)
    grid <- .auto.grid(length(panels), nrow, ncol)
    
    # Theme for panels - ensure consistent appearance
    panel.theme <- theme(
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(t = 5, r = 5, b = 5, l = 5)
    )
    
    panels.ready <- lapply(panels, function(p) p + panel.theme)
    
    # Wrap panels and collect guides to show single legend on right
    p <- wrap_plots(panels.ready, nrow = grid$nrow, ncol = grid$ncol) +
        plot_layout(guides = "collect") &
        theme(
            legend.position   = "right",
            legend.direction  = "vertical",
            legend.text       = element_text(size = 9),
            legend.key.width  = unit(1.2, "cm"),
            legend.key.height = unit(0.5, "cm"),
            legend.background = element_rect(fill = "white", colour = NA)
        )
    
    if (!is.null(title)) {
        ann <- list(title = title,
                    theme = theme(
                        plot.title      = element_text(size = 14, hjust = 0.5, face = "bold"),
                        plot.subtitle   = element_text(size = 11, hjust = 0.5),
                        plot.background = element_rect(fill = "white", colour = NA)))
        if (!is.null(subtitle)) ann$subtitle <- subtitle
        p <- p + do.call(plot_annotation, ann)
    }
    p
}

# Save a combined plot to disk
.save.plot <- function(combined, save.dir, filename, width, height, dpi, create.dirs, verbose) {
    ensure.plot.dir(save.dir, create.dirs)
    if (!grepl("\\.png$", filename)) filename <- paste0(filename, ".png")
    fp <- file.path(save.dir, filename)
    ggsave(fp, plot = combined, width = width, height = height, dpi = dpi)
    if (verbose) message("  Saved: ", fp)
    invisible(fp)
}

# Auto-scales figure height based on number of panel rows
.auto.height <- function(n.panels, ncol, nrow = NULL, panel.height = 3.5) {
    n.rows <- if (!is.null(nrow)) nrow else ceiling(n.panels / max(ncol, 1))
    n.rows * panel.height
}

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
    
    key.map <- list()
    for (i in seq_along(location.codes))
        for (cc in calibration.codes) {
            key <- .build.calib.key(location.names[i], cc)
            key.map[[key]] <- list(loc.code = location.codes[i], loc.name = location.names[i], calib.code = cc)
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
    
    missing.by.code <- list()
    for (k in missing.keys) missing.by.code[[key.map[[k]]$calib.code]] <-
        c(missing.by.code[[key.map[[k]]$calib.code]], k)
    
    n.loaded <- n.skipped <- 0
    
    for (cc in names(missing.by.code)) {
        keys.for.code <- missing.by.code[[cc]]
        locs.for.code <- sapply(keys.for.code, function(k) key.map[[k]]$loc.code)
        
        calib.info <- tryCatch(get.calibration.info(cc), error = function(e) NULL)
        
        if (!is.null(calib.info)) {
            if (calib.info$n.burn != 0) warning("Calibration '", cc, "' has n.burn != 0")
            
            
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
                        calibration.code = cc, n.sim ),
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
                n.sim
            } else if (!is.null(names(n.sim)) && cc %in% names(n.sim)) {
                n.sim[[cc]]
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
extract.calib.simsets <- function(calib.simsets,
                                  location    = NULL,
                                  calibration.code = NULL,
                                  exact            = FALSE) {
     # --- Resolve target name/code from the inputs ---
    if (is.null(location) && is.null(calibration.code))
        stop("Provide at least one of: location.name or calibration.code")

    if (!is.null(location)) {
        location.info=.extract.location.info(location)
        location.name=names(location.info)
        location.code=unname(location.info)[1]
        
        if (!is.null(calibration.code)) {
            if (exact) calib.simsets[paste0(location.name, " \u2013 ", calibration.code)]
            else       calib.simsets[grepl(location.name, names(calib.simsets), fixed = TRUE) &
                                         grepl(calibration.code, names(calib.simsets), fixed = TRUE)]
        } else {
            if (exact) calib.simsets[startsWith(names(calib.simsets), paste0(location.name, " \u2013 "))]
            else       calib.simsets[grepl(location.name, names(calib.simsets), fixed = TRUE)]
        }
        
    } else {
        if (exact) calib.simsets[endsWith(names(calib.simsets), paste0(" \u2013 ", calibration.code))]
        else       calib.simsets[grepl(calibration.code, names(calib.simsets), fixed = TRUE)]
    }
}


# ****  SECTION 4: CALIBRATION — PLOTTING  **** ----
# ****************************************************************************************************

## .make.stage.plots ----
.make.stage.plots <- function(entry, stage, plotting.path, style.manager) {
    last20   <- if (!is.null(entry$last20_sims)) entry$last20_sims else entry$full_simset
    last_sim <- entry$last_sim
    suffix   <- entry$title.suffix
    
    make_one <- function(outcome, facet.by =NULL, split.by = NULL,plot.which="sim.and.data") {
        p <- simplot(last20, last_sim, outcomes = outcome, 
                     facet.by = facet.by, split.by = split.by,plot.which=plot.which,
                     style.manager = style.manager, title.suffix = suffix,
                     dimension.values = list(year = 1970:2030))
        # browser()
      filename <- paste0(paste(.sanitize(outcome), collapse = "_"),
                           .build.file.suffix(split.by, facet.by,plot.which))
       ggsave(file.path(plotting.path, paste0(filename, ".png")),
               plot = p, width = 12, height = 7, dpi = 300)
    }
    
    if (stage == 0) {
        for (out in c("population", "deaths", "immigration", "emigration")) {
            
            # Unstratified
            make_one(out)
            
            # Faceted by one variable
            for (var in c("sex", "race", "age"))
                make_one(out, facet.by = var)
            
            # Faceted by one variable and split by one variable
            if (out %in% c("population", "deaths")) {
                for (pair in list(c("age", "sex"),
                                  c("age", "race"),
                                  c("sex", "race")))
                    make_one(out, facet.by = pair[1], split.by = pair[2])
            }
        }
        make_one("fertility.rate", facet.by =  "age", split.by =  "race")
        make_one("diagnosis.ps")
    }
    
    if (stage %in% c(1, 2,3)) {
         stage.outcomes <- c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
                            "diagnosis.late.misclassified", "hiv.testing")
        for (out in stage.outcomes) {

            # Unstratified
            make_one(out)

            # Faceted by one variable
            for (var in if (stage == 1) c("sex", "race") else c("sex", "race", "age"))
                make_one(out, facet.by  = var)

            # Faceted by one variable and split by one variable
            if (out != "hiv.testing") {
                for (pair in if (stage == 1) list(c("race", "sex")) else list(c("age", "sex"),
                                                                              c("age", "race"),
                                                                              c("sex", "race")))
                    make_one(out, facet.by=pair[1], split.by =pair[2])
            }
        }
    }
    
    if (stage %in% c(1, 2)) {
        stage.outcomes <- c("diagnosis.total", "diagnosis.ps")
        for (out in stage.outcomes) {
                make_one(out, facet.by = 'sex',plot.which="sim.only")
            
        }
    }
}


## plot.calib.stages ----
plot.calib.stages <- function(calib.simsets,
                              calibration.code,
                              stage,
                              locations     = NULL,
                              style.manager = NULL,
                              create.dirs   = TRUE,
                              verbose       = TRUE) {
    if (is.null(style.manager))
        style.manager <- create.style.manager(shape.data.by = "source", color.data.by = "stratum")
     
    # This new version makes sure the end result is a named vector.
    # We can't use "unique" without losing the names, hence the approach with "!duplicated".
    available <- setNames(
        sapply(extract.calib.simsets(calib.simsets, calibration.code = calibration.code),
               function(e) e$location.code),
        sapply(extract.calib.simsets(calib.simsets, calibration.code = calibration.code),
               function(e) e$location.name)
    )
    available <- available[!duplicated(available)]
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
plot.single.calib.single.location <- function(calib.simsets,
                                location,
                                calibration.code = NULL,
                                outcomes,
                                sim.subset        = "full",
                                split.by          = NULL,
                                facet.by          = NULL,
                                years             = 1970:2030,
                                plot.which        = "sim.and.data",
                                style.manager     = NULL,
                                summary.type      = "median.and.interval",
                                save              = TRUE,
                                save.dir          = NULL,
                                filename          = NULL,
                                width             = 12,
                                height            = 7,
                                dpi               = 300,
                                create.dirs       = FALSE) {
    
    if (length(location) > 1) {
        warning("Multiple locations provided; using only the first: '", location[1], "'")
        location <- location[1]
    }
    
    if (!is.null(calibration.code) && length(calibration.code) > 1) {
        warning("Multiple calibration codes provided; using only the first: '", calibration.code[1], "'")
        calibration.code <- calibration.code[1]
    }

    simset.list <- extract.calib.simsets(calib.simsets,calibration.code =calibration.code,  location = location)
    

    if (length(simset.list) == 0) stop("No simsets found for location: '", location, "'")
    
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    
    simset <-  .get.plot.simset(simset.list, sim.subset)
    labels      <- simset.list$calib.code
    p           <- .make.panel(simset, labels, outcomes, split.by, facet.by,
                               style.manager, summary.type, plot.which, years)
    if (is.null(p)) stop("Failed to generate plot for '", location, "'")
    p <- p + ggtitle(location)
    if (!save) return(p)
     
    if (is.null(save.dir)) save.dir <- file.path(SHIELD.PLOT.PATH, "calibrationPlots",calibration.code, location)
    if (is.null(filename))  filename <- paste0(paste(.sanitize(outcomes), collapse = "_"),
                                               .build.file.suffix( split.by, facet.by,plot.which))
    .save.plot(p, save.dir, filename, width, height, dpi, create.dirs, verbose = TRUE)
    invisible(p)
}


## plot.calib.comparison ----
plot.calib.comparison <- function(calib.simsets,
                                  calibration.codes = NULL,
                                  locations         = NULL,
                                  outcomes,
                                  separate.by       = c("outcome", "location", "calibration"),
                                 
                                  sim.subset        = "full",
                                  split.by          = NULL,
                                  facet.by          = NULL,
                                  years             = 1970:2030,
                                  nrow              = NULL,
                                  ncol              = NULL,
                                  plot.which        = "sim.and.data",
                                  style.manager     = NULL,
                                  summary.type      = "median.and.interval",
                                  save              = TRUE,
                                  save.dir          = NULL,
                                  folder.name       = NULL,
                                  width             = 20,
                                  height            = NULL,
                                  dpi               = 300,
                                  create.dirs       = TRUE,
                                  verbose           = TRUE) {
    # browser()
    if (!is.null(locations) && is.null(names(locations)))
        stop("Error: 'locations' must be a NAMED vector")
    
    separate.by <- match.arg(separate.by)
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    suffix      <- .build.file.suffix( split.by, facet.by,plot.which)
    
    all.calibs <- if (!is.null(calibration.codes)) calibration.codes else
        unique(sapply(calib.simsets, `[[`, "calib.code"))
    all.loc.names <- if (!is.null(locations)) {
        unique(names(.filter.to.requested.locations(locations,
                                                    setNames(sapply(calib.simsets, `[[`, "location.code"),
                                                             sapply(calib.simsets, `[[`, "location.name")),
                                                    "plot.calib.comparison")))
    } else unique(sapply(calib.simsets, `[[`, "location.name"))
 
   if (is.null(folder.name)){ folder.name <- all.calibs[1]
    if (length(all.calibs)>1) {folder.name<-paste0(all.calibs[1],"_vs_",all.calibs[1])} 
    if (length(all.calibs)>2) {folder.name<-paste0(all.calibs[1],"_vs_others")} }
    
    if (is.null(save.dir)) {
        save.dir <- file.path(SHIELD.PLOT.PATH, "calibrationPlots","comparison",folder.name,paste0("by_",separate.by))
     }
    
    loc.panel <- function(loc, outs) {
        # browser()
        entries <- extract.calib.simsets(calib.simsets, location = loc)
        entries <- entries[sapply(entries, function(e) e$calib.code %in% all.calibs)]
        if (length(entries) == 0) return(NULL)
        simsets <- lapply(entries, function(e) .get.plot.simset(e, sim.subset))
        labels  <- sapply(entries, `[[`, "calib.code")
        p <- .make.panel(simsets, labels, outs, split.by, facet.by, style.manager, summary.type, plot.which, years)
        if (!is.null(p)) p + ggtitle(loc) else NULL
    }
    
    output <- list()
     if (separate.by == "outcome") {
        for (oi in seq_along(outcomes)) {
            # browser()
            outcome <- outcomes[oi]
            if (verbose) message(sprintf("[%d/%d] Outcome: %s", oi, length(outcomes), outcome))
            panels   <- setNames(lapply(all.loc.names, loc.panel, outs = outcome), all.loc.names)
            combined <- .make.patchwork(panels, title = paste0("Outcome: ", outcome), nrow = nrow, ncol = ncol)
            if (is.null(combined)) { if (verbose) message("  No panels — skipping"); next }
            output[[outcome]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir,
                                 paste0("outcome_", .sanitize(outcome), "_by_location", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    if (separate.by == "location") {
        for (li in seq_along(all.loc.names)) {
            loc <- all.loc.names[li]
            if (verbose) message(sprintf("[%d/%d] Location: %s", li, length(all.loc.names), loc))
            panels <- setNames(lapply(outcomes, function(out) {
                p <- loc.panel(loc, out); if (!is.null(p)) p + ggtitle(out) else NULL
            }), outcomes)
            combined <- .make.patchwork(panels, title = loc, nrow = nrow, ncol = ncol)
            if (is.null(combined)) { if (verbose) message("  No panels — skipping"); next }
            output[[loc]] <- combined
            h <- if (is.null(height)) .auto.height(length(Filter(Negate(is.null), panels)), ncol = if (!is.null(ncol)) ncol else ceiling(sqrt(length(panels) * 1.5)), nrow = nrow) else height
            if (save) .save.plot(combined, save.dir,
                                 paste0("location_", .sanitize(loc), "_by_outcome", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    
    if (separate.by == "calibration") {
        for (cc in all.calibs) {
            if (verbose) message("Calibration: ", cc)
            panels <- setNames(lapply(all.loc.names, function(loc) {
                entry <- tryCatch(extract.calib.simsets(calib.simsets, location = loc, calibration.code = cc), error = function(e) NULL)
                if (is.null(entry)) return(NULL)
                simsets <- list(.get.plot.simset(entry, sim.subset))
                p <- .make.panel(simsets, NULL, outcomes, split.by, facet.by,
                                 style.manager, summary.type, plot.which, years)
                if (!is.null(p)) p + ggtitle(loc) else NULL
            }), all.loc.names)
            combined <- .make.patchwork(panels, title = paste0("Calibration: ", cc), nrow = nrow, ncol = ncol)
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
    # browser()
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


# ****  SECTION 6: INTERVENTION — SUBSETTING  **** ----
# ****************************************************************************************************

## extract.int.simsets ----
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
    
    if (!is.null(location) && !is.null(calib.code) && !is.null(intervention) && length(result) == 1)
        return(result[[1]])
    
    result
}


# ****  SECTION 7: INTERVENTION — PLOTTING  **** ----
# ****************************************************************************************************

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
                              create.dirs   = FALSE) {
    
    entries <- extract.int.simsets(int.simsets, location = location, calib.code = calib.code, exact = TRUE)
    
    if (!is.null(interventions))
        entries <- entries[sapply(entries, function(e)
            e$int.label %in% interventions || e$int.code %in% interventions)]
    if (length(entries) == 0) stop("No intervention simsets found for '", location, "' / '", calib.code, "'")
    
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    
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
                                verbose           = TRUE) {
    
    separate.by <- match.arg(separate.by)
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    suffix      <- .build.file.suffix( split.by, facet.by,plot.which)
    
    filtered <- int.simsets
    if (!is.null(calibration.codes))
        filtered <- filtered[sapply(filtered, function(e) e$calib.code  %in% calibration.codes)]
    if (!is.null(locations))
        filtered <- filtered[sapply(filtered, function(e) e$location.name %in% locations)]
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
    if (is.null(save.dir)) save.dir <- file.path(SHIELD.PLOT.PATH, "interventionPlots",
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
                    style.manager, summary.type, plot.which, years)
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

# **** SECTION 8: CALIBRATION VERIFICATION **** ----
# ****************************************************************************************************
verify_calibration <- function(calib.simsets,
                               calibration.codes,
                               generate.plots = TRUE,
                               locations     = NULL,
                               style.manager = NULL,
                               verbose = TRUE,
                               mixing.threshold = 100000,
                               unmixed.allowable = 2) {
    
    all.loc.names <- if (!is.null(locations)) {
        unique(names(.filter.to.requested.locations(locations,
                                                    setNames(sapply(calib.simsets, `[[`, "location.code"),
                                                             sapply(calib.simsets, `[[`, "location.name")),
                                                    "verify_calibration")))
    } else unique(sapply(calib.simsets, `[[`, "location.name"))
    
    setNames(lapply(calibration.codes, function(calib_code) {
        if (generate.plots) {
            plot.calib.comparison(
                calib.simsets,
                calibration.codes = calib_code,
                locations = locations,
                outcomes = c("population", "deaths", "diagnosis.ps"),
                separate.by = "location",
                sim.subset = "last1",
                style.manager = style.manager,
                save.dir = file.path(SHIELD.PLOT.PATH, "verificationPlots", calib_code)
            )
        }
        
        # Check if 2 or more parameters have a mixing statistic over 100,000
        rv <- !sapply(all.loc.names, function(loc) {
            tryCatch(
                {
                    simset <- extract.calib.simsets(calib.simsets, location = loc, calibration.code = calib_code, exact = T)[[1]]$full_simset
                    sum(simset$get.mcmc.mixing.statistic() > mixing.threshold) >= unmixed.allowable
                },
                error=function(e) {
                    print(paste0("Couldn't extract simset for ", loc, " in '", calib_code, "': returning FALSE"))
                    F
                })
        }, USE.NAMES = T)
        
        if (verbose && any(!rv)) {
            print(paste0("The following locations didn't mix in '", calib_code, "': ", paste0(all.loc.names[!rv], collapse = ", ")))
        }
        
        rv
        
    }), calibration.codes)
}

inspect_mixing <- function(calib.simsets,
                           calibration.codes,
                           locations          = NULL,
                           show.mixing        = FALSE,
                           mixing.n           = 6L,
                           mixing.threshold   = 100000,
                           unmixed.allowable  = 2,
                           verbose            = TRUE) {
  
  # Resolve location names
  all.loc.names <- if (!is.null(locations)) {
    unique(names(.filter.to.requested.locations(
      locations,
      setNames(sapply(calib.simsets, `[[`, "location.code"),
               sapply(calib.simsets, `[[`, "location.name")),
      "inspect_mixing"
    )))
  } else {
    unique(sapply(calib.simsets, `[[`, "location.name"))
  }
  
  setNames(lapply(calibration.codes, function(calib_code) {
    
    rv <- setNames(sapply(all.loc.names, function(loc) {
      
      tryCatch({
        simset <- extract.calib.simsets(
          calib.simsets,
          location         = loc,
          calibration.code = calib_code,
          exact            = TRUE
        )[[1]]$full_simset
        
        mixing.stats <- simset$get.mcmc.mixing.statistic()
        
        # --- Display mixing statistics as a table ---
        if (show.mixing) {
          n.display   <- min(mixing.n, length(mixing.stats))
          display.df  <- data.frame(
            parameter         = names(mixing.stats[,1][seq_len(n.display)]),
            mixing_statistic  = unname(mixing.stats[,1][seq_len(n.display)]),
            above_threshold   = mixing.stats[,1][seq_len(n.display)] > mixing.threshold,
            row.names         = NULL
          )
          
          cat("\n===== Mixing Statistics =====\n")
          cat("Location:         ", loc, "\n")
          cat("Calibration code: ", calib_code, "\n")
          cat("Showing:          ", n.display, " of ", length(mixing.stats), " parameters\n")
          cat("Threshold:        ", mixing.threshold, "\n\n")
          print(display.df, right = FALSE)
          cat("\n")
        }
        
        # --- Threshold check: FALSE if too many unmixed ---
        n.over <- sum(mixing.stats > mixing.threshold)
        pass   <- n.over < unmixed.allowable
        
        if (verbose && !pass) {
          cat(sprintf(
            "[WARN] %s in '%s': %d parameters above threshold (%d)\n",
            loc, calib_code, n.over, mixing.threshold
          ))
        }
        
        pass
        
      }, error = function(e) {
        if (verbose) {
          cat(sprintf(
            "[ERROR] Could not extract simset for %s in '%s': %s\n",
            loc, calib_code, conditionMessage(e)
          ))
        }
        FALSE
      })
      
    }), all.loc.names)
    
    if (verbose && any(!rv)) {
      cat(sprintf(
        "\nLocations that did not mix in '%s': %s\n",
        calib_code,
        paste0(all.loc.names[!rv], collapse = ", ")
      ))
    }
    
    rv
    
  }), calibration.codes)
}
