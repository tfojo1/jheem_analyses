# ****************************************************************************************************
# SHIELD CALIBRATION SIMSET HELPERS ----
# ****************************************************************************************************
# Loading, subsetting and plotting of CALIBRATION simsets.
# Intervention simsets are handled by analysis/intervention/int_simset_helper_functions.R.
#
# SIMSET SHAPE
#   2D: City x CalibrationCode. Key: "City – CalibCode".
#   Each entry stores full_simset, last20_sims, last_sim.
#
# LOCATION INPUT CONVENTION
#   A NAMED character vector: names = display names, values = MSA codes.
#     SHIELD.TEN.MSAS                all 10 cities
#     SHIELD.TEN.MSAS["Baltimore"]   one city
#     NULL                           all available
#
# PUBLIC API
#   load.calib.simsets()     read simsets from disk, with caching
#   extract.calib.simsets()  filter a loaded set by location / calibration code
#   plot.calib.stages()      per-city stage plots
#   plot.calib.comparison()  multi-panel comparison across cities or codes
#   inspect_mixing()         MCMC mixing statistics and threshold check
#
# EXTRA simplot() ARGUMENTS
#   plot.calib.stages() and plot.calib.comparison() take '...' and pass it to simplot().
#   Any simplot() argument that is not already an argument here works, with nothing to define:
#     plot.calib.comparison(calib.simsets = calib.simsets,
#                           outcomes = "diagnosis.ps",
#                           omit.data.years = 2020:2021)
#   dimension.values is merged with 'years', not replaced:
#     years = 1970:2030 plus dimension.values = list(sex = "male")
#     gives simplot list(year = 1970:2030, sex = "male").
#   A misspelled name stops with the list of arguments simplot() accepts.
# ****************************************************************************************************


source('../jheem_analyses/applications/SHIELD/analysis/shield_plot_core.R')


## .build.calib.key ----
.build.calib.key <- function(loc.name, calib.code)
    paste0(loc.name, " \u2013 ", calib.code)


## .filter.to.requested.locations ----
.filter.to.requested.locations <- function(requested, available, caller = "unknown") {
    # browser()
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


## .get.plot.simset ----
.get.plot.simset <- function(entry, sim.subset = "full") {
    
    if (sim.subset == "last20" && !is.null(entry$last20_sims)) return(entry$last20_sims)
    if (sim.subset == "last1"  && !is.null(entry$last_sim))   return(entry$last_sim)
    entry$full_simset
}


## .extract.location.info ----
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


## .detect.n.sim ----
.detect.n.sim <- function(calibration.code, root.dir = NULL) {
    base.path <- .shield.base.path(root.dir)
    if (!dir.exists(base.path)) return(NULL)
    dirs    <- list.dirs(base.path, recursive = FALSE, full.names = FALSE)
    escaped <- gsub("\\.", "\\\\.", calibration.code)
    matches <- grep(paste0("^", escaped, "-([0-9]+)$"), dirs, value = TRUE)
    if (length(matches) == 0) return(NULL)
    n.sims  <- as.integer(sub(paste0(".*-"), "", matches))
    max(n.sims)
}


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
                               version             = "shield",
                               root.dir            = NULL) {
    
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
                get.calibration.progress(version = version, locations = locs.for.code, calibration.code = cc,
                                         root.dir = root.dir),
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
                        calibration.code = cc, allow.incomplete = TRUE,
                        root.dir = root.dir),
                        error = function(e) { warning("Error assembling '", simset.key, "': ", e$message); NULL })
                } else {
                    tryCatch(retrieve.simulation.set(
                        version = version, location = loc.code,
                        calibration.code = cc, n.sim = n.sim,
                        root.dir = root.dir),
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
                detected <- .detect.n.sim(cc, root.dir = root.dir)
                if (is.null(detected)) {
                    warning("Could not auto-detect n.sim for '", cc,
                            "' — no directory matching '", cc, "-<number>' found in ",
                            .shield.base.path(root.dir), ".",
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
                dir.path <- file.path(.shield.base.path(root.dir), run.tag, loc.code)
                
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


## .make.stage.plots ----
.make.stage.plots <- function(entry, stage, plotting.path, style.manager,
                              extra.args = list()) {
    last20   <- if (!is.null(entry$last20_sims)) entry$last20_sims else entry$full_simset
    last_sim <- entry$last_sim
    suffix   <- entry$title.suffix
    
    make_one <- function(outcome, facet.by =NULL, split.by = NULL,plot.which="sim.and.data") {
        args <- list(outcomes = outcome,
                     facet.by = facet.by, split.by = split.by, plot.which = plot.which,
                     style.manager = style.manager, title.suffix = suffix,
                     dimension.values = list(year = 1970:2030))
        p <- do.call(simplot, c(list(last20, last_sim),
                                .merge.simplot.args(args, extra.args)))
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
                              verbose       = TRUE,
                              root.dir      = NULL,
                              ...) {
    
    # Anything else you pass goes straight to simplot(), e.g. omit.data.years = 2020:2021
    extra.args <- .check.simplot.args(list(...), "plot.calib.stages")
    
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
        
        out.path <- file.path(.shield.plot.path(root.dir), "calibrationPlots",
                              calibration.code, loc.code, "")
        tryCatch(ensure.plot.dir(out.path, create.dirs), error = function(e) stop(e$message))
        if (verbose) message(sprintf("  [%d/%d] '%s'", i, length(target), loc.name))
        
        tryCatch({ .make.stage.plots(entry, stage, out.path, style.manager, extra.args); successful <- c(successful, loc.code) },
                 error = function(e) warning("Error for '", loc.name, "': ", e$message))
    }
    
    if (verbose) message("Done. Stage ", stage, " plots for ", length(successful), "/", length(target), " location(s).")
    invisible(successful)
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
                                  verbose           = TRUE,
                                  root.dir          = NULL,
                                  ...) {
    
    # Anything else you pass goes straight to simplot(), e.g. omit.data.years = 2020:2021
    extra.args <- .check.simplot.args(list(...), "plot.calib.comparison")
    
    if (!is.null(locations) && is.null(names(locations)))
        stop("Error: 'locations' must be a NAMED vector")
    
    separate.by <- match.arg(separate.by)
    if (is.null(style.manager)) style.manager <- .auto.style.manager(split.by, facet.by)
    suffix      <- .build.file.suffix( split.by, facet.by,plot.which)
    
    all.calibs <- if (!is.null(calibration.codes)) calibration.codes else
        unique(sapply(calib.simsets, `[[`, "calib.code"))
    # browser()
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
        save.dir <- file.path(.shield.plot.path(root.dir), "calibrationPlots","comparison",folder.name,paste0("by_",separate.by))
    }
    loc.panel <- function(loc, outs) {
        entries <- extract.calib.simsets(calib.simsets, location = loc)
        entries <- entries[sapply(entries, function(e) e$calib.code %in% all.calibs)]
        if (length(entries) == 0) return(NULL)
        simsets <- lapply(entries, function(e) .get.plot.simset(e, sim.subset))
        labels  <- sapply(entries, `[[`, "calib.code")
        p <- .make.panel(simsets, labels, outs, split.by, facet.by, style.manager, summary.type, plot.which, years, extra.args)
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
                                 paste0("compare_",.sanitize(outcome), "_accross_locations", suffix),
                                 width, h, dpi, create.dirs, verbose)
        }
    }
    # browser()
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
                                 paste0("compare_outcomes_within_", .sanitize(loc), suffix),
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
                                 style.manager, summary.type, plot.which, years, extra.args)
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


## inspect_mixing ----
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
