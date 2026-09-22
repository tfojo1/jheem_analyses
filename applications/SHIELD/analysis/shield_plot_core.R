# ****************************************************************************************************
# SHIELD SIMSET PLOTTING CORE ----
# ****************************************************************************************************
# Utilities shared by the calibration and intervention simset helpers. Source this first;
# both of those files source it themselves, so you rarely need to source it by hand.
#
# WHAT LIVES HERE
#   1. Path helpers      .shield.base.path()  .shield.plot.path()  ensure.plot.dir()
#   2. Panel building    .make.panel()  .make.patchwork()  .auto.grid()  .auto.height()
#   3. Saving            .save.plot()
#   4. Naming and style  .sanitize()  .build.file.suffix()  .auto.style.manager()
#   5. Caching           .resolve.cache()
#
# ROOT DIRECTORY
#   Every function that reads or writes disk takes root.dir. Passing NULL (the default)
#   falls back to get.jheem.root.directory(). Pass a path to work outside the JHEEM root.
# ****************************************************************************************************


library(ggplot2)    # plotting primitives, themes, guides
library(patchwork)  # panel layout with wrap_plots and plot_annotation


# Every function that reads simsets from disk or writes plots to disk takes a root.dir argument.
#
#   1. root.dir = NULL (the default) falls back to get.jheem.root.directory(), so all existing
#      calls keep the behavior they had before.
#   2. Pass an explicit root.dir to read simsets from, or write plots to, a location outside
#      the JHEEM root -- for example a logs folder or a shared drive.
#
# These are functions rather than constants because a constant is frozen at the moment the file
# is sourced, which leaves no way for a per-call root.dir to override it.

## .shield.base.path ----
.shield.base.path <- function(root.dir = NULL) {
    if (is.null(root.dir)) root.dir <- get.jheem.root.directory()
    file.path(root.dir, "simulations", "shield")
}


## .shield.plot.path ----
.shield.plot.path <- function(root.dir = NULL) {
    if (is.null(root.dir)) root.dir <- get.jheem.root.directory()
    file.path(root.dir, "shield")
}


## ensure.plot.dir ----
ensure.plot.dir <- function(path, create.dirs = FALSE) {
    if (!dir.exists(path)) {
        if (!create.dirs) stop("Directory does not exist: ", path,
                               "\nSet create.dirs = TRUE to create it automatically.")
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        message("Created directory: ", path)
    }
    invisible(path)
}


## .resolve.cache ----
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


## .sanitize ----
.sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "_", gsub("\\.", "-", x))


## .build.file.suffix ----
.build.file.suffix <- function(split.by, facet.by,plot.which) {
    parts <- character(0)
    if (!is.null(split.by)) parts <- c(parts, paste0("split-", paste(split.by, collapse = "-")))
    if (!is.null(facet.by)) parts <- c(parts, paste0("facet-",  paste(facet.by, collapse = "-")))
    if (!is.null(plot.which) && plot.which == "sim.only") parts <- c(parts, "simOnly")
    if (length(parts) > 0) paste0("_", paste(parts, collapse = "_")) else ""
}


## .auto.style.manager ----
.auto.style.manager <- function(split.by, facet.by, n.simsets = NULL) {
    if (!is.null(split.by)) {
        create.style.manager(color.sim.by    = "stratum",
                             linetype.sim.by = "simset")
    } else {
        create.style.manager(color.sim.by = "simset")
    }
}


## .auto.grid ----
.auto.grid <- function(n, nrow = NULL, ncol = NULL) {
    if (!is.null(nrow) && !is.null(ncol)) return(list(nrow = nrow, ncol = ncol))
    if (!is.null(ncol)) return(list(nrow = ceiling(n / ncol), ncol = ncol))
    if (!is.null(nrow)) return(list(nrow = nrow, ncol = ceiling(n / nrow)))
    nc <- ceiling(sqrt(n * 1.5))
    list(nrow = ceiling(n / nc), ncol = nc)
}


## .auto.height ----
# Auto-scales figure height based on number of panel rows
.auto.height <- function(n.panels, ncol, nrow = NULL, panel.height = 3.5) {
    n.rows <- if (!is.null(nrow)) nrow else ceiling(n.panels / max(ncol, 1))
    n.rows * panel.height
}


## .make.panel ----
# Core simplot call from a named list of simset objects + display labels
.make.panel <- function(simset.list, labels, outcomes, split.by, facet.by,
                        style.manager, summary.type, plot.which, years) {
    # browser()
    if (length(simset.list) == 0) return(NULL)
    args <- list(outcomes = outcomes, dimension.values = list(year = years),
                 style.manager = style.manager, summary.type = summary.type,
                 plot.which = plot.which )
    if (!is.null(split.by)) args$split.by <- split.by
    if (!is.null(facet.by)) args$facet.by  <- facet.by
    if (length(simset.list) >= 1 && !is.null(labels)) args$simset.names <- unname(labels)
    
    do.call(simplot, c(unname(simset.list), args))
    
}


## .make.patchwork ----
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


## .save.plot ----
# Save a combined plot to disk
.save.plot <- function(combined, save.dir, filename, width, height, dpi, create.dirs, verbose) {
    ensure.plot.dir(save.dir, create.dirs)
    if (!grepl("\\.png$", filename)) filename <- paste0(filename, ".png")
    fp <- file.path(save.dir, filename)
    ggsave(fp, plot = combined, width = width, height = height, dpi = dpi)
    if (verbose) message("  Saved: ", fp)
    invisible(fp)
}
