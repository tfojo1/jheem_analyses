# ****************************************************************************************************
# PRETTY PLOTS: HELPER FUNCTIONS ----
# ****************************************************************************************************
# The functions behind the calibration figures. 06_pretty_plots.R sources this file.
# It defines functions only; sourcing it draws nothing and writes nothing.
#
# HOW TO USE IT
#   1. Load the calibration simsets with load.calib.simsets() (calibration_helper_functions.R).
#   2. Draw a figure with shield.plot(calib.simsets, "<outcome>", locations = ...).
#      Examples are at the bottom of this file.
#   3. Save it with ggsave(), or with save.shield.fig() from pretty_plots_style.R.
#
# WHAT IS IN IT
#   1. PICKING SIMSETS   find the simsets for the cities you ask for   (internal)
#   2. DATA LAYER        shield.prepare(): model and observed data as tidy tables
#                        shield.inspect(): check what came back
#   3. THE PLOT          shield.plot(): the one plotting function
#                        shield.source.names(): see the names each data source carries
#
# The look of the figures (years, colours, theme, labels, sizes) is set in pretty_plots_style.R,
# which this file sources. Edit that file, not this one, to change how figures look.
#
# DEPENDENCIES: jheem2 (prepare.plot), ggplot2, scales, R6; optionally ggnewscale (shape.by = "both")
# ****************************************************************************************************

source('../jheem_analyses/applications/SHIELD/analysis/intervention/pretty_plots_style.R')


# ****************************************************************************************************
# 1. PICKING SIMSETS
# ****************************************************************************************************
# calib.simsets entries are named "<City> - <calibration.code>", so `locations` accepts city
# names ("Atlanta"), location codes ("C.12060"), or full entry names.
# ****************************************************************************************************

# Separator between city and calibration code in calib.simsets names.
# Character class covers hyphen, en dash and em dash, since the names use an en dash.
SHIELD.ENTRY.SEP <- " [-\u2013\u2014] "

## shield.parse.entry.names() ----
# Splits c("Atlanta - calib.8.21.stage3.az") into location and calibration code.
shield.parse.entry.names <- function(x) {
    pat <- paste0("^(.*?)", SHIELD.ENTRY.SEP, "(.*)$")
    data.frame(entry            = x,
               location         = sub(pat, "\\1", x, perl = TRUE),
               calibration.code = sub(pat, "\\2", x, perl = TRUE),
               stringsAsFactors = FALSE)
}

## shield.select.simsets() ----
# Pulls the simsets you asked for out of calib.simsets, in the order you asked for them.
#   locations         city names, location codes, or full entry names. NULL = everything.
#   calibration.code  filter when the list holds more than one calibration version.
#   field             which simset to take: "full_simset" (all sims) or "last_sim".
# Returns a named list of simsets, names = city names (used as panel titles / legend labels).
shield.select.simsets <- function(calib.simsets,
                                  locations        = NULL,
                                  calibration.code = NULL,
                                  field            = "full_simset")
{
    info <- shield.parse.entry.names(names(calib.simsets))
    
    if (!is.null(calibration.code))
        info <- info[info$calibration.code %in% calibration.code, , drop = FALSE]
    
    if (!is.null(locations)) {
        # translate any location CODES into city names, so both spellings work
        wanted <- vapply(locations, function(l) {
            if (exists("SHIELD.MSAS.OF.INTEREST") && l %in% SHIELD.MSAS.OF.INTEREST)
                names(SHIELD.MSAS.OF.INTEREST)[match(l, SHIELD.MSAS.OF.INTEREST)] else l
        }, character(1), USE.NAMES = FALSE)
        
        info <- info[info$location %in% wanted | info$entry %in% wanted, , drop = FALSE]
        info <- info[order(match(info$location, wanted)), , drop = FALSE]   # keep your order
    }
    
    if (nrow(info) == 0)
        stop("No matching simsets. Available entries:\n  ",
             paste(names(calib.simsets), collapse = "\n  "))
    
    out <- lapply(info$entry, function(e) calib.simsets[[e]][[field]])
    names(out) <- info$location
    out
}

## .shield.rbind() ----
# rbind a list of data frames that may be NULL or have different columns.
# Missing columns are filled with NA rather than dropping the row.
.shield.rbind <- function(lst) {
    lst <- Filter(function(x) !is.null(x) && nrow(x) > 0, lst)
    if (length(lst) == 0) return(NULL)
    
    cols <- unique(unlist(lapply(lst, names)))
    lst  <- lapply(lst, function(d) {
        for (m in setdiff(cols, names(d))) d[[m]] <- NA
        d[cols]
    })
    do.call(rbind, lst)
}

## .shield.as.simset.list() ----
# Accepts any of the three things you might have in hand and returns a named list of simsets:
#   1. calib.simsets      the list from load.calib.simsets(), named "<City> - <code>"
#   2. a named list       of simsets you assembled yourself
#   3. a single simset    (named by its location)
.shield.as.simset.list <- function(simsets, locations = NULL, calibration.code = NULL,
                                   field = "full_simset")
{
    # 3. one simset
    if (R6::is.R6(simsets)) {
        nm <- tryCatch(get.location.name(simsets$location), error = function(e) simsets$location)
        return(setNames(list(simsets), nm))
    }
    
    if (!is.list(simsets)) stop("'simsets' must be a simset, a list of simsets, or calib.simsets")
    
    # 1. calib.simsets: every entry is a list holding $full_simset / $last_sim
    is.calib <- all(vapply(simsets,
                           function(x) is.list(x) && !R6::is.R6(x) && !is.null(x[[field]]),
                           logical(1)))
    if (is.calib)
        return(shield.select.simsets(simsets, locations, calibration.code, field))
    
    # 2. a plain named list of simsets
    if (is.null(names(simsets)))
        names(simsets) <- vapply(simsets, function(s)
            tryCatch(get.location.name(s$location), error = function(e) s$location), character(1))
    if (!is.null(locations)) simsets <- simsets[names(simsets) %in% locations]
    if (length(simsets) == 0) stop("No simsets left after filtering by 'locations'")
    simsets
}


# ****************************************************************************************************
# 2. DATA LAYER  (pull the model and observed data into tidy tables)
# ****************************************************************************************************
# WHAT THIS IS
#   simplot() is three steps glued together:
#       plot.data.validation()  ->  prepare.plot()  ->  execute.simplot()
#   The first step is where the restrictions live (all simsets must share a location; unnamed
#   simsets become "<environment>"). The third is rendering, which we want to do ourselves.
#   The MIDDLE step is the valuable part, and jheem2 exports it: prepare.plot() pulls the
#   observed data through the data manager and returns tidy data frames.
#
#   shield.prepare() calls prepare.plot() once per city and stacks the results. That is all.
#   No ggplot_build() harvesting, no hand-rolled quantiles, no per-stratum loop.
#
# WHAT prepare.plot() DOES FOR US (and why we should not reimplement it)
#   * finds the observed outcome that corresponds to a model outcome, from the sim's own
#     outcome.metadata$corresponding.observed.outcome
#   * pulls it with target.ontology = the sim's ontology and mapping allowed, which is what
#     reconciles model categories (msm / heterosexual_male) with each data source's categories
#   * multiplies BOTH sim and data by 100 when outcome.metadata$display.as.percent is TRUE,
#     so proportions show as 0-100 on both
#   * applies omit.data.years, twice (before the pull and after the melt)
#   * summarises sims into value.lower / value.median / value.upper when summary.type asks
#
# WHAT COMES BACK
#   $sim   one row per year x stratum x facet x simset (x sim, if individual.simulation)
#          columns include: year, value, value.lower, value.upper, stratum, facet.by1..n,
#          simset, outcome, outcome.display.name, city
#   $data  one row per observed point
#          columns include: year, location, source, value, stratum, facet.by1..n, outcome, city
#   $meta  y.label, outcome and source metadata, and the arguments used
#
# TWO COLUMNS THAT ARE NOT THE SAME THING
#   city      added by us: which simset (which MSA) the row belongs to. Use this for plotting.
#   location  comes from the data manager: the location the OBSERVED value was reported for.
#             It is often the MSA, but can be the state (e.g. "GA" for Atlanta) when that is
#             what the source publishes. Keep both -- `city` for grouping, `location` when you
#             need to know what geography a data point actually describes.
# ****************************************************************************************************

## shield.prepare() ----
# THE DATA LAYER. Returns tidy frames; draws nothing.
#   simsets           calib.simsets, a named list of simsets, or one simset
#   outcomes          one or more model outcomes
#   locations         which cities (names, codes, or entry names); NULL = all
#   calibration.code  filter when calib.simsets holds several versions
#   split.by          one dimension, kept in the data as the `stratum` column
#   facet.by          any number of dimensions, kept as facet.by1 .. facet.byN
#   window            time window (pretty_plots_style.R, section 1); supplies the years
#                     unless you pass your own
#   summary.type      "median.and.interval" gives value.lower/median/upper;
#                     "individual.simulation" gives one row per sim
#   show.data         FALSE skips the observed-data pull entirely (plot.which = "sim.only")
#   omit.data.years   years whose observed data are dropped (model output unaffected)
#   field             "full_simset" or "last_sim"
#   ...               passed through to prepare.plot() -- data.manager, target.ontology,
#                     dimension.values, dimension.values.post.mapping, data.locations, ...
shield.prepare <- function(simsets,
                           outcomes,
                           locations        = NULL,
                           calibration.code = NULL,
                           split.by         = NULL,
                           facet.by         = NULL,
                           window           = SHIELD.DEFAULT.WINDOW,
                           summary.type     = "median.and.interval",
                           show.data        = TRUE,
                           omit.data.years  = SHIELD.OMIT.DATA.YEARS,
                           field            = "full_simset",
                           ...)
{
    tw   <- shield.time(window)
    sims <- .shield.as.simset.list(simsets, locations, calibration.code, field)
    
    dots <- list(...)
    dv   <- dots$dimension.values
    dots$dimension.values <- NULL
    if (is.null(dv))      dv <- list()
    if (is.null(dv$year)) dv$year <- tw$years        # window supplies years unless you set them
    
    # One prepare.plot() call per city. Calling it once for all cities would pool the data
    # locations, and a row reported for "GA" could then not be traced back to a single MSA.
    parts <- lapply(names(sims), function(loc) {
        args <- c(list(simset.list      = setNames(list(sims[[loc]]), loc),
                       outcomes         = outcomes,
                       split.by         = split.by,
                       facet.by         = facet.by,
                       dimension.values = dv,
                       plot.which       = if (show.data) "sim.and.data" else "sim.only",
                       summary.type     = summary.type,
                       omit.data.years  = omit.data.years,
                       title            = loc),
                  dots)
        
        pp <- tryCatch(do.call(prepare.plot, args),
                       error = function(e) {
                           warning("prepare.plot() failed for ", loc, ": ", conditionMessage(e))
                           NULL
                       })
        if (is.null(pp)) return(NULL)
        
        if (!is.null(pp$df.sim))   pp$df.sim$city   <- loc
        if (!is.null(pp$df.truth)) pp$df.truth$city <- loc
        pp
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) stop("prepare.plot() returned nothing for any location.")
    
    sim  <- .shield.rbind(lapply(parts, function(x) x$df.sim))
    dat  <- .shield.rbind(lapply(parts, function(x) x$df.truth))
    
    # city as a factor in the order requested, so colours and legends stay stable
    if (!is.null(sim)) sim$city <- factor(sim$city, levels = names(sims))
    if (!is.null(dat)) dat$city <- factor(dat$city, levels = names(sims))
    
    if (show.data && is.null(dat))
        message("No observed data came back for these outcomes/locations.")
    
    list(sim  = sim,
         data = dat,
         meta = list(y.label          = parts[[1]]$details$y.label,
                     plot.title       = parts[[1]]$details$plot.title,
                     outcome.metadata = parts[[1]]$details$outcome.metadata.list,
                     source.metadata  = do.call(c, lapply(parts, function(x)
                         x$details$source.metadata.list)),
                     outcomes         = outcomes,
                     split.by         = split.by,
                     facet.by         = facet.by,
                     window           = window,
                     summary.type     = summary.type,
                     cities           = names(sims)))
}

## shield.inspect() ----
# Prints a compact summary of what shield.prepare() returned. Use this to check the data
# layer before trusting any figure built on it.
shield.inspect <- function(prepared) {
    cat("== SIM ==\n")
    if (is.null(prepared$sim)) cat("  (none)\n") else {
        cat("  rows:", nrow(prepared$sim), " columns:", paste(names(prepared$sim), collapse=", "), "\n")
        cat("  years:", min(prepared$sim$year), "-", max(prepared$sim$year), "\n")
        cat("  value range:", signif(range(prepared$sim$value, na.rm = TRUE), 4), "\n")
        cat("  rows per city:\n"); print(table(prepared$sim$city))
        if ("stratum" %in% names(prepared$sim))
            cat("  strata:", paste(unique(prepared$sim$stratum), collapse=" | "), "\n")
    }
    
    cat("== DATA ==\n")
    if (is.null(prepared$data)) cat("  (none)\n") else {
        cat("  rows:", nrow(prepared$data), " columns:", paste(names(prepared$data), collapse=", "), "\n")
        cat("  years:", min(prepared$data$year), "-", max(prepared$data$year), "\n")
        cat("  value range:", signif(range(prepared$data$value, na.rm = TRUE), 4), "\n")
        cat("  sources:", paste(unique(prepared$data$source), collapse=", "), "\n")
        cat("  data locations:", paste(unique(prepared$data$location), collapse=", "), "\n")
        cat("  rows per city:\n"); print(table(prepared$data$city))
        if ("stratum" %in% names(prepared$data))
            cat("  strata:", paste(unique(prepared$data$stratum), collapse=" | "), "\n")
    }
    
    cat("== META ==\n")
    cat("  y.label:", prepared$meta$y.label, "\n")
    cat("  split.by:", if (is.null(prepared$meta$split.by)) "(none)" else prepared$meta$split.by, "\n")
    cat("  facet.by:", if (is.null(prepared$meta$facet.by)) "(none)" else
        paste(prepared$meta$facet.by, collapse=", "), "\n")
    invisible(prepared)
}

# ---- CHECKING THE DATA LAYER --------------------------------------------------------
#   pp <- shield.prepare(calib.simsets, outcomes = "diagnosis.ps",
#                        locations = c("Atlanta", "Baltimore"), split.by = "sex")
#   shield.inspect(pp)
#
# What to confirm:
#   1. $data has rows for both cities, and its value range is the SAME order of magnitude
#      as $sim
#   2. $data$stratum shows the sex categories, matching $sim$stratum
#   3. $data$location tells you whether a source is MSA-level or state-level
#   4. $sim has value.lower / value.median / value.upper columns
# ****************************************************************************************************


# ****************************************************************************************************
# 3. THE PLOT  (shield.plot(), built on the tidy tables from section 2)
# ****************************************************************************************************
# ONE RULE DECIDES THE WHOLE FIGURE:
#
#   | locations | overlay.locations | colour is | linetype is | panels are        |
#   |-----------|-------------------|-----------|-------------|-------------------|
#   | 1         | n/a               | split.by  | --          | facet.by          |
#   | many      | TRUE  (default)   | city      | split.by    | facet.by          |
#   | many      | FALSE             | city      | split.by    | city + facet.by   |
#
# A city therefore keeps ONE colour everywhere -- overlaid or in its own panel, split or
# not -- so the same city reads the same across a multi-panel manuscript figure. Use
# colour.by = "split" to get the older behaviour where the split owned the colour.
#
# So overlay.locations answers one question -- does the city get a colour or a panel? --
# and everything else follows from it. Nothing else in your call changes.
#
# A NOTE ON STRATA (worth knowing before you read a split figure):
#   With show.data = TRUE the model is mapped into the DATA's categories, so split.by = "sex"
#   gives Male / Female. To see the model's own categories (msm / heterosexual_male / female),
#   use show.data = FALSE. This is jheem2's behaviour, not ours: prepare.plot() applies the
#   data ontology mapping to the sim so the two can be plotted on the same axis.
# ****************************************************************************************************

## .shield.source.labels() ----
# Data sources arrive as ids like "cdc.sti.surveillance.reports". The data manager usually
# carries a readable name for each in source.info; use it when present, and otherwise tidy
# the id into something printable rather than showing the raw string.
.shield.source.labels <- function(src, source.metadata = NULL, overrides = NULL) {
    # Precedence: this call's overrides, then SHIELD.SOURCE.LABELS, then whatever name the
    # data manager carries, then a tidied version of the id itself.
    lookup <- c(overrides, SHIELD.SOURCE.LABELS)
    lookup <- lookup[!duplicated(names(lookup))]

    vapply(as.character(src), function(one) {
        if (one %in% names(lookup)) return(unname(lookup[one]))
        info <- source.metadata[[one]]
        # FULL name first. The data manager usually carries both, and short.name is the
        # abbreviation ("cdc aggd county") that is unreadable in a manuscript legend.
        for (f in c("full.name", "display.name", "name", "long.name", "short.name"))
            if (!is.null(info[[f]]) && is.character(info[[f]])) return(info[[f]][1])
        gsub("\\.", " ", one)          # fallback: "cdc.aggregated.county" -> "cdc aggregated county"
    }, character(1), USE.NAMES = FALSE)
}

## shield.source.names() ----
# Prints every name the data manager holds for each source in a prepared object, so you can
# see what is on offer before deciding what the legend should say. Use it when a legend
# shows an abbreviation you want to replace.
shield.source.names <- function(prepared) {
    meta <- prepared$meta$source.metadata
    ids  <- unique(as.character(prepared$data$source))
    for (one in ids) {
        cat("\n", one, "\n", sep = "")
        info <- meta[[one]]
        if (is.null(info)) { cat("  (no metadata held for this source)\n"); next }
        fields <- Filter(function(f) is.character(info[[f]]) && length(info[[f]]) == 1,
                         names(info))
        if (length(fields) == 0) cat("  (no character fields)\n")
        for (f in fields) cat("  ", f, ": ", info[[f]], "\n", sep = "")
    }
    cat("\nUsed for the legend, in order: full.name, display.name, name, long.name,",
        "short.name\nOverride with SHIELD.SOURCE.LABELS or source.labels =\n")
    invisible(ids)
}

## shield.plot() ----
# THE ONE PLOTTING FUNCTION.
#   simsets/outcomes/locations/... see shield.prepare() in section 2
#   overlay.locations  TRUE = cities share a panel; FALSE = one panel per city. It changes
#                      the LAYOUT only -- colour stays with the city either way
#   colour.by          "auto" (default), "location", or "split" -- see the table above
#   show.sources       TRUE draws each observed point with a shape per DATA SOURCE, in its
#                      own "Data source" legend. Use it whenever a city has points from more
#                      than one source and you need to see which is which
#   sources            restrict the observed data to these source ids (see shield.inspect())
#   shape.by           "both" shows source AND reporting area at once: ring colour = city,
#                      fill shade = source, shape = area, with three separate legends
#                      (needs the ggnewscale package). Otherwise what the SHAPES mean:
#                      "source", "location" (the geography the
#                      value was reported for -- an MSA spanning states reports per state),
#                      "source.location", "stratum", "none", or "auto" (default)
#   source.labels      rename sources for THIS figure only, e.g.
#                        c(lhd = "Local health department"). For a name you want everywhere,
#                        edit SHIELD.SOURCE.LABELS in pretty_plots_style.R instead.
#                        Renaming is display-only -- `sources` still filters on the raw ids
#   source.legend      FALSE keeps the per-source shapes but hides the "Data source" legend.
#                      Use it when the points only need to look distinguishable, or when a
#                      neighbouring plot in an assembled figure already names the sources
#   log.y              TRUE puts the y axis on a log10 scale. Non-positive values are dropped
#                      (with a warning), ", log scale" is appended to the y-axis label, and
#                      minor gridlines are kept so the spacing stays readable
#   free.y             TRUE lets each panel scale its own y axis. Default FALSE so panels are
#                      comparable -- usually what you want when the panels are cities
#   title              the figure title. Default: the outcome's title from SHIELD.OUTCOMES
#   titles             a named lookup from level -> label, e.g.
#                        c(Atlanta = "Atlanta-Sandy Springs MSA", Male = "Men")
#                      Applied to panel strip labels when that variable is faceted, and to
#                      legend entries when it is the colour. Unnamed levels keep their text
#   panel.tags         TRUE prefixes "(a) ", "(b) " ... to the first panel variable's strips
#   legend             "none" / "bottom" / "right". NULL (default) shows the legend when it
#                      carries information. Set "none" on all but one plot when you are
#                      assembling several into one figure with patchwork
#   legend.titles      TRUE titles each legend ("Location", "Sex", "Data source") and stacks
#                      them vertically; FALSE never titles. Defaults to SHIELD.LEGEND.TITLES,
#                      which is NULL = decide per plot (title only when a plot has more than
#                      one legend). Set SHIELD.LEGEND.TITLES <- TRUE for a consistent look
#                      across an assembled multi-panel figure
#   prepared           reuse an object from shield.prepare() instead of pulling again
#   ...                passed to shield.prepare(), and on to prepare.plot()
shield.plot <- function(simsets           = NULL,
                        outcomes          = NULL,
                        locations         = NULL,
                        calibration.code  = NULL,
                        split.by          = NULL,
                        facet.by          = NULL,
                        overlay.locations = TRUE,
                        colour.by         = c("auto", "location", "split"),
                        log.y             = FALSE,
                        free.y            = FALSE,
                        window            = SHIELD.DEFAULT.WINDOW,
                        summary.type      = "median.and.interval",
                        show.data         = TRUE,
                        show.sources      = FALSE,
                        sources           = NULL,
                        source.legend     = TRUE,
                        source.labels     = NULL,
                        shape.by          = c("auto", "source", "location",
                                              "source.location", "both",
                                              "stratum", "none"),
                        omit.data.years   = SHIELD.OMIT.DATA.YEARS,
                        title             = NULL,
                        titles            = NULL,
                        panel.tags        = FALSE,
                        legend.titles     = SHIELD.LEGEND.TITLES,
                        legend            = NULL,
                        base.size         = 11,
                        ncol              = NULL,
                        show.proj.line    = TRUE,
                        field             = "full_simset",
                        prepared          = NULL,
                        ...)
{
    # ---- 1. GET THE DATA -------------------------------------------------------------
    if (is.null(prepared))
        prepared <- shield.prepare(simsets, outcomes, locations, calibration.code,
                                   split.by = split.by, facet.by = facet.by,
                                   window = window, summary.type = summary.type,
                                   show.data = show.data, omit.data.years = omit.data.years,
                                   field = field, ...)
    
    sim <- prepared$sim
    dat <- prepared$data
    if (is.null(outcomes)) outcomes <- prepared$meta$outcomes
    if (is.null(split.by)) split.by <- prepared$meta$split.by
    if (is.null(facet.by)) facet.by <- prepared$meta$facet.by

    # RELABELLING. `titles` is one lookup from a level's value to the label you want:
    #   titles = c(Atlanta = "Atlanta-Sandy Springs MSA", Male = "Men")
    # It is applied wherever that level appears -- as a panel strip when the variable is
    # faceted, or as a legend entry when it is the colour. Levels you do not name keep
    # their own text, so you can rename one panel and leave the rest alone.
    relabel <- function(x) {
        x <- as.character(x)
        if (is.null(titles) || is.null(names(titles))) return(x)
        hit <- match(x, names(titles))
        ifelse(is.na(hit), x, unname(titles[hit]))
    }
    
    # jheem2 converts year to numeric for the data frame but not for the sim frame
    sim$year <- as.numeric(as.character(sim$year))
    if (!is.null(dat)) dat$year <- as.numeric(as.character(dat$year))

    # Keep only the data sources asked for, and give each one a readable label.
    if (!is.null(dat) && !is.null(sources)) {
        dat <- dat[dat$source %in% sources, , drop = FALSE]
        if (nrow(dat) == 0) {
            warning("No observed data left after filtering to sources: ",
                    paste(sources, collapse = ", "))
            dat <- NULL
        }
    }
    if (!is.null(dat))
        dat$source.label <- .shield.source.labels(dat$source, prepared$meta$source.metadata,
                                                  source.labels)
    
    # Rename panels if asked (simplest way: relabel the factor before plotting)
    if (!is.null(titles) && !is.null(names(titles))) {
        hit <- match(levels(sim$city), names(titles))
        levels(sim$city)[!is.na(hit)] <- titles[hit[!is.na(hit)]]
        if (!is.null(dat)) levels(dat$city) <- levels(sim$city)
    }
    
    n.cities <- nlevels(sim$city)
    has.split <- !is.null(split.by) && "stratum" %in% names(sim) && any(sim$stratum != "")
    
    # ---- 2. DECIDE THE AESTHETICS (the table at the top of this section) --------------
    overlay <- n.cities > 1 && overlay.locations
    
    # COLOUR IS THE CITY WHENEVER THERE IS MORE THAN ONE, so a city keeps the same colour
    # across every panel and every figure in a multi-panel manuscript layout. A split then
    # goes to line type. overlay.locations only decides whether the cities share a panel.
    #   colour.by = "auto"      the rule above (default)
    #   colour.by = "location"  force colour = city
    #   colour.by = "split"     force colour = the split, the way it worked before
    colour.by  <- match.arg(colour.by)
    shape.by   <- match.arg(shape.by)
    colour.var <- switch(colour.by,
                         location = "city",
                         split    = if (has.split) "stratum" else "city",
                         auto     = if (n.cities > 1) "city" else
                                    if (has.split) "stratum" else "city")

    linetype.var <- if (identical(colour.var, "city") && has.split) "stratum" else NULL
    
    panel.vars <- character(0)
    if (n.cities > 1 && !overlay)  panel.vars <- c(panel.vars, "city")
    if (length(outcomes) > 1)      panel.vars <- c(panel.vars, "outcome.display.name")
    panel.vars <- c(panel.vars, grep("^facet\\.by[0-9]+$", names(sim), value = TRUE))

    # A variable is either a panel or a legend, never both. Panels are relabelled by the
    # labeller in section 6; a legend is relabelled here.
    if (!("city" %in% panel.vars) && !is.null(titles)) {
        levels(sim$city) <- relabel(levels(sim$city))
        if (!is.null(dat)) levels(dat$city) <- levels(sim$city)
    }

    # PANEL TAGS: prefix "(a) ", "(b) " ... to the FIRST panel variable, for manuscripts
    # that refer to sub-panels by letter. Applied to the data so the letters follow the
    # order the panels are actually drawn in.
    tags.applied <- FALSE
    if (isTRUE(panel.tags) && length(panel.vars) > 0) {
        v      <- panel.vars[1]
        lv     <- levels(factor(sim[[v]]))
        tagged <- paste0("(", letters[seq_along(lv)], ") ", relabel(lv))
        sim[[v]] <- factor(relabel(sim[[v]]), levels = relabel(lv), labels = tagged)
        if (!is.null(dat) && v %in% names(dat))
            dat[[v]] <- factor(relabel(dat[[v]]), levels = relabel(lv), labels = tagged)
        tags.applied <- TRUE
    }
    
    # Readability guards -- warn, do not block
    n.colour <- length(unique(sim[[colour.var]]))
    if (n.colour > length(SHIELD.PALETTE))
        warning(n.colour, " colour levels but only ", length(SHIELD.PALETTE),
                " distinct colours. Consider overlay.locations = FALSE.")
    if (!is.null(linetype.var) && length(unique(sim$stratum)) > 4)
        warning(length(unique(sim$stratum)), " linetypes; more than ~4 is hard to tell apart. ",
                "Consider colour.by = \"split\", which puts the split on colour instead.")
    
    # ---- 3. LOG SCALE ----------------------------------------------------------------
    if (log.y) {
        dropped <- sum(sim$value <= 0, na.rm = TRUE) +
            if (is.null(dat)) 0 else sum(dat$value <= 0, na.rm = TRUE)
        if (dropped > 0)
            warning(dropped, " non-positive value(s) dropped: they cannot be shown on a log axis.")
        sim <- sim[!is.na(sim$value) & sim$value > 0, , drop = FALSE]
        if (!is.null(dat)) dat <- dat[!is.na(dat$value) & dat$value > 0, , drop = FALSE]
        # A ribbon whose lower edge is <= 0 would vanish; pull it up to the smallest
        # positive value we are plotting
        if (!is.null(sim$value.lower)) {
            floor.value <- min(sim$value, na.rm = TRUE)
            sim$value.lower[!is.na(sim$value.lower) & sim$value.lower <= 0] <- floor.value
        }
    }
    
    # ---- 4. COLOURS ------------------------------------------------------------------
    colour.levels <- levels(factor(sim[[colour.var]]))
    pal <- rep(SHIELD.PALETTE, length.out = length(colour.levels))
    names(pal) <- colour.levels
    
    # ---- 5. BUILD THE PLOT -----------------------------------------------------------
    has.interval <- all(c("value.lower", "value.upper") %in% names(sim))
    
    # ONE SERIES = ONE GROUP. Both the line and the ribbon must be told this explicitly.
    # Left to itself a ribbon groups by fill (the city), so with two strata per city it
    # joins the Male and Female edges and comes out as a zigzag. Sorting by year within
    # each group matters for the same reason: a ribbon is drawn in row order.
    sim$grp <- if (is.null(linetype.var)) as.character(sim[[colour.var]])
    else paste(sim$city, sim$stratum, sep = " | ")
    if (identical(summary.type, "individual.simulation") && "sim" %in% names(sim))
        sim$grp <- paste(sim$grp, sim$sim, sep = " | ")
    sim <- sim[order(sim$grp, sim$year), , drop = FALSE]
    
    p <- ggplot(sim, aes(x = year, y = value, group = grp,
                         colour = .data[[colour.var]], fill = .data[[colour.var]]))
    
    if (has.interval)
        p <- p + geom_ribbon(aes(ymin = value.lower, ymax = value.upper),
                             alpha = 0.15, colour = NA)
    
    if (is.null(linetype.var))
        p <- p + geom_line(linewidth = 0.8)
    else
        p <- p + geom_line(aes(linetype = .data[[linetype.var]]), linewidth = 0.8)
    
    # Observed data. Shape is the one channel points have spare, and it can encode only
    # ONE thing, so:
    #   show.sources = TRUE  -> shape = data source (its own "Data source" legend)
    #   split on linetype    -> shape = stratum, since the points cannot carry a linetype
    #   neither              -> plain filled circles
    # If both apply, source wins (the strata are still readable from the line types) and
    # a message says so.
    # WHAT THE POINT SHAPES MEAN. Shape is the one channel a point has spare, so it can
    # encode exactly one thing. shape.by chooses which:
    #   "source"           the reporting source (CDC report, local health department, ...)
    #   "location"         the geography the value was REPORTED for. Not the same as the
    #                      city: an MSA spanning several states can have its data reported
    #                      per state, so Chicago-Naperville carries IL, IN and WI values
    #                      that are distinct observations, not duplicates
    #   "source.location"  both together, for when they cross
    #   "stratum"          the split (used automatically when the split is on line type)
    #   "none"             plain filled circles
    #   "auto" (default)   source if show.sources = TRUE, else stratum if the split is on
    #                      line type, else none
    shape.var   <- NULL
    shape.title <- NULL
    shape.from.data <- FALSE          # TRUE when the shape describes the data, not the model
    scales.added    <- FALSE          # TRUE once the city colour/fill scales are placed

    if (!is.null(dat) && nrow(dat) > 0) {
        use <- if (shape.by != "auto") shape.by
               else if (show.sources) "source"
               else if (!is.null(linetype.var)) "stratum" else "none"

        if (use == "source") {
            dat$shape.key <- dat$source.label
            shape.var <- "shape.key"; shape.title <- "Data source"; shape.from.data <- TRUE
        } else if (use == "location") {
            dat$shape.key <- as.character(dat$location)
            shape.var <- "shape.key"; shape.title <- "Reported for"; shape.from.data <- TRUE
        } else if (use == "source.location") {
            dat$shape.key <- paste(dat$source.label, dat$location, sep = " \u2013 ")
            shape.var <- "shape.key"; shape.title <- "Data source and area"; shape.from.data <- TRUE
        } else if (use == "stratum") {
            shape.var <- "stratum"; shape.title <- NULL
        }

        if (!is.null(shape.var) && use != "stratum" && !is.null(linetype.var))
            message("Points show ", tolower(shape.title), "; strata are shown by line type.")

        n.shapes <- if (is.null(shape.var)) 0 else length(unique(dat[[shape.var]]))
        if (n.shapes > 5)
            warning(n.shapes, " shape levels but only 5 fillable shapes exist; they will ",
                    "repeat. Consider shape.by = \"source\" or \"location\" alone.")

        # "both": the point carries THREE encodings at once, each with its own legend --
        # ring colour = city (shares the line scale), fill shade = data source, shape =
        # the area it was reported for. Needs ggnewscale, because the ribbon fill scale
        # is already spent on the city and a second fill scale has to be started.
        if (use == "both") {
            if (!requireNamespace("ggnewscale", quietly = TRUE)) {
                warning("shape.by = 'both' needs the ggnewscale package; ",
                        "falling back to shape.by = 'source.location'.")
                use <- "source.location"
                dat$shape.key <- paste(dat$source.label, dat$location, sep = " \u2013 ")
                shape.var <- "shape.key"; shape.title <- "Data source and area"
            }
        }

        if (identical(use, "both")) {
            areas   <- sort(unique(as.character(dat$location)))
            srcs    <- sort(unique(as.character(dat$source.label)))
            # light -> dark, so more than two sources stay separable in print
            shades  <- rep(c("white", "grey60", "grey30", "black", "grey80"),
                           length.out = length(srcs))
            names(shades) <- srcs

            if (length(areas) > 5)
                warning(length(areas), " reporting areas but only 5 fillable shapes exist.")

            p <- p +
                scale_color_manual(values = pal) +
                # guide = "none": the ribbon fill repeats what the line colour already says,
                # and being the FIRST of two fill scales it would otherwise appear under its
                # raw variable name ("city") in a legend of its own
                scale_fill_manual(values = pal, guide = "none") +
                ggnewscale::new_scale_fill() +
                geom_point(data = dat,
                           aes(x = year, y = value,
                               colour = .data[[colour.var]],
                               fill   = source.label,
                               shape  = location),
                           inherit.aes = FALSE,
                           show.legend = c(shape = TRUE, fill = TRUE, colour = FALSE),
                           size = 2.2, stroke = 0.7) +
                scale_fill_manual(values = shades, name = "Data source") +
                scale_shape_manual(values = rep(c(21, 22, 24, 23, 25),
                                                length.out = length(areas)),
                                   name = "Reported for") +
                guides(fill  = guide_legend(override.aes = list(shape = 21, colour = "grey30")),
                       shape = guide_legend(override.aes = list(fill = "grey40")))

            # Record the shape channel so the legend bookkeeping downstream (titles,
            # vertical stacking, source.legend) sees it. The branch chain has already been
            # taken, so this cannot re-enter the plain/shaped point layers below.
            shape.var       <- "location"
            shape.title     <- "Reported for"
            shape.from.data <- TRUE
            scales.added    <- TRUE
        } else if (is.null(shape.var))
            p <- p + geom_point(data = dat, aes(x = year, y = value, fill = .data[[colour.var]]),
                                inherit.aes = FALSE, show.legend = FALSE,
                                shape = 21, colour = "white", size = 1.9, stroke = 0.4)
        else
            p <- p + geom_point(data = dat,
                                aes(x = year, y = value, fill = .data[[colour.var]],
                                    shape = .data[[shape.var]]),
                                inherit.aes = FALSE,
                                # points belong in the shape legend only; drawing them into
                                # the colour key puts a white marker through the line and
                                # makes a solid series look dashed
                                show.legend = c(shape = TRUE, fill = FALSE, colour = FALSE),
                                colour = "white", size = 2.1, stroke = 0.4) +
                # 21-25 are the fillable shapes, so the city fill still shows through
                scale_shape_manual(values = rep(c(21, 22, 24, 23, 25), length.out = n.shapes),
                                   name = shape.title) +
                guides(shape = guide_legend(override.aes = list(fill = "grey40")))
    }


    # DRAWING the sources and NAMING them are separate choices. You usually want the points
    # to look different from each other; you do not always want to spend legend space saying
    # which is which. source.legend = FALSE keeps the shapes and drops the legend.
    shape.in.legend <- !is.null(shape.var)
    if (shape.from.data && !isTRUE(source.legend)) {
        p <- p + guides(shape = "none")
        shape.in.legend <- FALSE
    }

    # ---- 6. SCALES, PANELS, LABELS ---------------------------------------------------
    if (!scales.added) p <- p + scale_color_manual(values = pal) + scale_fill_manual(values = pal)

    p <- p +
        shield.x.scale(window)
    
    # Log axis: ticks at powers of ten (plus intermediates), and keep the minor gridlines,
    # which on a log scale carry real information about spacing.
    if (log.y)
        p <- p + scale_y_log10(labels = label_comma(), breaks = breaks_log(n = 6)) +
            theme(panel.grid.minor.y = element_line(colour = "grey92", linewidth = 0.2))
    else
        p <- p + shield.y.scale(outcomes[1])
    
    if (length(panel.vars) > 0)
        p <- p + facet_wrap(as.formula(paste("~", paste(panel.vars, collapse = " + "))),
                            scales = if (free.y) "free_y" else "fixed",
                            labeller = if (tags.applied) label_value
                                       else as_labeller(relabel),
                            ncol = ncol)
    
    # When the panels are cities and the colour is the city, the colour legend repeats the
    # strip labels, so drop it and keep only the legends that add something.
    redundant.colour <- identical(colour.var, "city") && "city" %in% panel.vars
    if (redundant.colour) p <- p + guides(colour = "none", fill = "none")

    show.legend <- (!redundant.colour && length(colour.levels) > 1) ||
                   !is.null(linetype.var) || shape.in.legend

    # Where the legend goes. NULL decides for you: show it when it carries information,
    # hide it when it does not. Pass "none" / "bottom" / "right" to take control -- which
    # is what you want when assembling several plots and only one should carry the legend.
    legend.pos <- if (!is.null(legend)) legend else if (show.legend) "bottom" else "none"

    # LEGEND TITLES. A figure can carry up to three legends at once -- colour, line type
    # and data source. Unlabelled they read as one undifferentiated strip of keys, and a
    # long one runs off the panel width. So when there is more than one legend, title each
    # of them and stack the boxes vertically. legend.titles = TRUE/FALSE overrides.
    nice <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
    colour.name <- if (identical(colour.var, "city")) "Location" else nice(split.by)

    n.legends <- sum(!redundant.colour && length(colour.levels) > 1,
                     !is.null(linetype.var), shape.in.legend)
    if (is.null(legend.titles)) legend.titles <- n.legends > 1

    if (isTRUE(legend.titles)) {
        p <- p + labs(colour = colour.name, fill = colour.name)
        if (!is.null(linetype.var)) p <- p + labs(linetype = nice(split.by))
    }
    
    # The y label states the units, and says so when those units sit on a log axis --
    # otherwise a reader has to infer the transform from the tick spacing.
    y.lab <- if (length(outcomes) == 1) shield.y.lab(outcomes) else prepared$meta$y.label
    if (log.y) y.lab <- paste0(y.lab, ", log scale")

    p <- p +
        theme.shield(base.size = base.size,
                     legend = legend.pos,
                     strips = length(panel.vars) > 0) +
        # Stack the legend boxes rather than running them along one line, which is what
        # truncates a long one, and show titles only when they were asked for.
        theme(legend.box   = if (n.legends > 1) "vertical" else "horizontal",
              legend.title = if (isTRUE(legend.titles))
                  element_text(size = base.size - 1, face = "bold") else element_blank()) +
        labs(title = if (!is.null(title)) title else
            if (length(outcomes) == 1) shield.title(outcomes) else NULL,
            y = y.lab,
            x = NULL)
    
    if (show.proj.line) p <- p + shield.proj.line(window)
    
    p
}

# ---- EXAMPLES ------------------------------------------------------------------------

if (1==2){
    # 1) One city, split by sex          colour = sex
    shield.plot(calib.simsets, "diagnosis.ps", locations = "Atlanta")
    shield.plot(calib.simsets, "diagnosis.ps", locations = "Atlanta",split.by = "sex")
    shield.plot(calib.simsets, "diagnosis.ps", locations = "Atlanta",split.by = "race")
    shield.plot(calib.simsets, "diagnosis.ps", locations = "Atlanta",facet.by = "race")
    #
    # 2) Two cities overlaid              colour = city
    shield.plot(calib.simsets, "diagnosis.ps", locations = c("Atlanta", "Baltimore"))

    # 3) Two cities AND a sex split       colour = city, linetype = sex, one panel
    shield.plot(calib.simsets, "diagnosis.ps", locations = c("Atlanta", "Baltimore"),
                split.by = "sex")

    # 4) Ten cities, one panel each       colour = sex, panels = city
    shield.plot(calib.simsets, "diagnosis.ps", split.by = "sex",
                overlay.locations = FALSE, ncol = 5)

    # 5) Model's own categories (MSM visible), log axis
      shield.plot(calib.simsets, "diagnosis.ps", locations = "Atlanta",
                  split.by = "sex", show.data = FALSE, log.y = TRUE)

    # 6) Two-way breakdown: colour = race, panels = city x sex
      shield.plot(calib.simsets, "diagnosis.ps", split.by = "race", facet.by = "sex",
                  overlay.locations = FALSE)

    # 7) Reuse one data pull for several figures
      pp <- shield.prepare(calib.simsets, "diagnosis.ps", split.by = "sex")
      shield.plot(prepared = pp)
      shield.plot(prepared = pp, log.y = TRUE, overlay.locations = FALSE)
      
      
      }

