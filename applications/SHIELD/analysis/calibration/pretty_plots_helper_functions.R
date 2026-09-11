# ****************************************************************************************************
# SHIELD PLOTTING FRAMEWORK
# ****************************************************************************************************
# WHAT THIS FILE IS
#   One place that defines *how* SHIELD figures look. It defines objects and functions ONLY --
#   sourcing it draws nothing and writes nothing to disk.
#
#   Any figure script (calibration, manuscript, future papers) should start with:
#       source('.../SHIELD/analysis/calibration/pretty_plots.R')
#   and then build plots from the pieces below, instead of retyping sizes, colors,
#   themes and axis labels inside every plot call.
#
# THE SIX PIECES  (in the order they appear below)
#   1. TIME      which years a figure covers, and how the x axis is drawn
#   2. COLOR     one colorblind-safe palette, plus named color "roles" built on it
#   3. THEME     fonts, text sizes, legend position, gridlines            -> theme.shield()
#   4. STYLE     how simplot() maps sims and data to color/linetype/alpha -> shield.style()
#   5. OUTCOMES  a lookup table of title + y-axis label per outcome
#   6. OUTPUT    canonical figure sizes, and one save function            -> save.shield.fig()
#   7. WRAPPER   the one function that combines all of the above          -> shield.trend.plot()
#
# THE TWO CONCEPTS THAT ARE EASY TO CONFUSE
#   THEME  = plain ggplot2. Controls the "furniture" of the plot: text size, legend
#            position, panel background, gridlines. Knows nothing about your model.
#   STYLE  = jheem2's style manager. Controls the "ink" of the plot: which model
#            quantity gets which color / linetype / transparency. It is needed because
#            simplot() builds its own aesthetic mapping internally, so ordinary
#            scale_color_manual() calls cannot reach those series.
#   Rule of thumb: if the setting is about *text and boxes*, it is a THEME setting.
#   If it is about *what the lines mean*, it is a STYLE setting.
#
# DEPENDENCIES: ggplot2, scales, jheem2 (simplot, create.style.manager),
#               optionally ragg (better text rendering when saving PNGs).
# ****************************************************************************************************

library(ggplot2)
library(scales)


# ****************************************************************************************************
# 1. TIME  ("the time manager")
# ****************************************************************************************************
# A time WINDOW bundles the three things that must always agree with each other:
#   $years     integer vector handed to simplot(dimension.values = list(year = ...))
#   $breaks    which years get an x-axis tick. Fewer ticks = readable small panels.
#   $data.end  last calendar year with observed surveillance data. Everything after
#              it is a projection, so this is where the dashed vertical line is drawn.
#
# Why a list of windows instead of one YEARS constant: the same figure code can be
# re-rendered for a different paper by changing one string ("manuscript" -> "long.projection"),
# with no edits inside the plotting functions.
# ****************************************************************************************************

# Last year of observed data feeding the SHIELD likelihood. CHECK THIS against the
# data manager before submission; it moves the projection line in every figure.
SHIELD.DATA.END.YEAR <- 2021

# Years whose observed data are suppressed from figures (COVID-era surveillance artifact).
# Passed to simplot(omit.data.years = ). Set to NULL to show everything.
SHIELD.OMIT.DATA.YEARS <- 2022:2025

SHIELD.TIME <- list(
    
    # DEFAULT FOR THIS MANUSCRIPT: calibration fit + baseline projection in one panel.
    manuscript = list(years    = 2010:2030,
                      breaks   = c(2010, 2015, 2020, 2025, 2030),
                      data.end = SHIELD.DATA.END.YEAR),
    
    # Fit period only -- for diagnostic "did we calibrate well" figures.
    calibration = list(years    = 2010:2023,
                       breaks   = c(2010, 2015, 2020, 2023),
                       data.end = SHIELD.DATA.END.YEAR),
    
    # Full model history -- for appendix figures showing the epidemic from the start.
    history = list(years    = 1990:2030,
                   breaks   = c(1990, 2000, 2010, 2020, 2030),
                   data.end = SHIELD.DATA.END.YEAR),
    
    # Longer horizon, e.g. for intervention papers.
    long.projection = list(years    = 2010:2040,
                           breaks   = c(2010, 2020, 2030, 2040),
                           data.end = SHIELD.DATA.END.YEAR)
)

# Which window figures use when nothing is specified.
SHIELD.DEFAULT.WINDOW <- "manuscript"

## shield.time() ----
# Look up a window by name. Returns the list described above.
shield.time <- function(window = SHIELD.DEFAULT.WINDOW) {
    if (is.list(window)) return(window)          # allow passing a custom window inline
    if (!window %in% names(SHIELD.TIME))
        stop("Unknown time window '", window, "'. Options: ",
             paste(names(SHIELD.TIME), collapse = ", "))
    SHIELD.TIME[[window]]
}

## shield.x.scale() ----
# The x axis for a window: ticks only at the window's break years, and no padding
# at the edges so the last projection year touches the panel border.
shield.x.scale <- function(window = SHIELD.DEFAULT.WINDOW) {
    tw <- shield.time(window)
    # slightly more room on the right so the final year label is not clipped
    scale_x_continuous(breaks = tw$breaks, expand = expansion(mult = c(0.02, 0.04)))
}

## shield.proj.line() ----
# Dashed vertical line separating observed years from projected years. This is what
# tells a reader that the same figure is both a calibration and a projection.
# label = TRUE adds a small "projection ->" annotation (skip it in multi-panel grids,
# where it repeats 10 times and adds clutter).
shield.proj.line <- function(window = SHIELD.DEFAULT.WINDOW, label = FALSE) {
    tw <- shield.time(window)
    if (is.null(tw$data.end) || tw$data.end >= max(tw$years)) return(NULL)
    out <- list(geom_vline(xintercept = tw$data.end ,
                           linetype = "dashed", linewidth = 0.3, colour = "grey45"))
    if (label)
        out <- c(out, list(annotate("text", x = tw$data.end + 0.7, y = Inf,
                                    label = "projection", hjust = 0, vjust = 1.4,
                                    size = 2.8, colour = "grey45")))
    out
}


# ****************************************************************************************************
# 2. COLOR  ("the palettes")
# ****************************************************************************************************
# Built on the Okabe-Ito palette: 8 colors chosen so that they stay distinguishable
# under deuteranopia and protanopia, which covers ~8% of men.
#
# Two rules keep figures consistent across papers:
#   RULE 1  Black is reserved for OBSERVED DATA. Model output never uses it.
#   RULE 2  A stratum keeps the same color in every figure. That is what the
#           SHIELD.COLORS.* registries below are for -- they are keyed by the exact
#           dimension values the model uses, so color and label can never drift apart.
# ****************************************************************************************************

OKABE.ITO <- c(black        = "#000000",
               orange       = "#E69F00",
               skyblue      = "#56B4E9",
               bluishgreen  = "#009E73",
               yellow       = "#F0E442",
               blue         = "#0072B2",
               vermillion   = "#D55E00",
               reddishpurple= "#CC79A7")

SHIELD.COLOR.DATA    <- OKABE.ITO[["black"]]   # observed surveillance data (RULE 1)
SHIELD.COLOR.MODEL   <- OKABE.ITO[["blue"]]    # default single-series model color
SHIELD.COLOR.MISSING <- "grey70"               # unmapped / "other" categories

# Ordered palette for generic series (scenarios, calibration versions, ...).
# Black excluded on purpose; yellow moved last because it is faint on white.
SHIELD.PALETTE <- unname(OKABE.ITO[c("blue", "vermillion", "bluishgreen",
                                     "orange", "reddishpurple", "skyblue", "yellow")])

# 10 MSAs exceed any colorblind-safe palette. Do NOT color by city: facet by city and
# keep one color per panel. This grey is for the rare figure that overlays all cities.
SHIELD.COLOR.CITY.OVERLAY <- "grey55"

# ---- Named color roles, keyed by the model's own dimension values --------------------
# VERIFY the level names against shield_specification.R when you add a dimension.

SHIELD.COLORS.SEX <- c(msm               = OKABE.ITO[["blue"]],
                       heterosexual_male = OKABE.ITO[["bluishgreen"]],
                       female            = OKABE.ITO[["vermillion"]])

SHIELD.LABELS.SEX <- c(msm               = "MSM",
                       heterosexual_male = "Heterosexual men",
                       female            = "Women")

SHIELD.COLORS.RACE <- c(black    = OKABE.ITO[["blue"]],
                        hispanic = OKABE.ITO[["orange"]],
                        other    = OKABE.ITO[["bluishgreen"]])

SHIELD.LABELS.RACE <- c(black    = "Black",
                        hispanic = "Hispanic",
                        other    = "Other")

# Scenario colors. baseline = the no-DoxyPEP counterfactual shown in this manuscript.
SHIELD.COLORS.SCENARIO <- c(baseline = OKABE.ITO[["blue"]],
                            doxypep  = OKABE.ITO[["vermillion"]],
                            partial  = OKABE.ITO[["orange"]],
                            targeted = OKABE.ITO[["bluishgreen"]])

SHIELD.LABELS.SCENARIO <- c(baseline = "No DoxyPEP",
                            doxypep  = "DoxyPEP",
                            partial  = "Partial coverage",
                            targeted = "Targeted coverage")

## shield.colors() / shield.labels() ----
# Fetch a registry by dimension name. Unknown levels fall back to grey rather than
# erroring, so a new race category shows up visibly wrong instead of silently missing.
shield.colors <- function(dimension) {
    switch(dimension,
           sex      = SHIELD.COLORS.SEX,
           race     = SHIELD.COLORS.RACE,
           scenario = SHIELD.COLORS.SCENARIO,
           stop("No color registry for dimension '", dimension, "'"))
}

shield.labels <- function(dimension) {
    switch(dimension,
           sex      = SHIELD.LABELS.SEX,
           race     = SHIELD.LABELS.RACE,
           scenario = SHIELD.LABELS.SCENARIO,
           stop("No label registry for dimension '", dimension, "'"))
}

## shield.scale.color() ----
# OPTIONAL override that relabels the legend using the registries above.
# Only use this when simplot() is coloring by that same dimension, otherwise ggplot2
# warns that a colour scale is already present. Normally the STYLE manager (section 4)
# supplies the colors and you do not need this.
shield.scale.color <- function(dimension) {
    list(scale_color_manual(values = shield.colors(dimension),
                            labels = shield.labels(dimension),
                            na.value = SHIELD.COLOR.MISSING),
         scale_fill_manual(values = shield.colors(dimension),
                           labels = shield.labels(dimension),
                           na.value = SHIELD.COLOR.MISSING))
}

## darken() ----
# Derive a darker shade of an existing color, for text annotations that must sit on
# top of a line of that color. Keeps annotation colors tied to series colors.
darken <- function(color, amount = 60) {
    rgb.val <- pmax(0, pmin(255, col2rgb(color) - amount))
    dim(rgb.val) <- c(3, length(color))   # pmax() drops the matrix dimensions; restore them
    rgb(rgb.val[1, ], rgb.val[2, ], rgb.val[3, ], maxColorValue = 255)
}


# ****************************************************************************************************
# 3. THEME  (plain ggplot2: text, legend, gridlines)
# ****************************************************************************************************
# One function replaces the old THEME.1 / THEME.2a / THEME.2b / THEME.3 / THEME.4 constants.
# Arguments, and what each one does to the plot:
#   base.size   size in points of the smallest text (axis labels). Titles and legend
#               scale off it. Use 11 for journal figures, 9 for 10-panel city grids,
#               14+ for slides.
#   legend      "none"   no legend            (use when the title says what the line is)
#               "bottom" legend under the panel, horizontal (best for 2-3 series)
#               "right"  legend beside the panel, vertical  (best for many series)
#   strips      TRUE keeps facet labels. Set FALSE only when every panel is labelled
#               some other way -- blanking strips on a faceted figure loses information.
#   grid        "y" horizontal gridlines only (default; the eye reads levels, not years)
#               "both" or "none"
#   base.family font family. Leave "" for the device default; set once here so all
#               figures in a paper share a typeface.
# ****************************************************************************************************
theme.shield <- function(base.size   = 11,
                         legend      = c("bottom", "none", "right"),
                         strips      = TRUE,
                         grid        = c("y", "both", "none"),
                         base.family = "")
{
    legend <- match.arg(legend)
    grid   <- match.arg(grid)
    
    th <- theme_minimal(base_size = base.size, base_family = base.family) +
        theme(
            # --- text ---
            plot.title      = element_text(size = base.size, face = "bold", hjust = 0.5,
                                           margin = margin(b = 4)),
            plot.subtitle   = element_text(size = base.size - 1, hjust = 0.5, colour = "grey30"),
            axis.title.y    = element_text(size = base.size, margin = margin(r = 4)),
            axis.title.x    = element_blank(),          # the x axis is always years
            axis.text       = element_text(size = base.size - 1, colour = "grey20"),
            
            # --- panel ---
            panel.border    = element_rect(colour = "grey80", fill = NA, linewidth = 0.3),
            panel.spacing   = unit(0.6, "lines"),
            plot.background = element_rect(fill = "white", colour = NA),  # no transparent PNGs
            
            # --- facet strips ---
            strip.background = element_blank(),
            strip.text       = element_text(size = base.size - 1, face = "bold",
                                            margin = margin(b = 2)),
            
            # --- legend ---
            legend.title      = element_blank(),
            legend.background = element_blank(),
            legend.key.height = unit(0.5, "lines"),
            legend.text       = element_text(size = base.size - 1),
            legend.margin     = margin(t = -4, b = 0)
        )
    
    th <- th + theme(legend.position = legend)
    if (legend == "bottom") th <- th + theme(legend.direction = "horizontal")
    
    if (!strips) th <- th + theme(strip.text = element_blank())
    
    if (grid == "y")    th <- th + theme(panel.grid.minor = element_blank(),
                                         panel.grid.major.x = element_blank())
    if (grid == "none") th <- th + theme(panel.grid = element_blank())
    
    th
}


# ****************************************************************************************************
# 4. STYLE  (jheem2 style manager: what the lines mean)
# ****************************************************************************************************
# create.style.manager() is the only way to control aesthetics inside simplot().
# The arguments it takes, in plain terms:
#   color.sim.by       what color encodes. "simset" = one color per simulation set
#                      (scenario / calibration version). "stratum" = one color per
#                      subgroup when you use split.by.
#   linetype.sim.by    the second visual channel, same options. Using color for
#                      stratum and linetype for simset lets one panel show both.
#   sim.palette        colors for model output.
#   data.palette       colors for observed data points.
#   alpha.line         transparency of individual simulation trajectories. 0.1 makes
#                      400 traces readable as a cloud; 1 makes them a black mess.
#   linewidth.baseline thickness of those trajectories.
#   shade.data.by      set NULL to suppress the shaded ribbon around observed data.
#
# MODES below are the three looks SHIELD actually needs. Pick by purpose, not by number.
#   "interval"  median + credible interval ribbon. USE THIS FOR MANUSCRIPT FIGURES.
#               Pair with simplot(summary.type = "median.and.interval").
#   "spaghetti" every simulation drawn faintly. Diagnostic look -- shows whether the
#               posterior is wide or the chains are stuck.
#   "single"    one thick line, for a single simulation (e.g. last_sim).
# ****************************************************************************************************
shield.style <- function(mode       = c("interval", "spaghetti", "single"),
                         color.by   = c("simset", "stratum"),
                         linetype.by = NULL,
                         palette    = SHIELD.PALETTE,
                         data.color = SHIELD.COLOR.DATA,
                         shade.data = TRUE)
{
    mode     <- match.arg(mode)
    color.by <- match.arg(color.by)
    
    args <- list(color.sim.by = color.by,
                 sim.palette  = palette,
                 data.palette = data.color)
    
    if (!is.null(linetype.by)) args$linetype.sim.by <- linetype.by
    # NOTE: args$shade.data.by <- NULL would DELETE the element rather than set it to
    # NULL, so the single-bracket form is required to actually pass NULL to jheem2.
    if (!shade.data)           args["shade.data.by"] <- list(NULL)
    
    if (mode == "spaghetti") {
        args$alpha.line         <- 0.10
        args$linewidth.baseline <- 0.25
    } else if (mode == "interval") {
        args$linewidth.baseline <- 0.80
    } else {                                  # single
        args$linewidth.baseline <- 1.20
    }
    
    do.call(create.style.manager, args)
}


# ****************************************************************************************************
# 5. OUTCOMES  (one lookup table instead of hand-typed labels)
# ****************************************************************************************************
# Every hand-typed ylab()/ggtitle() is a chance to put "Clients (n)" on a proportion
# axis. This table is the single source of truth. Add a row when you add an outcome.
#
# COLUMNS
#   outcome  exact SHIELD outcome name passed to simplot()
#   title    panel title
#   y.lab    y-axis label
#   type     "count"      -> thousands separators
#            "proportion" -> percent axis  (see the note on units below)
#            "rate"       -> plain numbers
#   y.zero   TRUE forces the axis to start at 0 (right for counts, usually wrong for
#            proportions that sit in a narrow band)
#
#   y.mult   multiplier applied to the AXIS LABELS only. Normally 1: simplot ALREADY
#            converts proportion outcomes to 0-100 for display, so multiplying again gives
#            "7200%" instead of "72%". Only set this if an outcome reaches the plot on a
#            scale that its axis label should not use directly.
#   get.mult VERIFIED CONVENTION: simset$get() returns proportions on 0-1, while simplot
#            displays them on 0-100. This is the factor that converts $get() output into
#            simplot's display units, and it is used ONLY by the "overlay" path in section 9
#            (which reads $get() directly). Set 100 for proportions, 1 for counts and rates.
#            Getting this wrong makes the overlay's model line and data points disagree by 100x.
# ****************************************************************************************************

SHIELD.OUTCOMES <- data.frame(
    outcome = c("incidence",
                "diagnosis.total",
                "diagnosis.ps",
                "diagnosis.el.misclassified",
                "diagnosis.late.misclassified",
                "hiv.testing",
                "sti.screening",
                "prop.male.ps.diag.among.msm",
                "ps.diag.rate.among.msm"),
    title   = c("Incident infections",
                "Reported diagnoses, all stages",
                "Primary and secondary syphilis diagnoses",
                "Early latent diagnoses (misclassified)",
                "Late latent diagnoses (misclassified)",
                "HIV testing",
                "STI screening",
                "Share of male P&S diagnoses among MSM",
                "P&S diagnosis rate among MSM"),
    y.lab   = c("Infections (n)",
                "Diagnoses (n)",
                "Diagnoses (n)",
                "Diagnoses (n)",
                "Diagnoses (n)",
                "Tests (n)",
                "Screens (n)",
                "Proportion (%)",
                "Rate per 100,000"),
    type    = c("count", "count", "count", "count", "count",
                "count", "count", "proportion", "rate"),
    y.zero  = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    y.mult  = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    get.mult = c(1, 1, 1, 1, 1, 1, 1, 100, 1),
    stringsAsFactors = FALSE
)

## shield.outcome.info() ----
# Row lookup. Unknown outcomes get a neutral default so a new outcome plots rather
# than stopping the script -- you will notice the generic label and add a row.
shield.outcome.info <- function(outcome) {
    i <- match(outcome, SHIELD.OUTCOMES$outcome)
    if (is.na(i)) {
        warning("Outcome '", outcome, "' is not in SHIELD.OUTCOMES; using generic labels.")
        return(list(outcome = outcome, title = outcome, y.lab = "Value",
                    type = "rate", y.zero = TRUE, y.mult = 1, get.mult = 1))
    }
    as.list(SHIELD.OUTCOMES[i, ])
}

shield.title <- function(outcome) shield.outcome.info(outcome)$title
shield.y.lab <- function(outcome) shield.outcome.info(outcome)$y.lab

## shield.y.scale() ----
# The y axis implied by the outcome's type: comma-separated counts, percent labels
# for proportions, plain numbers for rates.
shield.y.scale <- function(outcome) {
    info <- shield.outcome.info(outcome)
    lim  <- if (isTRUE(info$y.zero)) c(0, NA) else c(NA, NA)
    mult <- if (is.null(info$y.mult) || is.na(info$y.mult)) 1 else info$y.mult
    
    lab <- switch(info$type,
                  count      = label_comma(accuracy = 1),
                  proportion = function(x) paste0(format(x * mult, trim = TRUE), "%"),
                  function(x) label_comma()(x * mult))
    
    scale_y_continuous(labels = lab, limits = lim,
                       expand = expansion(mult = c(0.02, 0.05)))
}


# ****************************************************************************************************
# 6. OUTPUT  (figure sizes and saving)
# ****************************************************************************************************
# Named sizes, in inches, so no figure carries ad-hoc numbers. These are PER FIGURE,
# not per panel; for grids, multiply by the number of rows/columns (see save.shield.fig).
#   single  one panel, full text width  (a main-text figure)
#   double  two panels side by side
#   panel   one cell inside a multi-panel grid -- the building block for city grids
#   slide   wider and shorter, for presentations
# ****************************************************************************************************

SHIELD.FIG.SIZE <- list(single = c(width = 6.5, height = 4.5),
                        double = c(width = 6.5, height = 3.2),
                        panel  = c(width = 2.4, height = 2.0),
                        slide  = c(width = 10.0, height = 5.5))

SHIELD.FIG.DPI <- 300     # journal minimum for raster figures

## save.shield.fig() ----
# Saves one figure with consistent size, resolution and white background.
#   size    a name from SHIELD.FIG.SIZE, or c(width = , height = ) in inches
#   ncol/nrow  if given, the "panel" size is multiplied out to fit a grid
#   formats "png" for review copies, "pdf" for vector submission. Both by default.
# Uses ragg for PNGs when installed (better text rendering than grDevices::png).
save.shield.fig <- function(plot, filename, dir,
                            size    = "single",
                            ncol    = NULL,
                            nrow    = NULL,
                            formats = c("png", "pdf"),
                            dpi     = SHIELD.FIG.DPI)
{
    dims <- if (is.character(size)) SHIELD.FIG.SIZE[[size]] else size
    if (is.null(dims)) stop("Unknown figure size '", size, "'")
    
    w <- unname(dims["width"])  * if (is.null(ncol)) 1 else ncol
    h <- unname(dims["height"]) * if (is.null(nrow)) 1 else nrow
    
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    stem <- sub("\\.(png|pdf)$", "", filename)
    
    for (fmt in formats) {
        fp <- file.path(dir, paste0(stem, ".", fmt))
        if (fmt == "png" && requireNamespace("ragg", quietly = TRUE)) {
            ggsave(fp, plot, width = w, height = h, dpi = dpi,
                   device = ragg::agg_png, bg = "white")
        } else {
            ggsave(fp, plot, width = w, height = h, dpi = dpi, bg = "white")
        }
        message("Saved: ", fp)
    }
    invisible(file.path(dir, paste0(stem, ".", formats)))
}


# ****************************************************************************************************
# 7. WRAPPER  (a thin layer over simplot -- it does NOT re-declare simplot's arguments)
# ****************************************************************************************************
# DESIGN: everything you pass goes straight through to simplot(). The wrapper only
#   (a) fills in the year range from the time window, IF you did not set one yourself
#   (b) fills in a style manager,                     IF you did not supply one
#   (c) adds the theme, axis scales, title and projection line afterwards
# Anything you set explicitly always wins, so no simplot() argument is hidden from you.
#
# EVERY SIMPLOT ARGUMENT (jheem2 1.12.0) AND HOW TO REACH IT
#   <simsets>                     positionally: shield.trend.plot(ss1, ss2, outcomes = "incidence")
#   outcomes                      outcomes = "diagnosis.ps"  (also accepted unnamed, as simplot does;
#                                 naming it is what lets the wrapper look up title and y-axis label)
#   corresponding.data.outcomes   pass through
#   split.by                      pass through -- also switches the default legend on and colors by stratum
#   facet.by                      pass through -- also turns facet strips on
#   dimension.values              pass through; $year is filled from `window` only if you omit it,
#                                 so dimension.values = list(sex = "msm") keeps the window years
#   dimension.values.post.mapping pass through
#   target.ontology               pass through
#   plot.which                    pass through
#   summary.type                  pass through; defaults to "median.and.interval"
#   label.function                pass through
#   plot.year.lag.ratio           pass through
#   title                         pass through. If omitted, it is not sent to simplot at all
#                                 (simplot errors on title = NULL), and the SHIELD outcome title
#                                 replaces simplot's "location" title afterwards. Pass
#                                 title = "location" to get the city name back.
#   n.facet.rows                  pass through
#   append.url                    pass through
#   data.manager                  pass through
#   style.manager                 pass through; defaults to shield.style()
#   show.data.pull.error          pass through
#   debug                         pass through
#
# VERSION NOTE: omit.data.years is NOT in the 1.12.0 documentation but is used elsewhere in
# this project (calibration_plots.R). It is passed only if your installed simplot() actually
# has that formal argument, so this file degrades quietly on an older jheem2 rather than
# erroring. If your version supports simset.names / title.suffix, just pass them through ...
#
# SHIELD-ONLY ARGUMENTS (not simplot arguments)
#   window mode base.size legend grid strips show.proj.line proj.label omit.data.years
# ****************************************************************************************************
shield.trend.plot <- function(...,
                              outcomes        = NULL,
                              window          = SHIELD.DEFAULT.WINDOW,
                              mode            = NULL,
                              base.size       = 11,
                              legend          = NULL,
                              grid            = "y",
                              strips          = NULL,
                              show.proj.line  = TRUE,
                              proj.label      = FALSE,
                              omit.data.years = SHIELD.OMIT.DATA.YEARS)
{
    args <- list(...)
    tw   <- shield.time(window)
    
    # Outcomes: taken from the named argument, or from the first unnamed character
    # vector in ... (the form simplot itself accepts). Needed only for the labels.
    if (is.null(outcomes)) {
        nm <- names(args)
        if (is.null(nm)) nm <- rep("", length(args))
        i <- which(nm == "" & vapply(args, is.character, logical(1)))
        if (length(i) > 0) outcomes <- args[[i[1]]]
    } else {
        args$outcomes <- outcomes
    }
    
    # (a) YEARS -- the window fills in $year only if you did not specify it
    dv <- args$dimension.values
    if (is.null(dv)) dv <- list()
    if (is.null(dv$year)) dv$year <- tw$years
    args$dimension.values <- dv
    
    if (is.null(args$summary.type)) args$summary.type <- "median.and.interval"
    
    # Title: yours wins. If you omit it, `title` is NOT sent to simplot at all -- simplot
    # tests `if (title == "location")` without a length guard, so passing NULL errors with
    # "argument is of length zero". We let simplot build its default title and then replace
    # it with labs() below. Passing title = NULL yourself is also absorbed here.
    has.user.title <- "title" %in% names(args) && !is.null(args$title)
    if ("title" %in% names(args) && is.null(args$title)) args$title <- NULL   # drops the element
    
    # (b) STYLE -- interval look for summarised plots, spaghetti for raw trajectories
    if (is.null(args$style.manager)) {
        if (is.null(mode))
            mode <- if (grepl("interval", args$summary.type)) "interval" else "spaghetti"
        args$style.manager <- shield.style(
            mode     = mode,
            color.by = if (is.null(args$split.by)) "simset" else "stratum")
    }
    
    # Only pass omit.data.years if this jheem2 version has it (see VERSION NOTE above).
    # If it does not, say so rather than quietly plotting the years you asked to hide.
    if (!is.null(omit.data.years)) {
        if ("omit.data.years" %in% names(formals(simplot)))
            args$omit.data.years <- omit.data.years
        else
            warning("This jheem2 build's simplot() has no omit.data.years argument, so ",
                    "data for ", min(omit.data.years), "-", max(omit.data.years),
                    " will still be shown.")
    }
    
    p <- do.call(simplot, args)
    
    # (c) SHIELD LOOK
    single.outcome <- length(outcomes) == 1
    if (is.null(legend)) legend <- if (is.null(args$split.by)) "none" else "bottom"
    if (is.null(strips)) strips <- !is.null(args$facet.by) || !single.outcome
    
    p <- p + theme.shield(base.size = base.size, legend = legend,
                          strips = strips, grid = grid) +
        shield.x.scale(window)
    
    # An outcome-specific y axis and title only make sense for ONE outcome. With several,
    # simplot facets by outcome and labels each panel itself, so leave those alone.
    if (single.outcome)
        p <- p + shield.y.scale(outcomes) +
        labs(title = if (has.user.title) args$title else shield.title(outcomes),
             y     = shield.y.lab(outcomes),
             x     = NULL)
    
    if (show.proj.line) p <- p + shield.proj.line(window, label = proj.label)
    
    p
}


# ****************************************************************************************************
# 8. USAGE EXAMPLES  (commented out -- this file must have no side effects)
# ****************************************************************************************************
# SETUP
#   source('.../calibration/calibration_helper_functions.R')
#   source('.../calibration/pretty_plots.R')
#   FIG.DIR <- '.../SHIELD/analysis/figures'
#   ss <- calib.simsets[['Baltimore - calib.8.21.stage3.az']]$full_simset
#
# 1) One outcome, one city: fit plus baseline projection, 2010-2030
#   p <- shield.trend.plot(ss, outcomes = "diagnosis.ps")
#   save.shield.fig(p, "calib_baltimore_ps", FIG.DIR, size = "single")
#
# 2) Split by sex -- one color per group, legend appears automatically
#   p <- shield.trend.plot(ss, outcomes = "diagnosis.ps", split.by = "sex")
#
# 3) TWO simsets overlaid (baseline vs intervention, or two calibration versions).
#    Simsets are positional, exactly as in simplot.
#   p <- shield.trend.plot(ss.noint, ss.doxy, outcomes = "incidence")
#
# 4) Subset a dimension while keeping the window years
#   p <- shield.trend.plot(ss, outcomes = "diagnosis.ps",
#                          dimension.values = list(sex = "msm"))
#
# 5) Override the years for one figure only (your dimension.values$year wins)
#   p <- shield.trend.plot(ss, outcomes = "incidence",
#                          dimension.values = list(year = 2015:2035))
#
# 6) Diagnostic view: every trajectory instead of median + interval
#   p <- shield.trend.plot(ss, outcomes = "diagnosis.ps",
#                          summary.type = "individual.simulation")
#
# 7) Fit period only, no projection line
#   p <- shield.trend.plot(ss, outcomes = "hiv.testing", window = "calibration")
#
# 8) Any other simplot argument just passes through
#   p <- shield.trend.plot(ss, outcomes = "sti.screening",
#                          facet.by = "race", n.facet.rows = 1,
#                          plot.which = "sim.only",
#                          data.manager = SHIELD.DATA.MANAGER)
#
# 9) Ten-city grid, one panel per city, your own titles (needs patchwork)
#   panels <- lapply(names(calib.simsets), function(nm)
#       shield.trend.plot(calib.simsets[[nm]]$full_simset, outcomes = "diagnosis.ps",
#                         base.size = 9, title = nm))
#   grid <- patchwork::wrap_plots(panels, ncol = 5)
#   save.shield.fig(grid, "calib_ps_all_cities", FIG.DIR, size = "panel", ncol = 5, nrow = 2)
#
# 10) Change the horizon for a future paper without touching any plotting code
#   p <- shield.trend.plot(ss, outcomes = "incidence", window = "long.projection")
# ****************************************************************************************************


# ****************************************************************************************************
# 9. MULTI-CITY FIGURES
# ****************************************************************************************************
# WHY THIS SECTION EXISTS
#   simplot() refuses simsets from different locations:
#       "Cannot generate simplot: all simulation sets must have the same location"
#   So any figure spanning more than one MSA has to be assembled OUTSIDE simplot.
#   There are two ways to do that, and they are not interchangeable:
#
#   combine = "grid"      One simplot panel per city, arranged in a grid (patchwork).
#                         Keeps everything simplot gives you -- observed data points,
#                         split.by, facet.by, credible intervals. DEFAULT, and what
#                         manuscript city figures should use.
#
#   combine = "overlay"   All cities as lines in ONE panel. simplot cannot do this at all,
#                         so the median and interval are computed straight off the simsets
#                         with $get(), and the observed calibration points are harvested
#                         from a per-city simplot call (set show.data = FALSE to drop them).
#                         split.by IS supported (one dimension): the model is summarised per
#                         stratum and the data are harvested one stratum at a time.
#                         LIMITATIONS: one outcome at a time, no facet.by (split.by uses the
#                         facets). Intended for 2-4 cities; colors run out past 7.
#
# BOTH take `calib.simsets` and a `locations` field, so you never index the list by hand.
# calib.simsets entries are named "<City> - <calibration.code>", so `locations` accepts
# city names ("Atlanta"), location codes ("C.12060"), or full entry names.
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

## .shield.summarise() ----
# Median and credible interval per year, computed directly from a simset. Used only by
# the "overlay" path, because that path cannot go through simplot.
# $get() returns an array of year x sim; the quantiles are taken across sims.
# NOTE ON UNITS: $get() returns proportions on 0-1, but simplot -- and therefore the
# observed points harvested from it -- displays them on 0-100. The get.mult column of
# SHIELD.OUTCOMES converts, so both halves of the overlay end up on the same scale.
#
# split.by: when given, quantiles are computed per year AND per level of that dimension,
# and the level appears in the returned $stratum column. Without it, $stratum is NA.
.shield.summarise <- function(simset, outcome, dimension.values,
                              interval = c(0.025, 0.975), split.by = NULL) {
    keep <- c("year", split.by)
    arr  <- simset$get(outcome, keep.dimensions = keep,
                       dimension.values = dimension.values)
    
    probs <- c(interval[1], 0.5, interval[2])
    q     <- apply(arr, keep, stats::quantile, probs = probs, na.rm = TRUE)
    # q is [3 x year] with no split, or [3 x year x stratum] with one
    
    dnq   <- dimnames(q)
    years <- suppressWarnings(as.numeric(dnq[[2]]))
    if (any(is.na(years))) years <- dimension.values$year
    
    mult <- shield.outcome.info(outcome)$get.mult      # 0-1 -> simplot's display units
    if (is.null(mult) || is.na(mult)) mult <- 1
    
    if (length(dim(q)) == 2) {
        out <- data.frame(year = years, lower = q[1, ], median = q[2, ], upper = q[3, ],
                          stratum = NA_character_, stringsAsFactors = FALSE)
    } else {
        strata <- dnq[[3]]
        out <- do.call(rbind, lapply(seq_along(strata), function(k)
            data.frame(year = years, lower = q[1, , k], median = q[2, , k],
                       upper = q[3, , k], stratum = strata[k], stringsAsFactors = FALSE)))
    }
    
    out$lower  <- out$lower  * mult
    out$median <- out$median * mult
    out$upper  <- out$upper  * mult
    out
}

## .shield.data.points() ----
# Observed calibration data for ONE city, for the overlay path.
#
# HOW IT WORKS, and why it is done this way: rather than calling the data manager
# directly (whose pull API differs between jheem2 versions), this runs simplot() for the
# single city -- which is exactly how the grid path gets its data points -- and then reads
# the plotted points back out of the ggplot object. Whatever data simplot would have shown
# for that city is what you get here, including whichever data.manager you pass through.
#
# Returns a data.frame(year, value), or NULL when that city/outcome has no observed data
# (which is not an error -- the overlay simply shows no points for that city).
.shield.data.points <- function(simset, outcome, dimension.values, extra = list()) {
    args <- c(list(simset),
              list(outcomes = outcome, dimension.values = dimension.values,
                   plot.which = "sim.and.data"),
              extra)
    args <- args[names(args) == "" | !duplicated(names(args))]   # our settings win
    
    p <- try(do.call(simplot, args), silent = TRUE)
    if (inherits(p, "try-error")) return(NULL)
    
    # Observed data are the point layers; model output is drawn as lines/ribbons.
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    i <- which(geoms %in% c("GeomPoint", "GeomPointrange"))
    if (length(i) == 0) return(NULL)
    
    built <- ggplot2::ggplot_build(p)$data
    out <- do.call(rbind, lapply(i, function(k) {
        d <- built[[k]]
        if (!all(c("x", "y") %in% names(d))) return(NULL)
        data.frame(year = d$x, value = d$y)
    }))
    if (is.null(out) || nrow(out) == 0) return(NULL)
    out[!is.na(out$value) & !is.na(out$year), , drop = FALSE]
}

## .shield.titles() ----
# Resolves the `titles` argument into one title per city. Accepts:
#   NULL                     use the default (the city name)
#   "Syphilis in %s"         one string; %s, if present, is replaced by the city name
#   c("Panel A", "Panel B")  one per city, in the order the cities are plotted
#   c(Atlanta = "Panel A")   named by city; cities you do not name keep the default
#   function(loc) ...        called with the city name, returns the title
.shield.titles <- function(titles, locations, default) {
    if (is.null(titles))     return(default)
    if (is.function(titles)) return(vapply(locations, titles, character(1), USE.NAMES = FALSE))
    
    if (!is.null(names(titles))) {
        out <- unname(titles[locations])
        out[is.na(out)] <- default[is.na(out)]
        return(out)
    }
    if (length(titles) == 1 && grepl("%s", titles, fixed = TRUE))
        return(sprintf(titles, locations))
    
    rep(titles, length.out = length(locations))
}

## shield.city.plot() ----
# THE MULTI-CITY ENTRY POINT.
#   calib.simsets     the list returned by load.calib.simsets()
#   outcomes          one outcome for "overlay"; one or more for "grid"
#   locations         which cities (names, codes, or entry names). NULL = all.
#   calibration.code  which calibration version, if the list holds several
#   combine           "grid" (panels) or "overlay" (one panel)
#   omit.data.years   years whose OBSERVED data are hidden (model output is unaffected).
#                     Defaults to SHIELD.OMIT.DATA.YEARS, the COVID-era window. Applies to
#                     both grid and overlay; pass NULL to show every year.
#   titles            panel titles. NULL = city name (grid) or outcome title (overlay).
#                     Also accepts one string (with optional %s for the city name), one
#                     string per city, a vector named by city, or a function(city).
#   overall.title     grid only: a single title across the whole assembled figure
#   split.style       overlay only, with split.by: "facet" (one panel per stratum, default)
#                     or "linetype" (one panel, a different linetype per stratum)
#   show.data         overlay only: draw the observed calibration points (default TRUE)
#   data.scale        overlay only: multiplier for the observed points, for when data and
#                     model are stored on different scales (e.g. 0.01 for 0-100 vs 0-1).
#                     Affects the picture only -- it does not fix the underlying mismatch.
#   field             "full_simset" or "last_sim"
#   ncol              grid columns; sensible default if left NULL
#   interval          credible interval for the overlay path (default 95%)
#   ...               passed to shield.trend.plot(), and from there to simplot()
#                     -- so split.by, facet.by, plot.which, data.manager all work on "grid"
shield.city.plot <- function(calib.simsets,
                             outcomes,
                             locations        = NULL,
                             calibration.code = NULL,
                             combine          = c("grid", "overlay"),
                             field            = "full_simset",
                             window           = SHIELD.DEFAULT.WINDOW,
                             ncol             = NULL,
                             base.size        = 9,
                             interval         = c(0.025, 0.975),
                             show.proj.line   = TRUE,
                             show.data        = TRUE,
                             data.scale       = 1,
                             omit.data.years  = SHIELD.OMIT.DATA.YEARS,
                             titles           = NULL,
                             overall.title    = NULL,
                             split.style      = c("facet", "linetype"),
                             ...)
{
    combine     <- match.arg(combine)
    split.style <- match.arg(split.style)
    sims    <- shield.select.simsets(calib.simsets, locations, calibration.code, field)
    tw      <- shield.time(window)
    
    # ---------------- GRID: one simplot panel per city ----------------
    if (combine == "grid") {
        if (!requireNamespace("patchwork", quietly = TRUE))
            stop("combine = 'grid' needs the patchwork package: install.packages('patchwork')")
        
        dots <- list(...)
        # `title` given through ... is treated as `titles`, so both spellings work
        if (is.null(titles) && !is.null(dots$title)) titles <- dots$title
        dots$title <- NULL
        
        panel.titles <- .shield.titles(titles, names(sims), default = names(sims))
        
        panels <- lapply(seq_along(sims), function(i)
            do.call(shield.trend.plot,
                    c(list(sims[[i]], outcomes = outcomes, window = window,
                           base.size = base.size, title = panel.titles[i],
                           show.proj.line = show.proj.line,
                           omit.data.years = omit.data.years),
                      dots)))
        
        if (is.null(ncol)) ncol <- if (length(panels) <= 4) 2 else 5
        
        # With one simset per panel there is nothing useful to name, and jheem2 renders the
        # sim entries as "<environment>", so the legend is suppressed unless split.by is in
        # play -- in which case it labels the strata and is worth keeping.
        legend.pos <- if (is.null(dots$split.by)) "none" else "bottom"
        
        grid <- patchwork::wrap_plots(panels, ncol = ncol) +
            patchwork::plot_layout(guides = "collect") &
            theme(legend.position = legend.pos)
        
        if (!is.null(overall.title))
            grid <- grid + patchwork::plot_annotation(
                title = overall.title,
                theme = theme(plot.title = element_text(size = base.size + 4,
                                                        face = "bold", hjust = 0.5)))
        return(grid)
    }
    
    # ---------------- OVERLAY: all cities in one panel ----------------
    if (length(outcomes) != 1)
        stop("combine = 'overlay' plots one outcome at a time; got ", length(outcomes))
    if (length(sims) > length(SHIELD.PALETTE))
        warning("Overlaying ", length(sims), " cities but only ", length(SHIELD.PALETTE),
                " distinct colors are available. Use combine = 'grid' instead.")
    
    extra <- list(...)
    
    # split.by is handled here, not by simplot, so pull it out of the pass-through list
    split.by <- extra$split.by
    extra$split.by <- NULL
    if (length(split.by) > 1)
        stop("combine = 'overlay' supports at most one split.by dimension; got ",
             length(split.by))
    if (!is.null(extra$facet.by))
        stop("combine = 'overlay' does not support facet.by; use split.by, or combine = 'grid'")
    
    dv <- extra$dimension.values
    if (is.null(dv)) dv <- list()
    if (is.null(dv$year)) dv$year <- tw$years      # window supplies years unless you set them
    extra$dimension.values <- NULL                 # passed explicitly below instead
    if (is.null(extra$omit.data.years)) extra$omit.data.years <- omit.data.years
    
    panel.title <- if (is.null(titles)) shield.title(outcomes) else
        .shield.titles(titles, names(sims), default = names(sims))[1]
    if (!is.null(extra$title)) { panel.title <- extra$title; extra$title <- NULL }
    
    # ---- model output: median + interval per city (and per stratum, if split.by) ----
    df <- do.call(rbind, lapply(names(sims), function(loc) {
        d <- .shield.summarise(sims[[loc]], outcomes, dv, interval, split.by = split.by)
        d$location <- loc
        d
    }))
    df$location <- factor(df$location, levels = names(sims))
    strata.levels <- if (is.null(split.by)) NULL else unique(df$stratum)
    
    # ---- observed data, harvested from simplot one city (and stratum) at a time ----
    pts <- NULL
    if (show.data) {
        pts <- do.call(rbind, lapply(names(sims), function(loc) {
            if (is.null(split.by)) {
                d <- .shield.data.points(sims[[loc]], outcomes, dv, extra)
                if (is.null(d)) return(NULL)
                d$stratum <- NA_character_
                d$location <- loc
                return(d)
            }
            # One call per level, subsetting the dimension, so each returned point
            # carries a known stratum. Harvesting a split plot in one go would give
            # points with no way to tell the strata apart.
            do.call(rbind, lapply(strata.levels, function(lv) {
                dv2 <- dv; dv2[[split.by]] <- lv
                d <- .shield.data.points(sims[[loc]], outcomes, dv2, extra)
                if (is.null(d)) return(NULL)
                d$stratum  <- lv
                d$location <- loc
                d
            }))
        }))
    }
    
    if (!is.null(pts) && nrow(pts) > 0) {
        pts$location <- factor(pts$location, levels = levels(df$location))
        # Belt and braces: drop the omitted years here too, so this holds even on a
        # jheem2 build whose simplot lacks omit.data.years.
        if (!is.null(omit.data.years))
            pts <- pts[!(round(pts$year) %in% omit.data.years), , drop = FALSE]
        if (data.scale != 1) pts$value <- pts$value * data.scale
        
        # UNIT SANITY CHECK. Observed data and the model median should be the same order of
        # magnitude. A large ratio nearly always means one side is a 0-1 proportion and the
        # other is 0-100 -- a problem with the outcome definition or the calibration target,
        # not with the plot. We warn rather than silently rescale, which would hide it.
        ratio <- stats::median(pts$value, na.rm = TRUE) /
            stats::median(df$median, na.rm = TRUE)
        if (is.finite(ratio) && (ratio > 10 || ratio < 0.1))
            warning("Observed data are ~", signif(ratio, 2), "x the model median for '",
                    outcomes, "'. Sim and data look like they are on different scales ",
                    "(0-1 vs 0-100). Check get.mult in SHIELD.OUTCOMES and the calibration ",
                    "target; to rescale the points for plotting only, pass data.scale = ",
                    signif(1 / ratio, 3), ".")
    } else if (show.data) {
        message("No observed data found for this outcome; plotting model output only.")
    }
    
    # ---- relabel strata from the SHIELD registries (sex -> "MSM", "Women", ...) ----
    if (!is.null(split.by)) {
        lab <- tryCatch(shield.labels(split.by), error = function(e) NULL)
        pretty.levels <- strata.levels
        if (!is.null(lab)) {
            mapped <- unname(lab[strata.levels])
            pretty.levels <- ifelse(is.na(mapped), strata.levels, mapped)
        }
        df$stratum <- factor(df$stratum, levels = strata.levels, labels = pretty.levels)
        if (!is.null(pts) && nrow(pts) > 0)
            pts$stratum <- factor(pts$stratum, levels = strata.levels, labels = pretty.levels)
    }
    
    pal <- rep(SHIELD.PALETTE, length.out = nlevels(df$location))
    names(pal) <- levels(df$location)
    
    p <- ggplot(df, aes(x = year, y = median, colour = location, fill = location)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA)
    
    # Two ways to show a split: separate panels (default, stays readable), or one panel
    # with a different linetype per stratum (compact, but crowded past ~3 levels).
    if (!is.null(split.by) && split.style == "linetype") {
        p <- p + geom_line(aes(linetype = stratum), linewidth = 0.8)
    } else {
        p <- p + geom_line(linewidth = 0.8)
        if (!is.null(split.by)) p <- p + facet_wrap(~ stratum)
    }
    
    if (!is.null(pts) && nrow(pts) > 0) {
        p <- p + geom_point(data = pts, aes(x = year, y = value, fill = location),
                            inherit.aes = FALSE,
                            shape = 21, colour = "white", size = 1.9, stroke = 0.4)
    }
    
    p <- p +
        scale_color_manual(values = pal) +
        scale_fill_manual(values = pal) +
        theme.shield(base.size = base.size + 2, legend = "bottom",
                     strips = !is.null(split.by)) +
        shield.x.scale(window) +
        shield.y.scale(outcomes) +
        labs(title = panel.title, y = shield.y.lab(outcomes), x = NULL)
    
    if (show.proj.line) p <- p + shield.proj.line(window)
    
    p
}

# ---- EXAMPLES ----------------------------------------------------------------------
# A) Two cities side by side, with observed data (the usual case)
#   p <- shield.city.plot(calib.simsets, outcomes = "diagnosis.ps",
#                         locations = c("Atlanta", "Baltimore"))
#
# B) Same two cities overlaid in one panel (model output only)
#   p <- shield.city.plot(calib.simsets, outcomes = "diagnosis.ps",
#                         locations = c("Atlanta", "Baltimore"), combine = "overlay")
#
# C) All ten MSAs, 5 x 2 grid, split by sex within each panel
#   p <- shield.city.plot(calib.simsets, outcomes = "diagnosis.ps",
#                         split.by = "sex", ncol = 5)
#   save.shield.fig(p, "calib_ps_all_cities", FIG.DIR, size = "panel", ncol = 5, nrow = 2)
#
# D) Location codes work too, and so does picking the calibration version
#   p <- shield.city.plot(calib.simsets, outcomes = "incidence",
#                         locations = c("C.12060", "C.12580"),
#                         calibration.code = "calib.8.21.stage3.az")
# ****************************************************************************************************
# p <- shield.city.plot(calib.simsets, outcomes = "diagnosis.total",window = "manuscript",
#                       locations = c("Atlanta", "Baltimore"), combine = "overlay",omit.data.years = 2022:2025);p
# 
# p <- shield.city.plot(calib.simsets, outcomes = "diagnosis.ps",window = "history",
#                       locations = c("Atlanta", "Baltimore"), combine = "overlay");p
# 
# p <- shield.city.plot(calib.simsets, outcomes = "prop.male.ps.diag.among.msm",window = "history",
#                       locations = c("Atlanta", "Baltimore"), combine = "overlay");p
# 
# shield.city.plot(calib.simsets, outcomes = "prop.male.ps.diag.among.msm",window = "history",
#                  locations = "Atlanta")     # grid = plain simplot
# ss <- calib.simsets$`Atlanta – calib.8.21.stage3.az`$full_simset
# range(ss$get("prop.male.ps.diag.among.msm", keep.dimensions = "year",
#              dimension.values = list(year = 2010:2023)))

# ****************************************************************************************************
# 10. DATA LAYER  (stage 1 of the move off simplot)
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
#   * multiplies BOTH sim and data by 100 when outcome.metadata$display.as.percent is TRUE
#     -- this is the authoritative version of the 0-1 vs 0-100 question, so get.mult in
#     SHIELD.OUTCOMES is now redundant and will be removed in stage 4
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

## shield.prepare() ----
# THE DATA LAYER. Returns tidy frames; draws nothing.
#   simsets           calib.simsets, a named list of simsets, or one simset
#   outcomes          one or more model outcomes
#   locations         which cities (names, codes, or entry names); NULL = all
#   calibration.code  filter when calib.simsets holds several versions
#   split.by          one dimension, kept in the data as the `stratum` column
#   facet.by          any number of dimensions, kept as facet.by1 .. facet.byN
#   window            time window (section 1); supplies the years unless you pass your own
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

# ---- STAGE 1 CHECK ------------------------------------------------------------------
#   pp <- shield.prepare(calib.simsets, outcomes = "diagnosis.ps",
#                        locations = c("Atlanta", "Baltimore"), split.by = "sex")
#   shield.inspect(pp)
#
# What to confirm:
#   1. $data has rows for both cities, and its value range is the SAME order of magnitude
#      as $sim -- if so, display.as.percent has handled units and get.mult can go
#   2. $data$stratum shows the sex categories, matching $sim$stratum
#   3. $data$location tells you whether a source is MSA-level or state-level
#   4. $sim has value.lower / value.median / value.upper columns
# ****************************************************************************************************


# ****************************************************************************************************
# 11. THE PLOT  (stage 2: one renderer, built on the tidy frames from section 10)
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

## SHIELD.LEGEND.TITLES ----
# Whether legends are titled ("Location", "Sex", "Data source"), for every figure at once.
#   NULL   decide per plot: title them only when a plot carries more than one legend
#   TRUE   always title -- use this for a multi-panel figure, so panels that happen to have
#          one legend and panels that have two do not look like different designs
#   FALSE  never title
# Any single call can still override it with legend.titles =.
SHIELD.LEGEND.TITLES <- T

## SHIELD.SOURCE.LABELS ----
# How each data source should be NAMED in a legend. Sources arrive as ids like "lhd" or
# "cdc.sti.surveillance.reports"; set the readable version once here and every figure picks
# it up. Get the exact ids from shield.inspect(pp) -- they are what the `sources` filter
# uses too, and they never change, only their labels do.
SHIELD.SOURCE.LABELS <- c(
    lhd                          = "Local health department reports",
    mmwr                         = "MMWR",
    cdc.sti.surveillance.reports = "CDC STI surveillance reports",
    cdc.aggregated.county        = "CDC aggregated county data"
)

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
# THE ONE PLOTTING FUNCTION. Replaces shield.trend.plot() and shield.city.plot().
#   simsets/outcomes/locations/... see shield.prepare() in section 10
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
#                        edit SHIELD.SOURCE.LABELS near the top of section 11 instead.
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

