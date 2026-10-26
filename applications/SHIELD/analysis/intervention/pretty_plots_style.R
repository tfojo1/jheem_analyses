# ****************************************************************************************************
# PRETTY PLOTS: STYLE SETTINGS ----
# ****************************************************************************************************
# How the calibration figures look. This is the file to edit when you want to change a figure's
# years, colours, text, labels or size. It defines settings and small functions only; sourcing
# it draws nothing and writes nothing.
#
# You do not source it yourself: pretty_plots_helper_functions.R sources it.
#
# WHAT IS IN IT
#   1. TIME       which years a figure covers, its x-axis ticks, the projection line
#   2. COLOURS    the colour-blind-safe palette
#   3. THEME      fonts, text sizes, legend position, gridlines           -> theme.shield()
#   4. OUTCOMES   title and y-axis label for each outcome                  -> SHIELD.OUTCOMES
#   5. LEGENDS    legend titles, and readable names for the data sources   -> SHIELD.SOURCE.LABELS
#   6. SAVING     figure sizes, and one save function                      -> save.shield.fig()
#
# DEPENDENCIES: ggplot2, scales; optionally ragg (better text when saving PNGs).
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
    manuscript2010 = list(years    = 2010:2030,
                      breaks   = c(2010, 2015, 2020, 2025, 2030),
                      data.end = SHIELD.DATA.END.YEAR),
    manuscript2000 = list(years    = 2000:2030,
                      breaks   = c(2000,2005,2010, 2015, 2020, 2025, 2030),
                      data.end = SHIELD.DATA.END.YEAR),
    
    # Fit period only -- for diagnostic "did we calibrate well" figures.
    calibration = list(years    = 2010:2023,
                       breaks   = c(2010, 2015, 2020, 2023),
                       data.end = SHIELD.DATA.END.YEAR),
    
    # Full model history -- for appendix figures showing the epidemic from the start.
    history = list(years    = 1970:2030,
                   breaks   = c(1970, 1990, 2000, 2010, 2020, 2030),
                   data.end = SHIELD.DATA.END.YEAR),
    
    # Longer horizon, e.g. for intervention papers.
    long.projection = list(years    = 2010:2040,
                           breaks   = c(2010, 2020, 2030, 2040),
                           data.end = SHIELD.DATA.END.YEAR)
)

# Which window figures use when nothing is specified.
SHIELD.DEFAULT.WINDOW <- "history"

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
# 2. COLOURS
# ****************************************************************************************************
# Built on the Okabe-Ito palette: 8 colours that stay distinguishable for colour-blind readers.
# ****************************************************************************************************

OKABE.ITO <- c(black        = "#000000",
               orange       = "#E69F00",
               skyblue      = "#56B4E9",
               bluishgreen  = "#009E73",
               yellow       = "#F0E442",
               blue         = "#0072B2",
               vermillion   = "#D55E00",
               reddishpurple= "#CC79A7")

# The colours shield.plot() gives the series in a plot (cities, or strata), in this order.
# Black is left out on purpose; yellow is last because it is faint on white.
SHIELD.PALETTE <- unname(OKABE.ITO[c("blue", "vermillion", "bluishgreen",
                                     "orange", "reddishpurple", "skyblue", "yellow")])


# ****************************************************************************************************
# 3. THEME  (plain ggplot2: text, legend, gridlines)
# ****************************************************************************************************
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

## theme.shield ----
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
# 4. OUTCOMES  (one lookup table instead of hand-typed labels)
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
                    type = "rate", y.zero = TRUE, y.mult = 1))
    }
    as.list(SHIELD.OUTCOMES[i, ])
}

## shield.title ----
shield.title <- function(outcome) shield.outcome.info(outcome)$title

## shield.y.lab ----
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
# 5. LEGENDS  (legend titles and data-source names)
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
    mmwr                         = "Morbidity and Mortality Weekly Report",
    cdc.sti.surveillance.reports = "CDC STI surveillance reports",
    cdc.aggregated.county        = "CDC aggregated county data"
)


# ****************************************************************************************************
# 6. SAVING  (figure sizes and saving)
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
#   dir     defaults to FIG.DIR, the calibration's figures/ folder set by the calling script
# Uses ragg for PNGs when installed (better text rendering than grDevices::png).
save.shield.fig <- function(plot, filename, dir = FIG.DIR,
                            size    = "single",
                            ncol    = NULL,
                            nrow    = NULL,
                            formats = c("png"),
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
