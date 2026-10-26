# ============================================================================
# SHIELD / Doxy-PEP -- tables and figures
# ============================================================================
#
# HOW THE PIECES FIT TOGETHER
#
#   raw arrays  --> make_multi_location_table()  --> wide table (one row per
#                                                    location x subgroup)
#   wide table  --> table_to_long()              --> tidy long form
#   tidy long   --> plot_*()                     --> ggplot object
#
# The wide table is what you put in the manuscript. The same object is what
# every figure takes as input, so you build the table once and reuse it.
#
# ----------------------------------------------------------------------------
# WHAT CHANGED (2026-08-28 review) -- read this before comparing to old code
# ----------------------------------------------------------------------------
#
# A1. make_single_location_table() now emits a `location` column, so a
#     one-location table can be fed to the figures just like a multi-location
#     one. make_multi_location_table() no longer prepends the column itself --
#     it passes `location.label` down and lets the single-location builder do
#     it. make_multi_location_table() remains the function you normally call;
#     pass it one location or ten.
#
# A2. Both builders now default to stat.type = "median.ci". Previously the
#     multi-location builder silently defaulted to "mean.ci", so the same call
#     gave a different estimator depending on which builder you used.
#
# A3. Credible-interval rows are a TABLE feature. table_to_long() detects them
#     (the "[lower-upper]" strings) and drops them with a message, so passing a
#     median.ci table straight to a figure now plots the point estimates
#     instead of silently turning every value into NA.
#
# A4. The `id.cols` argument is gone from every figure. Identifier columns are
#     now worked out automatically: a column is a VALUE column if the table's
#     column map (or, failing that, `col.pattern`) says so, and an IDENTIFIER
#     column otherwise. This means `subgroup`, `subgroup.1`, `outcome.group` or
#     anything else you add is handled without being listed anywhere.
#
# A5. The builders attach a column map to the table they return
#     (attr(tbl, "col.map")): a small data frame saying which outcome /
#     intervention / year each value column came from. The figures read it
#     instead of reverse-engineering the column names with a regex, so
#     `row.vars` no longer breaks the figures. `col.pattern` is kept as a
#     fallback for tables that have been written to CSV and read back in,
#     because attributes do not survive write_csv()/read_csv().
#
# STRATIFICATION. location and subgroup are now kept as two separate columns
#     all the way through to the figures. Select rows with `locations =` and
#     `subgroup =` independently. The old style, where the stratum was glued
#     into the location name so you had to write locations = c("* - msm"), is
#     gone -- see the examples block at the bottom for the new equivalents.
#
# NOTE ON SOURCING. These same functions also exist in generate_table.R,
#     doxy_figures.R and generate_heatmap.R. Those copies are now STALE.
#     Source this file LAST so these definitions win.
# ============================================================================

library(tidyverse)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================================
# SHIELD FIGURE PALETTE
# ============================================================================
# One definition of colour for every SHIELD figure. It lives HERE rather than in
# the analysis scripts because it is a project constant: figures across the
# paper have to agree, and a colour changed in one script but not another is
# exactly the drift this file exists to prevent.
#
# THE FAMILY is ColorBrewer RdBu -- two hues, red and blue, which stays legible
# under every common colour-vision deficiency where red/green does not. Grey is
# the neutral for anything that is not one side of the epidemic: totals,
# individual MSAs, reference lines.
#
#   red   #B2182B   women; and, on the heat scale, harm (below zero)
#   tint  #92C5DE   the light step: benefit that does not reach the target
#   blue  #2166AC   MSM; and, on the heat scale, benefit that meets the target
#   grey  #4D4D4D   total population, single MSAs, neutral marks
#
# To restyle every SHIELD figure at once, edit this block. To restyle ONE
# script, reassign the derived constants at the top of that script -- the
# plotting defaults below resolve them when they are CALLED, not when they are
# defined, so a later reassignment wins.
SHIELD.PAL <- c(red  = "#B2182B",
                tint = "#D9D9D9",
                blue = "#2166AC",
                grey = "#4D4D4D",
                fit  = "grey60",    # fitted and reference lines
                line = "grey25",    # outlines: box borders, tile edges
                off  = "grey95",    # inactive / "not reached" fills
            cyan="#35978F")
# ---- semantic aliases: use THESE in scripts, never the raw hexes ------------
PAL.WOMEN <- unname(SHIELD.PAL["red"])
PAL.MSM   <- unname(SHIELD.PAL["blue"])
PAL.TOTAL <- unname(SHIELD.PAL["grey"])
PAL.TINT  <- unname(SHIELD.PAL["tint"])   # light fills, secondary series
PAL.BAR   <- PAL.MSM                      # bars where "reached" is the message
PAL.POINT <- unname(SHIELD.PAL["cyan"])               # one point per MSA: neutral, not a group
PAL.FIT   <- unname(SHIELD.PAL["fit"])
PAL.LINE  <- unname(SHIELD.PAL["line"])   # outlines
PAL.OFF   <- unname(SHIELD.PAL["off"])

# ---- the ordered 3-band heat scale -----------------------------------------
# These bands are ORDERED, not diverging: harm (< 0), benefit below the target,
# target met. "rdbu" walks the family above, so the heatmaps and the categorical
# figures are the same two hues. The alternatives are kept here so a palette can
# be compared without editing any figure code:
#     SHIELD.HEAT.COLS <- SHIELD.HEAT.PALETTES[["rdylbu"]]
SHIELD.HEAT.PALETTES <- list(
    rdbu   = unname(SHIELD.PAL[c("red", "tint", "blue")]),
    rdylbu = c("#D73027", "#FEE090", "#4575B4"),  # warm mid; strongest 3-way split
    teal   = c("#B2182B", "#D9D9D9", "#35978F"),  # neutral mid; CVD-safe vs red
    legacy = c("#e34948", "#2a78d6", "#008300")   # pre-2026-09 red/blue/green
)
SHIELD.HEAT.PAL  <- "rdbu"
SHIELD.HEAT.COLS <- SHIELD.HEAT.PALETTES[[SHIELD.HEAT.PAL]]

# TRUE ramps pale -> saturated WITHIN each band, and the label colour is then
# chosen per cell from that cell's own luminance. FALSE gives flat bands and a
# 3-swatch legend, which reads more cleanly -- BUT the flat branch in
# plot_coverage_heatmap() hardcodes WHITE labels, so it is legible only when all
# three bands are dark. That is true of "legacy" and of nothing else here.
SHIELD.HEAT.SHADE <- TRUE
# ============================================================================



# ============================================================================
# 1. ARRAY HELPERS
# ============================================================================

#' Subset an array by dimension NAME rather than position
#'
#' @param arr Array with named dimnames.
#' @param dim_indices Named list: names are dimension names (matching
#'   names(dimnames(arr))), values are the indices to keep along that dimension.
subset_array <- function(arr, dim_indices, drop = FALSE) {

    dn <- dimnames(arr)
    if (is.null(dn) || is.null(names(dn))) {
        stop("Array must have named dimnames to subset by dimension name.")
    }

    nd <- length(dim(arr))
    args <- rep(list(TRUE), nd)              # default: keep everything

    target_pos <- match(names(dim_indices), names(dn))
    if (any(is.na(target_pos))) {
        missing_names <- names(dim_indices)[is.na(target_pos)]
        stop("Dimension name(s) not found: ", paste(missing_names, collapse = ", "))
    }

    args[target_pos] <- dim_indices

    do.call(`[`, c(list(arr), args, list(drop = drop)))
}


#' Collapse the simulation dimension into a point estimate (and optionally a CI)
#'
#' @param keep.dimensions Dimensions to keep. EVERYTHING NOT LISTED HERE IS
#'   POOLED, including `sim`. That is the point for `sim`; it is a bug for
#'   anything else, which is why the table builders always pass every dimension
#'   they care about.
#' @param stat.type "median.ci" (default), "median", "mean.ci", or "mean".
get_stats <- function(arr,
                      keep.dimensions = "year",
                      stat.type = c("median.ci", "median", "mean.ci", "mean"),
                      round = TRUE,
                      digits = 0,
                      multiply.by.100 = FALSE,
                      floor = FALSE) {

    stat.type <- match.arg(stat.type)
    point.col <- if (grepl("^mean", stat.type)) "mean" else "median"
    show.ci   <- grepl("\\.ci$", stat.type)

    # which metrics to compute, in output order
    metrics <- point.col
    if (show.ci) metrics <- c(metrics, "lower", "upper")
    metric.fns <- list(
        mean     = function(x) mean(x),
        median   = function(x) median(x),
        lower    = function(x) unname(quantile(x, probs = 0.025)),
        upper    = function(x) unname(quantile(x, probs = 0.975))
    )

    arr_data <- apply(arr, keep.dimensions, function(x) {
        vapply(metric.fns[metrics], function(f) f(x), numeric(1))
    })

    # ORDER MATTERS. Scale first, then floor/round. Doing it the other way
    # round (the old order) rounded a proportion like 0.4567 to 0 with the
    # default digits = 0, and then multiplied 0 by 100.
    if (multiply.by.100) arr_data <- arr_data * 100
    if (floor)           arr_data <- floor(arr_data)
    if (round)           arr_data <- round(arr_data, digits = digits)

    final_dimnames <- c(list(metric = metrics),
                        dimnames(arr)[keep.dimensions])
    array(
        arr_data,
        dim = sapply(final_dimnames, length),
        dimnames = final_dimnames)
}


# ============================================================================
# 2. TABLE BUILDERS
# ============================================================================

#' Resolve location identifiers to codes and display labels
#'
#' Accepts either MSA codes ("C.12060") or city names ("Atlanta"), because the
#' location dimnames are codes whose names() are the city labels.
resolve_locations <- function(arr, locations) {
    dn <- dimnames(arr)$location
    if (is.null(dn)) stop("Array has no 'location' dimension.")
    nm <- names(dn)
    if (is.null(nm)) nm <- dn          # fall back to codes if unnamed

    codes <- ifelse(locations %in% dn, locations, dn[match(locations, nm)])
    if (any(is.na(codes)))
        stop("Location(s) not found: ", paste(locations[is.na(codes)], collapse = ", "))

    labels <- nm[match(codes, dn)]
    labels[is.na(labels) | !nzchar(labels)] <- codes[is.na(labels) | !nzchar(labels)]
    list(code = unname(codes), label = unname(labels))
}


# ============================================================================
# SAVING: ONE CONVENTION FOR TABLES AND FIGURES
#
#   save.dir   where the file goes. Defaults to the TABLE.DIR / FIG.DIR you
#              set in your driver script; falls back to "tables/" / "figures/"
#              when those are not defined.
#   filename   THE SWITCH. Give one and the file is written; leave it NULL
#              (the default) and nothing is. The extension is appended for you.
#
# So every table and figure function takes the same two arguments and behaves
# the same way, and no function writes to disk unless you name a file.
# ============================================================================

#' Default output directories, taken from the driver script if it set them
#' @noRd
.default_table_dir <- function() if (exists("TABLE.DIR")) get("TABLE.DIR") else "tables/"

#' @noRd
.default_fig_dir <- function() if (exists("FIG.DIR")) get("FIG.DIR") else "figures/"


#' Build the output path, or NULL when nothing should be written
#'
#' @param ext Extension appended when `filename` does not already carry it.
#' @param save Kept for older `save = TRUE / FALSE` calls: FALSE suppresses the
#'   write even when a filename is given, TRUE demands one. Leave it NULL and
#'   `filename` alone decides.
#' @noRd
.resolve_out_path <- function(save.dir, filename, ext, save = NULL) {

    if (isTRUE(save) && (is.null(filename) || !nzchar(filename)))
        stop("save = TRUE but no 'filename' was given.")
    if (isFALSE(save)) return(NULL)
    if (is.null(filename) || !nzchar(filename)) return(NULL)

    if (!grepl(paste0("\\.", ext, "$"), filename, ignore.case = TRUE))
        filename <- paste0(filename, ".", ext)

    target.dir <- if (is.null(save.dir) || !nzchar(save.dir)) "." else save.dir
    target.dir <- sub("(.)/+$", "\\1", target.dir)      # tolerate a trailing slash
    if (!dir.exists(target.dir))
        dir.create(target.dir, recursive = TRUE, showWarnings = FALSE)

    file.path(target.dir, filename)
}


#' Write a table to CSV, creating the directory if needed
#'
#' NOTE: the column map (see .attach_col_map) does NOT survive a trip through
#' CSV. A table read back from disk is still plottable, but the figures then
#' fall back to parsing the column names with `col.pattern`.
save_table_csv <- function(rv,
                           save.dir = .default_table_dir(),
                           filename = NULL,
                           save     = NULL) {
    path <- .resolve_out_path(save.dir, filename, "csv", save)
    if (is.null(path)) return(invisible(NULL))
    readr::write_csv(rv, file = path, na = "")
    message("Table written to: ", normalizePath(path, winslash = "/"))
    invisible(path)
}


#' Attach the column map to a finished table
#'
#' The map records, for every value column, which outcome / intervention / year
#' it came from. The figures read this instead of guessing from the column
#' name, which is what makes `row.vars` safe to use (issue A5).
#'
#' Only the id variables that were pivoted into COLUMNS appear in the map. Any
#' id variable you sent to the rows stays a real column of the table, and the
#' figures pick it up from there.
#' @noRd
.attach_col_map <- function(tbl, col.map) {
    attr(tbl, "col.map") <- col.map
    tbl
}


#' Build the column map for a given set of column variables
#'
#' pivot_wider() glues the values of `col_vars` together with "_" in the order
#' the variables are listed, so the names are fully predictable and we can
#' write them down rather than parse them back out.
#' @noRd
.build_col_map <- function(col_vars, outcomes, interventions, years) {
    value.lists <- list(outcome      = outcomes,
                        intervention = interventions,
                        year         = years)[col_vars]
    # expand.grid varies its FIRST argument fastest, but the table's columns
    # vary the LAST col_var fastest, so feed it the reversed list and put the
    # columns back in order afterwards. The map then lists the value columns
    # in exactly the order they appear in the table.
    map <- expand.grid(rev(value.lists), stringsAsFactors = FALSE,
                       KEEP.OUT.ATTRS = FALSE)[, col_vars, drop = FALSE]
    map$colname <- do.call(paste, c(as.list(map[col_vars]), sep = "_"))
    map
}


#' Does this array cover every requested year, intervention and the location?
#' @noRd
.covers <- function(arr, years, interventions, location.code) {
    dn <- dimnames(arr)
    all(years %in% dn$year) &&
        all(interventions %in% dn$intervention) &&
        location.code %in% dn$location
}


#' Say, in words, what an array is missing
#' @noRd
.coverage_gap <- function(arr, years, interventions, location.code) {
    dn <- dimnames(arr); bits <- character(0)
    miss.y <- setdiff(years, dn$year)
    miss.i <- setdiff(interventions, dn$intervention)
    if (length(miss.y)) bits <- c(bits, paste("year(s)", paste(miss.y, collapse = ", ")))
    if (length(miss.i)) bits <- c(bits, paste("intervention(s)", paste(miss.i, collapse = ", ")))
    if (!location.code %in% dn$location) bits <- c(bits, paste("location", location.code))
    paste(bits, collapse = "; ")
}


#' Melt one array's chosen outcomes into long form
#'
#' Returns one row per (stratum, outcome, intervention, year, stat), where
#' `stat` is "estimate" or "ci". Everything is character so that the pieces
#' coming from different arrays can be stacked and joined without type clashes.
#' @noRd
#' Digits for each outcome, from a scalar or a named vector
#'
#' `digits` may be a single number (all outcomes), or a named vector keyed by
#' outcome name with an optional ".default" entry. Unnamed outcomes get the
#' default, which is 0 -- the historical behaviour.
#' @noRd
.digits_for <- function(outcome, digits) {
    if (is.null(digits))        return(rep(0, length(outcome)))
    if (is.null(names(digits))) return(rep(digits[1], length(outcome)))
    d    <- unname(digits[as.character(outcome)])
    dflt <- if (".default" %in% names(digits)) digits[[".default"]] else 0
    d[is.na(d)] <- dflt
    d
}

.melt_outcomes <- function(arr, outcomes, interventions, years, location.code,
                           stratification_cols, stat.type, point.col, show.ci,
                           id_cols, digits = 0) {

    metric.cols <- c(point.col, if (show.ci) c("lower", "upper"))

    reshape2::melt(
        get_stats(subset_array(arr,
                               list(year = years,
                                    outcome = outcomes,
                                    intervention = interventions,
                                    location = location.code)),
                  keep.dimensions = c("year", "intervention", "outcome",
                                      stratification_cols),
                  stat.type = stat.type,
                  round     = FALSE)
    ) %>%
        pivot_wider(names_from = "metric") %>%
        # Rounding happens HERE, not inside get_stats(): get_stats() sees one
        # array spanning every outcome and can only apply a single `digits`,
        # but a rate wants 0 digits and a ratio wants 2-3. At digits = 0 this
        # is bit-identical to round(x, 0), so existing tables do not move.
        mutate(.dig = .digits_for(outcome, digits)) %>%
        mutate(across(all_of(metric.cols), ~ round(.x * 10^.dig) / 10^.dig)) %>%
        select(-.dig) %>%
        mutate(estimate = as.character(.data[[point.col]]),
               ci       = if (show.ci) paste0("[", lower, "-", upper, "]") else NULL) %>%
        select(all_of(c(stratification_cols, id_cols)),
               all_of(if (show.ci) c("estimate", "ci") else "estimate")) %>%
        pivot_longer(cols      = any_of(c("estimate", "ci")),
                     names_to  = "stat",
                     values_to = "value") %>%
        mutate(across(all_of(c(stratification_cols, id_cols, "stat")), as.character))
}


#' Say where each outcome came from, once per table
#'
#' Only speaks up when there is something to say: more than one array in a
#' stratification group, or an outcome that had to be filled with NA.
#' @noRd
.report_outcome_sources <- function(resolved, groups) {
    gaps <- any(vapply(resolved, function(r)
        any(vapply(r, function(x) is.na(x$src), logical(1))), logical(1)))
    if (!any(vapply(groups, length, integer(1)) > 1) && !gaps)
        return(invisible(NULL))

    for (g in seq_along(resolved)) {
        r   <- resolved[[g]]
        lab <- names(groups)[g]
        got <- names(r)[vapply(r, function(x) !is.na(x$src), logical(1))]
        if (length(got))
            message("  ", lab, ": ",
                    paste0(got, " <- data[[",
                           vapply(r[got], function(x) x$src, integer(1)), "]]",
                           collapse = "; "))
        for (nm in setdiff(names(r), got))
            message("  ", lab, ": ", nm, " -- ", r[[nm]]$why, ", filled NA")
    }
    invisible(NULL)
}


#' Build a table for ONE location
#'
#' Normally you call make_multi_location_table() instead -- it works for one
#' location as well as ten, and it is the entry point the figures are written
#' against. This function is the worker underneath it.
#'
#' @param data A list of arrays. Arrays that share a stratification level
#'   (e.g. total raw and total calculated results) are merged: each outcome is
#'   taken from whichever of them carries it, so you can pass raw and
#'   calculated results together and ask for outcomes from both. Arrays with
#'   DIFFERENT stratifications (total vs sex) contribute different sets of
#'   rows. An outcome that no array at a given level carries is filled with
#'   NA rather than raising an error; an outcome no array anywhere carries is
#'   an error. A bare array is accepted and wrapped in a list for you.
#'
#'   The table always has at least one `subgroup` column. It holds "Total"
#'   for unstratified rows, so a table built from totals-only data has the
#'   same columns as one built from totals plus sex.
#' @param quiet TRUE suppresses the note saying which array each outcome came
#'   from. make_multi_location_table() sets it after the first location so the
#'   note appears once per table rather than once per city.
#' @param location A SINGLE MSA code or city name. Supplying more than one is
#'   an error: the sims would be pooled across locations and the resulting
#'   median/CI would be meaningless.
#' @param outcomes,interventions Character vectors, matched against the
#'   corresponding dimnames.
#' @param years Character vector of years, e.g. c("2030") or
#'   as.character(2026:2040). Coerced to character for you, because a numeric
#'   year would index the array by POSITION instead of by name.
#' @param row.vars Which of outcome / intervention / year go down the rows;
#'   the rest become columns. "" (the default) sends all three to the columns.
#' @param stat.type "median.ci" (default), "median", "mean.ci" or "mean".
#'   The ".ci" variants add a second "[lower-upper]" row under each estimate.
#'   Those rows are for reading, not for plotting -- the figures drop them.
#' @param filter.by.strat Optional character vector of stratum values to keep.
#' @param location.label Name given to the location column.
#' @param save.dir,filename Where to write the CSV and what to call it.
#'   `filename` is the switch: give one and a CSV is written, leave it NULL
#'   (the default) and nothing is. The ".csv" extension is appended for you and
#'   `save.dir` defaults to TABLE.DIR when your driver script has set it.
#' @param save Only needed for older calls: `save = FALSE` suppresses the write
#'   even when a filename is given. Leave it NULL and `filename` alone decides.
make_single_location_table <- function(data,
                                       location,
                                       outcomes,
                                       interventions,
                                       years,
                                       row.vars = "",
                                       stat.type = c("median.ci", "median", "mean.ci", "mean"),
                                       digits = 0,
                                       filter.by.strat = NULL,
                                       location.label = "location",
                                       quiet = FALSE,
                                       save = NULL,
                                       save.dir = .default_table_dir(),
                                       filename = NULL,
                                       debug = FALSE
) {

    if (debug) browser()

    # a bare array is a common slip; wrap it rather than failing obscurely
    if (!is.list(data)) data <- list(data)

    # ---- location: exactly one, resolved to a code and a display label -----
    if (length(location) != 1)
        stop("'location' must be a single location. To combine several ",
             "locations in one table use make_multi_location_table().")
    loc <- resolve_locations(data[[1]], location)

    # ---- which id variables go to rows, which to columns -------------------
    id_cols <- c("outcome", "intervention", "year")

    # accept "", NULL, NA, or c() as "no row variables"
    if (is.null(row.vars)) row.vars <- character(0)
    row.vars <- row.vars[!is.na(row.vars) & nzchar(row.vars)]

    if (!all(row.vars %in% id_cols))
        stop("Error: 'row.vars' must be a subset of ", paste(id_cols, collapse = ", "),
             " (or blank for none)")
    col_vars <- setdiff(id_cols, row.vars)
    if (length(col_vars) == 0)
        stop("At least one of ", paste(id_cols, collapse = "/"),
             " must stay in the columns; 'row.vars' cannot list all three.")

    # years must be looked up by name, never by position
    years <- as.character(years)

    # ---- which point estimate, and whether to append a 95% interval row ----
    stat.type <- match.arg(stat.type)
    point.col <- if (grepl("^mean", stat.type)) "mean" else "median"
    show.ci   <- grepl("\\.ci$", stat.type)

    # ------------------------------------------------------------------
    # Group the supplied arrays by stratification level
    #
    # You typically pass four arrays: raw and calculated results at the total
    # level, and raw and calculated results at the sex level. Arrays that
    # share a stratification are two halves of ONE source -- each carries
    # some of the outcomes -- so they are merged. Arrays with different
    # stratifications become different sets of ROWS.
    # ------------------------------------------------------------------
    strat_cols_of <- function(arr)
        setdiff(names(dimnames(arr)), c(id_cols, "sim", "location"))

    sigs      <- vapply(data, function(a) paste(strat_cols_of(a), collapse = "|"),
                        character(1))
    group.ids <- unique(sigs)
    groups    <- lapply(group.ids, function(g) which(sigs == g))
    # the unstratified level has an empty signature; give it a printable name
    names(groups) <- ifelse(nzchar(group.ids), group.ids, "total")

    # arrays in the same group must agree on the stratum values, or the rows
    # they contribute would not line up
    for (idx in groups) {
        ref <- dimnames(data[[idx[1]]])[strat_cols_of(data[[idx[1]]])]
        for (i in idx)
            if (!identical(dimnames(data[[i]])[strat_cols_of(data[[i]])], ref))
                stop("data[[", i, "]] and data[[", idx[1], "]] have the same ",
                     "stratification dimensions but different values.")
    }

    # ---- which array supplies which outcome, at each stratification level -
    # An array can supply an outcome only if it also carries every requested
    # year and intervention and this location, otherwise subsetting it would
    # fail further down. When it cannot, we record WHY so the note below can
    # say so rather than just leaving a column of NAs unexplained.
    resolve <- function(idx) lapply(outcomes, function(o) {
        holders <- idx[vapply(idx, function(i) o %in% dimnames(data[[i]])$outcome,
                              logical(1))]
        if (length(holders) == 0)
            return(list(src = NA_integer_, why = "not present at this level"))
        ok <- holders[vapply(holders, function(i)
            .covers(data[[i]], years, interventions, loc$code), logical(1))]
        if (length(ok) > 0)
            return(list(src = ok[1], why = NA_character_))
        list(src = NA_integer_,
             why = paste0("in data[[", holders[1], "]], but that array is missing ",
                          .coverage_gap(data[[holders[1]]], years, interventions,
                                        loc$code)))
    })
    resolved  <- lapply(groups, function(idx) setNames(resolve(idx), outcomes))
    source_of <- lapply(resolved, function(r)
        vapply(r, function(x) x$src, integer(1)))

    # a name that appears in NO array is a typo, and is worth stopping for
    absent <- outcomes[vapply(outcomes, function(o)
        !any(vapply(data, function(a) o %in% dimnames(a)$outcome, logical(1))),
        logical(1))]
    if (length(absent) > 0)
        stop("None of the supplied arrays contains outcome(s): ",
             paste(absent, collapse = ", "),
             ".\nAvailable outcomes: ",
             paste(sort(unique(unlist(lapply(data, function(a) dimnames(a)$outcome)))),
                   collapse = ", "))

    # a name that exists but cannot be served anywhere yields an all-NA column
    unservable <- outcomes[vapply(outcomes, function(o)
        all(vapply(source_of, function(x) is.na(x[[o]]), logical(1))), logical(1))]
    if (length(unservable) > 0 && !quiet)
        warning("Outcome(s) ", paste(unservable, collapse = ", "),
                " could not be taken from any array, so their columns are all NA. ",
                "See the note above for what was missing.", call. = FALSE)

    if (!quiet) .report_outcome_sources(resolved, groups)

    # Every table gets at least one subgroup column, holding "Total" when the
    # data are not stratified, so the shape does not depend on what you pass.
    num_stratification_cols_for_table <-
        max(1L, vapply(data, function(a) length(strat_cols_of(a)), integer(1)))

    stat_levels <- if (show.ci) c("estimate", "ci") else "estimate"

    rv <- dplyr::bind_rows(lapply(seq_along(groups), function(g) {

        idx                 <- groups[[g]]
        stratification_cols <- strat_cols_of(data[[idx[1]]])
        src                 <- source_of[[g]]
        served              <- outcomes[!is.na(src)]

        # ---- pull each outcome from its array, one call per array ---------
        long.df <- NULL
        if (length(served) > 0) {
            by.source <- split(served, src[served])
            long.df <- dplyr::bind_rows(lapply(names(by.source), function(i)
                .melt_outcomes(arr                 = data[[as.integer(i)]],
                               outcomes            = by.source[[i]],
                               interventions       = interventions,
                               years               = years,
                               location.code       = loc$code,
                               stratification_cols = stratification_cols,
                               stat.type           = stat.type,
                               point.col           = point.col,
                               show.ci             = show.ci,
                               id_cols             = id_cols,
                               digits              = digits)))
        }

        # ---- fill the gaps ------------------------------------------------
        # Outcomes no array at this level carries become NA cells, so every
        # group contributes exactly the same columns and rbind cannot
        # misalign them.
        grid <- expand.grid(
            c(dimnames(data[[idx[1]]])[stratification_cols],
              list(outcome      = outcomes,
                   intervention = interventions,
                   year         = years,
                   stat         = stat_levels)),
            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
        grid <- tibble::as_tibble(grid)

        df <- if (is.null(long.df)) {
            grid %>% mutate(value = NA_character_)
        } else {
            dplyr::left_join(grid, long.df,
                             by = c(stratification_cols, id_cols, "stat"))
        }

        # order the id variables as they were REQUESTED, so the columns come
        # out in the order you asked for rather than alphabetically
        df <- df %>%
            mutate(stat         = factor(stat, levels = stat_levels),
                   outcome      = factor(outcome,      levels = outcomes),
                   intervention = factor(intervention, levels = interventions),
                   year         = factor(year,         levels = years)) %>%
            arrange(across(all_of(c(stratification_cols, row.vars, col_vars))), stat) %>%
            pivot_wider(names_from = all_of(col_vars), values_from = value) %>%
            # `stat` is kept (it used to be dropped here, leaving the leading
            # "[" as the only marker of a CI row), but as CHARACTER: it is a
            # factor at this point, and a factor/character mismatch is a
            # nuisance when joining two tables on c("location", "stat").
            mutate(stat = as.character(stat))

        # pad groups with fewer stratification dimensions so every group
        # contributes the same number of leading columns
        num_extra_cols_needed <- num_stratification_cols_for_table - length(stratification_cols)
        if (num_extra_cols_needed > 0) {
            pad <- as.data.frame(matrix("Total", nrow(df), num_extra_cols_needed),
                                 stringsAsFactors = FALSE)
            names(pad) <- paste0(".pad", seq_len(num_extra_cols_needed))
            df <- dplyr::bind_cols(pad, df)
        }

        colnames(df)[1:num_stratification_cols_for_table] <-
            make.unique(rep("subgroup", num_stratification_cols_for_table))

        df
    }))

    # ---- optional stratum filter ------------------------------------------
    if (!is.null(filter.by.strat)) {
        strat.cols <- grep("^subgroup", names(rv), value = TRUE)
        if (length(strat.cols) == 0)
            stop("No 'subgroup' column in the table -- nothing to filter on.")

        keep <- Reduce(`|`, lapply(rv[strat.cols],
                                   function(x) as.character(x) %in% filter.by.strat))

        if (!any(keep))
            stop("No rows match filter.by.strat = ",
                 paste(filter.by.strat, collapse = ", "), ".\nAvailable values: ",
                 paste(sort(unique(unlist(rv[strat.cols]))), collapse = ", "))

        rv <- rv[keep, , drop = FALSE]
    }

    # ---- the location column (A1) -----------------------------------------
    # Every figure needs this. Adding it here means a one-location table is
    # just as plottable as a multi-location one.
    rv <- rv %>% mutate(!!location.label := loc$label, .before = 1)

    # ---- record what the value columns mean (A5) --------------------------
    rv <- .attach_col_map(rv, .build_col_map(col_vars, outcomes, interventions, years))

    save_table_csv(rv, save.dir, filename, save)

    rv
}


#' Compare one or more locations in one table
#'
#' This is the function to call. It works for a single location too -- pass a
#' vector of length one.
#'
#' Rows: location (plus anything named in row.vars, plus the stat sub-rows).
#' Columns: whichever of outcome / intervention / year are not in row.vars.
#'
#' @param locations Character vector of MSA codes or city names, in the order
#'   you want them stacked.
#' @param stat.type "median.ci" (default), "median", "mean.ci" or "mean". Same
#'   default as make_single_location_table() -- these two used to disagree.
#' @param save.dir,filename Where to write the CSV and what to call it.
#'   `filename` is the switch: give one and a CSV is written, leave it NULL
#'   (the default) and nothing is. The ".csv" extension is appended for you and
#'   `save.dir` defaults to TABLE.DIR when your driver script has set it.
#' @param save Only needed for older calls: `save = FALSE` suppresses the write
#'   even when a filename is given. Leave it NULL and `filename` alone decides.
#' @param repeat.location.label If FALSE the label is printed only on the first
#'   sub-row of each location (manuscript style) and blank beneath. This makes
#'   the table PRINT-ONLY: the blank cells are real empty strings, so do not
#'   hand such a table to the figures. A warning is raised to that effect.
make_multi_location_table <- function(data,
                                      locations,
                                      outcomes,
                                      interventions,
                                      years,
                                      row.vars = "",
                                      stat.type = c("median.ci", "median", "mean.ci", "mean"),
                                      digits = 0,
                                      location.label = "location",
                                      repeat.location.label = TRUE,
                                      filter.by.strat = NULL,
                                      save = NULL,
                                      save.dir = .default_table_dir(),
                                      filename = NULL,
                                      debug = FALSE) {

    if (debug) browser()
    stat.type <- match.arg(stat.type)

    if (!is.list(data)) data <- list(data)

    loc <- resolve_locations(data[[1]], locations)

    per.loc <- lapply(seq_along(loc$code), function(i) {
        tryCatch(
            make_single_location_table(data            = data,
                                       location        = loc$code[i],
                                       outcomes        = outcomes,
                                       interventions   = interventions,
                                       years           = years,
                                       row.vars        = row.vars,
                                       filter.by.strat = filter.by.strat,
                                       stat.type       = stat.type,
                                       location.label  = location.label,
                                       quiet           = (i > 1),
                                       digits          = digits,
                                       save            = FALSE),
            error = function(e)
                stop("Failed at location ", loc$label[i], " (", loc$code[i], "): ",
                     conditionMessage(e), call. = FALSE)
        )
    })

    # every location must yield the same columns, or rbind would silently misalign
    ref.names <- names(per.loc[[1]])
    bad <- which(!vapply(per.loc, function(d) identical(names(d), ref.names), logical(1)))
    if (length(bad) > 0)
        stop("Column structure differs at location(s): ",
             paste(loc$label[bad], collapse = ", "))

    col.map <- attr(per.loc[[1]], "col.map")   # bind_rows drops attributes
    rv <- dplyr::bind_rows(per.loc)

    # ---- manuscript-style label blanking ----------------------------------
    if (!repeat.location.label) {
        warning("repeat.location.label = FALSE blanks the location cell on all ",
                "but the first row of each location. The result is for printing ",
                "only -- do not pass it to the figure functions.", call. = FALSE)
        lab <- as.character(rv[[location.label]])
        rv[[location.label]] <- ifelse(duplicated(lab), "", lab)
    }

    rv <- .attach_col_map(rv, col.map)

    save_table_csv(rv, save.dir, filename, save)

    rv
}


# ============================================================================
# 3. TABLE -> LONG (shared by every figure)
# ============================================================================

#' Work out which columns hold values, and what each one means
#'
#' Two routes, in order of preference:
#'   1. the column map the builder attached (exact, survives any `row.vars`)
#'   2. `col.pattern`, for a table that has been through CSV and lost the map
#'
#' Returns a data frame with one row per value column: colname, plus whichever
#' of outcome / intervention / year were pivoted into the columns.
#' @noRd
.value_columns <- function(tbl, col.pattern) {

    map <- attr(tbl, "col.map")

    if (!is.null(map) && "colname" %in% names(map)) {
        val.cols <- intersect(map$colname, names(tbl))
        if (length(val.cols) == 0)
            stop("The table's column map does not match any of its columns. ",
                 "Rebuild the table with make_multi_location_table().")
        return(map[match(val.cols, map$colname), , drop = FALSE])
    }

    # ---- fallback: read the meaning out of the column names ---------------
    parts  <- stringr::str_match(names(tbl), col.pattern)
    is.val <- !is.na(parts[, 1])
    if (!any(is.val))
        stop("No value columns found. The table carries no column map (was it ",
             "read back from CSV?) and no column name matches 'col.pattern':\n  ",
             col.pattern)

    data.frame(colname      = names(tbl)[is.val],
               outcome      = parts[is.val, 2],
               intervention = paste0("doxy.cov.", parts[is.val, 3]),
               year         = parts[is.val, 4],
               stringsAsFactors = FALSE)
}


#' Turn the "[lower-upper]" rows of a .ci table into numbers, or drop them (A3)
#'
#' CI cells are text, so they cannot be plotted as they stand. `keep.ci = FALSE`
#' (what every coverage figure wants) removes them and says so. `keep.ci = TRUE`
#' pairs each CI cell with its estimate and returns `lower` and `upper`
#' columns, which is what plot_trend_with_ci() shades.
#' @noRd
.split_ci_values <- function(long, keep.ci = FALSE) {

    # `stat` is exact; the regex is the fallback for a table that lost the
    # column (e.g. one built before stat was kept, or hand-assembled).
    is.ci <- if ("stat" %in% names(long)) as.character(long$stat) == "ci"
             else grepl("^\\s*\\[", as.character(long$value))

    if (!any(is.ci)) {
        if (keep.ci)
            stop("This table carries no credible intervals to shade. Rebuild it ",
                 "with stat.type = \"median.ci\" (or \"mean.ci\").")
        return(long)
    }

    if (!keep.ci) {
        message("Dropped ", sum(is.ci), " credible-interval cell(s): figures plot ",
                "point estimates only. Build the table with stat.type = \"median\" ",
                "or \"mean\" to avoid this message.")
        long <- long[!is.ci, , drop = FALSE]
        if (nrow(long) == 0)
            stop("Nothing left to plot after removing credible-interval rows.")
        return(long)
    }

    # every id combination has exactly one estimate row and one CI row, so the
    # two halves can be matched on everything except the value itself
    # `stat` MUST be excluded: it is "estimate" on one side of this join and
    # "ci" on the other, so including it would match nothing and silently
    # return all-NA lower/upper -- i.e. figures with no CI shading.
    keys <- setdiff(names(long), c("value", "stat"))
    est  <- long[!is.ci, , drop = FALSE]
    ci   <- long[ is.ci, , drop = FALSE]
    ci   <- dplyr::bind_cols(ci[keys], .parse_ci(ci$value))

    dplyr::left_join(est, ci, by = keys)
}


#' Pull the two numbers out of a "[lower-upper]" string
#'
#' Written to survive negative bounds ("[-5-3]" is lower -5, upper 3), which a
#' naive split on "-" would get wrong.
#' @noRd
.parse_ci <- function(x) {
    inner <- sub("^\\s*\\[\\s*(.*?)\\s*\\]\\s*$", "\\1", as.character(x))
    num   <- "-?[0-9.]+(?:[eE][-+]?[0-9]+)?"
    m     <- stringr::str_match(inner, paste0("^(", num, ")-(", num, ")$"))
    if (any(is.na(m[, 1])))
        warning("Could not read ", sum(is.na(m[, 1])), " credible interval(s), e.g. ",
                inner[is.na(m[, 1])][1], call. = FALSE)
    tibble::tibble(lower = as.numeric(m[, 2]), upper = as.numeric(m[, 3]))
}


#' Error helpfully when a figure needs a coverage level and the table has none
#' @noRd
.require_coverage <- function(long, cov.pattern) {
    if (all(is.na(long$coverage)))
        stop("This figure plots impact against coverage, but no coverage level ",
             "could be read from the intervention names using 'cov.pattern':\n  ",
             cov.pattern, "\nIntervention(s) seen: ",
             paste(unique(long$intervention), collapse = ", "),
             "\nFor a single scenario over time, use plot_trend_with_ci().")
    if (any(is.na(long$coverage))) {
        message("Ignoring intervention(s) with no coverage level: ",
                paste(unique(long$intervention[is.na(long$coverage)]), collapse = ", "))
        long <- long[!is.na(long$coverage), , drop = FALSE]
    }
    long
}


#' Convert a wide table into the tidy long form every figure works from
#'
#' @param tbl A table from make_multi_location_table(), or one that is already
#'   long (i.e. already has `coverage` and `value` columns).
#' @param location.col Name of the location column.
#' @param col.pattern Fallback regex, used only when the column map is missing.
#'   Three capture groups: outcome, coverage, year.
#' @param cov.pattern Regex with one capture group pulling the coverage level
#'   out of an intervention name. Change this if your scenarios are not named
#'   "doxy.cov.NN". An intervention the pattern cannot read (e.g. "noint")
#'   gets coverage NA rather than raising an error -- the figures that need a
#'   coverage level complain for themselves.
#' @param keep.ci TRUE returns `lower` and `upper` columns parsed from the
#'   "[lower-upper]" rows of a .ci table, for shading. FALSE (the default)
#'   drops those rows, which is what the coverage figures want.
#' @return Tibble with columns location, subgroup, outcome, intervention,
#'   coverage (int, may be NA), year (int), value (num), plus lower and upper
#'   when `keep.ci` is TRUE. `subgroup` is NA when the table has no
#'   stratification column.
table_to_long <- function(tbl,
                          location.col = "location",
                          col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                          cov.pattern  = "doxy\\.cov\\.(\\d+)",
                          keep.ci      = FALSE) {

    # ---- already long? ----------------------------------------------------
    if (all(c("coverage", "value") %in% names(tbl))) {
        if (!"subgroup" %in% names(tbl))     tbl$subgroup     <- NA_character_
        if (!"intervention" %in% names(tbl)) tbl$intervention <- NA_character_
        if (!"location" %in% names(tbl) && location.col %in% names(tbl))
            tbl <- dplyr::rename(tbl, location = all_of(location.col))
        keep <- c("location", "subgroup", "outcome", "intervention",
                  "coverage", "year", "value",
                  intersect(c("lower", "upper"), names(tbl)))
        return(tibble::as_tibble(tbl) %>%
                   mutate(value    = as.numeric(value),
                          coverage = as.integer(coverage),
                          year     = as.integer(year)) %>%
                   select(all_of(keep)))
    }

    if (!location.col %in% names(tbl))
        stop("Column '", location.col, "' not found in 'tbl'. Tables from ",
             "make_multi_location_table() always carry one.")

    # ---- split the columns into values and identifiers (A4) ---------------
    meta     <- .value_columns(tbl, col.pattern)
    val.cols <- meta$colname
    id.cols  <- setdiff(names(tbl), val.cols)

    # id variables that went to the ROWS stay as real columns; everything else
    # that is left over is a stratification column
    # "stat" is excluded or it would be pasted into `subgroup` ("msm / estimate")
    # and every subgroup filter downstream would match nothing. It is dropped
    # entirely by the whitelist select() at the end of this function, so the
    # long form handed to the figures is unchanged.
    strat.cols <- setdiff(id.cols,
                          c(location.col, "outcome", "intervention", "year", "stat"))

    # ---- long form --------------------------------------------------------
    long <- tbl %>%
        select(all_of(c(id.cols, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(all_of(val.cols), names_to = "colname", values_to = "value") %>%
        left_join(meta, by = "colname") %>%
        select(-colname)

    long <- .split_ci_values(long, keep.ci = keep.ci)

    # ---- one subgroup column, however many stratification columns there are -
    if (length(strat.cols) == 0) {
        long$subgroup <- NA_character_
    } else if (length(strat.cols) == 1) {
        long$subgroup <- as.character(long[[strat.cols]])
    } else {
        long$subgroup <- do.call(paste, c(lapply(long[strat.cols], as.character),
                                          sep = " / "))
    }

    # ---- coverage, year, value as numbers ---------------------------------
    if (!"coverage" %in% names(long)) {
        if (!"intervention" %in% names(long))
            stop("Cannot work out coverage: the table has neither a 'coverage' ",
                 "nor an 'intervention' column.")
        # An intervention with no number in its name -- "noint" -- gets NA.
        # That is not an error here: only the coverage figures care, and they
        # say so themselves via .require_coverage().
        long$coverage <- as.integer(
            stringr::str_match(as.character(long$intervention), cov.pattern)[, 2])
    }

    if (!"outcome" %in% names(long))
        stop("The table has no 'outcome' information in either its columns or ",
             "its column map.")
    if (!"year" %in% names(long))
        stop("The table has no 'year' information in either its columns or ",
             "its column map.")

    long <- long %>%
        mutate(value    = suppressWarnings(as.numeric(value)),
               coverage = as.integer(coverage),
               year     = as.integer(as.character(year)))

    n.bad <- sum(is.na(long$value))
    if (n.bad == nrow(long))
        stop("None of the values could be read as numbers. If the table was ",
             "built with repeat.location.label = FALSE, rebuild it with TRUE.")
    if (n.bad > 0) {
        message("Dropped ", n.bad, " cell(s) with no numeric value.")
        long <- long[!is.na(long$value), , drop = FALSE]
    }

    long %>% select(all_of(c("location", "subgroup", "outcome", "intervention",
                             "coverage", "year", "value",
                             intersect(c("lower", "upper"), names(long)))))
}


#' Back-compatible alias for table_to_long()
#' @noRd
parse_coverage_table <- function(tbl, location.col = "location", ...) {
    table_to_long(tbl, location.col = location.col, ...)
}


#' Keep only the requested locations. Exact names or glob patterns ("Atl*").
#' @noRd
.filter_locations <- function(long, locations) {
    if (is.null(locations)) return(long)

    keep  <- long$location %in% locations
    globs <- locations[grepl("[*?]", locations)]
    if (length(globs) > 0)
        keep <- keep | Reduce(`|`, lapply(globs, function(g)
            grepl(utils::glob2rx(g), long$location)))

    if (!any(keep))
        stop("No rows match locations = ", paste(locations, collapse = ", "),
             ".\nAvailable: ", paste(sort(unique(long$location)), collapse = ", "))

    long[keep, , drop = FALSE]
}


#' Keep only the requested strata.
#' @noRd
.filter_subgroup <- function(long, subgroup) {
    if (is.null(subgroup)) return(long)
    if (all(is.na(long$subgroup)))
        stop("'subgroup' supplied but the table has no stratification column. ",
             "Build it with data = list(total_raw_results, sex_results).")

    keep <- as.character(long$subgroup) %in% subgroup
    if (!any(keep))
        stop("No rows match subgroup = ", paste(subgroup, collapse = ", "),
             ".\nAvailable: ",
             paste(sort(unique(na.omit(long$subgroup))), collapse = ", "))

    long[keep, , drop = FALSE]
}


#' Same rule as .series_levels(), but returning location levels only
#' @noRd
.location_order <- function(present, locations, order.by, value.order) {
    mode <- if (!is.null(order.by))
        match.arg(order.by, c("input", "value", "alpha"))
    else if (!is.null(locations)) "input" else "value"

    u   <- unique(as.character(present))
    lev <- switch(mode,
                  input = c(intersect(as.character(locations), u),
                            setdiff(u, as.character(locations))),
                  value = as.character(value.order),
                  alpha = sort(u))
    c(intersect(lev, u), setdiff(u, lev))
}


#' Decide the order of the rows / series of a figure
#'
#' ONE RULE FOR EVERY FIGURE, so panels can be compared side by side:
#'   * pass `locations` and that order is used, in all five figures;
#'   * pass nothing and the figure falls back to its own value-based ranking
#'     (coverage needed, terminal impact, and so on);
#'   * `order.by` overrides either way -- "input", "value" or "alpha".
#'
#' Within a location, strata follow the order you gave in `subgroup`.
#' A location named in `locations` but absent from the data is skipped; one
#' present but not named is appended, so a level can never be lost.
#'
#' @param df Data frame carrying `location`, `subgroup` and `series` columns.
#' @param value.order Series labels in the figure's own ranking, best first.
#' @noRd
.series_levels <- function(df, locations, subgroup, order.by, value.order) {

    mode <- if (!is.null(order.by))
        match.arg(order.by, c("input", "value", "alpha"))
    else if (!is.null(locations)) "input" else "value"

    if (mode == "value") return(as.character(value.order))

    u.loc   <- unique(as.character(df$location))
    loc.lev <- if (mode == "alpha") sort(u.loc)
               else c(intersect(as.character(locations), u.loc),
                      setdiff(u.loc, as.character(locations)))
    loc.lev <- c(intersect(loc.lev, u.loc), setdiff(u.loc, loc.lev))

    u.sub   <- unique(as.character(df$subgroup))
    sub.lev <- if (is.null(subgroup)) u.sub
               else c(intersect(as.character(subgroup), u.sub),
                      setdiff(u.sub, as.character(subgroup)))

    o <- order(match(as.character(df$location), loc.lev),
               match(as.character(df$subgroup), sub.lev))
    unique(as.character(df$series)[o])
}


#' Row / series label: the city on its own, or "City - stratum" when more than
#' one stratum is on the plot.
#' @noRd
.series_label <- function(long, row.sep = " — ") {
    if (dplyr::n_distinct(long$subgroup) > 1 && !all(is.na(long$subgroup)))
        paste0(long$location, row.sep, long$subgroup)
    else
        as.character(long$location)
}


#' Append the stratum to a title when exactly one was selected
#' @noRd
.strat_suffix <- function(subgroup)
    if (!is.null(subgroup) && length(subgroup) == 1) paste0(" (", subgroup, ")") else ""


#' Write a figure if a filename was supplied -- same rule as save_table_csv()
#' @noRd
.save_fig <- function(p, save.dir, filename, width, height, dpi, save = NULL) {
    path <- .resolve_out_path(save.dir, filename, "png", save)
    if (!is.null(path)) {
        # bg = "white": without it the PNG can carry a transparent
        # background, which renders as black in some viewers and in slides
        ggsave(path, p, width = width, height = height, dpi = dpi, bg = "white")
        message("Figure written to: ", normalizePath(path, winslash = "/"))
    }
    invisible(p)
}


#' Build a labelling function for stratum display names
#'
#' Returns identity when `map` is NULL or empty. Levels absent from `map` are
#' passed through unchanged rather than becoming NA.
#' @noRd
.strat_labeller <- function(map) {
    if (is.null(map) || length(map) == 0) return(function(x) x)
    function(x) {
        x   <- as.character(x)
        out <- unname(map[x])
        ifelse(is.na(out), x, out)
    }
}


#' Build a labelling function from a flexible spec
#'
#' Accepts NULL (identity), a function, a named character vector (value -> label
#' lookup, unmapped values pass through), or a single unnamed string used as a
#' template in which `{x}` is replaced by the value.
#' @noRd
.make_labeller <- function(spec) {
    if (is.null(spec)) return(function(x) as.character(x))
    if (is.function(spec)) return(function(x) as.character(spec(x)))
    if (!is.null(names(spec)) && any(nzchar(names(spec))))
        return(.strat_labeller(spec))
    if (length(spec) == 1)
        return(function(x) vapply(as.character(x),
                                  function(v) gsub("{x}", v, spec, fixed = TRUE),
                                  character(1), USE.NAMES = FALSE))
    stop("Label spec must be NULL, a function, a named vector, or a single template string.")
}


#' Shared front end for every figure: long form, then the two row filters.
#' @noRd
.prep_long <- function(tbl, location.col, locations, subgroup, col.pattern,
                       cov.pattern, keep.ci = FALSE) {
    long <- table_to_long(tbl, location.col = location.col,
                          col.pattern = col.pattern, cov.pattern = cov.pattern,
                          keep.ci = keep.ci)

    # a blank location means the table was built with
    # repeat.location.label = FALSE, which is a print-only format
    if (any(is.na(long$location) | !nzchar(as.character(long$location))))
        stop("Some rows have a blank location. That happens when the table was ",
             "built with repeat.location.label = FALSE, which is for printing ",
             "only. Rebuild it with repeat.location.label = TRUE.")

    long <- .filter_subgroup(long, subgroup)
    long <- .filter_locations(long, locations)
    if (nrow(long) == 0) stop("No rows left after filtering.")
    long
}

#' Ten hues for one-line-per-location figures
#'
#' Slots 1-8 are a validated categorical palette; 9-10 are appended for the
#' ten-MSA figures. At ten overlapping series NO palette is reliably
#' colourblind-safe, which is why plot_trend_by_location() labels the lines
#' by default: the label is the identity channel and colour only helps the
#' eye follow one line, and trace a city between panels.
SHIELD.PALETTE.10 <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4",
                       "#008300", "#4a3aa7", "#e34948", "#8c564b", "#00a2c7")


#' One panel, one line per location -- the comparative view
#'
#' The companion to plot_trend_with_ci(), which facets by location and answers
#' "what does each city do". This function answers the other question -- "how
#' do the cities compare" -- which a facet grid cannot: free.y gives every
#' panel its own axis, and a shared axis is squeezed by the largest city.
#'
#' Deliberate differences from plot_trend_with_ci():
#'
#'   MEDIANS ONLY. Ten overlapping credible-interval ribbons is unreadable, so
#'   CI rows are dropped (with a message) and the intervals live in the table.
#'   Build with stat.type = "median" to silence the message.
#'
#'   NO FACETING, EVER. That is the point; there is no facet argument to set
#'   by accident.
#'
#'   ONE LINE PER LOCATION. If the filtered table still holds several series
#'   per city (>1 subgroup or intervention) that is an error, not a silently
#'   overplotted panel -- narrow it with `subgroup` / `interventions`.
#'
#'   COLOUR FOLLOWS THE CITY, NOT ITS RANK. Levels come from `locations` as
#'   supplied, or alphabetically -- never from the values. A city keeps its
#'   colour across panels whose orderings differ, which is what lets a
#'   multi-panel figure read as one figure.
#'
#' @param dashed Locations drawn dashed. Highlights an exception without
#'   spending a second colour on it, and unlike colour it survives greyscale
#'   printing.
#' @param label.lines Print each city's name at the end of its line. TRUE by
#'   default and effectively required beyond ~8 series. Uses ggrepel to avoid
#'   collisions when installed, a plain offset when not.
#' @param target Optional horizontal reference line, e.g. 1 on a fold-change
#'   panel, where it separates growth from decline.
#' @param log.y Draw the y axis on a log10 scale. Two things it buys, and one
#'   it costs:
#'
#'   It un-squashes a wide range. Incidence rates run ~50 to ~960 per 100,000,
#'   so on a linear axis the low-burden cities and the whole 2022 baseline
#'   flatten onto the bottom of the panel and a decline of 67 -> 50 is
#'   invisible beside a rise of 182 -> 960.
#'
#'   It makes the SHAPE readable: on a log axis a constant growth rate is a
#'   straight line, so acceleration and deceleration can be read off directly,
#'   and equal vertical distances are equal PROPORTIONAL change. On a
#'   fold-change panel this also makes the axis symmetric about the reference
#'   line -- a doubling and a halving are the same distance from 1.
#'
#'   The cost is that readers routinely misread log axes, and the values no
#'   longer correspond to evenly spaced gridlines the way a table does. The
#'   axis title gains "(log scale)" automatically for that reason. Zero and
#'   negative values are dropped by log10 -- a warning says how many.
#' @param y.breaks Explicit y breaks. Worth setting with `log.y`, where the
#'   default breaks are often too sparse, e.g. c(50, 100, 200, 400, 800).
#' @param x.breaks Explicit x (year) breaks. NULL derives them from the data
#'   and then CLIPS them to the observed year range. That clipping matters:
#'   scales::breaks_pretty() wraps base pretty(), which is documented not to
#'   keep its breaks inside the data range, and ggplot computes a panel's
#'   breaks over the EXPANDED range rather than the data range -- so the gap
#'   left on the right for the direct labels could otherwise produce a tick
#'   for a year that was never modelled (2032 on a 2022-2030 panel). On a
#'   projection figure that is not cosmetic: the axis would imply results
#'   that do not exist.
#' @param y.limits Zoom the y axis, as c(low, high). Applied with
#'   coord_cartesian(), which CLIPS the view: lines that leave the range are
#'   cut at the edge and re-enter, and every point still contributes to the
#'   panel. The obvious alternative, scale_y_continuous(limits = ...), does
#'   something quite different -- it DROPS every observation outside the range
#'   before anything is drawn, so a trajectory that briefly exceeds the limit
#'   is broken into disconnected pieces and any summary layer is silently
#'   recomputed on the survivors. That is a good way to publish a figure that
#'   disagrees with its own table, which is why this argument does not expose
#'   it. Note that zooming hides data by design: say so in the caption, and
#'   prefer `log.y` when the aim is only to stop a large series squashing the
#'   small ones.
plot_trend_by_location <- function(tbl,
                                   locations     = NULL,
                                   subgroup      = NULL,
                                   interventions = NULL,
                                   outcome       = NULL,
                                   year.range    = NULL,
                                   dashed        = NULL,
                                   label.lines   = TRUE,
                                   label.size    = 3.0,
                                   palette       = SHIELD.PALETTE.10,
                                   loc.labels    = NULL,
                                   target        = NULL,
                                   log.y         = FALSE,
                                   y.breaks      = NULL,
                                   y.limits      = NULL,
                                   x.breaks      = NULL,
                                   x.lab         = "Year",
                                   y.lab         = NULL,
                                   title         = NA,
                                   location.col  = "location",
                                   col.pattern   = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                   cov.pattern   = "doxy\\.cov\\.(\\d+)",
                                   save.dir      = .default_fig_dir(),
                                   filename      = NULL,
                                   width = 6, height = 4.5, dpi = 300) {

    # keep.ci = FALSE: medians only, by design
    long <- .prep_long(tbl, location.col, locations, subgroup,
                       col.pattern, cov.pattern, keep.ci = FALSE)

    # ---- exactly one outcome ----------------------------------------------
    if (!is.null(outcome)) {
        .outcome <- outcome
        long <- long %>% filter(outcome == .outcome)
        if (nrow(long) == 0) stop("Outcome '", .outcome, "' not present in table.")
    }
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")

    # ---- remaining subsets -------------------------------------------------
    if (!is.null(interventions)) {
        miss <- setdiff(interventions, unique(long$intervention))
        if (length(miss))
            stop("Intervention(s) not in table: ", paste(miss, collapse = ", "),
                 ".\nAvailable: ", paste(unique(long$intervention), collapse = ", "))
        long <- long %>% filter(intervention %in% interventions)
    }
    if (!is.null(year.range))
        long <- long %>% filter(year >= min(year.range), year <= max(year.range))

    if (nrow(long) == 0) stop("No rows left after filtering.")
    if (dplyr::n_distinct(long$year) < 2)
        stop("Need >= 2 years to draw a trajectory. Build the table with, say, ",
             "years = as.character(2022:2030).")

    # ---- one line per location, or say so ----------------------------------
    dup <- duplicated(paste(long$location, long$year, sep = "\r"))
    if (any(dup))
        stop("More than one series per location per year: this figure draws ONE ",
             "line per city. Narrow the table with 'subgroup' and/or ",
             "'interventions' first.\n  subgroups present: ",
             paste(unique(as.character(long$subgroup)), collapse = ", "),
             "\n  interventions present: ",
             paste(unique(as.character(long$intervention)), collapse = ", "))

    # ---- colour follows the city, never its rank ---------------------------
    lev <- if (!is.null(locations))
               c(intersect(locations, unique(as.character(long$location))),
                 setdiff(unique(as.character(long$location)), locations))
           else sort(unique(as.character(long$location)))
    long$location <- factor(as.character(long$location), levels = lev)

    if (length(lev) > length(palette))
        warning(length(lev), " locations but only ", length(palette), " hues. ",
                "Colours will repeat -- pass a longer 'palette', or split the ",
                "figure.", call. = FALSE)
    pal <- stats::setNames(rep_len(palette, length(lev)), lev)

    bad <- setdiff(dashed, lev)
    if (length(bad))
        warning("dashed: no such location: ", paste(bad, collapse = ", "),
                call. = FALSE)
    long$.lt <- ifelse(as.character(long$location) %in% dashed, "dashed", "solid")

    # ---- labels ------------------------------------------------------------
    lab.fn <- .make_labeller(loc.labels)
    if (is.null(y.lab)) y.lab <- unique(as.character(long$outcome))
    # never let a log axis go unlabelled as one
    if (log.y && !grepl("log", y.lab, ignore.case = TRUE))
        y.lab <- paste0(y.lab, " (log scale)")
    if (length(title) == 1 && is.na(title)) title <- NULL

    # ---- plot --------------------------------------------------------------
    p <- ggplot(long, aes(x = year, y = value,
                          colour = location, group = location))

    if (!is.null(target))
        p <- p + geom_hline(yintercept = target, linetype = "dashed",
                            colour = "grey35", linewidth = 0.4)

    p <- p +
        geom_line(aes(linetype = .lt), linewidth = 0.9) +
        scale_linetype_manual(values = c(solid = "solid", dashed = "22"),
                              guide = "none") +
        scale_colour_manual(values = pal, name = NULL, labels = lab.fn)

    if (label.lines) {
        ends <- long[long$year == max(long$year), ]
        p <- p + if (requireNamespace("ggrepel", quietly = TRUE))
                     ggrepel::geom_text_repel(
                         data = ends, aes(label = lab.fn(as.character(location))),
                         size = label.size, hjust = 0, direction = "y",
                         nudge_x = 0.45, segment.size = 0.2,
                         min.segment.length = 0, seed = 1, show.legend = FALSE)
                 else
                     geom_text(data = ends,
                               aes(label = lab.fn(as.character(location))),
                               size = label.size, hjust = 0, nudge_x = 0.15,
                               show.legend = FALSE)
    }

    # ---- y scale -----------------------------------------------------------
    if (log.y) {
        n.bad <- sum(long$value <= 0, na.rm = TRUE)
        if (n.bad > 0)
            warning("log.y = TRUE: log10 is undefined at zero and below, so ",
                    n.bad, " value(s) will be dropped from the panel.",
                    call. = FALSE)
        p <- p + scale_y_log10(
            breaks = if (is.null(y.breaks)) waiver() else y.breaks,
            labels = scales::comma)
    } else if (!is.null(y.breaks)) {
        p <- p + scale_y_continuous(breaks = y.breaks)
    }

    if (!is.null(y.limits)) {
        if (length(y.limits) != 2 || any(is.na(y.limits)))
            stop("y.limits must be c(low, high).")
        n.out <- sum(long$value < min(y.limits) | long$value > max(y.limits),
                     na.rm = TRUE)
        if (n.out > 0)
            message("y.limits: ", n.out, " point(s) fall outside the view and ",
                    "are clipped, not dropped. Note the zoom in the caption.")
        p <- p + coord_cartesian(ylim = y.limits)
    }

    # ---- x breaks: never invent a year the model did not run --------------
    if (is.null(x.breaks)) {
        yr.rng   <- range(long$year, na.rm = TRUE)
        x.breaks <- scales::breaks_pretty(4)(yr.rng)
        x.breaks <- x.breaks[x.breaks >= yr.rng[1] & x.breaks <= yr.rng[2]]
        # the direct labels sit at the final year, so always tick it
        if (max(x.breaks) < yr.rng[2]) x.breaks <- c(x.breaks, yr.rng[2])
    }

    p <- p +
        scale_x_continuous(
            breaks = x.breaks,
            # room on the right for the direct labels
            expand = if (label.lines) expansion(mult = c(0.02, 0.20))
                     else waiver()) +
        labs(x = x.lab, y = y.lab, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              # a legend repeating the direct labels is redundant clutter
              legend.position  = if (label.lines) "none" else "bottom")

    .save_fig(p, save.dir, filename, width, height, dpi)
    p
}



#' Keep one year, with a helpful message when it isn't there
#' @noRd
.filter_year <- function(long, year) {
    if (is.null(year)) return(long)
    keep <- long$year %in% year
    if (!any(keep))
        stop("No rows for year = ", paste(year, collapse = ", "),
             ".\nAvailable: ", paste(sort(unique(long$year)), collapse = ", "))
    long[keep, , drop = FALSE]
}


# ============================================================================
# 4. FIGURES
# ============================================================================

# ----------------------------------------------------------------------------
# HEAT MAP: locations (x strata) down the side, coverage across the top
# ----------------------------------------------------------------------------
#' @param tbl Wide table from make_multi_location_table(), or a long one.
#' @param locations,subgroup Optional row filters.
#' @param year Optional year filter. NULL keeps every year in the table and
#'   facets when there is more than one.
#' @param midpoint Value placed at the neutral (white) colour.
#' @param threshold Value used for row ordering; defaults to `midpoint`.
#' @param limits Fill scale bounds. NULL uses c(0, 100).
#' @param higher.is.better FALSE flips the palette and the ordering test.
#' @param order.rows "threshold" (lowest coverage reaching `threshold`), "max",
#'   "alpha", or "none".
#' @param row.sep Separator used when both location and subgroup label a row.
#' @param save.dir,filename Where to write the figure and what to call it.
#'   `filename` is the switch: give one and a PNG is written, leave it NULL
#'   (the default) and nothing is. The ".png" extension is appended for you and
#'   `save.dir` defaults to FIG.DIR when your driver script has set it. Same
#'   two arguments, same behaviour, as the table functions.
#' Blend a colour towards white. amount 0 = unchanged, 1 = white.
#' @noRd
.lighten <- function(hex, amount) {
    v <- grDevices::col2rgb(hex) / 255
    grDevices::rgb(t(v + (1 - v) * amount))
}

#' WCAG relative luminance of one or more colours
#' @noRd
.rel_lum <- function(hex) {
    m   <- grDevices::col2rgb(hex) / 255
    lin <- ifelse(m <= 0.03928, m / 12.92, ((m + 0.055) / 1.055)^2.4)
    as.numeric(0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ])
}

#' Pick white or near-black text for a given fill, by contrast ratio
#'
#' A shaded band runs from a pale end to a saturated one, so no single text
#' colour works across it -- this picks per cell instead of guessing.
#' @noRd
.text_on <- function(hex) {
    L <- .rel_lum(hex)
    ifelse(1.05 / (L + 0.05) >= (L + 0.05) / 0.0694, "white", "grey15")
}


plot_coverage_heatmap <- function(tbl,
                                  location.col = "location",
                                  locations    = NULL,
                                  subgroup     = NULL,
                                  order.by     = NULL,
                                  year         = NULL,
                                  col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  cov.pattern  = "doxy\\.cov\\.(\\d+)",
                                  midpoint     = 50,
                                  threshold    = midpoint,
                                  limits       = NULL,
                                  higher.is.better = TRUE,
                                  order.rows   = c("threshold", "max", "alpha", "none"),
                                  fill.style   = c("diverging", "banded"),
                                  band.breaks  = NULL,
                                  # SHIELD FIGURE PALETTE, top of this file
                                  band.colours = SHIELD.HEAT.COLS,
                                  band.shade   = SHIELD.HEAT.SHADE,
                                  band.light   = 0.78,
                                  legend.dir   = c("vertical", "horizontal"),
                                  legend.breaks = NULL,
                                  # "auto" marks a squished end with <= / >=
                                  # only when THIS panel's data runs past the
                                  # limit. That makes the labels depend on the
                                  # data, so two panels sharing a scale can end
                                  # up with different legends -- and patchwork's
                                  # guides = "collect" then cannot merge them,
                                  # which is how a 2x2 ends up with two
                                  # colourbars. Force "always" or "never" when
                                  # panels must share one legend.
                                  squish.marks = c("auto", "always", "never"),
                                  legend.labels = NULL,
                                  label.digits = 0,
                                  show.labels  = TRUE,
                                  # NULL picks per cell by contrast against the
                                  # fill; a colour string uses that everywhere
                                  label.colour = NULL,
                                  row.sep      = " — ",
                                  title        = NULL,
                                  x.lab        = "Doxy-PEP coverage (%)",
                                  fill.lab     = NULL,
                                  # TRUE locks the panel's aspect with
                                  # coord_fixed(), which is what leaves an
                                  # empty margin when the saved width/height
                                  # do not match that aspect. Set FALSE for a
                                  # panel going into a patchwork grid, so the
                                  # tiles stretch to fill their cell instead.
                                  fixed.aspect = TRUE,
                                  save.dir     = .default_fig_dir(),
                                  filename     = NULL,
                                  width = 8, height = 5, dpi = 300) {

    order.rows <- match.arg(order.rows)
    fill.style <- match.arg(fill.style)
    legend.dir   <- match.arg(legend.dir)
    squish.marks <- match.arg(squish.marks)

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)
    long <- .require_coverage(long, cov.pattern)

    long <- .filter_year(long, year)

    subgroup.order <- subgroup          # shadowed by the column inside dplyr
    long$row.id <- .series_label(long, row.sep)

    # ---- row ordering ------------------------------------------------------
    # `order.rows` picks WHICH value ranking to use; `order.by` decides whether
    # a value ranking is used at all (see .series_levels).
    crossed <- function(v) if (higher.is.better) v >= threshold else v <= threshold
    ord <- long %>%
        group_by(row.id) %>%
        summarise(cross = suppressWarnings(min(coverage[crossed(value)])),
                  best  = if (higher.is.better) max(value, na.rm = TRUE)
                          else min(value, na.rm = TRUE),
                  .groups = "drop")
    value.order <- switch(order.rows,
                        threshold = ord %>% arrange(cross, if (higher.is.better) desc(best) else best) %>% pull(row.id),
                        max       = ord %>% arrange(if (higher.is.better) desc(best) else best) %>% pull(row.id),
                        alpha     = sort(unique(long$row.id)),
                        none      = unique(long$row.id))
    row.order <- .series_levels(
        dplyr::distinct(long, location, subgroup, series = row.id),
        locations, subgroup.order, order.by, value.order)

    long <- long %>%
        mutate(row.id   = factor(row.id, levels = rev(row.order)),
               coverage = factor(coverage, levels = sort(unique(coverage))))

    # ---- fill scale --------------------------------------------------------
    # "banded" cuts the values at meaningful thresholds instead of shading them
    # continuously. Two reasons to prefer it here:
    #
    #   The default continuous scale runs limits = c(0, 100) with
    #   oob = squish, so a NEGATIVE value -- the intervention leaving a group
    #   worse off -- is clamped to 0 and painted the same as no effect at all.
    #   A band below zero gives that its own colour.
    #
    #   The cut points are the ones the figure already reasons about: 0
    #   separates benefit from harm, and `threshold` is the policy target that
    #   crossed() and the row ordering already use. Banding makes "did this
    #   city clear the target" a colour rather than a value to be read off.
    #
    # The cost is that magnitude within a band is no longer encoded, which is
    # why show.labels should stay TRUE (see below -- it is also what makes the
    # red/green pair legitimate).
    if (fill.style == "banded") {
        if (is.null(band.breaks)) band.breaks <- c(0, threshold)
        band.breaks <- sort(unique(band.breaks))
        if (length(band.colours) != length(band.breaks) + 1L)
            stop("band.colours needs one more entry than band.breaks: ",
                 length(band.breaks) + 1L, " colours for breaks ",
                 paste(band.breaks, collapse = ", "), ", got ",
                 length(band.colours), ".")

        # The default limits of c(0, 100) would squish every negative value
        # onto the bottom of the scale, which is the thing this style exists
        # to stop -- so span the data instead.
        if (is.null(limits))
            limits <- range(c(long$value, band.breaks), na.rm = TRUE)

        if (band.shade) {
            # A piecewise ramp: the hue changes abruptly at every break, and
            # within a band the colour runs pale -> saturated so magnitude is
            # still readable. Band 1 is reversed (darkest at the LOW end), so
            # "more negative" and "more positive" both read as more intense.
            edges    <- c(limits[1], band.breaks, limits[2])
            stop.pos <- numeric(0); stop.col <- character(0)
            for (k in seq_along(band.colours)) {
                base <- unname(band.colours[k])
                pale <- .lighten(base, band.light)
                stop.col <- c(stop.col, if (k == 1) c(base, pale) else c(pale, base))
                stop.pos <- c(stop.pos, edges[k], edges[k + 1])
            }
            stop.pos <- scales::rescale(stop.pos, from = limits)
            # gradient_n_pal() needs strictly increasing stops; the duplicated
            # boundary is what makes the hue change a step rather than a blend
            for (s in seq_along(stop.pos)[-1])
                if (stop.pos[s] <= stop.pos[s - 1])
                    stop.pos[s] <- stop.pos[s - 1] + 1e-6

            # Legend ticks at the band edges, because those are the values
            # that mean something. Where data is being squished onto an end,
            # the tick says so -- otherwise "-100" reads as the minimum
            # observed rather than "everything at or below this".
            if (is.null(legend.breaks))
                legend.breaks <- sort(unique(c(limits[1], band.breaks, limits[2])))
            mark.lo <- switch(squish.marks,
                              always = TRUE, never = FALSE,
                              auto   = any(long$value < limits[1], na.rm = TRUE))
            mark.hi <- switch(squish.marks,
                              always = TRUE, never = FALSE,
                              auto   = any(long$value > limits[2], na.rm = TRUE))

            legend.labs <- format(legend.breaks, trim = TRUE)
            if (mark.lo)
                legend.labs[legend.breaks == limits[1]] <-
                    paste0("\u2264 ", format(limits[1], trim = TRUE))
            if (mark.hi)
                legend.labs[legend.breaks == limits[2]] <-
                    paste0("\u2265 ", format(limits[2], trim = TRUE))

            # explicit labels win outright -- the last resort for making
            # several panels present byte-identical guides
            if (!is.null(legend.labels)) {
                if (length(legend.labels) != length(legend.breaks))
                    stop("legend.labels must have one entry per legend break (",
                         length(legend.breaks), " needed, got ",
                         length(legend.labels), ").")
                legend.labs <- legend.labels
            }

            band.pal   <- scales::gradient_n_pal(stop.col, values = stop.pos)
            long$.txt  <- .text_on(band.pal(scales::rescale(
                              scales::squish(long$value, limits), from = limits)))
        } else {
            band.labs <- c(
                paste0("< ", band.breaks[1]),
                if (length(band.breaks) > 1)
                    paste0(utils::head(band.breaks, -1), " to <", band.breaks[-1]),
                paste0("\u2265 ", band.breaks[length(band.breaks)]))
            # right = FALSE so the top band is [threshold, Inf), matching the
            # crossed() test above, which counts v >= threshold as crossed
            long$.band <- cut(long$value, breaks = c(-Inf, band.breaks, Inf),
                              labels = band.labs, right = FALSE)
            names(band.colours) <- band.labs
        }
    }

    if (is.null(limits)) limits <- c(0, 100)
    # RdBu, not red/green: same family as the banded scale above, and legible
    # under colour-vision deficiency. PAL.WOMEN/PAL.MSM are the same two hexes.
    pal <- if (higher.is.better) c(PAL.WOMEN, PAL.MSM) else c(PAL.MSM, PAL.WOMEN)

    if (is.null(fill.lab)) fill.lab <- paste(unique(long$outcome), collapse = " / ")

    if (is.null(title) && dplyr::n_distinct(long$year) == 1) {
        title <- paste0(fill.lab, ", ", unique(long$year))
        if (!is.null(subgroup) && length(subgroup) == 1)
            title <- paste0(title, " (", subgroup, ")")
    }

    banded.flat <- fill.style == "banded" && !band.shade
    fill.col    <- if (banded.flat) ".band" else "value"

    p <- ggplot(long, aes(x = coverage, y = row.id, fill = .data[[fill.col]])) +
        geom_tile(color = "white", linewidth = 0.6)

    p <- p + if (banded.flat)
                 scale_fill_manual(values = band.colours, name = fill.lab,
                                   drop = FALSE, na.value = "grey90")
             else if (fill.style == "banded")
                 scale_fill_gradientn(
                     colours = stop.col,
                     values  = stop.pos,
                     limits  = limits,
                     oob     = scales::squish,
                     name    = fill.lab,
                     breaks  = legend.breaks,
                     labels  = legend.labs,
                     guide   = if (legend.dir == "horizontal")
                                   guide_colourbar(direction = "horizontal",
                                                   title.position = "top",
                                                   barwidth  = unit(7, "cm"),
                                                   barheight = unit(0.45, "cm"))
                               else
                                   guide_colourbar(barwidth  = unit(0.5, "cm"),
                                                   barheight = unit(4.5, "cm")))
             else
                 scale_fill_gradientn(colours = c(pal[1], "#F7F7F7", pal[2]),
                                      values  = scales::rescale(
                                          c(limits[1], midpoint, limits[2]),
                                          from = limits),
                                      limits  = limits,
                                      oob     = scales::squish,
                                      name    = fill.lab)

    p <- p +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 13) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(face = "bold", size = 12),
              legend.position = if (legend.dir == "horizontal") "bottom" else "right",
              # the colourbar guides above set their own size in banded mode;
              # this stays for the continuous default
              legend.key.height = if (fill.style == "banded") NULL
                                  else unit(1.2, "cm"))

    if (show.labels) {
        val.lab <- aes(label = format(round(value, label.digits),
                                      nsmall = label.digits))
        if (!is.null(label.colour)) {
            # One colour for every cell. Mixed black/white labels are chosen
            # for contrast, but they read as if they encode something, so a
            # single colour is often the better call -- CHECK IT AGAINST THE
            # DARKEST FILL FIRST. The old "black is fine everywhere" note was
            # measured against the legacy red/blue/green bands and does NOT
            # carry over: on the rdbu bands, black over #2166AC is about 3.5:1.
            # Pass label.colour = NULL for per-cell contrast instead.
            p <- p + geom_text(val.lab, colour = label.colour, size = 3.4,
                               fontface = "bold", show.legend = FALSE)
        } else if (banded.flat) {
            # NOTE white is hardcoded here, which assumes all three bands are
            # dark. True of the legacy red/blue/green bands (3.9 / 4.4 / 4.9
            # against white); NOT true of any palette with a pale middle band,
            # including the rdbu default, whose #92C5DE mid-band would render
            # white numbers essentially invisible. Use band.shade = TRUE with
            # those, or pass an explicit label.colour.
            p <- p + geom_text(val.lab, colour = "white", size = 3.4,
                               fontface = "bold", show.legend = FALSE)
        } else if (fill.style == "banded") {
            # shaded bands run pale -> saturated, so the text colour is chosen
            # per cell from the actual fill's luminance
            p <- p + geom_text(aes(label = format(round(value, label.digits),
                                                  nsmall = label.digits),
                                   colour = .txt),
                               size = 3.4, fontface = "bold",
                               show.legend = FALSE) +
                     scale_colour_identity()
        } else {
            p <- p +
                geom_text(aes(label = format(round(value, label.digits),
                                             nsmall = label.digits),
                              color = ifelse(value >= midpoint,
                                             (value - midpoint) / (limits[2] - midpoint),
                                             (midpoint - value) / (midpoint - limits[1])) > 0.55),
                          size = 3.4, fontface = "bold", show.legend = FALSE) +
                scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"))
        }
    }

    n.facet <- dplyr::n_distinct(paste(long$outcome, long$year))
    if (n.facet > 1)          p <- p + facet_wrap(~ outcome + year, scales = "free_x")
    else if (fixed.aspect)    p <- p + coord_fixed(ratio = 0.75)

    .save_fig(p, save.dir, filename, width, height, dpi)
    p
}


# ----------------------------------------------------------------------------
# FIGURE 1: Ranked bar -- coverage needed to reach the target
# ----------------------------------------------------------------------------
#' Minimum coverage required to reach a target impact, ranked
#'
#' One bar per series, where a series is a location, or a location x subgroup
#' pair when the table holds more than one stratum. (It used to be one bar per
#' LOCATION regardless, which silently took the best stratum.)
#'
#' @param target Impact threshold, e.g. 50 for a 50% reduction.
#' @param year Which year to evaluate. Defaults to the latest in the data.
#' @param subgroup Optional stratum filter, e.g. "msm".
#' @param higher.is.better TRUE if larger values are the goal (e.g. % averted).
#' @param locations Optional subset of locations to show.
#' @param save.dir,filename Where to write the figure and what to call it.
#'   `filename` is the switch: give one and a PNG is written, leave it NULL
#'   (the default) and nothing is. The ".png" extension is appended for you and
#'   `save.dir` defaults to FIG.DIR when your driver script has set it. Same
#'   two arguments, same behaviour, as the table functions.
plot_coverage_needed <- function(tbl,
                                 target       = 50,
                                 year         = NULL,
                                 locations    = NULL,
                                 subgroup     = NULL,
                                 order.by     = NULL,
                                 higher.is.better = TRUE,
                                 location.col = "location",
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 cov.pattern  = "doxy\\.cov\\.(\\d+)",
                                 row.sep      = " — ",
                                 title        = NULL,
                                 x.lab        = "Coverage required (%)",
                                 bar.fill     = PAL.BAR,   # SHIELD FIGURE PALETTE
                                 unreached.lab = "not reached",
                                 save.dir     = .default_fig_dir(),
                                 filename     = NULL,
                                 width = 7, height = 4.5, dpi = 300) {

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)
    long <- .require_coverage(long, cov.pattern)

    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- .filter_year(long, year)

    subgroup.order <- subgroup          # shadowed by the column inside dplyr
    long$series <- .series_label(long, row.sep)

    max.cov <- max(long$coverage, na.rm = TRUE)
    hit <- function(v) if (higher.is.better) v >= target else v <= target

    summ <- long %>%
        group_by(location, subgroup, series) %>%
        summarise(cov.needed = suppressWarnings(min(coverage[hit(value)])),
                  best       = if (higher.is.better) max(value, na.rm = TRUE)
                               else min(value, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(reached  = is.finite(cov.needed),
               bar.len  = ifelse(reached, cov.needed, max.cov),
               lab      = ifelse(reached, paste0(cov.needed, "%"),
                                 paste0(unreached.lab, " (max ",
                                        round(best), "%)")))

    # this figure's own ranking: reached first, then by the coverage needed
    value.order <- summ %>%
        arrange(desc(reached), cov.needed, desc(best)) %>% pull(series)
    summ <- summ %>%
        mutate(series = factor(series,
                               levels = rev(.series_levels(summ, locations,
                                                           subgroup.order, order.by,
                                                           value.order))))

    if (is.null(title))
        title <- paste0("Doxy-PEP coverage needed to reach ", target,
                        "% reduction by ", year, .strat_suffix(subgroup))

    p <- ggplot(summ, aes(x = bar.len, y = series)) +
        geom_col(aes(fill = reached, color = reached),
                 linewidth = 0.6, width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = lab, hjust = ifelse(reached, -0.15, 1.05),
                      color = reached),
                  size = 3.3, fontface = "bold", show.legend = FALSE) +
        scale_fill_manual(values  = c(`TRUE` = bar.fill, `FALSE` = PAL.OFF)) +
        scale_color_manual(values = c(`TRUE` = bar.fill, `FALSE` = "grey45")) +
        scale_x_continuous(limits = c(0, max.cov * 1.25),
                           breaks = seq(0, max.cov, by = 20), expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor   = element_blank(),
              axis.ticks         = element_blank(),
              plot.title         = element_text(face = "bold", size = 12))

    .save_fig(p, save.dir, filename, width, height, dpi)
    p
}


# ----------------------------------------------------------------------------
# FIGURE 2: Dose-response -- impact vs coverage, at a fixed year
# ----------------------------------------------------------------------------
#' One line per series (location, or location x subgroup when several strata
#' are present), ordered by the impact reached at the highest coverage.
#' @param save.dir,filename Where to write the figure and what to call it.
#'   `filename` is the switch: give one and a PNG is written, leave it NULL
#'   (the default) and nothing is. The ".png" extension is appended for you and
#'   `save.dir` defaults to FIG.DIR when your driver script has set it. Same
#'   two arguments, same behaviour, as the table functions.
plot_dose_response <- function(tbl,
                               target       = 50,
                               year         = NULL,
                               locations    = NULL,
                               subgroup     = NULL,
                               order.by     = NULL,
                               location.col = "location",
                               col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               cov.pattern  = "doxy\\.cov\\.(\\d+)",
                               row.sep      = " — ",
                               title        = NULL,
                               x.lab        = "Doxy-PEP coverage (%)",
                               y.lab        = NULL,
                               direct.label = TRUE,
                               palette      = NULL,
                               save.dir     = .default_fig_dir(),
                               filename     = NULL,
                               width = 7.5, height = 5, dpi = 300) {

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)
    long <- .require_coverage(long, cov.pattern)

    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- .filter_year(long, year)

    subgroup.order <- subgroup          # shadowed by the column inside dplyr
    long$series <- .series_label(long, row.sep)

    # this figure's own ranking: terminal impact, so the key reads as a ranking
    value.order <- long %>%
        group_by(series) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(series)
    long <- long %>%
        mutate(series = factor(series,
                               levels = .series_levels(
                                   dplyr::distinct(long, location, subgroup, series),
                                   locations, subgroup.order, order.by, value.order)))

    ends <- long %>% group_by(series) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>% ungroup()

    if (is.null(y.lab)) y.lab <- paste0(unique(long$outcome), collapse = " / ")
    if (is.null(title))
        title <- paste0(y.lab, " by coverage level, ", year, .strat_suffix(subgroup))

    max.cov <- max(long$coverage, na.rm = TRUE)

    p <- ggplot(long, aes(x = coverage, y = value,
                          color = series, group = series)) +
        geom_hline(yintercept = target, linetype = "dashed",
                   color = "grey35", linewidth = 0.5) +
        annotate("text", x = 0, y = target, label = paste0(target, "% target"),
                 hjust = -0.05, vjust = -0.6, size = 3, color = "grey35") +
        geom_line(linewidth = 0.8) +
        geom_point(size = 1.6) +
        scale_x_continuous(breaks = sort(unique(long$coverage)),
                           limits = c(0, max.cov * ifelse(direct.label, 1.28, 1.02))) +
        labs(x = x.lab, y = y.lab, title = title, color = NULL) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (direct.label) "none" else "right")

    if (direct.label)
        p <- p + geom_text(data = ends, aes(label = series),
                           hjust = -0.15, size = 3.1, fontface = "bold",
                           show.legend = FALSE)

    if (!is.null(palette)) p <- p + scale_color_manual(values = palette)

    .save_fig(p, save.dir, filename, width, height, dpi)
    p
}


# ----------------------------------------------------------------------------
# FIGURE 3: Impact over time -- x axis is year
# ----------------------------------------------------------------------------
#' Trajectory plot of impact over time
#'
#' Draws one line per series, where a series is a location x coverage x stratum
#' combination. Colour goes to the STRATUM whenever the table holds more than
#' one (this is what `strat.palette` and `strat.labels` are for -- they used to
#' be unreachable); otherwise colour goes to `color.by` and the remaining
#' dimension becomes facets.
#'
#' Ordering: when `locations` and/or `subgroup` are supplied, their order is
#' respected in facets, legends and line stacking. Otherwise locations are
#' ranked by their endpoint value at the highest coverage.
#'
#' @param color.by Dimension mapped to colour when only one stratum is present.
#' @param locations,coverages,subgroup Optional subsets. Supplied order is kept.
#' @param strat.palette ColorBrewer palette used when the stratum takes colour.
#' @param strat.labels Named character vector mapping stratum values to display
#'   labels. Unmapped levels pass through unchanged; NULL keeps raw values.
#' @param year.range Two-element numeric range, inclusive.
#' @param target Horizontal reference line; NULL to omit.
#' @param outcome Required when the table holds more than one outcome.
#' @param title Overall plot title; NULL auto-generates one, NA suppresses it.
#' @param loc.labels Panel titles for location facets.
#' @param cov.label Panel titles for coverage facets; "{x}" is replaced by the
#'   coverage value.
#' @param show.strip FALSE hides all panel titles.
#' @param free.y Free y scales across facets.
#' @param direct.label End-of-line labels when colouring by location.
#' @param annotate.ends Endpoint value labels; single-series plots only.
#' @param save.dir,filename Where to write the figure and what to call it.
#'   `filename` is the switch: give one and a PNG is written, leave it NULL
#'   (the default) and nothing is. The ".png" extension is appended for you and
#'   `save.dir` defaults to FIG.DIR when your driver script has set it. Same
#'   two arguments, same behaviour, as the table functions.
plot_impact_over_time <- function(tbl,
                                  color.by      = c("coverage", "location"),
                                  locations     = NULL,
                                  coverages     = NULL,
                                  subgroup      = NULL,
                                  order.by      = NULL,
                                  year.range    = NULL,
                                  target        = 50,
                                  outcome       = NULL,
                                  location.col  = "location",
                                  col.pattern   = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  cov.pattern   = "doxy\\.cov\\.(\\d+)",
                                  facet.ncol    = NULL,
                                  loc.labels    = NULL,
                                  cov.label     = "{x}% coverage",
                                  show.strip    = TRUE,
                                  free.y        = FALSE,
                                  direct.label  = TRUE,
                                  annotate.ends = TRUE,
                                  strat.palette = "Set1",
                                  strat.labels  = c(Total             = "Total population",
                                                    msm               = "MSM",
                                                    heterosexual_male = "Heterosexual men",
                                                    female            = "Women"),
                                  x.lab         = "Year",
                                  y.lab         = NULL,
                                  title         = NULL,
                                  save.dir      = .default_fig_dir(),
                                  filename      = NULL,
                                  width = 10, height = 6, dpi = 300) {

    color.by <- match.arg(color.by)

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)
    long <- .require_coverage(long, cov.pattern)

    # ---- outcome ----------------------------------------------------------
    if (!is.null(outcome)) {
        .outcome <- outcome
        long <- long %>% filter(outcome == .outcome)
        if (nrow(long) == 0) stop("Outcome '", .outcome, "' not present in table.")
    }
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")

    # ---- remaining subsets -------------------------------------------------
    if (!is.null(coverages)) {
        miss <- setdiff(coverages, unique(long$coverage))
        if (length(miss)) stop("Coverage level(s) not in table: ", paste(miss, collapse = ", "))
        long <- long %>% filter(coverage %in% coverages)
    }
    if (!is.null(year.range))
        long <- long %>% filter(year >= min(year.range), year <= max(year.range))

    if (nrow(long) == 0) stop("No rows left after filtering.")
    if (dplyr::n_distinct(long$year) < 2)
        stop("Need >= 2 years. Build the table with years = as.character(2026:2035).")

    # ---- is the stratum in play? ------------------------------------------
    has.strat <- !all(is.na(long$subgroup)) && dplyr::n_distinct(long$subgroup) > 1

    # ---- ordering ---------------------------------------------------------
    # user-supplied order wins; anything unmatched is appended rather than dropped
    # this figure's own ranking: endpoint value at the highest coverage
    value.order <- long %>%
        filter(coverage == max(coverage)) %>%
        group_by(location) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(location) %>% as.character()
    ord.loc <- .location_order(long$location, locations, order.by, value.order)
    long <- long %>% mutate(location = factor(as.character(location), levels = ord.loc))

    if (has.strat) {
        u.str <- as.character(unique(long$subgroup))
        ord.str <- if (!is.null(subgroup))
            c(intersect(subgroup, u.str), setdiff(u.str, subgroup)) else u.str
        long$subgroup <- factor(as.character(long$subgroup), levels = ord.str)
    }

    # ---- series identifier, ordered location -> subgroup -> coverage ------
    if (has.strat) {
        long <- long %>%
            arrange(location, subgroup, coverage) %>%
            mutate(series = paste(location, subgroup, coverage, sep = "|"))
    } else {
        long <- long %>%
            arrange(location, coverage) %>%
            mutate(series = paste(location, coverage, sep = "|"))
    }
    long <- long %>% mutate(series = factor(series, levels = unique(series)))

    # ---- dimension counts -------------------------------------------------
    n.loc   <- dplyr::n_distinct(long$location)
    n.cov   <- dplyr::n_distinct(long$coverage)
    n.strat <- if (has.strat) dplyr::n_distinct(long$subgroup) else 1L
    single.line <- (n.loc == 1 && n.cov == 1 && n.strat == 1)

    # ---- colour and facet assignment --------------------------------------
    # stratum takes the colour channel whenever it varies; location and
    # coverage then compete for the facet channel
    strat.colour <- has.strat
    if (strat.colour) {
        facet.dims <- c(if (n.loc > 1) "location", if (n.cov > 1) "coverage")
    } else {
        fb <- setdiff(c("coverage", "location"), color.by)
        facet.dims <- if ((fb == "location" && n.loc > 1) ||
                          (fb == "coverage" && n.cov > 1)) fb else character(0)
    }

    # ---- labels -----------------------------------------------------------
    if (is.null(y.lab)) y.lab <- unique(long$outcome)
    if (length(title) == 1 && is.na(title)) {
        title <- NULL
    } else if (is.null(title)) {
        title <- if (single.line)
            paste0(y.lab, ": ", levels(droplevels(long$location))[1],
                   " at ", unique(long$coverage), "% coverage")
        else if (strat.colour)
            paste0(y.lab, " over time, by subgroup")
        else if (color.by == "coverage")
            paste0(y.lab, " over time, by coverage level", .strat_suffix(subgroup))
        else
            paste0(y.lab, " over time, by city", .strat_suffix(subgroup))
    }

    use.direct.label <- direct.label && !strat.colour &&
        color.by == "location" && !single.line
    max.yr <- max(long$year, na.rm = TRUE)
    pad    <- if (use.direct.label) 4 else 0

    # ---- base plot --------------------------------------------------------
    p <- ggplot(long, aes(x = year, y = value))

    if (!is.null(target))
        p <- p + geom_hline(yintercept = target, linetype = "dashed",
                            color = "grey35", linewidth = 0.4)

    if (single.line) {
        p <- p + geom_line(linewidth = 1, color = "#2166AC") +
            geom_point(size = 1.8, color = "#2166AC")
        if (annotate.ends) {
            ends <- long %>% filter(year %in% range(year))
            p <- p + geom_text(data = ends,
                               aes(label = paste0(round(value, 1), "%")),
                               vjust = -1, size = 3.2, fontface = "bold",
                               color = "#2166AC")
        }

    } else if (strat.colour) {
        p <- p +
            geom_line(aes(color = subgroup, group = series), linewidth = 0.9) +
            geom_point(aes(color = subgroup), size = 1.5) +
            scale_color_brewer(palette = strat.palette, name = NULL, drop = FALSE,
                               labels = .strat_labeller(strat.labels))

    } else if (color.by == "coverage") {
        p <- p +
            geom_line(aes(color = coverage, group = series), linewidth = 0.9) +
            scale_color_viridis_c(option = "C", end = 0.92,
                                  name = "Doxy-PEP\ncoverage (%)",
                                  breaks = sort(unique(long$coverage)))

    } else {
        p <- p + geom_line(aes(color = location, group = series), linewidth = 0.85)
        if (use.direct.label) {
            ends <- long %>% group_by(series) %>%
                slice_max(year, n = 1, with_ties = FALSE) %>% ungroup()
            p <- p + geom_text(data = ends, aes(label = location, color = location),
                               hjust = -0.1, size = 2.9, fontface = "bold",
                               show.legend = FALSE)
        }
    }

    # ---- faceting ---------------------------------------------------------
    loc.fn <- .make_labeller(loc.labels)
    cov.fn <- .make_labeller(cov.label)
    if (length(facet.dims) == 2) {
        p <- p + facet_grid(location ~ coverage,
                            labeller = labeller(location = loc.fn, coverage = cov.fn),
                            scales = if (free.y) "free_y" else "fixed")
    } else if (length(facet.dims) == 1) {
        lab.fn <- if (facet.dims == "coverage") as_labeller(cov.fn) else as_labeller(loc.fn)
        p <- p + facet_wrap(vars(.data[[facet.dims]]), ncol = facet.ncol,
                            labeller = lab.fn,
                            scales = if (free.y) "free_y" else "fixed")
    }

    p <- p +
        scale_x_continuous(limits = c(min(long$year), max.yr + pad)) +
        labs(x = x.lab, y = y.lab, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              strip.text       = if (show.strip) element_text(face = "bold")
                                 else element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (single.line || use.direct.label) "none" else "right")

    .save_fig(p, save.dir, filename, width, height, dpi)
    p
}



# ----------------------------------------------------------------------------
# FIGURE 4: Trend over time with a shaded credible interval
# ----------------------------------------------------------------------------
#' Median trajectory with a shaded credible interval, one panel per city
#'
#' The one figure that USES the CI rows instead of dropping them, so the table
#' must be built with stat.type = "median.ci" (or "mean.ci"). It also does not
#' need a coverage level in the intervention name, so a "noint"-only table --
#' the natural summary of the baseline scenario -- plots fine here.
#'
#' Layout is read off the table and can be overridden:
#'   colour  the stratum when several are present, otherwise the intervention
#'           when several are present, otherwise a single colour
#'   facets  every remaining dimension that varies (location first, then
#'           whichever of subgroup / intervention did not take the colour)
#'
#' @param tbl A table from make_multi_location_table() built with a .ci stat.
#' @param locations,subgroup,interventions Optional subsets. The order you
#'   supply is respected in facets, legends and line stacking.
#' @param outcome Required when the table holds more than one outcome.
#' @param year.range Two-element numeric range, inclusive.
#' @param facet.by Any of "location", "subgroup", "intervention". NULL works it
#'   out; character(0) forces a single panel. Two entries give a grid, with the
#'   first across and the second down.
#' @param color.by One of the same three, or NA for a single colour. NULL works
#'   it out.
#' @param show.ribbon FALSE draws the median lines alone.
#' @param ribbon.alpha Opacity of the interval band. Lower it when many bands
#'   overlap in one panel.
#' @param target Horizontal reference line; NULL to omit.
#' @param free.y Free y scales across panels.
#' @param strat.labels Named vector mapping stratum values to display labels.
#' @param palette Named vector of colours for the colour dimension; NULL uses
#'   the ColorBrewer palette named in `strat.palette`.
#' @param title NULL auto-generates one, NA suppresses it.
#' @return A ggplot object.
#' @param save.dir,filename Where to write the figure and what to call it.
#'   `filename` is the switch: give one and a PNG is written, leave it NULL
#'   (the default) and nothing is. The ".png" extension is appended for you and
#'   `save.dir` defaults to FIG.DIR when your driver script has set it. Same
#'   two arguments, same behaviour, as the table functions.
plot_trend_with_ci <- function(tbl,
                               locations     = NULL,
                               subgroup      = NULL,
                               interventions = NULL,
                               outcome       = NULL,
                               order.by      = NULL,
                               year.range    = NULL,
                               facet.by      = NULL,
                               color.by      = NULL,
                               show.ribbon   = TRUE,
                               ribbon.alpha  = 0.20,
                               target        = NULL,
                               facet.ncol    = NULL,
                               free.y        = FALSE,
                               location.col  = "location",
                               col.pattern   = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               cov.pattern   = "doxy\\.cov\\.(\\d+)",
                               strat.palette = "Set1",
                               strat.labels  = c(Total             = "Total population",
                                                 msm               = "MSM",
                                                 heterosexual_male = "Heterosexual men",
                                                 female            = "Women"),
                               palette       = NULL,
                               loc.labels    = NULL,
                               x.lab         = "Year",
                               y.lab         = NULL,
                               title         = NULL,
                               save.dir      = .default_fig_dir(),
                               filename      = NULL,
                               width = 11, height = 6, dpi = 300) {

    # the `subgroup` argument is shadowed by the column of the same name once
    # we are inside mutate(), so keep a copy of the requested order
    subgroup.order <- subgroup

    long <- .prep_long(tbl, location.col, locations, subgroup,
                       col.pattern, cov.pattern, keep.ci = TRUE)

    # ---- outcome ----------------------------------------------------------
    if (!is.null(outcome)) {
        .outcome <- outcome
        long <- long %>% filter(outcome == .outcome)
        if (nrow(long) == 0) stop("Outcome '", .outcome, "' not present in table.")
    }
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")

    # ---- remaining subsets -------------------------------------------------
    if (!is.null(interventions)) {
        miss <- setdiff(interventions, unique(long$intervention))
        if (length(miss))
            stop("Intervention(s) not in table: ", paste(miss, collapse = ", "),
                 ".\nAvailable: ", paste(unique(long$intervention), collapse = ", "))
        long <- long %>% filter(intervention %in% interventions)
    }
    if (!is.null(year.range))
        long <- long %>% filter(year >= min(year.range), year <= max(year.range))

    if (nrow(long) == 0) stop("No rows left after filtering.")
    if (dplyr::n_distinct(long$year) < 2)
        stop("Need >= 2 years to draw a trajectory. Build the table with, say, ",
             "years = as.character(2022:2030).")

    # ---- ordering: what you asked for first, anything else appended -------
    ord <- function(x, wanted) {
        u <- as.character(unique(x))
        if (is.null(wanted)) u else c(intersect(wanted, u), setdiff(u, wanted))
    }
    # this figure's own ranking: value in the final year, averaged over series
    value.order <- long %>%
        filter(year == max(year)) %>%
        group_by(location) %>% summarise(v = mean(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(v)) %>% pull(location) %>% as.character()

    long <- long %>%
        mutate(location     = factor(as.character(location),
                                     levels = .location_order(location, locations,
                                                              order.by, value.order)),
               subgroup     = factor(as.character(subgroup),
                                     levels = ord(subgroup, subgroup.order)),
               intervention = factor(as.character(intervention),
                                     levels = ord(intervention, interventions)),
               # one line per location x subgroup x intervention
               .series = paste(location, subgroup, intervention, sep = "|"))

    # ---- which dimensions vary, and what each one is used for -------------
    n.of <- c(location     = dplyr::n_distinct(long$location),
              subgroup     = dplyr::n_distinct(long$subgroup),
              intervention = dplyr::n_distinct(long$intervention))
    varies <- names(n.of)[n.of > 1]
    if (all(is.na(long$subgroup))) varies <- setdiff(varies, "subgroup")

    if (is.null(color.by))
        color.by <- if ("subgroup" %in% varies) "subgroup"
                    else if ("intervention" %in% varies) "intervention"
                    else NA_character_
    if (is.null(facet.by))
        facet.by <- setdiff(varies, if (is.na(color.by)) character(0) else color.by)
    facet.by <- intersect(facet.by, c("location", "subgroup", "intervention"))

    # ---- labels ------------------------------------------------------------
    if (is.null(y.lab)) y.lab <- unique(as.character(long$outcome))
    if (length(title) == 1 && is.na(title)) {
        title <- NULL
    } else if (is.null(title)) {
        ci.lab <- "median and 95% credible interval"
        title  <- paste0(y.lab, " over time (", ci.lab, ")")
    }

    # ---- plot --------------------------------------------------------------
    p <- ggplot(long, aes(x = year, y = value, group = .series))

    if (!is.null(target))
        p <- p + geom_hline(yintercept = target, linetype = "dashed",
                            color = "grey35", linewidth = 0.4)

    if (is.na(color.by)) {
        if (show.ribbon)
            p <- p + geom_ribbon(aes(ymin = lower, ymax = upper),
                                 fill = "#2166AC", alpha = ribbon.alpha, colour = NA)
        p <- p + geom_line(linewidth = 0.9, colour = "#2166AC")

    } else {
        lab.fn <- if (color.by == "subgroup") .strat_labeller(strat.labels)
                  else function(x) x
        if (show.ribbon)
            p <- p + geom_ribbon(aes(ymin = lower, ymax = upper,
                                     fill = .data[[color.by]]),
                                 alpha = ribbon.alpha, colour = NA,
                                 show.legend = FALSE)
        p <- p + geom_line(aes(colour = .data[[color.by]]), linewidth = 0.9)

        if (!is.null(palette))
            p <- p + scale_colour_manual(values = palette, name = NULL, labels = lab.fn) +
                     scale_fill_manual(values = palette, guide = "none")
        else
            p <- p + scale_colour_brewer(palette = strat.palette, name = NULL,
                                         drop = FALSE, labels = lab.fn) +
                     scale_fill_brewer(palette = strat.palette, guide = "none",
                                       drop = FALSE)
    }

    # ---- faceting ----------------------------------------------------------
    lab.for <- function(d)
        if (d == "location") .make_labeller(loc.labels)
        else if (d == "subgroup") .strat_labeller(strat.labels)
        else function(x) x

    scales.arg <- if (free.y) "free_y" else "fixed"
    if (length(facet.by) >= 2) {
        lg <- list(); lg[[facet.by[1]]] <- lab.for(facet.by[1])
        lg[[facet.by[2]]] <- lab.for(facet.by[2])
        p <- p + facet_grid(stats::as.formula(paste(facet.by[2], "~", facet.by[1])),
                            labeller = do.call(labeller, lg), scales = scales.arg)
    } else if (length(facet.by) == 1) {
        p <- p + facet_wrap(stats::as.formula(paste("~", facet.by)),
                            ncol = facet.ncol,
                            labeller = as_labeller(lab.for(facet.by)),
                            scales = scales.arg)
    }

    p <- p +
        # fewer year breaks and a gap between panels, so neighbouring axes do
        # not run their labels together when there are many cities
        scale_x_continuous(breaks = scales::breaks_pretty(4)) +
        labs(x = x.lab, y = y.lab, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              panel.spacing.x  = unit(0.9, "lines"),
              strip.text       = element_text(face = "bold"),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (is.na(color.by)) "none" else "bottom")

    .save_fig(p, save.dir, filename, width, height, dpi)
    p
}


# ============================================================================
# EXAMPLES
# ============================================================================
if (1 == 2) {

    FIG.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/figures/") else "figures/"

    # ------------------------------------------------------------------------
    # TABLES
    # ------------------------------------------------------------------------
    # Same builder whether you want one city or ten. stat.type = "median.ci"
    # is the default and is what you want for the manuscript table.
    atlanta.tbl <- make_multi_location_table(
        data          = list(total_raw_results, sex_results),
        locations     = "Atlanta",
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.", seq(10, 100, 10)),
        years         = c("2030"),
        stat.type     = "median.ci"
    )

    ten.city.tbl <- make_multi_location_table(
        data          = list(total_raw_results, sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.", seq(10, 100, 10)),
        years         = c("2030"),
        stat.type     = "median.ci"
    )

    # Both tables can go straight into any figure. The CI rows are dropped
    # automatically (with a message); pass stat.type = "median" to silence it.

    # ------------------------------------------------------------------------
    # FIGURE 1: coverage needed
    # ------------------------------------------------------------------------
    # Strata are selected with subgroup =, not by globbing the location name.
    f1 <- plot_coverage_needed(
        ten.city.tbl,
        target    = 50,
        subgroup  = "msm",
        title     = "Doxy coverage needed to reach 50% incidence reduction among MSM by 2030",
        filename  = "fig1_coverage_needed")
    f1

    # ------------------------------------------------------------------------
    # FIGURE 2: dose-response
    # ------------------------------------------------------------------------
    f2 <- plot_dose_response(ten.city.tbl,
                             subgroup  = "msm",
                             target    = 50,
                             y.lab     = "Diagnoses averted (%)",
                             filename  = "fig2_dose_response_msm")
    f2

    # all subgroups within one city   (was: locations = c("Atlanta *"))
    plot_dose_response(ten.city.tbl,
                       locations = "Atlanta",
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")

    # MSM across all cities           (was: locations = c("* - msm"))
    plot_dose_response(ten.city.tbl,
                       subgroup  = "msm",
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")

    # ------------------------------------------------------------------------
    # FIGURE 3: trajectories
    # ------------------------------------------------------------------------
    traj.tbl <- make_multi_location_table(
        data          = list(total_raw_results, sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.", seq(10, 100, 10)),
        years         = as.character(2022:2040),
        stat.type     = "median"
    )

    # View 1: one city, all subgroups, one coverage level
    #         subgroup now takes the colour channel, using strat.labels
    plot_impact_over_time(traj.tbl,
                          locations  = "Baltimore",
                          coverages  = 10,
                          year.range = c(2022, 2040))

    # View 2: one city, one subgroup, all coverage levels
    plot_impact_over_time(traj.tbl,
                          locations  = "Baltimore",
                          subgroup   = "Total",
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          y.lab      = "Diagnoses averted in Baltimore (%)")

    # View 3: all cities, women only, all coverage levels (small multiples)
    plot_impact_over_time(traj.tbl,
                          subgroup   = "female",
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          facet.ncol = 5,
                          y.lab      = "Diagnoses averted (%)")

    # View 3b: panels are coverage levels, lines are cities
    plot_impact_over_time(traj.tbl,
                          subgroup   = "Total",
                          color.by   = "location",
                          coverages  = c(10, 30, 60, 90),
                          year.range = c(2026, 2035),
                          facet.ncol = 4,
                          filename   = "fig3_multi_by_location")

    # ------------------------------------------------------------------------
    # FIGURE 4: trend with a shaded credible interval
    # ------------------------------------------------------------------------
    # Needs a .ci table. Works with a single scenario, so "noint" is fine --
    # the coverage figures cannot take a noint-only table because there is no
    # coverage level to put on the x axis.
    summary_no_int <- make_multi_location_table(
        data          = list(total_raw_results, total_calc_results, sex_calc_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("rate_diagnosis_total_per_pop"),
        interventions = "noint",
        years         = as.character(2022:2030),
        stat.type     = "median.ci")

    # one panel per city, one line per stratum, ribbon = 95% CrI
    plot_trend_with_ci(summary_no_int,
                       facet.ncol   = 5,
                       ribbon.alpha = 0.15,
                       y.lab        = "Diagnoses per 100,000",
                       title        = "Diagnosis rate with no intervention",
                       filename     = "fig4_noint_trend")

    # a single stratum: one line and one band per panel
    plot_trend_with_ci(summary_no_int, subgroup = "msm", facet.ncol = 5)

    # four cities, strata down the side, one line per panel
    plot_trend_with_ci(summary_no_int,
                       locations = c("Atlanta", "Baltimore", "Chicago", "Dallas"),
                       facet.by  = c("location", "subgroup"),
                       color.by  = NA)

    # comparing scenarios instead of strata: colour falls to intervention
    plot_trend_with_ci(
        make_multi_location_table(
            data            = list(total_calc_results, sex_calc_results),
            locations       = names(SHIELD.TEN.MSAS),
            outcomes        = "rate_diagnosis_total_per_pop",
            interventions   = c("noint", "doxy.cov.50", "doxy.cov.100"),
            years           = as.character(2022:2030),
            stat.type       = "median.ci",
            filter.by.strat = "msm"),
        facet.ncol = 5)

    # ------------------------------------------------------------------------
    # HEAT MAP
    # ------------------------------------------------------------------------
    plot_coverage_heatmap(ten.city.tbl,
                          subgroup  = "msm",
                          midpoint  = 50,
                          filename  = "heatmap_msm_2030")

    # ------------------------------------------------------------------------
    # row.vars now works with the figures too (issue A5): the table records
    # what its columns mean, so the figures no longer have to guess.
    # ------------------------------------------------------------------------
    by.year.rows <- make_multi_location_table(
        data          = list(total_raw_results, sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.", seq(10, 100, 10)),
        years         = as.character(c(2030, 2035)),
        row.vars      = "year",          # year goes down the rows
        stat.type     = "median")
    plot_coverage_needed(by.year.rows, subgroup = "msm", year = 2035)
}

.remove.duplicate.rows<-function(tbl){
    # REMOVE Duplicate Location and Population names
    #    Compute BOTH flags before overwriting either column, or the second
    #    duplicated() call sees the blanks the first one just wrote.
    dup <- duplicated(paste(tbl$Population, tbl$MSA, sep = "\r"))
    tbl$MSA        <- ifelse(dup, "", as.character(tbl$MSA))
    tbl$Population <- ifelse(dup, "", as.character(tbl$Population))
    tbl
}
