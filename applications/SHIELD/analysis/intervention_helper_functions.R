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


#' Write a table to CSV, creating the directory if needed
#'
#' NOTE: the column map (see .attach_col_map) does NOT survive a trip through
#' CSV. A table read back from disk is still plottable, but the figures then
#' fall back to parsing the column names with `col.pattern`.
save_table_csv <- function(rv, save.dir = "", filename = NULL) {
    if (is.null(filename) || !nzchar(filename))
        stop("Error: 'filename' must be supplied when save = TRUE")
    if (!grepl("\\.csv$", filename, ignore.case = TRUE))
        filename <- paste0(filename, ".csv")
    target.dir <- if (nzchar(save.dir)) save.dir else "."
    if (!dir.exists(target.dir))
        dir.create(target.dir, recursive = TRUE, showWarnings = FALSE)
    full.path <- file.path(target.dir, filename)
    readr::write_csv(rv, file = full.path, na = "")
    message("Table written to: ", normalizePath(full.path, winslash = "/"))
    invisible(full.path)
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
    map <- expand.grid(value.lists, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    map$colname <- do.call(paste, c(as.list(map[col_vars]), sep = "_"))
    map
}


#' Build a table for ONE location
#'
#' Normally you call make_multi_location_table() instead -- it works for one
#' location as well as ten, and it is the entry point the figures are written
#' against. This function is the worker underneath it.
#'
#' @param data A list of arrays, each with a different stratification (totals,
#'   sex-stratified, age-stratified, ...). Each array contributes a set of rows:
#'   one row for a totals-level array, one row per stratum for a stratified one.
#'   A bare array is accepted and wrapped in a list for you.
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
make_single_location_table <- function(data,
                                       location,
                                       outcomes,
                                       interventions,
                                       years,
                                       row.vars = "",
                                       stat.type = c("median.ci", "median", "mean.ci", "mean"),
                                       filter.by.strat = NULL,
                                       location.label = "location",
                                       save = FALSE,
                                       save.dir = "",
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

    num_stratification_cols_for_table <- max(sapply(data, function(arr) {
        length(setdiff(names(dim(arr)),
                       c(id_cols, "sim", "location")))
    }))

    rv <- Reduce(rbind, lapply(data, function(arr) {

        if (!all(interventions %in% dimnames(arr)$intervention))
            stop("Error: at least one intervention in 'interventions' isn't present in one of the supplied arrays")
        if (!all(outcomes %in% dimnames(arr)$outcome))
            stop("Error: at least one outcome in 'outcomes' isn't present in one of the supplied arrays")
        if (!all(years %in% dimnames(arr)$year))
            stop("Error: year(s) not present in one of the supplied arrays: ",
                 paste(setdiff(years, dimnames(arr)$year), collapse = ", "))

        stratification_cols <- setdiff(names(dim(arr)),
                                       c(id_cols, "sim", "location"))

        # long form: one row per id combination per stat (estimate, then ci)
        long.df <- reshape2::melt(
            get_stats(subset_array(arr,
                                   list(year = years,
                                        outcome = outcomes,
                                        intervention = interventions,
                                        location = loc$code)),
                      keep.dimensions = c("year", "intervention", "outcome",
                                          stratification_cols),
                      stat.type = stat.type)
        ) %>%
            pivot_wider(names_from = "metric") %>%
            mutate(estimate = as.character(.data[[point.col]]),
                   ci       = if (show.ci) paste0("[", lower, "-", upper, "]") else NULL) %>%
            select(all_of(c(stratification_cols, id_cols)),
                   all_of(if (show.ci) c("estimate", "ci") else "estimate")) %>%
            pivot_longer(
                cols = any_of(c("estimate", "ci")),
                names_to = "stat",
                values_to = "value"
            ) %>%
            mutate(stat = factor(stat, levels = c("estimate", "ci"))) %>%
            arrange(across(all_of(c(stratification_cols, row.vars, col_vars))), stat)

        # wide form: col_vars become columns, joined by "_" in that order
        df <- long.df %>%
            pivot_wider(
                names_from = all_of(col_vars),
                values_from = value
            ) %>%
            select(-stat)

        # pad arrays with fewer stratification dimensions so every array
        # contributes the same number of leading columns and rbind lines up
        num_extra_cols_needed <- num_stratification_cols_for_table - length(stratification_cols)
        if (num_extra_cols_needed > 0) {
            for (i in 1:num_extra_cols_needed) {
                df <- cbind(rep("Total", nrow(df)), df)
            }
        }

        if (num_stratification_cols_for_table > 0) {
            colnames(df)[1:num_stratification_cols_for_table] <-
                make.unique(rep("subgroup", num_stratification_cols_for_table))
        }

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

    if (save) save_table_csv(rv, save.dir, filename)

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
                                      location.label = "location",
                                      repeat.location.label = TRUE,
                                      filter.by.strat = NULL,
                                      save = FALSE,
                                      save.dir = "",
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

    if (save) save_table_csv(rv, save.dir, filename)

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


#' Drop the "[lower-upper]" rows a .ci table carries (A3)
#'
#' CI rows are a table feature. They are text, not numbers, so they cannot be
#' plotted -- previously they became NA silently. Now they are removed and you
#' are told about it.
#' @noRd
.drop_ci_values <- function(long) {
    is.ci <- grepl("^\\s*\\[", as.character(long$value))
    if (any(is.ci)) {
        message("Dropped ", sum(is.ci), " credible-interval cell(s): figures plot ",
                "point estimates only. Build the table with stat.type = \"median\" ",
                "or \"mean\" to avoid this message.")
        long <- long[!is.ci, , drop = FALSE]
    }
    if (nrow(long) == 0)
        stop("Nothing left to plot after removing credible-interval rows.")
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
#'   "doxy.cov.NN".
#' @return Tibble with columns location, subgroup, outcome, coverage (int),
#'   year (int), value (num). `subgroup` is NA when the table has no
#'   stratification column.
table_to_long <- function(tbl,
                          location.col = "location",
                          col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                          cov.pattern  = "doxy\\.cov\\.(\\d+)") {

    # ---- already long? ----------------------------------------------------
    if (all(c("coverage", "value") %in% names(tbl))) {
        if (!"subgroup" %in% names(tbl)) tbl$subgroup <- NA_character_
        if (!"location" %in% names(tbl) && location.col %in% names(tbl))
            tbl <- dplyr::rename(tbl, location = all_of(location.col))
        return(tibble::as_tibble(tbl) %>%
                   mutate(value    = as.numeric(value),
                          coverage = as.integer(coverage),
                          year     = as.integer(year)) %>%
                   select(location, subgroup, outcome, coverage, year, value))
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
    strat.cols <- setdiff(id.cols, c(location.col, "outcome", "intervention", "year"))

    # ---- long form --------------------------------------------------------
    long <- tbl %>%
        select(all_of(c(id.cols, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(all_of(val.cols), names_to = "colname", values_to = "value") %>%
        left_join(meta, by = "colname") %>%
        select(-colname)

    long <- .drop_ci_values(long)

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
        cov <- stringr::str_match(as.character(long$intervention), cov.pattern)[, 2]
        if (all(is.na(cov)))
            stop("No coverage level could be read from the intervention names ",
                 "using 'cov.pattern':\n  ", cov.pattern,
                 "\nIntervention(s) seen: ",
                 paste(unique(long$intervention), collapse = ", "))
        long$coverage <- as.integer(cov)
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

    long %>% select(location, subgroup, outcome, coverage, year, value)
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


#' Save a figure if a path was supplied
#' @noRd
.save_fig <- function(p, save.path, width, height, dpi) {
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
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
.prep_long <- function(tbl, location.col, locations, subgroup, col.pattern, cov.pattern) {
    long <- table_to_long(tbl, location.col = location.col,
                          col.pattern = col.pattern, cov.pattern = cov.pattern)

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
plot_coverage_heatmap <- function(tbl,
                                  location.col = "location",
                                  locations    = NULL,
                                  subgroup     = NULL,
                                  year         = NULL,
                                  col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  cov.pattern  = "doxy\\.cov\\.(\\d+)",
                                  midpoint     = 50,
                                  threshold    = midpoint,
                                  limits       = NULL,
                                  higher.is.better = TRUE,
                                  order.rows   = c("threshold", "max", "alpha", "none"),
                                  label.digits = 0,
                                  show.labels  = TRUE,
                                  row.sep      = " — ",
                                  title        = NULL,
                                  x.lab        = "Doxy-PEP coverage (%)",
                                  fill.lab     = NULL,
                                  save.path    = NULL,
                                  width = 8, height = 5, dpi = 300) {

    order.rows <- match.arg(order.rows)

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)

    long <- .filter_year(long, year)

    long$row.id <- .series_label(long, row.sep)

    # ---- row ordering ------------------------------------------------------
    crossed <- function(v) if (higher.is.better) v >= threshold else v <= threshold
    ord <- long %>%
        group_by(row.id) %>%
        summarise(cross = suppressWarnings(min(coverage[crossed(value)])),
                  best  = if (higher.is.better) max(value, na.rm = TRUE)
                          else min(value, na.rm = TRUE),
                  .groups = "drop")
    row.order <- switch(order.rows,
                        threshold = ord %>% arrange(cross, if (higher.is.better) desc(best) else best) %>% pull(row.id),
                        max       = ord %>% arrange(if (higher.is.better) desc(best) else best) %>% pull(row.id),
                        alpha     = sort(unique(long$row.id)),
                        none      = unique(long$row.id))

    long <- long %>%
        mutate(row.id   = factor(row.id, levels = rev(row.order)),
               coverage = factor(coverage, levels = sort(unique(coverage))))

    # ---- fill scale --------------------------------------------------------
    if (is.null(limits)) limits <- c(0, 100)
    pal <- if (higher.is.better) c("#B2182B", "#1A9850") else c("#1A9850", "#B2182B")

    if (is.null(fill.lab)) fill.lab <- paste(unique(long$outcome), collapse = " / ")

    if (is.null(title) && dplyr::n_distinct(long$year) == 1) {
        title <- paste0(fill.lab, ", ", unique(long$year))
        if (!is.null(subgroup) && length(subgroup) == 1)
            title <- paste0(title, " (", subgroup, ")")
    }

    p <- ggplot(long, aes(x = coverage, y = row.id, fill = value)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradientn(colours = c(pal[1], "#F7F7F7", pal[2]),
                             values  = scales::rescale(c(limits[1], midpoint, limits[2]),
                                                       from = limits),
                             limits  = limits,
                             oob     = scales::squish,
                             name    = fill.lab) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(face = "bold", size = 12),
              legend.key.height = unit(1.2, "cm"))

    if (show.labels)
        p <- p +
        geom_text(aes(label = format(round(value, label.digits),
                                     nsmall = label.digits),
                      color = ifelse(value >= midpoint,
                                     (value - midpoint) / (limits[2] - midpoint),
                                     (midpoint - value) / (midpoint - limits[1])) > 0.55),
                  size = 3.4, fontface = "bold", show.legend = FALSE) +
        scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"))

    n.facet <- dplyr::n_distinct(paste(long$outcome, long$year))
    if (n.facet > 1) p <- p + facet_wrap(~ outcome + year, scales = "free_x")
    else             p <- p + coord_fixed(ratio = 0.75)

    .save_fig(p, save.path, width, height, dpi)
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
plot_coverage_needed <- function(tbl,
                                 target       = 50,
                                 year         = NULL,
                                 locations    = NULL,
                                 subgroup     = NULL,
                                 higher.is.better = TRUE,
                                 location.col = "location",
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 cov.pattern  = "doxy\\.cov\\.(\\d+)",
                                 row.sep      = " — ",
                                 title        = NULL,
                                 x.lab        = "Coverage required (%)",
                                 bar.fill     = "#2166AC",
                                 unreached.lab = "not reached",
                                 save.path = NULL, width = 7, height = 4.5, dpi = 300) {

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)

    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- .filter_year(long, year)

    long$series <- .series_label(long, row.sep)

    max.cov <- max(long$coverage, na.rm = TRUE)
    hit <- function(v) if (higher.is.better) v >= target else v <= target

    summ <- long %>%
        group_by(series) %>%
        summarise(cov.needed = suppressWarnings(min(coverage[hit(value)])),
                  best       = if (higher.is.better) max(value, na.rm = TRUE)
                               else min(value, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(reached  = is.finite(cov.needed),
               bar.len  = ifelse(reached, cov.needed, max.cov),
               lab      = ifelse(reached, paste0(cov.needed, "%"),
                                 paste0(unreached.lab, " (max ",
                                        round(best), "%)"))) %>%
        arrange(desc(reached), cov.needed, desc(best)) %>%
        mutate(series = factor(series, levels = rev(series)))

    if (is.null(title))
        title <- paste0("Doxy-PEP coverage needed to reach ", target,
                        "% reduction by ", year, .strat_suffix(subgroup))

    p <- ggplot(summ, aes(x = bar.len, y = series)) +
        geom_col(aes(fill = reached, color = reached),
                 linewidth = 0.6, width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = lab, hjust = ifelse(reached, -0.15, 1.05),
                      color = reached),
                  size = 3.3, fontface = "bold", show.legend = FALSE) +
        scale_fill_manual(values  = c(`TRUE` = bar.fill, `FALSE` = "grey95")) +
        scale_color_manual(values = c(`TRUE` = bar.fill, `FALSE` = "grey45")) +
        scale_x_continuous(limits = c(0, max.cov * 1.25),
                           breaks = seq(0, max.cov, by = 20), expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor   = element_blank(),
              axis.ticks         = element_blank(),
              plot.title         = element_text(face = "bold", size = 12))

    .save_fig(p, save.path, width, height, dpi)
    p
}


# ----------------------------------------------------------------------------
# FIGURE 2: Dose-response -- impact vs coverage, at a fixed year
# ----------------------------------------------------------------------------
#' One line per series (location, or location x subgroup when several strata
#' are present), ordered by the impact reached at the highest coverage.
plot_dose_response <- function(tbl,
                               target       = 50,
                               year         = NULL,
                               locations    = NULL,
                               subgroup     = NULL,
                               location.col = "location",
                               col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               cov.pattern  = "doxy\\.cov\\.(\\d+)",
                               row.sep      = " — ",
                               title        = NULL,
                               x.lab        = "Doxy-PEP coverage (%)",
                               y.lab        = NULL,
                               direct.label = TRUE,
                               palette      = NULL,
                               save.path = NULL, width = 7.5, height = 5, dpi = 300) {

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)

    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- .filter_year(long, year)

    long$series <- .series_label(long, row.sep)

    # order the key by terminal impact so it reads as a ranking
    ord <- long %>%
        group_by(series) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(series)
    long <- long %>% mutate(series = factor(series, levels = ord))

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

    .save_fig(p, save.path, width, height, dpi)
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
plot_impact_over_time <- function(tbl,
                                  color.by      = c("coverage", "location"),
                                  locations     = NULL,
                                  coverages     = NULL,
                                  subgroup      = NULL,
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
                                  save.path = NULL, width = 10, height = 6, dpi = 300) {

    color.by <- match.arg(color.by)

    long <- .prep_long(tbl, location.col, locations, subgroup, col.pattern, cov.pattern)

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
    u.loc <- as.character(unique(long$location))
    ord.loc <- if (!is.null(locations)) {
        c(intersect(locations, u.loc), setdiff(u.loc, locations))
    } else {
        long %>%
            filter(coverage == max(coverage)) %>%
            group_by(location) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
            arrange(desc(value)) %>% pull(location) %>% as.character()
    }
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

    .save_fig(p, save.path, width, height, dpi)
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
        save.path = paste0(FIG.DIR, "fig1_coverage_needed.png"))
    f1

    # ------------------------------------------------------------------------
    # FIGURE 2: dose-response
    # ------------------------------------------------------------------------
    f2 <- plot_dose_response(ten.city.tbl,
                             subgroup  = "msm",
                             target    = 50,
                             y.lab     = "Diagnoses averted (%)",
                             save.path = paste0(FIG.DIR, "fig2_dose_response_msm.png"))
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
                          save.path  = paste0(FIG.DIR, "fig3_multi_by_location.png"))

    # ------------------------------------------------------------------------
    # HEAT MAP
    # ------------------------------------------------------------------------
    plot_coverage_heatmap(ten.city.tbl,
                          subgroup  = "msm",
                          midpoint  = 50,
                          save.path = paste0(FIG.DIR, "heatmap_msm_2030.png"))

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
