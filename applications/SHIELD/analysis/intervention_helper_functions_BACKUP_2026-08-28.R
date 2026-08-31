subset_array <- function(arr, dim_indices, drop = FALSE) {
    # dim_indices: named list where names are dimension *names*
    # (matching names(dimnames(arr))) and values are the indices
    # you want to keep along that dimension.
    
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

get_stats <- function(arr, 
                      keep.dimensions='year', 
                      stat.type = c("median.ci", "median","mean.ci", "mean"), #takes the first argument as default
                      round=T, 
                      digits=0, 
                      multiply.by.100=F, 
                      floor=F) {
    stat.type <- match.arg(stat.type)
    point.col <- if (grepl("^mean", stat.type)) "mean" else "median"
    show.ci   <- grepl("\\.ci$", stat.type)
    
    #which metrics to compute, in output order
    metrics <- point.col
    if (show.ci)  metrics <- c(metrics, "lower", "upper")
    metric.fns <- list(
        mean     = function(x) mean(x),
        median   = function(x) median(x),
        lower    = function(x) unname(quantile(x, probs = 0.025)),
        upper    = function(x) unname(quantile(x, probs = 0.975))
    )
    
    arr_data <- apply(arr, keep.dimensions, function(x) {
        vapply(metric.fns[metrics], function(f) f(x), numeric(1))
    })
    
    if (floor) arr_data <- floor(arr_data)
    if (round) arr_data <- round(arr_data, digits=digits)
    if (multiply.by.100) arr_data <- arr_data * 100
    
    final_dimnames <- c(list(metric = metrics),
                        dimnames(arr)[keep.dimensions])
    array(
        arr_data,
        dim = sapply(final_dimnames, length),
        dimnames = final_dimnames)
}

# Make a table with 1 row
# and a column for diagnosis total in 2022, 2026, and 2030
# Each cell having mean above and credible interval below

# Dimensions: Metric, year, intervention

#' @param data A list of arrays, each with a different stratification, eg.
#' totals, sex-stratified, age-stratified, etc. Each array will correspond to a
#' set of rows of the table. For totals-level arrays, there will be just one
#' row, but for a stratified array, there will be as many rows as there are
#' values for the stratified dimension. For instance, a sex-stratified array
#' might become three rows: one for female, one for MSM, and one for het male.
make_single_location_table <- function(data,
                                       location,
                                       outcomes,
                                       interventions,
                                       years,
                                       row.vars="",
                                       stat.type = c("median.ci", "median","mean.ci", "mean"), #takes the first argument as default
                                       filter.by.strat=NULL,
                                       save = FALSE,
                                       save.dir = "",
                                       filename = NULL,
                                       debug = F
) {
    
    if (debug) browser()
    
    # for stratifications that don't have an outcome, fill with NA
    
    # all of variables: 
    id_cols <- c("outcome", "intervention", "year")
    
    # arranging those that go to rows vs columns
    # accept "", NULL, NA, or c() as "no row variables"
    if (is.null(row.vars)) row.vars <- character(0)
    row.vars <- row.vars[!is.na(row.vars) & nzchar(row.vars)]
    
    if (!all(row.vars %in% id_cols))
        stop("Error: 'row.vars' must be a subset of ", paste(id_cols, collapse = ", "),
             " (or blank for none)")
    col_vars <- setdiff(id_cols, row.vars)
    
    # which point estimate, and whether to append a 95% interval row
    stat.type <- match.arg(stat.type)
    point.col <- if (grepl("^mean", stat.type)) "mean" else "median"
    show.ci   <- grepl("\\.ci$", stat.type)
    
    num_stratification_cols_for_table <- max(sapply(data, function(arr) {
        length(setdiff(names(dim(arr)),
                       c(id_cols, "sim", "location")))
    }))
    
    rv<-Reduce(rbind, lapply(data, function(arr) {
        
        if (!all(interventions %in% dimnames(arr)$intervention))
            stop("Error: at least one intervention in 'interventions' isn't present in one of the supplied arrays")
        if (!all(outcomes %in% dimnames(arr)$outcome))
            stop("Error: at least one outcome in 'outcomes' isn't present in one of the supplied arrays")
        
        stratification_cols <- setdiff(names(dim(arr)),
                                       c(id_cols, "sim", "location"))
        
        df <- reshape2::melt(
            # allow asking for median alone, median and 95% CI, mean alone, and mean and 95% CI
            get_stats(subset_array(arr,
                                   list(year = years,
                                        outcome = outcomes,
                                        intervention = interventions,
                                        location = location)),
                      keep.dimensions = c("year", "intervention", "outcome", stratification_cols),
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
            arrange(across(all_of(c(stratification_cols, row.vars, col_vars))), stat) %>%
            pivot_wider(
                names_from = all_of(col_vars),
                values_from = value
            ) %>%
            select(-stat)
        
        # 
        # pivot_wider(names_from = "metric") %>%
        # select(-median) %>%
        # mutate(ci = paste0("[", lower, "-", upper, "]")) %>%
        # select(all_of(c(stratification_cols,id_cols)), mean, ci) %>%
        # mutate(mean = as.character(mean)) %>%
        # pivot_longer(
        #     cols = c(mean, ci),
        #     names_to = "stat",
        #     values_to = "value"
        # ) %>%
        # mutate(stat = factor(stat, levels = c("mean", "ci"))) %>%  # ensures mean comes before ci
        # arrange(across(all_of(c(stratification_cols,row.vars, col_vars))), stat) %>%
        # pivot_wider(
        #     names_from = all_of(col_vars),
        #     values_from = value
        # ) %>%
        # select(-stat)
        
        num_extra_cols_needed <- num_stratification_cols_for_table - length(stratification_cols)
        if (num_extra_cols_needed > 0) {
            for (i in 1:num_extra_cols_needed) {
                df <- cbind(rep("Total", nrow(df)), df)
            }
        }
        
        if (num_stratification_cols_for_table > 0) {
            # colnames(df)[1:num_stratification_cols_for_table] <- LETTERS[1:num_stratification_cols_for_table]
            colnames(df)[1:num_stratification_cols_for_table] <-
                make.unique(rep("subgroup", num_stratification_cols_for_table))
        } 
        
        df
    }))
    # filter for a stratification
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
    
    # Save results 
    if (save) {
        if (is.null(filename) || !nzchar(filename))
            stop("Error: 'filename' must be supplied when save = TRUE")
        
        # auto-append extension if absent
        if (!grepl("\\.csv$", filename, ignore.case = TRUE))
            filename <- paste0(filename, ".csv")
        
        # "" resolves to the working directory
        target.dir <- if (nzchar(save.dir)) save.dir else "."
        if (!dir.exists(target.dir))
            dir.create(target.dir, recursive = TRUE, showWarnings = FALSE)
        
        full.path <- file.path(target.dir, filename)
        readr::write_csv(rv, file = full.path, na = "")
        message("Table written to: ", normalizePath(full.path, winslash = "/"))
    }
    
    rv
}
# Resolve location identifiers to codes and display labels
resolve_locations <- function(arr, locations) {
    dn <- dimnames(arr)$location
    if (is.null(dn)) stop("Array has no 'location' dimension.")
    nm <- names(dn)
    if (is.null(nm)) nm <- dn          # fall back to codes if unnamed
    
    # accept either MSA codes ("C.12060") or city names ("Atlanta")
    codes <- ifelse(locations %in% dn, locations, dn[match(locations, nm)])
    if (any(is.na(codes)))
        stop("Location(s) not found: ", paste(locations[is.na(codes)], collapse = ", "))
    
    labels <- nm[match(codes, dn)]
    labels[is.na(labels) | !nzchar(labels)] <- codes[is.na(labels) | !nzchar(labels)]
    list(code = unname(codes), label = unname(labels))
}
#  Write a table to CSV, creating the directory if needed
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

#' Compare multiple locations in one table.
#' Rows: location (plus anything named in row.vars, plus the stat sub-rows).
#' Columns: whichever of outcome / intervention / year are not in row.vars.
#' @param locations Character vector of MSA codes or city names, in the order
#'   you want them stacked.
#' @param repeat.location.label If FALSE, the label is printed only on the first
#'   sub-row of each location (manuscript style) and blank beneath.
make_multi_location_table <- function(data,
                                      locations,
                                      outcomes,
                                      interventions,
                                      years,
                                      row.vars = "",
                                      stat.type = c("mean.ci", "mean", "median.ci", "median"),
                                      location.label = "location",
                                      repeat.location.label = TRUE,
                                      filter.by.strat=NULL,
                                      save = FALSE,
                                      save.dir = "",
                                      filename = NULL,
                                      debug = FALSE) {
    
    if (debug) browser()
    stat.type <- match.arg(stat.type)
    
    loc <- resolve_locations(data[[1]], locations)
    
    per.loc <- lapply(seq_along(loc$code), function(i) {
        tbl <- tryCatch(
            make_single_location_table(data          = data,
                                       location      = loc$code[i],
                                       outcomes      = outcomes,
                                       interventions = interventions,
                                       years         = years,
                                       row.vars      = row.vars,
                                       filter.by.strat   = filter.by.strat,
                                       stat.type     = stat.type,
                                       save          = FALSE),
            error = function(e)
                stop("Failed at location ", loc$label[i], " (", loc$code[i], "): ",
                     conditionMessage(e), call. = FALSE)
        )
        
        lab <- rep(loc$label[i], nrow(tbl))
        if (!repeat.location.label && nrow(tbl) > 1) lab[-1] <- ""
        
        tbl %>% mutate(!!location.label := lab, .before = 1)
    })
    
    # every location must yield the same columns, or rbind would silently misalign
    ref.names <- names(per.loc[[1]])
    bad <- which(!vapply(per.loc, function(d) identical(names(d), ref.names), logical(1)))
    if (length(bad) > 0)
        stop("Column structure differs at location(s): ",
             paste(loc$label[bad], collapse = ", "))
    
    rv <- dplyr::bind_rows(per.loc)
    
    
    if (save) save_table_csv(rv, save.dir, filename)
    
    rv
}


#' Heat-map a locations x coverage table with a diverging fill
#'
#' Works with or without stratification columns. A table with no `subgroup`
#' column behaves exactly as before: one row per location. A table carrying a
#' `subgroup` column (from `data = list(total_raw_results, sex_results)`) can either
#' be filtered to one stratum, or plotted with a location x subgroup y axis.
#'
#' @param tbl Wide data frame: identifier column(s) plus one column per
#'   outcome x coverage x year combination.
#' @param location.col Name of the location column.
#' @param id.cols Additional non-value identifier columns. Any that are absent
#'   from `tbl` are silently ignored, so the default is safe for totals-only
#'   tables.
#' @param subgroup Optional character vector of stratum values to retain, e.g.
#'   "Total" or c("Total", "msm"). NULL keeps every row present.
#' @param col.pattern Regex with three capture groups: outcome, coverage, year.
#' @param midpoint Value placed at the neutral (white) colour.
#' @param threshold Value used for row ordering; defaults to `midpoint`.
#' @param limits Fill scale bounds. NULL auto-computes a range symmetric about
#'   `midpoint`, which is what keeps white pinned to the threshold.
#' @param higher.is.better FALSE flips the palette and the ordering test.
#' @param order.rows "threshold" (lowest coverage reaching `threshold`), "max",
#'   "alpha", or "none".
#' @param row.sep Separator used when both location and subgroup label a row.
#' @return A ggplot object, invisibly saved to `save.path` if supplied.
plot_coverage_heatmap <- function(tbl,
                                  location.col = "location",
                                  id.cols      = c("subgroup", "outcome.group"),
                                  subgroup     = NULL,
                                  col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  midpoint     = 50,
                                  threshold    = midpoint,
                                  limits       = NULL,
                                  higher.is.better = TRUE,
                                  order.rows   = c("threshold", "max", "alpha", "none"),
                                  label.digits = 0,
                                  show.labels  = TRUE,
                                  row.sep      = " \u2014 ",
                                  title        = NULL,
                                  x.lab        = "Doxy-PEP coverage (%)",
                                  fill.lab     = NULL,
                                  save.path    = NULL,
                                  width = 8, height = 5, dpi = 300) {
    
    order.rows <- match.arg(order.rows)
    
    if (!location.col %in% names(tbl))
        stop("Column '", location.col, "' not found in 'tbl'.")
    
    # ---- identifier columns actually present ------------------------------
    # anything matching col.pattern is a value column, never an identifier
    id.cols <- intersect(id.cols, names(tbl))
    id.cols <- id.cols[is.na(stringr::str_match(id.cols, col.pattern)[, 1])]
    
    # ---- optional stratum filter ------------------------------------------
    if (!is.null(subgroup)) {
        if (length(id.cols) == 0)
            stop("'subgroup' supplied but 'tbl' has no stratification column.")
        keep <- Reduce(`|`, lapply(tbl[id.cols],
                                   function(x) as.character(x) %in% subgroup))
        if (!any(keep))
            stop("No rows match subgroup = ", paste(subgroup, collapse = ", "),
                 ".\nAvailable: ",
                 paste(sort(unique(as.character(unlist(tbl[id.cols])))),
                       collapse = ", "))
        tbl <- tbl[keep, , drop = FALSE]
    }
    
    # ---- parse the value columns ------------------------------------------
    val.cols <- setdiff(names(tbl), c(location.col, id.cols))
    if (length(val.cols) == 0) stop("No value columns left after removing identifiers.")
    
    parts <- stringr::str_match(val.cols, col.pattern)
    if (any(is.na(parts[, 1])))
        stop("Column(s) not matching 'col.pattern': ",
             paste(val.cols[is.na(parts[, 1])], collapse = ", "),
             "\nIf these are identifier columns, add them to 'id.cols'.")
    
    long <- tbl %>%
        select(all_of(c(location.col, id.cols, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(all_of(val.cols), names_to = "colname", values_to = "value") %>%
        left_join(tibble(colname  = val.cols,
                         outcome  = parts[, 2],
                         coverage = as.integer(parts[, 3]),
                         year     = parts[, 4]),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(-colname)
    
    # ---- build the row label ----------------------------------------------
    # only append the stratum when more than one is being shown, so a
    # totals-only or single-stratum table keeps clean city names
    strat.col <- if (length(id.cols) > 0) id.cols[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) > 1)
        long <- long %>%
        mutate(row.id = paste0(location, row.sep, .data[[strat.col]]))
    else
        long <- long %>% mutate(row.id = location)
    
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
        scale_fill_gradientn( colours = c(pal[1], "#F7F7F7", pal[2]),
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
    
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
    }
    
    p
}
library(tidyverse)

# ============================================================================
# HELPERS ----
# ============================================================================

#' Convert a wide locations x (outcome_coverage_year) table to long format
#'
#' Works with or without stratification columns. A table with no `subgroup`
#' column behaves exactly as before. A table carrying one can be filtered to a
#' single stratum, or plotted with location x subgroup as the series label.
#'
#' @param tbl Wide data frame: identifier column(s) plus value columns.
#' @param location.col Name of the location column.
#' @param id.cols Additional non-value identifier columns. Any absent from
#'   `tbl` are silently ignored, so the default is safe for totals-only tables.
#' @param subgroup Optional vector of stratum values to keep, e.g. "Total" or
#'   c("Total", "msm"). NULL keeps every row present.
#' @param row.sep Separator used when >1 stratum is retained and the series
#'   label becomes "Atlanta - msm".
#' @return Tibble: location, outcome, coverage (int), year (int), value (num).
parse_coverage_table <- function(tbl,
                                 location.col = "location",
                                 id.cols      = c("subgroup", "outcome.group"),
                                 subgroup     = NULL,
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 row.sep      = " \u2014 ") {
    
    if (!location.col %in% names(tbl))
        stop("Column '", location.col, "' not found in 'tbl'.")
    
    # identifier columns actually present; anything matching col.pattern is a
    # value column and can never be an identifier
    id.cols <- intersect(id.cols, names(tbl))
    if (length(id.cols) > 0)
        id.cols <- id.cols[is.na(stringr::str_match(id.cols, col.pattern)[, 1])]
    
    # ---- optional stratum filter ------------------------------------------
    if (!is.null(subgroup)) {
        if (length(id.cols) == 0)
            stop("'subgroup' supplied but 'tbl' has no stratification column.")
        keep <- Reduce(`|`, lapply(tbl[id.cols],
                                   function(x) as.character(x) %in% subgroup))
        if (!any(keep))
            stop("No rows match subgroup = ", paste(subgroup, collapse = ", "),
                 ".\nAvailable: ",
                 paste(sort(unique(as.character(unlist(tbl[id.cols])))),
                       collapse = ", "))
        tbl <- tbl[keep, , drop = FALSE]
    }
    
    # ---- parse value columns ----------------------------------------------
    val.cols <- setdiff(names(tbl), c(location.col, id.cols))
    if (length(val.cols) == 0)
        stop("No value columns left after removing identifiers.")
    
    parts <- stringr::str_match(val.cols, col.pattern)
    if (any(is.na(parts[, 1])))
        stop("Column(s) not matching 'col.pattern': ",
             paste(val.cols[is.na(parts[, 1])], collapse = ", "),
             "\nIf these are identifier columns, add them to 'id.cols'.")
    
    long <- tbl %>%
        select(all_of(c(location.col, id.cols, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(all_of(val.cols), names_to = "colname", values_to = "value") %>%
        left_join(tibble(colname  = val.cols,
                         outcome  = parts[, 2],
                         coverage = as.integer(parts[, 3]),
                         year     = as.integer(parts[, 4])),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(-colname)
    
    # ---- series label ------------------------------------------------------
    # append the stratum only when more than one is shown, so a totals-only or
    # single-stratum table keeps clean city names
    strat.col <- if (length(id.cols) > 0) id.cols[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) > 1)
        long <- long %>%
        mutate(location = paste0(location, row.sep, .data[[strat.col]]))
    
    long %>% select(location, outcome, coverage, year, value)
}


#' Internal: accept either a wide table or an already-long one
.prep_long <- function(tbl, location.col, id.cols, subgroup, col.pattern, row.sep) {
    if (all(c("coverage", "value") %in% names(tbl))) {
        # already long -- apply the same stratum filter if a strat column exists
        sc <- intersect(id.cols, names(tbl))
        if (!is.null(subgroup)) {
            if (length(sc) == 0)
                stop("'subgroup' supplied but the long table has no stratification column.")
            tbl <- tbl %>% filter(if_any(all_of(sc), ~ as.character(.x) %in% subgroup))
            if (nrow(tbl) == 0) stop("No rows match subgroup = ",
                                     paste(subgroup, collapse = ", "))
        }
        if (length(sc) > 0 && dplyr::n_distinct(tbl[[sc[1]]]) > 1)
            tbl <- tbl %>% mutate(location = paste0(location, row.sep,
                                                    .data[[sc[1]]]))
        return(tbl)
    }
    parse_coverage_table(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
}

#' Internal: filter locations by full label, base city name, or glob pattern
.filter_locations <- function(long, locations, row.sep = " \u2014 ") {
    if (is.null(locations)) return(long)
    
    base.loc <- sub(paste0(row.sep, ".*$"), "", long$location)
    keep <- long$location %in% locations | base.loc %in% locations
    
    globs <- locations[grepl("[*?]", locations)]
    if (length(globs) > 0)
        keep <- keep | Reduce(`|`, lapply(globs, function(g)
            grepl(utils::glob2rx(g), long$location)))
    
    if (!any(keep))
        stop("No rows match locations = ", paste(locations, collapse = ", "),
             ".\nAvailable cities: ", paste(sort(unique(base.loc)), collapse = ", "),
             "\nAvailable series: ", paste(sort(unique(long$location)), collapse = ", "))
    
    long[keep, , drop = FALSE]
}

#' Internal: append the stratum to a title when exactly one was selected
.strat_suffix <- function(subgroup)
    if (!is.null(subgroup) && length(subgroup) == 1) paste0(" (", subgroup, ")") else ""


#' Internal: save a figure if a path was supplied
.save_fig <- function(p, save.path, width, height, dpi) {
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
    }
    invisible(p)
}


# ============================================================================
# FIGURE 1: Ranked bar -- coverage needed to reach the target ----
# ============================================================================
#' Minimum coverage required to reach a target impact, ranked by city
#'
#' @param tbl Wide table, or the long output of parse_coverage_table().
#' @param target Impact threshold, e.g. 50 for a 50% reduction.
#' @param year Which year to evaluate. Defaults to the latest in the data.
#' @param subgroup Optional stratum filter, e.g. "Total" or "msm".
#' @param higher.is.better TRUE if larger values are the goal (e.g. % averted).
#' @param locations Optional subset of locations to show.
plot_coverage_needed <- function(tbl,
                                 target       = 50,
                                 year         = NULL,
                                 subgroup     = NULL,
                                 higher.is.better = TRUE,
                                 locations    = NULL,
                                 location.col = "location",
                                 id.cols      = c("subgroup", "outcome.group"),
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 row.sep      = " \u2014 ",
                                 title        = NULL,
                                 x.lab        = "Coverage required (%)",
                                 bar.fill     = "#2166AC",
                                 unreached.lab = "not reached",
                                 save.path = NULL, width = 7, height = 4.5, dpi = 300) {
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    # .prep_long folds the stratum into `location` as "City <sep> stratum".
    # Split it back out so location and stratum can be ordered independently.
    if (length(intersect(id.cols, names(long))) == 0 &&
        any(grepl(row.sep, long$location, fixed = TRUE))) {
        parts <- stringr::str_split_fixed(as.character(long$location), row.sep, 2)
        long$location <- trimws(parts[, 1])
        long$subgroup <- trimws(parts[, 2])
        if (!"subgroup" %in% id.cols) id.cols <- c("subgroup", id.cols)
    }
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    long <- .filter_locations(long, locations, row.sep)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    max.cov <- max(long$coverage, na.rm = TRUE)
    hit <- function(v) if (higher.is.better) v >= target else v <= target
    
    summ <- long %>%
        group_by(location) %>%
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
        mutate(location = factor(location, levels = rev(location)))
    
    if (is.null(title))
        title <- paste0("Doxy-PEP coverage needed to reach ", target,
                        "% reduction by ", year, .strat_suffix(subgroup))
    
    p <- ggplot(summ, aes(x = bar.len, y = location)) +
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


# ============================================================================
# FIGURE 2: Dose-response -- impact vs coverage ----
# ============================================================================
#' Impact as a function of coverage, at a fixed year
plot_dose_response <- function(tbl,
                               target       = 50,
                               year         = NULL,
                               subgroup     = NULL,
                               locations    = NULL,
                               location.col = "location",
                               id.cols      = c("subgroup", "outcome.group"),
                               col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               row.sep      = " \u2014 ",
                               title        = NULL,
                               x.lab        = "Doxy-PEP coverage (%)",
                               y.lab        = NULL,
                               direct.label = TRUE,
                               palette      = NULL,
                               save.path = NULL, width = 7.5, height = 5, dpi = 300) {
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    long <- .filter_locations(long, locations, row.sep)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    # order labels by terminal impact so the key reads as a ranking
    ord <- long %>%
        group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(location)
    long <- long %>% mutate(location = factor(location, levels = ord))
    
    ends <- long %>% group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>% ungroup()
    
    if (is.null(y.lab)) y.lab <- paste0(unique(long$outcome), collapse = " / ")
    if (is.null(title))
        title <- paste0(y.lab, " by coverage level, ", year, .strat_suffix(subgroup))
    
    max.cov <- max(long$coverage, na.rm = TRUE)
    
    p <- ggplot(long, aes(x = coverage, y = value,
                          color = location, group = location)) +
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
        p <- p + geom_text(data = ends, aes(label = location),
                           hjust = -0.15, size = 3.1, fontface = "bold",
                           show.legend = FALSE)
    
    if (!is.null(palette)) p <- p + scale_color_manual(values = palette)
    
    .save_fig(p, save.path, width, height, dpi)
    p
}


# ============================================================================
# FIGURE 3: Impact over time -- x axis is year ----
# .strat_labeller ----
#' Build a labelling function for stratum display names
#'
#' Returns identity when `map` is NULL or empty, so callers that pass nothing
#' keep the raw factor levels. Levels absent from `map` are passed through
#' unchanged rather than becoming NA.
#' @param map Named character vector: names are stratum values, values are labels.
#' @noRd
.strat_labeller <- function(map) {
    if (is.null(map) || length(map) == 0) return(function(x) x)
    function(x) {
        x   <- as.character(x)
        out <- unname(map[x])
        ifelse(is.na(out), x, out)
    }
}
# .make_labeller ----
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
# plot_impact_over_time ----
#' Trajectory plot of impact over time
#'
#' Draws one line per series, where a series is a location x coverage x stratum
#' combination. Colour is assigned to the stratum when more than one stratum is
#' present, otherwise to `color.by`; the remaining dimensions become facets.
#'
#' Ordering: when `locations` and/or `subgroup` are supplied, their order is
#' respected in facets, legends and line stacking. Otherwise locations are
#' ranked by their endpoint value at the highest coverage.
#'
#' @param tbl Wide table as produced upstream.
#' @param color.by Dimension mapped to colour when only one stratum is present.
#' @param locations,coverages,subgroup Optional subsets. Supplied order is kept.
#' @param strat.palette ColorBrewer palette used when the stratum takes the
#'   colour channel.
#' @param strat.labels Named character vector mapping stratum values to display
#'   labels for the legend. Unmapped levels pass through unchanged; NULL keeps
#'   the raw values.
#' @param year.range Two-element numeric range, inclusive.
#' @param target Horizontal reference line; NULL to omit.
#' @param outcome Required when the table holds more than one outcome.
#' @param x.lab,y.lab Axis labels. `y.lab` defaults to the outcome name.
#' @param title Overall plot title; NULL auto-generates one, NA suppresses it.
#' @param loc.labels Panel titles for location facets. NULL keeps the raw
#'   location names; supply a named character vector to rename them, or a
#'   function.
#' @param cov.label Panel titles for coverage facets. A template string in which
#'   `{x}` is replaced by the coverage value, a named vector, or a function.
#' @param show.strip FALSE hides all panel titles.
#' @param free.y Free y scales across facets.
#' @param direct.label End-of-line labels when colouring by location.
#' @param annotate.ends Endpoint value labels; single-series plots only.
#' @return A ggplot object.
plot_impact_over_time <- function(tbl,
                                  color.by      = c("coverage", "location"),
                                  locations     = NULL,
                                  coverages     = NULL,
                                  year.range    = NULL,
                                  target        = 50,
                                  outcome       = NULL,
                                  subgroup      = NULL,
                                  location.col  = "location",
                                  id.cols       = c("subgroup", "outcome.group"),
                                  col.pattern   = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  row.sep       = " \u2014 ",
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
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    # ---- outcome ----------------------------------------------------------
    if (!is.null(outcome)) {
        .outcome <- outcome
        long <- long %>% filter(outcome == .outcome)
        if (nrow(long) == 0) stop("Outcome '", .outcome, "' not present in table.")
    }
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")
    
    # ---- subsets ----------------------------------------------------------
    long <- .filter_locations(long, locations, row.sep)
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
    
    # ---- stratification column -------------------------------------------
    # resolved before any of the dimension counts are used
    strat.col <- intersect(id.cols, names(long))
    strat.col <- if (length(strat.col)) strat.col[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) < 2)
        strat.col <- NULL
    
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
    
    if (!is.null(strat.col)) {
        u.str <- as.character(unique(long[[strat.col]]))
        ord.str <- if (!is.null(subgroup))
            c(intersect(subgroup, u.str), setdiff(u.str, subgroup)) else u.str
        long[[strat.col]] <- factor(as.character(long[[strat.col]]), levels = ord.str)
    }
    
    long <- long %>%
        mutate(coverage = as.numeric(coverage), year = as.numeric(year))
    
    # ---- series identifier, ordered location -> subgroup -> coverage ------
    if (!is.null(strat.col)) {
        long <- long %>%
            arrange(location, .data[[strat.col]], coverage) %>%
            mutate(series = paste(location, .data[[strat.col]], coverage, sep = "|"))
    } else {
        long <- long %>%
            arrange(location, coverage) %>%
            mutate(series = paste(location, coverage, sep = "|"))
    }
    long <- long %>% mutate(series = factor(series, levels = unique(series)))
    
    # ---- dimension counts -------------------------------------------------
    n.loc   <- dplyr::n_distinct(long$location)
    n.cov   <- dplyr::n_distinct(long$coverage)
    n.strat <- if (!is.null(strat.col)) dplyr::n_distinct(long[[strat.col]]) else 1L
    single.line <- (n.loc == 1 && n.cov == 1 && n.strat == 1)
    
    # ---- colour and facet assignment --------------------------------------
    # stratum takes the colour channel whenever it varies; location and
    # coverage then compete for the facet channel
    strat.colour <- n.strat > 1
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
            geom_line(aes(color = .data[[strat.col]], group = series),
                      linewidth = 0.9) +
            geom_point(aes(color = .data[[strat.col]]), size = 1.5) +
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

# examples ----
if (1==2){
    FIG.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/figures/") else "figures/"
    
    # --- Figure 1: headline ------------------------------------------------------
    # Needs a table using a single year (e.g., 2035) and spanning multiple locations. Build it with the multi-location
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(total_raw_results,sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        filter.by.strat = "msm",
        save          = F
    )
    f1 <- plot_coverage_needed(pct.inc.ave.tbl,
                               target    = 50,
                               title = "Doxy coverage needed to reach 50% incidence reduction among MSM by 2030",
                               save.path = paste0(FIG.DIR, "fig1_coverage_needed.png"))
    f1
    # --- Figure 2: dose-response -------------------------------------------------
    f2 <- plot_dose_response(pct.inc.ave.tbl,
                             target    = 50,
                             y.lab     = "Diagnoses averted (%)",
                             save.path = paste0(FIG.DIR, "fig2_dose_response_msm.png"))
    
    f2
    # comparing the 3 sexes in a single city
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        save          = F
    )
    # all groups in Atlanta
    plot_dose_response(pct.inc.ave.tbl,
                       locations = c("Atlanta *" ),
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")
    # all MSM in different cities
    plot_dose_response(pct.inc.ave.tbl,
                       locations = c("* — msm" ),
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")
    
    # --- Figure 3: trajectories --------------------------------------------------
    
    traj.tbl = make_multi_location_table(
        data          = list(total_raw_results,sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = as.character(c(2022:2040)),
        stat.type     = "median",
        save          = F
    )
    
    # ---- View 1: one city, one scenario ----------------------------------------
    plot_impact_over_time(traj.tbl,
                          locations = c("Baltimore — *"    ),
                          coverages = 10,
                          year.range = c(2022, 2040),
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 2: one city, all coverage levels ---------------------------------
    plot_impact_over_time(traj.tbl,
                          locations  = c("Baltimore — *"    ),
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          y.lab      = "Diagnoses averted in Baltimore (%)",
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 3: all cities, all coverage levels (small multiples) -------------
    plot_impact_over_time(traj.tbl,
                          locations  = c("* — female" ),
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          facet.ncol = 5,
                          y.lab      = "Diagnoses averted (%)"
                          # save.path = paste0(FIG.DIR, "fig3_multi_by_coverage.png"
    ) 
    
    # ---- View 3b: flipped -- panels are coverage levels, lines are cities ------
    plot_impact_over_time(traj.tbl,
                          color.by   = "location",
                          coverages  = c(10, 30, 60, 90),
                          year.range = c(2026, 2035),
                          facet.ncol = 4,
                          save.path = paste0(FIG.DIR, "fig3_multi_by_location.png") )
    
}