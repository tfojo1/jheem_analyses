library(tidyverse)
# ROOT.DIR # is set by the specification
BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
# ****************************************************************************************************

total_results=get(load(file = paste0(BASE.PATH,"/total_results.Rdata")))
# age_results=get(load(file = paste0(BASE.PATH,"/age_results.Rdata")))


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
                df <- cbind(rep(NA, nrow(df)), df)
            }
        }
        
        if (num_stratification_cols_for_table > 0) {
            colnames(df)[1:num_stratification_cols_for_table] <- LETTERS[1:num_stratification_cols_for_table]
        } 
        
        df
    }))
    
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
# added new integrated outcome, and interventions run, but they don't report it.
# what does it take to report a new outcome without re-calibrating?

xx=make_single_location_table(data = list(total_results),
                              location = "C.12060",
                              outcomes = c("diagnosis.total","diagnosis.ps","incidence_averted"),
                              interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
                              years = c("2022", "2026", "2035"),
                              stat.type = "mean.ci",
                              save = F,save.dir = paste0(BASE.PATH,"/tables/"),filename = "total"
)

x1=make_single_location_table(data = list(total_results),
                              location = "C.12060",
                              outcomes = c("diagnosis.total", "incidence_averted"),
                              interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
                              years = c("2022", "2026", "2035"),
                              row.vars="intervention",  
                              save = T,save.dir = paste0(BASE.PATH,"/tables/"),filename = "total.by.int"
)

# % incidence averted
loc.tbl = make_multi_location_table(
    data          = list(total_results),
    locations     = names(SHIELD.TEN.MSAS),
    outcomes      = c("pct_cum_incidence_averted"),
    interventions = paste0("doxy.cov.",seq(10,100,10)),
    years         = c("2035"),
    stat.type     = "median",
    save          = TRUE,
    save.dir      = paste0(BASE.PATH, "/tables/"),
    filename      = "multi.loc_pct.inc.averted_2035"
)
# % diagnosis averted
loc.tbl = make_multi_location_table(
    data          = list(total_results),
    locations     = names(SHIELD.TEN.MSAS),
    outcomes      = c("pct_diagnosis_averted"),
    interventions = paste0("doxy.cov.",seq(10,100,10)),
    years         = c("2035"),
    stat.type     = "median",
    save          = TRUE,
    save.dir      = paste0(BASE.PATH, "/tables/"),
    filename      = "multi.loc_pct.diag.averted_2035"
)
# cumulative incidence averted
loc.tbl = make_multi_location_table(
    data          = list(total_results),
    locations     = names(SHIELD.TEN.MSAS),
    outcomes      = c("cum_incidence_averted"),
    interventions = paste0("doxy.cov.",seq(10,100,10)),
    years         = c("2035"),
    stat.type     = "median",
    save          = TRUE,
    save.dir      = paste0(BASE.PATH, "/tables/"),
    filename      = "multi.loc_cum.inc.averted_2035"
)
#cumulative diagnosis averted
loc.tbl = make_multi_location_table(
    data          = list(total_results),
    locations     = names(SHIELD.TEN.MSAS),
    outcomes      = c("cum_diagnosis_averted"),
    interventions = paste0("doxy.cov.",seq(10,100,10)),
    years         = c("2035"),
    stat.type     = "median",
    save          = TRUE,
    save.dir      = paste0(BASE.PATH, "/tables/"),
    filename      = "multi.loc_cum.diag.averted_2035"
)
#annual incidence averted
loc.tbl = make_multi_location_table(
    data          = list(total_results),
    locations     = names(SHIELD.TEN.MSAS),
    outcomes      = c("cum_incidence_averted"),
    interventions = paste0("doxy.cov.",seq(10,100,10)),
    years         = c("2035"),
    stat.type     = "median",
    save          = TRUE,
    save.dir      = paste0(BASE.PATH, "/tables/"),
    filename      = "multi.loc_inc.averted_2035"
)
