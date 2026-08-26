library(tidyverse)
# ROOT.DIR # is set by the specification
BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
# ****************************************************************************************************

total_results=get(load(file = paste0(BASE.PATH,"/total_results.Rdata")))
age_results=get(load(file = paste0(BASE.PATH,"/age_results.Rdata")))


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

get_stats <- function(arr, keep.dimensions='year', round=T, digits=0, include.mean=T, include.quartiles=F, multiply.by.100=F, floor=F) {
    arr_data <- apply(arr, keep.dimensions, function(x) {
        rv <- c(lower = quantile(x, probs=0.025), median = median(x), upper = quantile(x, probs=0.975))
        if (include.quartiles) rv <- c(rv,
                                       lowermid = quantile(x, probs=0.25),
                                       uppermid = quantile(x, probs=0.75))
        if (include.mean) rv <- c(rv, mean = mean(x))
        rv
    })
    if (floor) arr_data <- floor(arr_data)
    if (round) arr_data <- round(arr_data, digits=digits)
    if (multiply.by.100) arr_data <- arr_data * 100
    metric_dimension = c("lower", "median", "upper")
    if (include.quartiles) metric_dimension <- c(metric_dimension, "lowermid", "uppermid")
    if (include.mean) metric_dimension <- c(metric_dimension, "mean")
    final_dimnames <- c(list(metric=metric_dimension),
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
                      keep.dimensions = c("year", "intervention", "outcome", stratification_cols))
        ) %>%
            pivot_wider(names_from = "metric") %>%
            select(-median) %>%
            mutate(ci = paste0("[", lower, "-", upper, "]")) %>%
            select(all_of(c(stratification_cols,id_cols)), mean, ci) %>%
            mutate(mean = as.character(mean)) %>%
            pivot_longer(
                cols = c(mean, ci),
                names_to = "stat",
                values_to = "value"
            ) %>%
            mutate(stat = factor(stat, levels = c("mean", "ci"))) %>%  # ensures mean comes before ci
            arrange(across(all_of(c(stratification_cols,row.vars, col_vars))), stat) %>%
            pivot_wider(
                names_from = all_of(col_vars),
                values_from = value
            ) %>%
            select(-stat)
        
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

# added new integrated outcome, and interventions run, but they don't report it.
# what does it take to report a new outcome without re-calibrating?

xx=make_single_location_table(data = list(total_results),
                              location = "C.12060",
                              outcomes = c("diagnosis.total","diagnosis.ps","incidence_averted"),
                              interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
                              years = c("2022", "2026", "2035"),
                              save = T,save.dir = paste0(BASE.PATH,"/tables/"),filename = "total"
)

x1=make_single_location_table(data = list(total_results),
                              location = "C.12060",
                              outcomes = c("diagnosis.total", "incidence_averted"),
                              interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
                              years = c("2022", "2026", "2035"),
                              row.vars="intervention",  
                              save = T,save.dir = paste0(BASE.PATH,"/tables/"),filename = "total.by.int"
)
