library(tidyverse)

total_results=get(load(file = "Q:/shield/outputs/calib.8.21.stage3.az/total_results.Rdata"))
age_results=get(load(file = "Q:/shield/outputs/calib.8.21.stage3.az/age_results.Rdata"))

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
                                       debug = F) {
    
    if (debug) browser()
    
    # for stratifications that don't have an outcome, fill with NA
    
    id_cols <- c("outcome", "intervention", "year")
    
    num_stratification_cols_for_table <- max(sapply(data, function(arr) {
        length(setdiff(names(dim(arr)),
                       c(id_cols, "sim", "location")))
    }))
    
    Reduce(rbind, lapply(data, function(arr) {
        
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
            select(all_of(c(id_cols, stratification_cols)), mean, ci) %>%
            mutate(mean = as.character(mean)) %>%
            pivot_longer(
                cols = c(mean, ci),
                names_to = "stat",
                values_to = "value"
            ) %>%
            mutate(stat = factor(stat, levels = c("mean", "ci"))) %>%  # ensures mean comes before ci
            arrange(across(all_of(c(id_cols, stratification_cols))), stat) %>%
            pivot_wider(
                names_from = all_of(id_cols),
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
}

# added new integrated outcome, and interventions run, but they don't report it.
# what does it take to report a new outcome without re-calibrating?

xx=make_single_location_table(data = list(total_results,
                                          age_results),
                              location = "C.12060",
                              outcomes = c("diagnosis.total", "incidence_averted"),
                              interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
                              years = c("2022", "2026", "2035")
)
write_csv(xx, file = "applications/SHIELD/analysis/example_table.csv")
