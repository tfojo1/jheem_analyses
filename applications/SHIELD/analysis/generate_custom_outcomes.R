
# number of cases averted (incidence and diagnosis)
# cumulative cases (relative to a start year that user specifies) (incidence and diagnosis)
# percent reduction in cumulative cases (incidence and diagnosis)
# percent reduction in cases (incidence and diagnosis)

CALIB_CODE <- "calib.8.21.stage3.az"
NOINT = "noint"

# Need to find a single home for this
# Also change "dim_indices" to "dimension.values", because that's what it is
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

calculate_custom_outcomes <- function(raw_results, debug=F) {
    if (debug) browser()
    
    # STRATIFICATION_DIMENSIONS = NULL
    STRATIFICATION_DIMENSIONS = setdiff(names(dim(raw_results)), c("year", "sim", "intervention", "location", "outcome"))
    
    print("Re-ordering results to have outcome last")
    results <- apply(raw_results,
                           c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location", "outcome"),
                           function(x) {x})
    
    dn_one_outcome <- dimnames(results)[c("intervention", "year", STRATIFICATION_DIMENSIONS, "sim","location")]
    
    # Num incidence cases averted vs. noint
    print("Calculating outcome 1/8")
    incidence_averted_count <-
        apply(array(apply(subset_array(results, list(outcome = "incidence"), drop=T),
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              x[NOINT] - x
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    
    print("Calculating outcome 2/8")
    percent_reduction_incidence <-
        apply(array(apply(subset_array(results, list(outcome = "incidence"), drop=T),
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              100 * (x[NOINT] - x) / x[NOINT]
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    
    # Num diagnoses averted vs. noint
    
    print("Calculating outcome 3/8")
    diagnosis_averted_count <-
        apply(array(apply(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              x[NOINT] - x
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    
    print("Calculating outcome 4/8")
    percent_reduction_diagnosis <-
        apply(array(apply(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              100 * (x[NOINT] - x) / x[NOINT]
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    
    # Cumulative incidence averted
    print("Calculating outcome 5/8")
    cumulative_incidence <- 
        array(
            apply(subset_array(results, list(outcome = "incidence"), drop=T),
                  c(STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
                  cumsum),
            dim(subset_array(results, list(outcome = "incidence"), drop=T)),
            dimnames(subset_array(results, list(outcome = "incidence"), drop=T))
        )
    cumulative_incidence_averted <-
        apply(array(apply(cumulative_incidence,
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              x[NOINT] - x
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    print("Calculating outcome 6/8")
    percent_reduction_cumulative_incidence <-
        cumulative_incidence_averted <-
        apply(array(apply(cumulative_incidence,
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              100 * (x[NOINT] - x) / x[NOINT]
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    
    # Cumulative diagnoses averted
    print("Calculating outcome 7/8")
    cumulative_diagnosis <- 
        array(
            apply(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                  c(STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
                  cumsum),
            dim(subset_array(results, list(outcome = "incidence"), drop=T)),
            dimnames(subset_array(results, list(outcome = "incidence"), drop=T))
        )
    cumulative_diagnosis_averted <-
        apply(array(apply(cumulative_diagnosis,
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              x[NOINT] - x
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    print("Calculating outcome 8/8")
    percent_reduction_cumulative_diagnosis <-
        apply(array(apply(cumulative_diagnosis,
                          c("year", STRATIFICATION_DIMENSIONS, "sim", "location"),
                          function(x) {
                              # Will be positive if cases have been averted
                              100 * (x[NOINT] - x) / x[NOINT]
                          }),
                    sapply(dn_one_outcome, length),
                    dn_one_outcome),
              c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location"),
              function(x) {x})
    
    # Combine
    dn_w_custom <- dimnames(results)
    dn_w_custom[["outcome"]] <- c(dn_w_custom[["outcome"]],
                                  "incidence_averted",
                                  "pct_incidence_averted",
                                  "diagnosis_averted",
                                  "pct_diagnosis_averted",
                                  "cum_incidence_averted",
                                  "pct_cum_incidence_averted",
                                  "cum_diagnosis_averted",
                                  "pct_cum_diagnosis_averted")
    results_w_custom <-
        array(
            c(results,
              incidence_averted_count,
              percent_reduction_incidence,
              diagnosis_averted_count,
              percent_reduction_diagnosis,
              cumulative_incidence_averted,
              percent_reduction_cumulative_incidence,
              cumulative_diagnosis_averted,
              percent_reduction_cumulative_diagnosis),
            sapply(dn_w_custom, length),
            dn_w_custom
        )
}


# Verify accuracy of these transformations
# total_results["2026", 3, "noint", "C.12580", "incidence"] -
#     total_results["2026", 3, "doxy.cov.10", "C.12580", "incidence"] ==
#     total_results_w_custom["2026", 3, "doxy.cov.10", "C.12580", "incidence_averted"]

if (1==2) {
    total_raw_results <- get(load(paste0("Q:/shield/outputs/", CALIB_CODE, "/total_raw_results.Rdata")))
    total_results <- calculate_custom_outcomes(total_results)
    save(total_results,
         file = paste0("Q:/shield/outputs/", CALIB_CODE, "/total_results.Rdata"))
}
if (1==2) {
    age_raw_results <- get(load(paste0("Q:/shield/outputs/", CALIB_CODE, "/age_raw_results.Rdata")))
    age_results <- calculate_custom_outcomes(age_raw_results)
    save(age_results,
         file = paste0("Q:/shield/outputs/", CALIB_CODE, "/age_results.Rdata"))
}