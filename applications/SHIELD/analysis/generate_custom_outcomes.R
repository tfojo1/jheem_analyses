
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

calculate_averted_count <- function(arr, dn.one.outcome, noint = NOINT) {
    stratification_dimensions <- setdiff(names(dn.one.outcome), c("year", "sim", "intervention", "location"))
    apply(array(apply(arr,
                      c("year", stratification_dimensions, "sim", "location"),
                      function(x) {
                          x[noint] - x
                      }),
                sapply(dn.one.outcome, length),
                dn.one.outcome),
          c("year", stratification_dimensions, "sim", "intervention", "location"),
          function(x) {x})
}

calculate_averted_pct <- function(arr, dn.one.outcome, noint = NOINT) {
    stratification_dimensions <- setdiff(names(dn.one.outcome), c("year", "sim", "intervention", "location"))
    apply(array(apply(arr,
                      c("year", stratification_dimensions, "sim", "location"),
                      function(x) {
                          100 * (x[noint] - x) / x[noint]
                      }),
                sapply(dn.one.outcome, length),
                dn.one.outcome),
          c("year", stratification_dimensions, "sim", "intervention", "location"),
          function(x) {x})
}

calculate_rate <- function(arr, num.outcome, denom.outcome, dn.one.outcome) {
    # browser()
    stratification_dimensions <- setdiff(names(dn.one.outcome), c("year", "sim", "intervention", "location"))
    apply(arr,
          c("year", stratification_dimensions, "sim", "intervention","location"),
          function(x) {
              100000 * (x[num.outcome]) /x[denom.outcome]
          })
    
}

calculate_rate_averted <- function(arr, num.outcome, denom.outcome, dn.one.outcome, noint = NOINT) {
    stratification_dimensions <- setdiff(names(dn.one.outcome), c("year", "sim", "intervention", "location"))
    apply(array(apply(arr,
                      c("year", stratification_dimensions, "sim", "location"),
                      function(x) {
                          100000 * (x[noint, num.outcome] - x[,num.outcome]) /
                              x[,denom.outcome]
                      }),
                sapply(dn.one.outcome, length),
                dn.one.outcome),
          c("year", stratification_dimensions, "sim", "intervention", "location"),
          function(x) {x})
}

calculate_cumulative <- function(arr, stratification.dimensions = NULL) {
    array(
        apply(arr,
              c(stratification.dimensions, "sim", "intervention", "location"),
              cumsum),
        dim(arr),
        dimnames(arr)
    )
}

calculate_pct_reduction_versus_year <- function(arr, yr, dn.one.outcome, noint = NOINT) {
    stratification_dimensions <- setdiff(names(dn.one.outcome), c("year", "sim", "intervention", "location"))
    apply(array(apply(arr,
                      c(stratification_dimensions, "sim", "intervention", "location"),
                      function(x) {
                          100 * (x[yr] - x) / x[yr]
                      }),
                sapply(dn.one.outcome, length),
                dn.one.outcome),
          c("year", stratification_dimensions, "sim", "intervention", "location"),
          function(x) {x})
}

calculate_custom_outcomes <- function(raw_results, baseline.year, debug=F) {
    if (debug) browser()
    
    end_year <- dimnames(raw_results)$year[dim(raw_results)["year"]]
    YEARS_TO_KEEP <- as.character(baseline.year:end_year)
    
    STRATIFICATION_DIMENSIONS = setdiff(names(dim(raw_results)), c("year", "sim", "intervention", "location", "outcome"))
    if (length(STRATIFICATION_DIMENSIONS)==0) STRATIFICATION_DIMENSIONS <- NULL
    
    print("Re-ordering results to have outcome last")
    
    # This has to have all outcomes we'll ever use for calculations (it gets extended just below)
    # Subsetting now lets us shrink the array and speed up the runtime
    CHOSEN_OUTCOMES <- c("incidence", "diagnosis.total", "diagnosis.ps", "population")
    if (is.null(STRATIFICATION_DIMENSIONS))
        CHOSEN_OUTCOMES <- c(CHOSEN_OUTCOMES, "population.msm", "doxy.coverage")
    results <- subset_array(raw_results, list(year = YEARS_TO_KEEP, outcome = CHOSEN_OUTCOMES))
    results <- apply(results,
                     c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention", "location", "outcome"),
                     function(x) {x})
    
    # Intervention is on the front because most of the apply's will leave it on the front
    dn_one_outcome <- dimnames(results)[c("intervention", "year", STRATIFICATION_DIMENSIONS, "sim","location")]
    
    # But if we're doing something based on year (like percent reduction relative to 2022), year will remain in front
    dn_one_outcome_yr_first <- dimnames(results)[c("year", STRATIFICATION_DIMENSIONS, "sim","intervention", "location")]
    
    print("Calculating incidence outcomes")
    incidence_averted <-
        calculate_averted_count(subset_array(results, list(outcome = "incidence"), drop=T),
                                dn_one_outcome)
    incidence_averted_percent <-
        calculate_averted_pct(subset_array(results, list(outcome = "incidence"), drop=T),
                              dn_one_outcome)
    incidence_averted_rate_per_pop <-
        calculate_rate_averted(subset_array(results, list(outcome = c("incidence", "population")), drop=T),
                               num.outcome = "incidence",
                               denom.outcome = "population",
                               dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        # For total results only
        incidence_averted_rate_per_msm <-
            calculate_rate_averted(subset_array(results, list(outcome = c("incidence", "population.msm")), drop=T),
                                   num.outcome = "incidence",
                                   denom.outcome = "population.msm",
                                   dn_one_outcome)
        incidence_averted_ppy_doxy <-
            calculate_rate_averted(subset_array(results, list(outcome = c("incidence", "doxy.coverage")), drop=T),
                                   num.outcome = "incidence",
                                   denom.outcome = "doxy.coverage",
                                   dn_one_outcome)
    }
    pct_incidence_reduction_vs_2022 <-
        calculate_pct_reduction_versus_year(subset_array(results, list(outcome = "incidence"), drop=T),
                                            "2022",
                                            dn_one_outcome_yr_first)
    incidence_rate <- 
        calculate_rate(subset_array(results, list(outcome = c("incidence","population")), drop=T), 
                       num.outcome = "incidence",
                       denom.outcome = "population",
                       dn_one_outcome)
    
    print("Calculating total diagnosis outcomes")
    diagnosis_total_averted <-
        calculate_averted_count(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                                dn_one_outcome)
    
    diagnosis_total_averted_percent <-
        calculate_averted_pct(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                              dn_one_outcome)
    diagnosis_total_averted_rate_per_pop <-
        calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.total", "population")), drop=T),
                               num.outcome = "diagnosis.total",
                               denom.outcome = "population",
                               dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        # For total results only
        diagnosis_total_averted_rate_per_msm <-
            calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.total", "population.msm")), drop=T),
                                   num.outcome = "diagnosis.total",
                                   denom.outcome = "population.msm",
                                   dn_one_outcome)
        diagnosis_total_averted_rate_ppy_doxy <-
            calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.total", "doxy.coverage")), drop=T),
                                   num.outcome = "diagnosis.total",
                                   denom.outcome = "doxy.coverage",
                                   dn_one_outcome 
            )
    }
    pct_diagnosis_total_reduction_vs_2022 <-
        calculate_pct_reduction_versus_year(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                                            "2022",
                                            dn_one_outcome_yr_first)
    diagnosis_total_rate <- 
        calculate_rate(subset_array(results, list(outcome = c("diagnosis.total","population")), drop=T), 
                       num.outcome = "diagnosis.total",
                       denom.outcome = "population",
                       dn_one_outcome)
    
    print("Calculating PS diagnosis outcomes")
    diagnosis_ps_averted <-
        calculate_averted_count(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                                dn_one_outcome)
    diagnosis_ps_averted_percent <-
        calculate_averted_pct(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                              dn_one_outcome)
    diagnosis_ps_averted_rate_per_pop <-
        calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.ps", "population")), drop=T),
                               num.outcome = "diagnosis.ps",
                               denom.outcome = "population",
                               dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        # For total results only
        diagnosis_ps_averted_rate_per_msm <-
            calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.ps", "population.msm")), drop=T),
                                   num.outcome = "diagnosis.ps",
                                   denom.outcome = "population.msm",
                                   dn_one_outcome)
        diagnosis_ps_averted_rate_ppy_doxy <-
            calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.ps", "doxy.coverage")), drop=T),
                                   num.outcome = "diagnosis.ps",
                                   denom.outcome = "doxy.coverage",
                                   dn_one_outcome )
    }
    pct_diagnosis_ps_reduction_vs_2022 <-
        calculate_pct_reduction_versus_year(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                                            "2022",
                                            dn_one_outcome_yr_first)
    diagnosis_ps_rate <- 
        calculate_rate(subset_array(results, list(outcome = c("diagnosis.ps","population")), drop=T), 
                       num.outcome = "diagnosis.ps",
                       denom.outcome = "population",
                       dn_one_outcome)
    
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        print("Calculating cumulative doxy coverage")
        cum_doxy_coverage <-
            calculate_cumulative(subset_array(results, list(outcome = "doxy.coverage"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    }
    
    # Cumulative incidence averted
    print("Calculating cumulative incidence outcomes")
    cum_incidence <-
        calculate_cumulative(subset_array(results, list(outcome = "incidence"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    cum_incidence_averted <-
        calculate_averted_count(cum_incidence, dn_one_outcome)
    cum_incidence_averted_percent <-
        calculate_averted_pct(cum_incidence, dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        rate_cum_incidence_averted_ppy_doxy <-
            100000 * cum_incidence_averted / cum_doxy_coverage
    }
    
    # Cumulative diagnoses averted
    print("Calculating cumulative total diagnosis outcomes")
    cum_diagnosis_total <-
        calculate_cumulative(subset_array(results, list(outcome = "diagnosis.total"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    cum_diagnosis_total_averted <-
        calculate_averted_count(cum_diagnosis_total, dn_one_outcome)
    cum_diagnosis_total_averted_percent <-
        calculate_averted_pct(cum_diagnosis_total, dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        rate_cum_diagnosis_total_averted_ppy_doxy <-
            100000 * cum_diagnosis_total_averted / cum_doxy_coverage
    }
    
    print("Calculating cumulative PS diagnosis outcomes")
    cum_diagnosis_ps <-
        calculate_cumulative(subset_array(results, list(outcome = "diagnosis.ps"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    cum_diagnosis_ps_averted <-
        calculate_averted_count(cum_diagnosis_ps, dn_one_outcome)
    cum_diagnosis_ps_averted_percent <-
        calculate_averted_pct(cum_diagnosis_ps, dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        rate_cum_diagnosis_ps_averted_ppy_doxy <-
            100000 * cum_diagnosis_ps / cum_doxy_coverage
    }
    
    # Combine, but DO NOT INCLUDE ORIGINAL OUTCOMES (to keep it smaller)
    dn_w_custom <- dimnames(results)
    # Add names of locations back in
    dn_w_custom[["location"]] <- dimnames(raw_results)[["location"]]
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        dn_w_custom[["outcome"]] <- c(
            
            "incidence_averted",
            "incidence_averted_percent",
            "incidence_averted_rate_per_pop",
            "incidence_averted_rate_per_msm",
            "incidence_averted_ppy_doxy",
            "pct_incidence_reduction_vs_2022",
            "incidence_rate",
            
            "diagnosis_total_averted",
            "diagnosis_total_averted_percent",
            "diagnosis_total_averted_rate_per_pop",
            "diagnosis_total_averted_rate_per_msm",
            "diagnosis_total_averted_rate_ppy_doxy",
            "pct_diagnosis_total_reduction_vs_2022",
            "diagnosis_total_rate",
            
            "diagnosis_ps_averted",
            "diagnosis_ps_averted_percent",
            "diagnosis_ps_averted_rate_per_pop",
            "diagnosis_ps_averted_rate_per_msm",
            "diagnosis_ps_averted_rate_ppy_doxy",
            "pct_diagnosis_ps_reduction_vs_2022",
            "diagnosis_ps_rate",
            
            "cum_doxy_coverage",
            
            "cum_incidence",
            "cum_incidence_averted",
            "cum_incidence_averted_percent",
            "rate_cum_incidence_averted_ppy_doxy",
            
            "cum_diagnosis_total",
            "cum_diagnosis_total_averted",
            "cum_diagnosis_total_averted_percent",
            "rate_cum_diagnosis_total_averted_ppy_doxy",
            
            "cum_diagnosis_ps",
            "cum_diagnosis_ps_averted",
            "cum_diagnosis_ps_averted_percent",
            "rate_cum_diagnosis_ps_averted_ppy_doxy"
        )
        results_w_custom <-
            array(
                c(
                    incidence_averted,
                    incidence_averted_percent,
                    incidence_averted_rate_per_pop,
                    incidence_averted_rate_per_msm,
                    incidence_averted_ppy_doxy,
                    pct_incidence_reduction_vs_2022,
                    incidence_rate,
                    
                    diagnosis_total_averted,
                    diagnosis_total_averted_percent,
                    diagnosis_total_averted_rate_per_pop,
                    diagnosis_total_averted_rate_per_msm,
                    diagnosis_total_averted_rate_ppy_doxy,
                    pct_diagnosis_total_reduction_vs_2022,
                    diagnosis_total_rate,
                    
                    diagnosis_ps_averted,
                    diagnosis_ps_averted_percent,
                    diagnosis_ps_averted_rate_per_pop,
                    diagnosis_ps_averted_rate_per_msm,
                    diagnosis_ps_averted_rate_ppy_doxy,
                    pct_diagnosis_ps_reduction_vs_2022,
                    diagnosis_ps_rate,
                    
                    cum_doxy_coverage,
                    
                    cum_incidence,
                    cum_incidence_averted,
                    cum_incidence_averted_percent,
                    rate_cum_incidence_averted_ppy_doxy,
                    
                    cum_diagnosis_total,
                    cum_diagnosis_total_averted,
                    cum_diagnosis_total_averted_percent,
                    rate_cum_diagnosis_total_averted_ppy_doxy,
                    
                    cum_diagnosis_ps,
                    cum_diagnosis_ps_averted,
                    cum_diagnosis_ps_averted_percent,
                    rate_cum_diagnosis_ps_averted_ppy_doxy
                ),
                sapply(dn_w_custom, length),
                dn_w_custom
            )
    } else {
        dn_w_custom[["outcome"]] <- c("incidence_averted",
                                      "incidence_averted_percent",
                                      "incidence_averted_rate_per_pop",
                                      "pct_incidence_reduction_vs_2022",
                                      "incidence_rate",
                                      
                                      "diagnosis_total_averted",
                                      "diagnosis_total_averted_percent",
                                      "diagnosis_total_averted_rate_per_pop",
                                      "pct_diagnosis_total_reduction_vs_2022",
                                      "diagnosis_total_rate",
                                      
                                      "diagnosis_ps_averted",
                                      "diagnosis_ps_averted_percent",
                                      "diagnosis_ps_averted_rate_per_pop",
                                      "pct_diagnosis_ps_reduction_vs_2022",
                                      "diagnosis_ps_rate",
                                      
                                      "cum_incidence",
                                      "cum_incidence_averted",
                                      "cum_incidence_averted_percent",
                                      
                                      "cum_diagnosis_total",
                                      "cum_diagnosis_total_averted",
                                      "cum_diagnosis_total_averted_percent",
                                      
                                      "cum_diagnosis_ps",
                                      "cum_diagnosis_ps_averted",
                                      "cum_diagnosis_ps_averted_percent"
        )
        results_w_custom <-
            array(
                c(incidence_averted,
                  incidence_averted_percent,
                  incidence_averted_rate_per_pop,
                  pct_incidence_reduction_vs_2022,
                  incidence_rate,
                  
                  diagnosis_total_averted,
                  diagnosis_total_averted_percent,
                  diagnosis_total_averted_rate_per_pop,
                  pct_diagnosis_total_reduction_vs_2022,
                  diagnosis_total_rate,
                  
                  diagnosis_ps_averted,
                  diagnosis_ps_averted_percent,
                  diagnosis_ps_averted_rate_per_pop,
                  pct_diagnosis_ps_reduction_vs_2022,
                  diagnosis_ps_rate,
                  
                  cum_incidence,
                  cum_incidence_averted,
                  cum_incidence_averted_percent,
                  
                  cum_diagnosis_total,
                  cum_diagnosis_total_averted,
                  cum_diagnosis_total_averted_percent,
                  
                  cum_diagnosis_ps,
                  cum_diagnosis_ps_averted,
                  cum_diagnosis_ps_averted_percent
                ),
                sapply(dn_w_custom, length),
                dn_w_custom
            )
    }
    
}


# Verify accuracy of these transformations
# total_results["2026", 3, "noint", "C.12580", "incidence"] -
#     total_results["2026", 3, "doxy.cov.10", "C.12580", "incidence"] ==
#     total_results_w_custom["2026", 3, "doxy.cov.10", "C.12580", "incidence_averted"]

# BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")


if (1==2) {
    BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
    
    total_raw_results <- get(load(paste0(BASE.PATH, "/total_raw_results.Rdata")))
    total_calc_results <- calculate_custom_outcomes(total_raw_results, baseline.year = "2022")
    
    save(total_calc_results,
         file = paste0(BASE.PATH, "/total_calc_results1.Rdata"))
    
}
if (1==2) {
    BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
    sex_raw_results <- get(load(paste0(BASE.PATH, "/sex_raw_results.Rdata")))
    sex_calc_results <- calculate_custom_outcomes(sex_raw_results, baseline.year = "2022")
    save(sex_calc_results,
         file = paste0(BASE.PATH, "/sex_results.Rdata"))
}

#cumulative incidence : add a baseline year: 2022 update all instances 
#filter calculated outcomes to post 2022
# double checking ppy calculation 
# add incidence rate, and total diagnosis rate
