# Interactive Exploration of Flagged Problematic Stratifications
# Shows raw county-level data to understand WHY each combination was flagged
# Allows team to assess whether removals are justified

library(jheem2)
library(locations)

# Load data
load("cached/syphilis.manager.rdata")
census.manager <- get(load("cached/census.manager.rdata"))
load("cached/stratification_analysis_results.rdata")

# Helper function to analyze a specific problematic combination
analyze_flagged_combination <- function(combo_index = NULL, msa = NULL, year = NULL, outcome = NULL) {
  
  # If specific parameters provided, find the matching combination
  if (!is.null(msa) || !is.null(year) || !is.null(outcome)) {
    for (i in seq_along(problematic_combinations)) {
      combo <- problematic_combinations[[i]]
      if ((is.null(msa) || combo$msa == msa) && 
          (is.null(year) || combo$year == year) &&
          (is.null(outcome) || combo$outcome == outcome)) {
        combo_index <- i
        break
      }
    }
  }
  
  if (is.null(combo_index) || combo_index > length(problematic_combinations)) {
    cat("Invalid combination index or parameters\n")
    return(invisible())
  }
  
  combo <- problematic_combinations[[combo_index]]
  
  cat("================================================================================\n")
  cat(sprintf("ANALYZING FLAGGED COMBINATION %d/%d\n", combo_index, length(problematic_combinations)))
  cat(sprintf("%s %s %s [%s stratification]\n", combo$msa_name, combo$year, combo$outcome, combo$stratification))
  cat(sprintf("Population coverage: %.1f%% (< 90%% threshold)\n", combo$coverage * 100))
  cat("================================================================================\n\n")
  
  # Get all counties in this MSA
  all_counties <- locations::get.contained.locations(combo$msa, "COUNTY")
  cat(sprintf("MSA contains %d counties: %s\n\n", length(all_counties), paste(all_counties, collapse = ", ")))
  
  # Get the county-level data
  county_source <- "cdc.sti"
  
  # Get stratified data
  strat_data <- syphilis.manager$data[[combo$outcome]]$estimate[[county_source]][[combo$ontology]][[combo$stratification]]
  total_data <- syphilis.manager$data[[combo$outcome]]$estimate[[county_source]][[combo$ontology]]$year__location
  
  if (is.null(strat_data) || is.null(total_data)) {
    cat("ERROR: Could not find county-level data for this combination\n")
    return(invisible())
  }
  
  # Check which counties have data for this year
  counties_with_any_data <- intersect(all_counties, dimnames(strat_data)$location)
  counties_with_any_data <- counties_with_any_data[counties_with_any_data %in% dimnames(total_data)$location]
  
  cat("COUNTY-LEVEL BREAKDOWN:\n")
  cat("--------------------------------------------------\n")
  
  county_details <- list()
  total_cases_msa <- 0
  total_strat_cases_msa <- 0
  sufficient_data_counties <- character(0)
  
  for (county in counties_with_any_data) {
    if (!(combo$year %in% dimnames(strat_data)$year) || !(combo$year %in% dimnames(total_data)$year)) {
      next
    }
    
    # Get total cases for this county/year
    county_total <- total_data[combo$year, county]
    
    if (is.na(county_total) || is.nan(county_total)) {
      cat(sprintf("%s: No total data\n", county))
      next
    }
    
    # Get stratified cases
    county_strat_data <- strat_data[combo$year, county, ]
    county_strat_sum <- sum(county_strat_data, na.rm = TRUE)
    
    # Calculate coverage
    coverage <- if (county_total > 0) county_strat_sum / county_total else 0
    
    # Check if this county has sufficient stratification coverage (≥80%)
    sufficient <- coverage >= 0.8
    if (sufficient) {
      sufficient_data_counties <- c(sufficient_data_counties, county)
    }
    
    # Store details
    county_details[[county]] <- list(
      total = county_total,
      strat_sum = county_strat_sum,
      coverage = coverage,
      sufficient = sufficient
    )
    
    total_cases_msa <- total_cases_msa + county_total
    total_strat_cases_msa <- total_strat_cases_msa + county_strat_sum
    
    # Display
    status <- if (sufficient) "✅ SUFFICIENT" else "❌ INSUFFICIENT"
    cat(sprintf("%s: %d total, %d stratified (%.1f%% coverage) %s\n", 
                county, county_total, county_strat_sum, coverage * 100, status))
  }
  
  cat("--------------------------------------------------\n")
  cat(sprintf("MSA TOTALS: %d total cases, %d stratified cases (%.1f%% overall coverage)\n", 
              total_cases_msa, total_strat_cases_msa, total_strat_cases_msa/total_cases_msa*100))
  
  cat(sprintf("Counties with sufficient data (≥80%% coverage): %d/%d\n", 
              length(sufficient_data_counties), length(counties_with_any_data)))
  
  # Population analysis
  cat("\nPOPULATION COVERAGE ANALYSIS:\n")
  cat("--------------------------------------------------\n")
  
  # Get population data
  tryCatch({
    total_pop_data <- census.manager$pull(
      outcome = "population",
      source = "census.population", 
      from.ontology.names = "census",
      dimension.values = list(location = all_counties, year = combo$year),
      keep.dimensions = c("location"),
      na.rm = TRUE
    )
    
    sufficient_pop <- if (length(sufficient_data_counties) > 0) {
      census.manager$pull(
        outcome = "population",
        source = "census.population",
        from.ontology.names = "census", 
        dimension.values = list(location = sufficient_data_counties, year = combo$year),
        keep.dimensions = c("location"),
        na.rm = TRUE
      )
    } else {
      0
    }
    
    total_population <- sum(total_pop_data, na.rm = TRUE)
    population_with_sufficient_data <- sum(sufficient_pop, na.rm = TRUE)
    pop_coverage <- population_with_sufficient_data / total_population
    
    cat(sprintf("Total MSA population (%s): %s\n", combo$year, format(total_population, big.mark = ",")))
    cat(sprintf("Population in counties with sufficient data: %s\n", format(population_with_sufficient_data, big.mark = ",")))
    cat(sprintf("Population coverage: %.1f%% (threshold: 90%%)\n", pop_coverage * 100))
    
    if (pop_coverage < 0.9) {
      cat("❌ FLAGGED FOR REMOVAL: Population coverage below 90% threshold\n")
    } else {
      cat("✅ WOULD BE PRESERVED: Population coverage above 90% threshold\n")
    }
    
  }, error = function(e) {
    cat("ERROR getting population data:", e$message, "\n")
  })
  
  # Show the resulting MSA-level aggregated data issue
  cat("\nMSA-LEVEL AGGREGATED DATA (what models see):\n")
  cat("--------------------------------------------------\n")
  
  msa_total <- syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location[combo$year, combo$msa]
  msa_strat_data <- syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]][[combo$stratification]][combo$year, combo$msa, ]
  
  cat(sprintf("MSA total: %s\n", msa_total))
  cat("MSA stratification breakdown:\n")
  for (i in seq_along(msa_strat_data)) {
    strat_name <- names(msa_strat_data)[i]
    value <- msa_strat_data[i]
    if (is.nan(value)) {
      cat(sprintf("  %s: NaN ⚠️\n", strat_name))
    } else {
      cat(sprintf("  %s: %s\n", strat_name, value))
    }
  }
  msa_strat_sum <- sum(msa_strat_data, na.rm = TRUE)
  cat(sprintf("MSA stratification sum: %s (missing: %s cases)\n", 
              msa_strat_sum, msa_total - msa_strat_sum))
  
  cat("\n================================================================================\n\n")
  
  return(invisible(county_details))
}

# Quick overview of all flagged combinations
show_all_flagged_summary <- function() {
  cat("ALL FLAGGED COMBINATIONS SUMMARY:\n")
  cat("================================================================================\n")
  
  for (i in seq_along(problematic_combinations)) {
    combo <- problematic_combinations[[i]]
    cat(sprintf("%2d. %s %s %s [%.1f%% pop coverage]\n", 
                i, combo$msa_name, combo$year, combo$outcome, combo$coverage * 100))
  }
  
  cat("\nTo analyze a specific combination, run:\n")
  cat("analyze_flagged_combination(combo_index = X)  # where X is the number above\n")
  cat("OR\n")
  cat("analyze_flagged_combination(msa = 'C.12580', year = '2022', outcome = 'ps.syphilis.diagnoses')\n")
  cat("================================================================================\n\n")
}

cat("PROBLEMATIC STRATIFICATIONS EXPLORATION TOOL\n")
cat("================================================================================\n")
cat("This script helps you understand WHY each combination was flagged for removal.\n\n")

cat("Available functions:\n")
cat("1. show_all_flagged_summary()           - Overview of all 15 flagged combinations\n")
cat("2. analyze_flagged_combination(N)       - Detailed analysis of combination N\n")
cat("3. analyze_flagged_combination(msa=..., year=..., outcome=...)  - Find by parameters\n\n")

cat("EXAMPLE USAGE:\n")
cat("show_all_flagged_summary()                    # See all flagged combinations\n")
cat("analyze_flagged_combination(1)                # Analyze Baltimore 2022 PS syphilis\n")
cat("analyze_flagged_combination(14)               # Analyze Atlanta 2022 Unknown duration\n\n")

# Show summary by default
show_all_flagged_summary()

# Example: Analyze a few key cases
cat("SAMPLE ANALYSIS - Baltimore 2022 PS Syphilis (most problematic?):\n")
analyze_flagged_combination(1)

cat("SAMPLE ANALYSIS - Atlanta 2022 Unknown Duration (lowest coverage):\n")
analyze_flagged_combination(14)
