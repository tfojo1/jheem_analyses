# Identify Problematic Stratifications - Population Coverage Analysis
# Finds MSA/year/outcome/stratification combinations with insufficient population coverage
# Uses county-level data to assess coverage, applies removal to MSA-level aggregated data

library(jheem2)
library(locations)

# Load data managers
cat("Loading data managers...\n")
load("cached/syphilis.manager.rdata")
census.manager <- get(load("cached/census.manager.rdata"))

# Configuration
POPULATION_COVERAGE_THRESHOLD <- 0.9  # Require 90% population coverage
COUNTY_STRATIFICATION_THRESHOLD <- 0.8  # Require 80% stratification coverage within each county
TARGET_MSAS <- c("C.12580", "C.35620", "C.33100", "C.12060")  # Baltimore, NYC, Miami, Atlanta
MSA_NAMES <- c("Baltimore", "NYC", "Miami", "Atlanta")
names(MSA_NAMES) <- TARGET_MSAS

TARGET_OUTCOMES <- c("ps.syphilis.diagnoses", "early.syphilis.diagnoses", "unknown.duration.or.late.syphilis.diagnoses")
COUNTY_SOURCE_NAME <- "cdc.sti"  # County-level data for coverage analysis
MSA_SOURCE_NAME <- "cdc.aggregated.county"  # MSA-level data to potentially remove

# Start with race stratifications, but make extensible
TARGET_STRATIFICATIONS <- c("year__location__race")
# Future: c("year__location__race", "year__location__age", "year__location__sex", "year__location__age__race", etc.)

# Check what years are actually available first
sample_data <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[COUNTY_SOURCE_NAME]]$cdc.sti$year__location__race
available_years <- dimnames(sample_data)$year
cat(sprintf("Available years: %s\n", paste(tail(available_years, 10), collapse=", ")))

# Our target years
TARGET_YEARS_DESIRED <- c("2020", "2021", "2022", "2023")
TARGET_YEARS <- intersect(TARGET_YEARS_DESIRED, available_years)
if (length(TARGET_YEARS) == 0) {
  cat("No target years 2020-2023 found, using most recent available years\n")
  TARGET_YEARS <- tail(available_years, 4)
}
cat(sprintf("Using years: %s\n", paste(TARGET_YEARS, collapse=", ")))

cat("\n=== POPULATION COVERAGE ANALYSIS ===\n")
cat(sprintf("Threshold: %.0f%% population coverage required\n", POPULATION_COVERAGE_THRESHOLD * 100))
cat(sprintf("Target MSAs: %s\n", paste(MSA_NAMES, collapse=", ")))
cat(sprintf("Target years: %s\n", paste(TARGET_YEARS, collapse=", ")))
cat(sprintf("Target stratifications: %s\n", paste(TARGET_STRATIFICATIONS, collapse=", ")))
cat("\n")

# Function to get counties with sufficient stratified data from county-level source
get_counties_with_sufficient_data <- function(outcome, ontology, stratification, msa, year) {
  
  # Get all counties in MSA
  all_counties <- locations::get.contained.locations(msa, "COUNTY")
  
  # Get the county-level data arrays
  strat_data <- syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]][[stratification]]
  total_data <- syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]]$year__location
  
  if (is.null(strat_data) || is.null(total_data)) return(character(0))
  
  # Check which counties have sufficient stratification coverage for this year
  if (!"year" %in% names(dim(strat_data))) return(character(0))
  if (!(year %in% dimnames(strat_data)$year)) return(character(0))
  if (!"location" %in% names(dim(strat_data))) return(character(0))
  
  counties_with_sufficient_data <- character(0)
  for (county in intersect(all_counties, dimnames(strat_data)$location)) {
    # Get stratified data for this county/year
    county_strat_data <- strat_data[year, county, ]
    
    # Get total data for this county/year
    if (county %in% dimnames(total_data)$location && year %in% dimnames(total_data)$year) {
      county_total <- total_data[year, county]
      
      if (!is.na(county_total) && county_total > 0) {
        # Calculate stratification coverage within this county
        strat_sum <- sum(county_strat_data, na.rm = TRUE)
        county_coverage <- strat_sum / county_total
        
        # County needs ≥80% stratification coverage to count as "having sufficient data"
        if (county_coverage >= COUNTY_STRATIFICATION_THRESHOLD) {
          counties_with_sufficient_data <- c(counties_with_sufficient_data, county)
        }
      }
    }
  }
  
  return(counties_with_sufficient_data)
}

# Function to check population coverage
check_population_coverage <- function(msa, year, counties_with_sufficient_data) {
  
  all_counties <- locations::get.contained.locations(msa, "COUNTY")
  
  tryCatch({
    # Get total population
    total_pop_data <- census.manager$pull(
      outcome = "population",
      source = "census.population",
      from.ontology.names = "census",
      dimension.values = list(location = all_counties, year = year),
      keep.dimensions = c("location"),
      na.rm = TRUE
    )
    
    # Get population for counties with sufficient data
    counties_with_sufficient_data_in_msa <- intersect(counties_with_sufficient_data, all_counties)
    if (length(counties_with_sufficient_data_in_msa) == 0) {
      return(list(coverage = 0, total_population = 0, population_with_data = 0, error = "No counties with sufficient data"))
    }
    
    pop_with_data <- census.manager$pull(
      outcome = "population",
      source = "census.population",
      from.ontology.names = "census",
      dimension.values = list(location = counties_with_sufficient_data_in_msa, year = year),
      keep.dimensions = c("location"),
      na.rm = TRUE
    )
    
    total_population <- sum(total_pop_data, na.rm = TRUE)
    population_with_data <- sum(pop_with_data, na.rm = TRUE)
    
    if (total_population == 0) {
      return(list(coverage = 0, total_population = 0, population_with_data = 0, error = "No population data"))
    }
    
    coverage <- population_with_data / total_population
    
    return(list(
      coverage = coverage,
      total_population = total_population,
      population_with_data = population_with_data,
      counties_total = length(all_counties),
      counties_with_sufficient_data = length(counties_with_sufficient_data_in_msa),
      missing_counties = setdiff(all_counties, counties_with_sufficient_data_in_msa)
    ))
    
  }, error = function(e) {
    return(list(coverage = 0, error = e$message))
  })
}

# Main analysis loop
problematic_combinations <- list()
analysis_summary <- data.frame()

for (outcome in TARGET_OUTCOMES) {
  cat(sprintf("Analyzing %s...\n", outcome))
  
  # Determine ontology for this outcome
  if (outcome == "ps.syphilis.diagnoses") {
    ontology <- "cdc.sti"
  } else {
    ontology <- "cdc.sti.two"  # Early and Unknown use different ontology
  }
  
  # Check if county-level data exists for this outcome/source/ontology
  if (is.null(syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]])) {
    cat(sprintf("  No county-level data found for %s/%s/%s\n", outcome, COUNTY_SOURCE_NAME, ontology))
    next
  }
  
  # Check if MSA-level data exists for this outcome/source/ontology
  if (is.null(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]])) {
    cat(sprintf("  No MSA-level data found for %s/%s/%s\n", outcome, MSA_SOURCE_NAME, ontology))
    next
  }
  
  for (stratification in TARGET_STRATIFICATIONS) {
    
    # Check if this stratification exists in county-level data
    if (!(stratification %in% names(syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Stratification %s not found in county-level data for %s\n", stratification, outcome))
      next
    }
    
    # Check if this stratification exists in MSA-level data
    if (!(stratification %in% names(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Stratification %s not found in MSA-level data for %s\n", stratification, outcome))
      next
    }
    
    for (msa in TARGET_MSAS) {
      for (year in TARGET_YEARS) {
        
        # Get counties with sufficient stratified data from county-level source
        counties_with_sufficient_data <- get_counties_with_sufficient_data(outcome, ontology, stratification, msa, year)
        
        if (length(counties_with_sufficient_data) == 0) {
          # No counties have sufficient stratified data - skip
          next
        }
        
        # Check population coverage
        coverage_result <- check_population_coverage(msa, year, counties_with_sufficient_data)
        
        if (!is.null(coverage_result$error)) {
          cat(sprintf("  Error for %s %s %s: %s\n", MSA_NAMES[msa], year, outcome, coverage_result$error))
          next
        }
        
        # Record results
        analysis_row <- data.frame(
          outcome = outcome,
          ontology = ontology,
          stratification = stratification,
          msa = msa,
          msa_name = MSA_NAMES[msa],
          year = year,
          coverage = coverage_result$coverage,
          counties_total = coverage_result$counties_total,
          counties_with_sufficient_data = coverage_result$counties_with_sufficient_data,
          population_total = coverage_result$total_population,
          population_with_data = coverage_result$population_with_data,
          needs_removal = coverage_result$coverage < POPULATION_COVERAGE_THRESHOLD,
          stringsAsFactors = FALSE
        )
        
        analysis_summary <- rbind(analysis_summary, analysis_row)
        
        # Track problematic combinations
        if (coverage_result$coverage < POPULATION_COVERAGE_THRESHOLD) {
          key <- paste(outcome, ontology, stratification, msa, year, sep = "|")
          problematic_combinations[[key]] <- list(
            outcome = outcome,
            ontology = ontology,
            stratification = stratification,
            msa = msa,
            msa_name = MSA_NAMES[msa],
            year = year,
            coverage = coverage_result$coverage,
            missing_counties = coverage_result$missing_counties,
            counties_with_sufficient_data = coverage_result$counties_with_sufficient_data,
            counties_total = coverage_result$counties_total
          )
          
          cat(sprintf("  ❌ %s %s %s [%s]: %.1f%% population coverage\n",
                     MSA_NAMES[msa], year, outcome, stratification, coverage_result$coverage * 100))
        }
      }
    }
  }
}

# Results
cat("\n=== ANALYSIS RESULTS ===\n")

if (nrow(analysis_summary) == 0) {
  cat("No data found for analysis\n")
} else {
  
  cat(sprintf("Total combinations analyzed: %d\n", nrow(analysis_summary)))
  cat(sprintf("Combinations needing removal: %d\n", sum(analysis_summary$needs_removal)))
  cat(sprintf("Combinations to preserve: %d\n", sum(!analysis_summary$needs_removal)))
  
  cat("\nBY MSA:\n")
  for (msa in TARGET_MSAS) {
    msa_data <- analysis_summary[analysis_summary$msa == msa, ]
    if (nrow(msa_data) > 0) {
      removals <- sum(msa_data$needs_removal)
      total <- nrow(msa_data)
      cat(sprintf("  %s: %d/%d need removal (%.1f%%)\n", 
                 MSA_NAMES[msa], removals, total, removals/total*100))
    }
  }
  
  cat("\nBY OUTCOME:\n")
  for (outcome in TARGET_OUTCOMES) {
    outcome_data <- analysis_summary[analysis_summary$outcome == outcome, ]
    if (nrow(outcome_data) > 0) {
      removals <- sum(outcome_data$needs_removal)
      total <- nrow(outcome_data)
      cat(sprintf("  %s: %d/%d need removal (%.1f%%)\n", 
                 outcome, removals, total, removals/total*100))
    }
  }
  
  if (length(problematic_combinations) > 0) {
    cat("\nPROBLEMATIC COMBINATIONS (need removal):\n")
    for (i in seq_along(problematic_combinations)) {
      combo <- problematic_combinations[[i]]
      cat(sprintf("  %s %s %s [%s]: %.1f%% coverage (%d/%d counties)\n",
                 combo$msa_name, combo$year, combo$outcome, combo$stratification, 
                 combo$coverage * 100, combo$counties_with_sufficient_data, combo$counties_total))
    }
  }
  
  cat("\nDETAILED RESULTS:\n")
  print(analysis_summary[, c("msa_name", "year", "outcome", "stratification", "coverage", "counties_with_sufficient_data", "counties_total", "needs_removal")])
}

# Save results for later use
cat(sprintf("\nSaving analysis results...\n"))
save(analysis_summary, problematic_combinations, 
     file = "cached/stratification_analysis_results.rdata")

cat(sprintf("\nNote: Analysis uses county-level data (%s) to assess coverage.\n", COUNTY_SOURCE_NAME))
cat(sprintf("Counties need ≥%.0f%% stratification coverage to count as 'having sufficient data'.\n", COUNTY_STRATIFICATION_THRESHOLD * 100))
cat(sprintf("MSAs need ≥%.0f%% population coverage to preserve stratifications.\n", POPULATION_COVERAGE_THRESHOLD * 100))
cat(sprintf("Removal will be applied to MSA-level data (%s).\n", MSA_SOURCE_NAME))

cat("\n=== ANALYSIS COMPLETE ===\n")
