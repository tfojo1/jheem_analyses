# Explore Flagged Combinations - Simple Cases-Based Analysis
# Shows why each combination was flagged using direct case coverage comparison

library(jheem2)
library(locations)

# Load data
load("cached/syphilis.manager.rdata")
load("cached/stratification_analysis_results_cases_based.rdata")

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
  cat(sprintf("Case coverage: %.1f%% (< 90%% threshold)\n", combo$case_coverage * 100))
  cat("================================================================================\n\n")
  
  # Show the core calculation
  cat("CORE ANALYSIS:\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("MSA total cases: %d\n", combo$total_cases))
  cat(sprintf("Cases with race data: %d\n", combo$race_cases))
  cat(sprintf("Case coverage: %.1f%% (%d/%d)\n", 
              combo$case_coverage * 100, combo$race_cases, combo$total_cases))
  cat(sprintf("Threshold: 90%%\n"))
  cat(sprintf("Result: %.1f%% < 90%% → FLAGGED FOR REMOVAL\n", combo$case_coverage * 100))
  
  # Show the detailed race breakdown
  cat("\nRACE STRATIFICATION BREAKDOWN:\n")
  cat("--------------------------------------------------\n")
  
  msa_strat_data <- syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]][[combo$stratification]][combo$year, combo$msa, ]
  
  missing_cases <- combo$total_cases - combo$race_cases
  
  cat("Race category breakdown:\n")
  for (i in seq_along(msa_strat_data)) {
    race_name <- names(msa_strat_data)[i]
    value <- msa_strat_data[i]
    if (is.nan(value)) {
      cat(sprintf("  %s: NaN ⚠️ (missing data)\n", race_name))
    } else {
      cat(sprintf("  %s: %d\n", race_name, value))
    }
  }
  
  cat(sprintf("\nSummary:\n"))
  cat(sprintf("  Cases with race data: %d\n", combo$race_cases))
  cat(sprintf("  Cases missing race data: %d\n", missing_cases))
  cat(sprintf("  Total cases: %d\n", combo$total_cases))
  
  if (missing_cases > 0) {
    cat(sprintf("\nImpact: %d cases (%.1f%%) are missing race information, making\n", 
                missing_cases, missing_cases/combo$total_cases*100))
    cat("race-stratified modeling targets unreliable for this MSA/year/outcome.\n")
  }
  
  # Add county-level breakdown to understand WHY data is missing
  cat("\nCOUNTY-LEVEL BREAKDOWN (underlying cause):\n")
  cat("--------------------------------------------------\n")
  
  # Get all counties in this MSA
  all_counties <- locations::get.contained.locations(combo$msa, "COUNTY")
  cat(sprintf("MSA contains %d counties: %s\n\n", length(all_counties), paste(all_counties, collapse = ", ")))
  
  # Get county-level data
  county_source <- "cdc.sti"
  
  # Determine which ontology to use for county data
  county_ontology <- if (combo$outcome == "ps.syphilis.diagnoses") "cdc.sti" else "cdc.sti.two"
  
  # Get county-level stratified and total data
  county_strat_data <- syphilis.manager$data[[combo$outcome]]$estimate[[county_source]][[county_ontology]][[combo$stratification]]
  county_total_data <- syphilis.manager$data[[combo$outcome]]$estimate[[county_source]][[county_ontology]]$year__location
  
  if (!is.null(county_strat_data) && !is.null(county_total_data)) {
    # Check which counties have data for this year
    counties_with_data <- intersect(all_counties, dimnames(county_strat_data)$location)
    counties_with_data <- counties_with_data[counties_with_data %in% dimnames(county_total_data)$location]
    
    if (combo$year %in% dimnames(county_strat_data)$year && combo$year %in% dimnames(county_total_data)$year) {
      
      county_details <- list()
      
      for (county in counties_with_data) {
        # Get total cases for this county/year
        county_total <- county_total_data[combo$year, county]
        
        if (!is.na(county_total) && !is.nan(county_total) && county_total > 0) {
          # Get stratified cases
          county_strat_values <- county_strat_data[combo$year, county, ]
          county_strat_sum <- sum(county_strat_values, na.rm = TRUE)
          
          # Calculate coverage
          coverage <- county_strat_sum / county_total
          
          # Assess data quality
          quality_status <- if (coverage >= 0.9) "✅ EXCELLENT" else if (coverage >= 0.8) "✅ GOOD" else if (coverage >= 0.5) "⚠️ POOR" else "❌ VERY POOR"
          
          county_details[[county]] <- list(
            total = county_total,
            strat_sum = county_strat_sum,
            coverage = coverage,
            status = quality_status
          )
          
          cat(sprintf("%s: %d total, %d race (%.1f%% coverage) %s\n", 
                      county, county_total, county_strat_sum, coverage * 100, quality_status))
        } else if (!is.na(county_total)) {
          cat(sprintf("%s: %s total cases\n", county, county_total))
        }
      }
      
      # Show which counties are driving the problem
      cat("\nPROBLEM ANALYSIS:\n")
      poor_counties <- names(county_details)[sapply(county_details, function(x) x$coverage < 0.8)]
      good_counties <- names(county_details)[sapply(county_details, function(x) x$coverage >= 0.8)]
      
      if (length(poor_counties) > 0) {
        poor_cases <- sum(sapply(county_details[poor_counties], function(x) x$total))
        poor_missing <- sum(sapply(county_details[poor_counties], function(x) x$total - x$strat_sum))
        cat(sprintf("Counties with poor race reporting (%d): %s\n", length(poor_counties), paste(poor_counties, collapse = ", ")))
        cat(sprintf("  Cases from poor counties: %d\n", poor_cases))
        cat(sprintf("  Missing race data from these counties: %d\n", poor_missing))
      }
      
      if (length(good_counties) > 0) {
        good_cases <- sum(sapply(county_details[good_counties], function(x) x$total))
        cat(sprintf("Counties with good race reporting (%d): %s\n", length(good_counties), paste(good_counties, collapse = ", ")))
        cat(sprintf("  Cases from good counties: %d\n", good_cases))
      }
      
      # Summary insight
      total_cases_analyzed <- sum(sapply(county_details, function(x) x$total))
      if (length(poor_counties) > 0 && total_cases_analyzed > 0) {
        poor_contribution <- poor_cases / total_cases_analyzed
        cat(sprintf("\nKey insight: %.1f%% of cases come from counties with poor race reporting.\n", poor_contribution * 100))
      }
      
    } else {
      cat("County-level data not available for this year.\n")
    }
  } else {
    cat("County-level data not available for this outcome/ontology combination.\n")
  }
  
  cat("\n================================================================================\n\n")
  
  return(invisible())
}

# Quick overview of all flagged combinations
show_all_flagged_summary <- function() {
  cat("ALL FLAGGED COMBINATIONS SUMMARY:\n")
  cat("================================================================================\n")
  cat("Using simple cases-based analysis: race_cases / total_cases < 90%\n\n")
  
  for (i in seq_along(problematic_combinations)) {
    combo <- problematic_combinations[[i]]
    cat(sprintf("%2d. %s %s %s [%.1f%% coverage, %d/%d cases]\n", 
                i, combo$msa_name, combo$year, combo$outcome, 
                combo$case_coverage * 100, combo$race_cases, combo$total_cases))
  }
  
  cat("\nTo analyze a specific combination, run:\n")
  cat("analyze_flagged_combination(combo_index = X)  # where X is the number above\n")
  cat("OR\n")
  cat("analyze_flagged_combination(msa = 'C.12580', year = '2022', outcome = 'ps.syphilis.diagnoses')\n")
  cat("================================================================================\n\n")
}

# Show borderline cases (close to threshold)
show_borderline_cases <- function(threshold_buffer = 0.05) {
  borderline_threshold <- 0.9 + threshold_buffer  # e.g., 0.95 for 5% buffer above 90%
  
  cat("BORDERLINE CASES (close to 90% threshold):\n")
  cat("================================================================================\n")
  
  # Find cases that are flagged but close to the threshold
  borderline_flagged <- problematic_combinations[sapply(problematic_combinations, function(x) x$case_coverage >= 0.9 - threshold_buffer && x$case_coverage < 0.9)]
  
  # Find cases that are preserved but close to the threshold  
  borderline_preserved <- analysis_summary[analysis_summary$case_coverage >= 0.9 & analysis_summary$case_coverage <= borderline_threshold & !analysis_summary$needs_removal, ]
  
  if (length(borderline_flagged) > 0) {
    cat("FLAGGED cases close to threshold:\n")
    for (i in seq_along(borderline_flagged)) {
      combo <- borderline_flagged[[i]]
      cat(sprintf("  %s %s %s: %.1f%% coverage (%d/%d cases) - FLAGGED\n",
                  combo$msa_name, combo$year, combo$outcome, 
                  combo$case_coverage * 100, combo$race_cases, combo$total_cases))
    }
  }
  
  if (nrow(borderline_preserved) > 0) {
    cat("\nPRESERVED cases close to threshold:\n")
    for (i in 1:nrow(borderline_preserved)) {
      row <- borderline_preserved[i, ]
      cat(sprintf("  %s %s %s: %.1f%% coverage (%d/%d cases) - PRESERVED\n",
                  row$msa_name, row$year, row$outcome,
                  row$case_coverage * 100, row$race_cases, row$total_cases))
    }
  }
  
  cat("================================================================================\n\n")
}

# Compare with a different threshold
test_different_threshold <- function(test_threshold = 0.85) {
  cat(sprintf("COMPARISON: What if threshold was %.0f%% instead of 90%%?\n", test_threshold * 100))
  cat("================================================================================\n")
  
  current_flagged <- sum(analysis_summary$needs_removal)
  test_flagged <- sum(analysis_summary$case_coverage < test_threshold)
  
  cat(sprintf("Current (90%% threshold): %d combinations flagged\n", current_flagged))
  cat(sprintf("Test (%.0f%% threshold): %d combinations would be flagged\n", test_threshold * 100, test_flagged))
  cat(sprintf("Difference: %+d combinations\n", test_flagged - current_flagged))
  
  if (test_threshold < 0.9) {
    # Show what would be newly preserved
    newly_preserved <- analysis_summary[analysis_summary$case_coverage >= test_threshold & analysis_summary$case_coverage < 0.9, ]
    if (nrow(newly_preserved) > 0) {
      cat(sprintf("\nCombinations that would be NEWLY PRESERVED with %.0f%% threshold:\n", test_threshold * 100))
      for (i in 1:nrow(newly_preserved)) {
        row <- newly_preserved[i, ]
        cat(sprintf("  %s %s %s: %.1f%% coverage\n", row$msa_name, row$year, row$outcome, row$case_coverage * 100))
      }
    }
  } else {
    # Show what would be newly flagged
    newly_flagged <- analysis_summary[analysis_summary$case_coverage < test_threshold & analysis_summary$case_coverage >= 0.9, ]
    if (nrow(newly_flagged) > 0) {
      cat(sprintf("\nCombinations that would be NEWLY FLAGGED with %.0f%% threshold:\n", test_threshold * 100))
      for (i in 1:nrow(newly_flagged)) {
        row <- newly_flagged[i, ]
        cat(sprintf("  %s %s %s: %.1f%% coverage\n", row$msa_name, row$year, row$outcome, row$case_coverage * 100))
      }
    }
  }
  
  cat("================================================================================\n\n")
}
show_detailed_results <- function() {
  cat("DETAILED ANALYSIS RESULTS:\n")
  cat("================================================================================\n")
  
  if (nrow(analysis_summary) == 0) {
    cat("No analysis results found\n")
    return(invisible())
  }
  
  # Add a formatted coverage column
  analysis_summary$coverage_formatted <- sprintf("%.1f%%", analysis_summary$case_coverage * 100)
  analysis_summary$status <- ifelse(analysis_summary$needs_removal, "REMOVE", "PRESERVE")
  
  # Show summary by MSA
  cat("BY MSA:\n")
  for (msa in unique(analysis_summary$msa)) {
    msa_data <- analysis_summary[analysis_summary$msa == msa, ]
    msa_name <- msa_data$msa_name[1]
    removals <- sum(msa_data$needs_removal)
    total <- nrow(msa_data)
    cat(sprintf("  %s: %d/%d flagged (%.1f%%)\n", 
                msa_name, removals, total, removals/total*100))
  }
  
  cat("\nFULL RESULTS TABLE:\n")
  print(analysis_summary[, c("msa_name", "year", "outcome", "total_cases", "race_cases", "coverage_formatted", "status")])
  
  cat("================================================================================\n\n")
}

cat("SIMPLE CASES-BASED ANALYSIS EXPLORATION TOOL\n")
cat("================================================================================\n")
cat("This tool shows why combinations were flagged using direct case coverage comparison.\n")
cat("Logic: race_cases / total_cases < 90% → flag for removal\n\n")

cat("Available functions:\n")
cat("1. show_all_flagged_summary()           - Overview of all flagged combinations\n")
cat("2. analyze_flagged_combination(N)       - Detailed analysis with county breakdown\n") 
cat("3. show_detailed_results()              - Full analysis table\n")
cat("4. show_borderline_cases()              - Cases close to 90% threshold\n")
cat("5. test_different_threshold(0.85)       - Compare with different threshold\n\n")

cat("EXAMPLE USAGE:\n")
cat("show_all_flagged_summary()              # See all flagged combinations\n")
cat("analyze_flagged_combination(1)          # Deep dive with county breakdown\n")
cat("show_borderline_cases()                 # Cases close to threshold\n")
cat("test_different_threshold(0.85)          # Test 85% threshold\n")
cat("show_detailed_results()                 # See full results table\n\n")

# Show summary by default
show_all_flagged_summary()
