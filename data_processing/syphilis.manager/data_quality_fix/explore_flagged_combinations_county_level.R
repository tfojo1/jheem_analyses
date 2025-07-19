# Explore Flagged Combinations - County-Level Analysis with MSA Comparison
# Shows county-level data quality and MSA aggregation artifacts

library(jheem2)
library(locations)

# Load data
load("cached/syphilis.manager.rdata")
load("cached/stratification_analysis_results_county_based.rdata")

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
  cat(sprintf("County coverage: %.1f%% (< 90%% threshold) → FLAGGED\n", combo$county_coverage * 100))
  cat("================================================================================\n\n")
  
  # Show the dual-level analysis
  cat("DUAL-LEVEL ANALYSIS:\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("COUNTY LEVEL (actual data quality):\n"))
  cat(sprintf("  Total cases: %d\n", combo$county_total_cases))
  cat(sprintf("  Cases with race data: %d\n", combo$county_race_cases))
  cat(sprintf("  Coverage: %.1f%% (%d/%d)\n", 
              combo$county_coverage * 100, combo$county_race_cases, combo$county_total_cases))
  cat(sprintf("  Counties analyzed: %d\n", combo$counties_analyzed))
  
  cat(sprintf("\nMSA LEVEL (after aggregation):\n"))
  cat(sprintf("  Total cases: %d\n", combo$msa_total_cases))
  cat(sprintf("  Cases with race data: %d\n", combo$msa_race_cases))
  cat(sprintf("  Coverage: %.1f%%\n", combo$msa_coverage * 100))
  
  cat(sprintf("\nAGGREGATION IMPACT:\n"))
  if (combo$aggregation_loss > 0) {
    cat(sprintf("  Cases lost during aggregation: %d (%.1f%% of county race data)\n", 
                combo$aggregation_loss, combo$aggregation_loss_pct))
    cat(sprintf("  Reason: Privacy suppression of small race categories\n"))
  } else {
    cat(sprintf("  No cases lost during aggregation\n"))
  }
  
  cat(sprintf("\nFLAGGING DECISION:\n"))
  cat(sprintf("  Based on: County-level coverage (%.1f%% < 90%%)\n", combo$county_coverage * 100))
  cat(sprintf("  This indicates: Poor county-level race data collection\n"))
  
  # Show detailed county breakdown
  cat("\nCOUNTY-LEVEL BREAKDOWN:\n")
  cat("--------------------------------------------------\n")
  
  # Get all counties in this MSA
  all_counties <- locations::get.contained.locations(combo$msa, "COUNTY")
  cat(sprintf("MSA contains %d counties: %s\n\n", length(all_counties), paste(all_counties, collapse = ", ")))
  
  # Get county-level data
  county_source <- "cdc.sti"
  county_ontology <- combo$ontology
  
  # Get county-level stratified and total data
  county_strat_data <- syphilis.manager$data[[combo$outcome]]$estimate[[county_source]][[county_ontology]][[combo$stratification]]
  county_total_data <- syphilis.manager$data[[combo$outcome]]$estimate[[county_source]][[county_ontology]]$year__location
  
  if (!is.null(county_strat_data) && !is.null(county_total_data)) {
    # Check which counties have data for this year
    counties_with_data <- intersect(all_counties, dimnames(county_strat_data)$location)
    counties_with_data <- counties_with_data[counties_with_data %in% dimnames(county_total_data)$location]
    
    if (combo$year %in% dimnames(county_strat_data)$year && combo$year %in% dimnames(county_total_data)$year) {
      
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
          
          cat(sprintf("%s: %d total, %d race (%.1f%% coverage) %s\n", 
                      county, county_total, county_strat_sum, coverage * 100, quality_status))
        } else if (!is.na(county_total)) {
          cat(sprintf("%s: %s total cases\n", county, county_total))
        }
      }
      
      # Summary insight about problematic counties
      cat(sprintf("\nThis combination is flagged because overall county-level race\n"))
      cat(sprintf("data collection is insufficient (%.1f%% < 90%% threshold).\n", combo$county_coverage * 100))
      
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
  cat("ALL FLAGGED COMBINATIONS SUMMARY (County-Level Analysis):\n")
  cat("================================================================================\n")
  cat("Using county-level analysis: county_race_cases / county_total_cases < 90%\n")
  cat("This measures actual data collection quality, not aggregation artifacts\n\n")
  
  for (i in seq_along(problematic_combinations)) {
    combo <- problematic_combinations[[i]]
    cat(sprintf("%2d. %s %s %s [%.1f%% county coverage, MSA: %.1f%%]\n", 
                i, combo$msa_name, combo$year, combo$outcome, 
                combo$county_coverage * 100, combo$msa_coverage * 100))
  }
  
  cat("\nTo analyze a specific combination, run:\n")
  cat("analyze_flagged_combination(combo_index = X)  # where X is the number above\n")
  cat("OR\n")
  cat("analyze_flagged_combination(msa = 'C.12580', year = '2022', outcome = 'ps.syphilis.diagnoses')\n")
  cat("================================================================================\n\n")
}

# Show aggregation impact analysis
show_aggregation_impact <- function() {
  cat("AGGREGATION IMPACT ANALYSIS:\n")
  cat("================================================================================\n")
  cat("Shows how MSA aggregation affects race data through suppression\n\n")
  
  if (nrow(analysis_summary) == 0) {
    cat("No analysis results found\n")
    return(invisible())
  }
  
  # Overall statistics
  cat("OVERALL AGGREGATION STATISTICS:\n")
  cat(sprintf("Average aggregation loss: %.1f%% of county race cases\n", mean(analysis_summary$aggregation_loss_pct, na.rm = TRUE)))
  cat(sprintf("Median aggregation loss: %.1f%% of county race cases\n", median(analysis_summary$aggregation_loss_pct, na.rm = TRUE)))
  cat(sprintf("Maximum aggregation loss: %.1f%% of county race cases\n", max(analysis_summary$aggregation_loss_pct, na.rm = TRUE)))
  
  # High loss cases
  high_loss_cases <- analysis_summary[analysis_summary$aggregation_loss_pct > 10, ]
  if (nrow(high_loss_cases) > 0) {
    cat(sprintf("\nCASES WITH HIGH AGGREGATION LOSS (>10%%):\n"))
    for (i in 1:nrow(high_loss_cases)) {
      row <- high_loss_cases[i, ]
      cat(sprintf("  %s %s %s: %.1f%% loss (%d cases lost)\n",
                  row$msa_name, row$year, row$outcome, row$aggregation_loss_pct, row$aggregation_loss))
    }
  }
  
  # Cases where decisions differ
  discrepant_cases <- analysis_summary[
    (analysis_summary$county_coverage >= 0.9 & analysis_summary$msa_coverage < 0.9) |
    (analysis_summary$county_coverage < 0.9 & analysis_summary$msa_coverage >= 0.9),
  ]
  
  if (nrow(discrepant_cases) > 0) {
    cat(sprintf("\nCASES WHERE COUNTY vs MSA DECISIONS DIFFER:\n"))
    cat("(These show the impact of aggregation artifacts)\n")
    for (i in 1:nrow(discrepant_cases)) {
      row <- discrepant_cases[i, ]
      county_decision <- if (row$county_coverage >= 0.9) "PRESERVE" else "REMOVE"
      msa_decision <- if (row$msa_coverage >= 0.9) "PRESERVE" else "REMOVE"
      cat(sprintf("  %s %s %s: County=%.1f%% (%s), MSA=%.1f%% (%s)\n",
                  row$msa_name, row$year, row$outcome, 
                  row$county_coverage * 100, county_decision,
                  row$msa_coverage * 100, msa_decision))
    }
  }
  
  cat("================================================================================\n\n")
}

# Show all analysis results in table format
show_detailed_results <- function() {
  cat("DETAILED ANALYSIS RESULTS:\n")
  cat("================================================================================\n")
  
  if (nrow(analysis_summary) == 0) {
    cat("No analysis results found\n")
    return(invisible())
  }
  
  # Add formatted columns
  analysis_summary$county_coverage_formatted <- sprintf("%.1f%%", analysis_summary$county_coverage * 100)
  analysis_summary$msa_coverage_formatted <- sprintf("%.1f%%", analysis_summary$msa_coverage * 100)
  analysis_summary$aggregation_loss_formatted <- sprintf("%.1f%%", analysis_summary$aggregation_loss_pct)
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
  print(analysis_summary[, c("msa_name", "year", "outcome", "county_coverage_formatted", "msa_coverage_formatted", "aggregation_loss_formatted", "status")])
  
  cat("================================================================================\n\n")
}

# Compare with previous MSA-based approach
compare_with_msa_approach <- function() {
  cat("COMPARISON WITH PREVIOUS MSA-BASED APPROACH:\n")
  cat("================================================================================\n")
  
  if (nrow(analysis_summary) == 0) {
    cat("No analysis results found\n")
    return(invisible())
  }
  
  # Count decisions
  county_flagged <- sum(analysis_summary$county_coverage < 0.9)
  msa_flagged <- sum(analysis_summary$msa_coverage < 0.9)
  
  cat(sprintf("County-level approach: %d combinations flagged\n", county_flagged))
  cat(sprintf("MSA-level approach: %d combinations flagged\n", msa_flagged))
  cat(sprintf("Difference: %+d combinations\n", county_flagged - msa_flagged))
  
  # Show cases that changed
  changed_to_preserve <- analysis_summary[analysis_summary$county_coverage >= 0.9 & analysis_summary$msa_coverage < 0.9, ]
  changed_to_remove <- analysis_summary[analysis_summary$county_coverage < 0.9 & analysis_summary$msa_coverage >= 0.9, ]
  
  if (nrow(changed_to_preserve) > 0) {
    cat(sprintf("\nCASES NOW PRESERVED (good county data, MSA suppression artifacts):\n"))
    for (i in 1:nrow(changed_to_preserve)) {
      row <- changed_to_preserve[i, ]
      cat(sprintf("  %s %s %s: County %.1f%%, MSA %.1f%% (%.1f%% aggregation loss)\n",
                  row$msa_name, row$year, row$outcome, 
                  row$county_coverage * 100, row$msa_coverage * 100, row$aggregation_loss_pct))
    }
  }
  
  if (nrow(changed_to_remove) > 0) {
    cat(sprintf("\nCASES NOW REMOVED (poor county data, good MSA aggregation):\n"))
    for (i in 1:nrow(changed_to_remove)) {
      row <- changed_to_remove[i, ]
      cat(sprintf("  %s %s %s: County %.1f%%, MSA %.1f%%\n",
                  row$msa_name, row$year, row$outcome, 
                  row$county_coverage * 100, row$msa_coverage * 100))
    }
  }
  
  cat("================================================================================\n\n")
}

cat("COUNTY-LEVEL DATA QUALITY ANALYSIS EXPLORATION TOOL\n")
cat("================================================================================\n")
cat("This tool analyzes county-level data quality and MSA aggregation artifacts.\n")
cat("Flagging decisions based on county-level coverage, MSA data for comparison.\n\n")

cat("Available functions:\n")
cat("1. show_all_flagged_summary()           - Overview of all flagged combinations\n")
cat("2. analyze_flagged_combination(N)       - Detailed analysis with dual-level view\n") 
cat("3. show_aggregation_impact()            - Analysis of aggregation loss/artifacts\n")
cat("4. show_detailed_results()              - Full analysis table\n")
cat("5. compare_with_msa_approach()          - Compare with previous MSA-based approach\n\n")

cat("EXAMPLE USAGE:\n")
cat("show_all_flagged_summary()              # See all flagged combinations\n")
cat("analyze_flagged_combination(1)          # Deep dive with county breakdown\n")
cat("show_aggregation_impact()               # See aggregation artifacts\n")
cat("compare_with_msa_approach()             # Compare methodologies\n")
cat("show_detailed_results()                 # See full results table\n\n")

# Show summary by default
show_all_flagged_summary()
