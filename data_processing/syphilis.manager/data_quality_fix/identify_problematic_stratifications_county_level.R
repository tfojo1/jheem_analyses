# Dual-Level Analysis: County-Level Data Quality vs MSA-Level Aggregation Artifacts
# Flags based on county-level coverage but tracks MSA discrepancies

library(jheem2)
library(locations)

# Load data managers
cat("Loading data managers...\n")
load("cached/syphilis.manager.rdata")

# Configuration
COUNTY_COVERAGE_THRESHOLD <- 0.9  # Base flagging decision on county-level coverage
TARGET_MSAS <- c("C.12580", "C.35620", "C.33100", "C.12060")  # Baltimore, NYC, Miami, Atlanta
MSA_NAMES <- c("Baltimore", "NYC", "Miami", "Atlanta")
names(MSA_NAMES) <- TARGET_MSAS

TARGET_OUTCOMES <- c("ps.syphilis.diagnoses", "early.syphilis.diagnoses", "unknown.duration.or.late.syphilis.diagnoses", "total.syphilis.diagnoses")
COUNTY_SOURCE_NAME <- "cdc.sti"  # County-level data for coverage analysis
MSA_SOURCE_NAME <- "cdc.aggregated.county"  # MSA-level data for comparison

# Target stratifications
TARGET_STRATIFICATIONS <- c("year__location__race")

# Check what years are available
sample_data <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[MSA_SOURCE_NAME]]$cdc.sti$year__location__race
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

cat("\n=== DUAL-LEVEL ANALYSIS ===\n")
cat(sprintf("Flagging threshold: %.0f%% county-level coverage required\n", COUNTY_COVERAGE_THRESHOLD * 100))
cat(sprintf("Logic: County race_sum / county_total >= %.0f%%\n", COUNTY_COVERAGE_THRESHOLD * 100))
cat(sprintf("Also tracking: MSA aggregation artifacts and discrepancies\n"))
cat(sprintf("Target MSAs: %s\n", paste(MSA_NAMES, collapse=", ")))
cat(sprintf("Target years: %s\n", paste(TARGET_YEARS, collapse=", ")))
cat("\n")

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
  
  # Check if both county and MSA data exist
  if (is.null(syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]])) {
    cat(sprintf("  No county-level data found for %s/%s/%s\n", outcome, COUNTY_SOURCE_NAME, ontology))
    next
  }
  
  if (is.null(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]])) {
    cat(sprintf("  No MSA-level data found for %s/%s/%s\n", outcome, MSA_SOURCE_NAME, ontology))
    next
  }
  
  for (stratification in TARGET_STRATIFICATIONS) {
    
    # Check if stratification exists at both levels
    if (!(stratification %in% names(syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Stratification %s not found in county-level data for %s\n", stratification, outcome))
      next
    }
    
    if (!(stratification %in% names(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Stratification %s not found in MSA-level data for %s\n", stratification, outcome))
      next
    }
    
    if (!("year__location" %in% names(syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Total data not found in county-level data for %s\n", outcome))
      next
    }
    
    if (!("year__location" %in% names(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Total data not found in MSA-level data for %s\n", outcome))
      next
    }
    
    for (msa in TARGET_MSAS) {
      for (year in TARGET_YEARS) {
        
        # Get MSA-level data (for comparison)
        msa_total_data <- syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]$year__location
        msa_strat_data <- syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]][[stratification]]
        
        if (!(year %in% dimnames(msa_total_data)$year) || !(msa %in% dimnames(msa_total_data)$location)) {
          next
        }
        
        msa_total <- msa_total_data[year, msa]
        if (is.na(msa_total) || is.nan(msa_total) || msa_total <= 0) {
          next
        }
        
        msa_race_data <- msa_strat_data[year, msa, ]
        msa_race_sum <- sum(msa_race_data, na.rm = TRUE)
        msa_coverage <- msa_race_sum / msa_total
        
        # Get county-level data (for primary flagging decision)
        county_total_data <- syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]]$year__location
        county_strat_data <- syphilis.manager$data[[outcome]]$estimate[[COUNTY_SOURCE_NAME]][[ontology]][[stratification]]
        
        # Get all counties in MSA
        all_counties <- locations::get.contained.locations(msa, "COUNTY")
        counties_with_data <- intersect(all_counties, dimnames(county_total_data)$location)
        counties_with_data <- counties_with_data[counties_with_data %in% dimnames(county_strat_data)$location]
        
        if (!(year %in% dimnames(county_total_data)$year) || !(year %in% dimnames(county_strat_data)$year)) {
          next
        }
        
        # Calculate county-level totals
        county_total_sum <- 0
        county_race_sum <- 0
        counties_analyzed <- 0
        
        for (county in counties_with_data) {
          county_total <- county_total_data[year, county]
          if (!is.na(county_total) && !is.nan(county_total) && county_total > 0) {
            county_race_values <- county_strat_data[year, county, ]
            county_race_total <- sum(county_race_values, na.rm = TRUE)
            
            county_total_sum <- county_total_sum + county_total
            county_race_sum <- county_race_sum + county_race_total
            counties_analyzed <- counties_analyzed + 1
          }
        }
        
        if (county_total_sum <= 0) {
          next
        }
        
        # Calculate county-level coverage
        county_coverage <- county_race_sum / county_total_sum
        
        # Determine if problematic based on county-level coverage
        needs_removal <- county_coverage < COUNTY_COVERAGE_THRESHOLD
        
        # Calculate discrepancy between county and MSA
        aggregation_loss <- county_race_sum - msa_race_sum
        aggregation_loss_pct <- aggregation_loss / county_race_sum * 100
        
        # Record results
        analysis_row <- data.frame(
          outcome = outcome,
          ontology = ontology,
          stratification = stratification,
          msa = msa,
          msa_name = MSA_NAMES[msa],
          year = year,
          counties_analyzed = counties_analyzed,
          county_total_cases = county_total_sum,
          county_race_cases = county_race_sum,
          county_coverage = county_coverage,
          msa_total_cases = msa_total,
          msa_race_cases = msa_race_sum,
          msa_coverage = msa_coverage,
          aggregation_loss = aggregation_loss,
          aggregation_loss_pct = aggregation_loss_pct,
          needs_removal = needs_removal,
          stringsAsFactors = FALSE
        )
        
        analysis_summary <- rbind(analysis_summary, analysis_row)
        
        # Track problematic combinations (based on county-level coverage)
        if (needs_removal) {
          key <- paste(outcome, ontology, stratification, msa, year, sep = "|")
          problematic_combinations[[key]] <- list(
            outcome = outcome,
            ontology = ontology,
            stratification = stratification,
            msa = msa,
            msa_name = MSA_NAMES[msa],
            year = year,
            counties_analyzed = counties_analyzed,
            county_total_cases = county_total_sum,
            county_race_cases = county_race_sum,
            county_coverage = county_coverage,
            msa_coverage = msa_coverage,
            aggregation_loss = aggregation_loss,
            aggregation_loss_pct = aggregation_loss_pct
          )
          
          cat(sprintf("  ❌ %s %s %s [%s]: %.1f%% county coverage (MSA: %.1f%%, loss: %.1f%%)\n",
                     MSA_NAMES[msa], year, outcome, stratification, 
                     county_coverage * 100, msa_coverage * 100, aggregation_loss_pct))
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
  cat(sprintf("Combinations needing removal (county coverage < %.0f%%): %d\n", COUNTY_COVERAGE_THRESHOLD * 100, sum(analysis_summary$needs_removal)))
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
  
  # Aggregation loss analysis
  cat("\nAGGREGATION ANALYSIS:\n")
  cat(sprintf("Average aggregation loss: %.1f%% of county race cases\n", mean(analysis_summary$aggregation_loss_pct, na.rm = TRUE)))
  cat(sprintf("Median aggregation loss: %.1f%% of county race cases\n", median(analysis_summary$aggregation_loss_pct, na.rm = TRUE)))
  
  high_loss_cases <- analysis_summary[analysis_summary$aggregation_loss_pct > 10, ]
  if (nrow(high_loss_cases) > 0) {
    cat(sprintf("Cases with >10%% aggregation loss: %d\n", nrow(high_loss_cases)))
  }
  
  # Show cases where MSA and county decisions would differ
  discrepant_cases <- analysis_summary[
    (analysis_summary$county_coverage >= COUNTY_COVERAGE_THRESHOLD & analysis_summary$msa_coverage < 0.9) |
    (analysis_summary$county_coverage < COUNTY_COVERAGE_THRESHOLD & analysis_summary$msa_coverage >= 0.9),
  ]
  
  if (nrow(discrepant_cases) > 0) {
    cat(sprintf("\nCases where county vs MSA decisions differ: %d\n", nrow(discrepant_cases)))
    cat("(These show the impact of aggregation artifacts)\n")
  }
  
  if (length(problematic_combinations) > 0) {
    cat("\nPROBLEMATIC COMBINATIONS (county coverage < 90%):\n")
    for (i in seq_along(problematic_combinations)) {
      combo <- problematic_combinations[[i]]
      cat(sprintf("  %s %s %s [%s]: %.1f%% county coverage (vs %.1f%% MSA)\n",
                 combo$msa_name, combo$year, combo$outcome, combo$stratification, 
                 combo$county_coverage * 100, combo$msa_coverage * 100))
    }
  }
}

# Save results for later use
cat(sprintf("\nSaving analysis results...\n"))
save(analysis_summary, problematic_combinations, 
     file = "cached/stratification_analysis_results_county_based.rdata")

cat(sprintf("\nApproach: County-level data quality assessment with MSA comparison\n"))
cat(sprintf("Flagging threshold: County race coverage must be ≥%.0f%%\n", COUNTY_COVERAGE_THRESHOLD * 100))
cat(sprintf("Comparison: Also tracks MSA aggregation artifacts and discrepancies\n"))

cat("\n=== ANALYSIS COMPLETE ===\n")
