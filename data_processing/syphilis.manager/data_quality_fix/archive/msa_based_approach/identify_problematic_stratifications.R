# Identify Problematic Stratifications - Simple Cases-Based Analysis
# Flags MSA/year/outcome/stratification combinations where race data coverage is insufficient
# Uses direct comparison: race_sum / total_cases < threshold

library(jheem2)

# Load data managers
cat("Loading data managers...\n")
load("cached/syphilis.manager.rdata")

# Configuration
CASE_COVERAGE_THRESHOLD <- 0.9  # Require 90% case coverage in race stratification
TARGET_MSAS <- c("C.12580", "C.35620", "C.33100", "C.12060")  # Baltimore, NYC, Miami, Atlanta
MSA_NAMES <- c("Baltimore", "NYC", "Miami", "Atlanta")
names(MSA_NAMES) <- TARGET_MSAS

TARGET_OUTCOMES <- c("ps.syphilis.diagnoses", "early.syphilis.diagnoses", "unknown.duration.or.late.syphilis.diagnoses")
MSA_SOURCE_NAME <- "cdc.aggregated.county"  # MSA-level data to analyze

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

cat("\n=== SIMPLE CASES-BASED ANALYSIS ===\n")
cat(sprintf("Threshold: %.0f%% case coverage required\n", CASE_COVERAGE_THRESHOLD * 100))
cat(sprintf("Logic: race_sum / total_cases >= %.0f%%\n", CASE_COVERAGE_THRESHOLD * 100))
cat(sprintf("Target MSAs: %s\n", paste(MSA_NAMES, collapse=", ")))
cat(sprintf("Target years: %s\n", paste(TARGET_YEARS, collapse=", ")))
cat(sprintf("Target stratifications: %s\n", paste(TARGET_STRATIFICATIONS, collapse=", ")))
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
  
  # Check if MSA-level data exists for this outcome/source/ontology
  if (is.null(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]])) {
    cat(sprintf("  No MSA-level data found for %s/%s/%s\n", outcome, MSA_SOURCE_NAME, ontology))
    next
  }
  
  for (stratification in TARGET_STRATIFICATIONS) {
    
    # Check if this stratification exists in MSA-level data
    if (!(stratification %in% names(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Stratification %s not found in MSA-level data for %s\n", stratification, outcome))
      next
    }
    
    # Check if total data exists
    if (!("year__location" %in% names(syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]))) {
      cat(sprintf("  Total data (year__location) not found for %s\n", outcome))
      next
    }
    
    for (msa in TARGET_MSAS) {
      for (year in TARGET_YEARS) {
        
        # Get MSA total data
        total_data <- syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]]$year__location
        if (!(year %in% dimnames(total_data)$year) || !(msa %in% dimnames(total_data)$location)) {
          next
        }
        
        msa_total <- total_data[year, msa]
        if (is.na(msa_total) || is.nan(msa_total) || msa_total <= 0) {
          next
        }
        
        # Get MSA stratified data
        strat_data <- syphilis.manager$data[[outcome]]$estimate[[MSA_SOURCE_NAME]][[ontology]][[stratification]]
        if (!(year %in% dimnames(strat_data)$year) || !(msa %in% dimnames(strat_data)$location)) {
          next
        }
        
        msa_race_data <- strat_data[year, msa, ]
        msa_race_sum <- sum(msa_race_data, na.rm = TRUE)
        
        # Calculate case coverage
        case_coverage <- msa_race_sum / msa_total
        
        # Determine if problematic
        needs_removal <- case_coverage < CASE_COVERAGE_THRESHOLD
        
        # Record results
        analysis_row <- data.frame(
          outcome = outcome,
          ontology = ontology,
          stratification = stratification,
          msa = msa,
          msa_name = MSA_NAMES[msa],
          year = year,
          total_cases = msa_total,
          race_cases = msa_race_sum,
          case_coverage = case_coverage,
          needs_removal = needs_removal,
          stringsAsFactors = FALSE
        )
        
        analysis_summary <- rbind(analysis_summary, analysis_row)
        
        # Track problematic combinations
        if (needs_removal) {
          key <- paste(outcome, ontology, stratification, msa, year, sep = "|")
          problematic_combinations[[key]] <- list(
            outcome = outcome,
            ontology = ontology,
            stratification = stratification,
            msa = msa,
            msa_name = MSA_NAMES[msa],
            year = year,
            total_cases = msa_total,
            race_cases = msa_race_sum,
            case_coverage = case_coverage
          )
          
          cat(sprintf("  ❌ %s %s %s [%s]: %.1f%% case coverage (%d/%d cases)\n",
                     MSA_NAMES[msa], year, outcome, stratification, 
                     case_coverage * 100, msa_race_sum, msa_total))
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
      cat(sprintf("  %s %s %s [%s]: %.1f%% coverage (%d/%d cases)\n",
                 combo$msa_name, combo$year, combo$outcome, combo$stratification, 
                 combo$case_coverage * 100, combo$race_cases, combo$total_cases))
    }
  }
  
  cat("\nDETAILED RESULTS:\n")
  print(analysis_summary[, c("msa_name", "year", "outcome", "stratification", "total_cases", "race_cases", "case_coverage", "needs_removal")])
}

# Save results for later use
cat(sprintf("\nSaving analysis results...\n"))
save(analysis_summary, problematic_combinations, 
     file = "cached/stratification_analysis_results_cases_based.rdata")

cat(sprintf("\nApproach: Simple cases-based analysis\n"))
cat(sprintf("Threshold: Cases with race data must be ≥%.0f%% of total cases\n", CASE_COVERAGE_THRESHOLD * 100))
cat(sprintf("Logic: Directly compare race_sum / total_cases at MSA level\n"))

cat("\n=== ANALYSIS COMPLETE ===\n")
