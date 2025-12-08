# Data Quality Fix: Remove Problematic Race Stratifications
# Complete implementation with audit trail and rollback capability

library(jheem2)
source("data_processing/syphilis.manager/data_quality_fix/operations_manager.R")

cat("=== SYPHILIS DATA QUALITY FIX ===\n")
cat(sprintf("Started: %s\n\n", Sys.time()))

# Load data and analysis results
cat("Loading data...\n")
#load("cached/syphilis.manager.rdata")
load("Q:/data_managers/stratification_analysis_results_county_based.rdata")

cat(sprintf("Found %d problematic combinations to remove\n\n", length(problematic_combinations)))

# === CREATE OPERATION BACKUP ===
cat("=== CREATING OPERATION BACKUP ===\n")
backup_file <- create_operation_backup(
  manager = syphilis.manager,
  operation_name = "county_race_quality_fix",
  combinations = problematic_combinations,
  script_name = "implement_removals.R",
  criteria_description = "County race coverage < 90% threshold"
)

# === PERFORM REMOVALS ===
cat("\n=== PERFORMING REMOVALS ===\n")

# Create clean copy
clean.syphilis.manager <- copy.data.manager(
  syphilis.manager,
  name = "clean.syphilis.manager",
  description = "Syphilis manager with problematic race stratifications removed"
)

# Get race values
race_values <- dimnames(syphilis.manager$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location__race)$race

# Process each problematic combination
removal_results <- list()

for (i in seq_along(problematic_combinations)) {
  combo <- problematic_combinations[[i]]

  cat(sprintf("Processing %d/%d: %s %s %s [%.1f%% coverage]\n",
    i, length(problematic_combinations),
    combo$msa_name, combo$year, combo$outcome, combo$county_coverage * 100
  ))

  # Record before state
  before_total <- tryCatch({
    clean.syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location[combo$year, combo$msa]
  }, error = function(e) NA)

  before_race_sum <- tryCatch({
    race_data <- clean.syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location__race[combo$year, combo$msa, ]
    sum(race_data, na.rm = TRUE)
  }, error = function(e) NA)

  # Apply removal
  removal_success <- tryCatch({
    remove.data(
      data.manager = clean.syphilis.manager,
      outcome = combo$outcome,
      source = "cdc.aggregated.county",
      ontology.name = combo$ontology,
      dimension.values = list(location = combo$msa, year = combo$year, race = race_values),
      details.for.removal = sprintf("County coverage %.1f%% < 90%% threshold", combo$county_coverage * 100),
      url.for.removal = "data_quality_fix_county_coverage_analysis"
    )
    TRUE
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    FALSE
  })

  # Record after state
  after_total <- tryCatch({
    clean.syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location[combo$year, combo$msa]
  }, error = function(e) NA)

  after_race_sum <- tryCatch({
    race_data <- clean.syphilis.manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location__race[combo$year, combo$msa, ]
    sum(race_data, na.rm = TRUE)
  }, error = function(e) NA)

  # Store results
  removal_results[[i]] <- list(
    combo = combo,
    success = removal_success,
    before_total = before_total,
    after_total = after_total,
    before_race_sum = before_race_sum,
    after_race_sum = after_race_sum,
    total_preserved = identical(before_total, after_total),
    race_removed = is.na(after_race_sum) || after_race_sum == 0
  )

  # Status
  if (removal_success) {
    if (identical(before_total, after_total) && (is.na(after_race_sum) || after_race_sum == 0)) {
      cat("  ✅ Success: Total preserved, race data removed\n")
    } else {
      cat(sprintf("  ⚠️  Warning: Total %s→%s, Race %s→%s\n",
        before_total, after_total, before_race_sum, after_race_sum))
    }
  }
}

# === SUMMARY ===
cat("\n=== REMOVAL SUMMARY ===\n")

successful_removals <- sum(sapply(removal_results, function(x) x$success))
total_preserved_count <- sum(sapply(removal_results, function(x) x$total_preserved))
race_removed_count <- sum(sapply(removal_results, function(x) x$race_removed))

cat(sprintf("Successful removals: %d/%d\n", successful_removals, length(problematic_combinations)))
cat(sprintf("Totals preserved: %d/%d\n", total_preserved_count, length(problematic_combinations)))
cat(sprintf("Race data removed: %d/%d\n", race_removed_count, length(problematic_combinations)))

if (successful_removals == length(problematic_combinations) &&
  total_preserved_count == length(problematic_combinations) &&
  race_removed_count == length(problematic_combinations)) {
  cat("✅ ALL REMOVALS SUCCESSFUL\n")
} else {
  cat("❌ Some removals had issues - check details above\n")
}

# === SAVE RESULTS ===
# cat("\n=== SAVING RESULTS ===\n")
# 
# # Save clean manager
# cat("Saving clean manager...\n")
# syphilis.manager <- clean.syphilis.manager
# save(syphilis.manager, file = "cached/syphilis.manager.rdata")
# 
# cat(sprintf("\n=== OPERATION COMPLETE ===\n"))
# cat(sprintf("Completed: %s\n", Sys.time()))
# cat("\nFiles created:\n")
# cat("  - cached/syphilis.manager.rdata (main deliverable)\n")
# cat(sprintf("  - %s (operation backup)\n", backup_file))
# 
# cat("\nTo view operations: list_operations()\n")
# cat("To rollback: restore_operation(syphilis.manager, 'path/to/backup.rdata')\n")
# 
# cat("\nNext steps:\n")
# cat("1. Add total syphilis diagnosis outcome\n")
# cat("2. Validate results\n")
# cat("3. Package for team\n")
