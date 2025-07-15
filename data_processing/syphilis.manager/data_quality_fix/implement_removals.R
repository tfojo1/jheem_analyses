# Implement Race Stratification Removals
# Remove problematic race stratifications while preserving totals

library(jheem2)

cat("=== IMPLEMENTING SYPHILIS DATA QUALITY FIX ===\n")
cat(sprintf("Started: %s\n\n", Sys.time()))

# Load data and analysis results
cat("Loading data...\n")
load("cached/syphilis.manager.rdata")
load("cached/stratification_analysis_results.rdata")

# Create clean copy
cat("Creating clean manager copy...\n")
clean.syphilis.manager <- copy.data.manager(
  syphilis.manager,
  name = "clean.syphilis.manager",
  description = "Syphilis manager with problematic race stratifications removed"
)

cat(sprintf("Found %d problematic combinations to remove\n\n", length(problematic_combinations)))

# Get race values for explicit specification
race_values <- dimnames(syphilis.manager$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location__race)$race
cat(sprintf("Race values: %s\n\n", paste(race_values, collapse = ", ")))

# Process each problematic combination
removal_results <- list()

for (i in seq_along(problematic_combinations)) {
  combo <- problematic_combinations[[i]]

  cat(sprintf(
    "Processing %d/%d: %s %s %s [%.1f%% coverage]\n",
    i, length(problematic_combinations),
    combo$msa_name, combo$year, combo$outcome, combo$coverage * 100
  ))

  # Determine source and ontology
  msa_source <- "cdc.aggregated.county"

  # Record before state for verification
  before_total <- tryCatch(
    {
      clean.syphilis.manager$data[[combo$outcome]]$estimate[[msa_source]][[combo$ontology]]$year__location[combo$year, combo$msa]
    },
    error = function(e) NA
  )

  before_race_sum <- tryCatch(
    {
      race_data <- clean.syphilis.manager$data[[combo$outcome]]$estimate[[msa_source]][[combo$ontology]]$year__location__race[combo$year, combo$msa, ]
      sum(race_data, na.rm = TRUE)
    },
    error = function(e) NA
  )

  # Apply removal using our validated approach
  removal_success <- tryCatch(
    {
      remove.data(
        data.manager = clean.syphilis.manager,
        outcome = combo$outcome,
        source = msa_source,
        ontology.name = combo$ontology,
        dimension.values = list(location = combo$msa, year = combo$year, race = race_values),
        details.for.removal = sprintf(
          "Removed due to insufficient population coverage (%.1f%% < 90%% threshold)",
          combo$coverage * 100
        ),
        url.for.removal = "data_quality_fix_population_coverage_analysis"
      )
      TRUE
    },
    error = function(e) {
      cat(sprintf("  ERROR: %s\n", e$message))
      FALSE
    }
  )

  # Record after state
  after_total <- tryCatch(
    {
      clean.syphilis.manager$data[[combo$outcome]]$estimate[[msa_source]][[combo$ontology]]$year__location[combo$year, combo$msa]
    },
    error = function(e) NA
  )

  after_race_sum <- tryCatch(
    {
      race_data <- clean.syphilis.manager$data[[combo$outcome]]$estimate[[msa_source]][[combo$ontology]]$year__location__race[combo$year, combo$msa, ]
      sum(race_data, na.rm = TRUE)
    },
    error = function(e) NA
  )

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

  # Quick status
  if (removal_success) {
    if (identical(before_total, after_total) && (is.na(after_race_sum) || after_race_sum == 0)) {
      cat("  ✅ Success: Total preserved, race data removed\n")
    } else {
      cat(sprintf(
        "  ⚠️  Warning: Total %s→%s, Race %s→%s\n",
        before_total, after_total, before_race_sum, after_race_sum
      ))
    }
  }
}

cat("\n=== REMOVAL SUMMARY ===\n")

# Count results
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

# Save clean manager
cat("\nSaving clean manager...\n")
save(clean.syphilis.manager, file = "cached/clean.syphilis.manager.rdata")

# Save removal results for validation
save(removal_results, file = "cached/removal_results.rdata")

cat(sprintf("\n=== REMOVAL IMPLEMENTATION COMPLETE ===\n"))
cat(sprintf("Completed: %s\n", Sys.time()))
cat("Files created:\n")
cat("  - cached/clean.syphilis.manager.rdata (main deliverable)\n")
cat("  - cached/removal_results.rdata (verification data)\n")

cat("\nNext steps:\n")
cat("1. Run validation script to verify results\n")
cat("2. Add total syphilis diagnosis outcome\n")
cat("3. Package for colleague\n")
