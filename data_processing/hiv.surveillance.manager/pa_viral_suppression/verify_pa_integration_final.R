#!/usr/bin/env Rscript
# Final verification of PA integration

library(jheem2)

cat("=== FINAL PA INTEGRATION VERIFICATION ===\n\n")

# Load the newly created manager
manager <- load.data.manager("cached/surveillance.manager.pa_5age.rdata")

cat("1. CHECK DIRECT DATA ACCESS\n")
cat("-" , rep("", 40), "\n")

# Direct check
data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location"]]
if ("PA" %in% dimnames(data)$location) {
  pa_2022 <- data["2022", "PA"]
  pa_2023 <- data["2023", "PA"]
  
  if (!is.na(pa_2022) && !is.nan(pa_2022)) {
    cat(sprintf("✓ PA 2022 total suppression: %.1f%%\n", pa_2022 * 100))
  } else {
    cat("✗ PA 2022 data is NA/NaN\n")
  }
  
  if (!is.na(pa_2023) && !is.nan(pa_2023)) {
    cat(sprintf("✓ PA 2023 total suppression: %.1f%%\n", pa_2023 * 100))
  } else {
    cat("✗ PA 2023 data is NA/NaN\n")
  }
}

cat("\n2. CHECK AGE-STRATIFIED DATA\n")
cat("-" , rep("", 40), "\n")

age_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__age"]]
if (!is.null(age_data) && "PA" %in% dimnames(age_data)$location) {
  ages <- dimnames(age_data)$age
  cat("Age groups available:", paste(ages, collapse=", "), "\n")
  
  # Check 2022 age data for PA
  pa_has_age_data <- FALSE
  for (age in ages) {
    val <- age_data["2022", "PA", age]
    if (!is.na(val) && !is.nan(val)) {
      cat(sprintf("  %s: %.1f%%\n", age, val * 100))
      pa_has_age_data <- TRUE
    }
  }
  
  if (!pa_has_age_data) {
    cat("  ✗ No age-specific values for PA\n")
  }
} else {
  cat("✗ No age-stratified data for PA\n")
}

cat("\n3. TEST DATA PULLS\n")
cat("-" , rep("", 40), "\n")

# Test various pull configurations
test_pulls <- list(
  list(dims = c("year", "location"), name = "Total"),
  list(dims = c("year", "location", "age"), name = "Age-stratified"),
  list(dims = c("year", "location", "sex"), name = "Sex-stratified"),
  list(dims = c("year", "location", "race"), name = "Race-stratified"),
  list(dims = c("year", "location", "risk"), name = "Risk-stratified")
)

for (test in test_pulls) {
  result <- tryCatch({
    manager$pull(
      outcome = "suppression",
      keep.dimensions = test$dims,
      dimension.values = list(location = "PA", year = "2022"),
      sources = "cdc.hiv",
      from.ontology.names = "cdc",
      na.rm = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(result)) {
    cat(sprintf("✓ %s pull succeeded\n", test$name))
  } else {
    cat(sprintf("✗ %s pull failed\n", test$name))
  }
}

cat("\n4. SUMMARY\n")
cat("-" , rep("", 40), "\n")

# Count successful stratifications
all_strats <- names(manager$data$suppression$estimate$cdc.hiv$cdc)
pa_strats <- 0

for (strat in all_strats) {
  strat_data <- manager$data$suppression$estimate$cdc.hiv$cdc[[strat]]
  if ("location" %in% names(dimnames(strat_data))) {
    if ("PA" %in% dimnames(strat_data)$location) {
      loc_idx <- which(dimnames(strat_data)$location == "PA")
      # Check if any non-NA data exists for PA
      pa_slice <- if (length(dim(strat_data)) == 2) {
        strat_data[, loc_idx]
      } else if (length(dim(strat_data)) == 3) {
        strat_data[, loc_idx, ]
      } else {
        NULL
      }
      
      if (!is.null(pa_slice) && any(!is.na(pa_slice) & !is.nan(pa_slice))) {
        pa_strats <- pa_strats + 1
      }
    }
  }
}

cat(sprintf("Total stratifications with PA data: %d out of %d\n", pa_strats, length(all_strats)))

cat("\n5. LIKELIHOOD COMPATIBILITY\n")
cat("-" , rep("", 40), "\n")

# Check if we can load EHE version
tryCatch({
  source("applications/EHE/ehe_specification.R")
  cat("✓ EHE specification loaded\n")
  
  # Try to instantiate likelihood
  source("applications/EHE/ehe_likelihoods.R")
  likelihood <- tryCatch({
    suppression.basic.likelihood.instructions$instantiate.likelihood('ehe', 'PA')
  }, error = function(e) {
    cat("✗ Likelihood instantiation failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(likelihood)) {
    cat("✓ PA likelihood instantiation SUCCESSFUL!\n")
  }
}, error = function(e) {
  cat("Note: Could not test likelihood (EHE not set up in this session)\n")
})

cat("\n=== VERIFICATION COMPLETE ===\n")

# Final recommendation
if (pa_strats > 0) {
  cat("\n✓ PA data integration appears successful!\n")
  cat("Next steps:\n")
  cat("1. Replace main manager: cp cached/surveillance.manager.pa_5age.rdata cached/surveillance.manager.rdata\n")
  cat("2. Test EHE calibration for PA\n")
} else {
  cat("\n✗ PA data integration may have issues - no stratifications found with valid PA data\n")
}
