#!/usr/bin/env Rscript
# Test script to verify PA integration works with likelihood system

source("use_jheem2_package_setting.R")
library(jheem2)

cat("=== TESTING PA LIKELIHOOD COMPATIBILITY ===\n\n")

# Load the manager (use the newly saved one after running integration)
manager_file <- "cached/surveillance.manager.pa_5age.rdata"
if (!file.exists(manager_file)) {
  manager_file <- "cached/surveillance.manager.rdata"
  cat("Note: Using original manager file. Run integration script first for updated version.\n\n")
}

manager <- load.data.manager(manager_file)

# Test 1: Direct data pull (what we know works)
cat("TEST 1: Direct PA data pull\n")
cat("-" , rep("", 40), "\n")

pa_total <- tryCatch({
  manager$pull(
    outcome = "suppression",
    keep.dimensions = c("year", "location"),
    dimension.values = list(location = "PA"),
    sources = "cdc.hiv",
    from.ontology.names = "cdc",  # Specifically use cdc ontology
    na.rm = TRUE
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(pa_total)) {
  cat("✓ Total suppression pull successful\n")
  years_with_data <- dimnames(pa_total)$year[!is.na(pa_total[1,,1])]
  cat("  Years with data:", paste(years_with_data, collapse=", "), "\n")
} else {
  cat("✗ Total suppression pull failed\n")
}

# Test 2: Age-stratified pull (what was failing)
cat("\nTEST 2: Age-stratified data pull\n")
cat("-" , rep("", 40), "\n")

pa_age <- tryCatch({
  manager$pull(
    outcome = "suppression",
    keep.dimensions = c("year", "location", "age"),
    dimension.values = list(location = "PA", year = "2022"),
    sources = "cdc.hiv",
    from.ontology.names = "cdc",
    na.rm = TRUE
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(pa_age)) {
  cat("✓ Age-stratified pull successful\n")
  cat("  Age groups:", paste(dimnames(pa_age)$age, collapse=", "), "\n")
  
  # Show the values
  if (dim(pa_age)[2] > 0 && dim(pa_age)[3] > 0) {
    cat("\n  Suppression by age (2022):\n")
    for (age in dimnames(pa_age)$age) {
      val <- pa_age[1, "2022", "PA", age]
      if (!is.na(val)) {
        cat(sprintf("    %s: %.1f%%\n", age, val * 100))
      }
    }
  }
} else {
  cat("✗ Age-stratified pull failed\n")
}

# Test 3: Multi-dimensional pulls (what likelihood uses)
cat("\nTEST 3: Multi-dimensional pulls (likelihood parameters)\n")
cat("-" , rep("", 40), "\n")

test_dimensions <- list(
  c("age"),
  c("sex"),
  c("race"),
  c("risk"),
  c("age", "sex"),
  c("age", "race"),
  c("age", "risk")
)

for (dims in test_dimensions) {
  dims_with_base <- c("year", "location", dims)
  
  result <- tryCatch({
    manager$pull(
      outcome = "suppression",
      keep.dimensions = dims_with_base,
      dimension.values = list(location = "PA", year = "2022"),
      sources = "cdc.hiv",
      from.ontology.names = "cdc",
      na.rm = TRUE
    )
  }, error = function(e) {
    NULL
  })
  
  status <- if (!is.null(result)) "✓" else "✗"
  cat(sprintf("%s Pull with dimensions: %s\n", status, paste(dims, collapse="+")))
}

# Test 4: Simulate likelihood instantiation
cat("\nTEST 4: Simulating likelihood instantiation\n")
cat("-" , rep("", 40), "\n")

# Load the likelihood configuration
source("applications/EHE/ehe_likelihoods.R")

# Try to instantiate for PA
cat("Attempting to instantiate suppression likelihood for PA...\n")

likelihood <- tryCatch({
  suppression.basic.likelihood.instructions$instantiate.likelihood('ehe', 'PA')
}, error = function(e) {
  cat("ERROR during instantiation:", e$message, "\n")
  NULL
})

if (!is.null(likelihood)) {
  cat("✓ Likelihood instantiation SUCCESSFUL!\n")
  cat("  PA can now be used for EHE calibration\n")
} else {
  cat("✗ Likelihood instantiation failed\n")
  cat("  Check error message above for details\n")
}

cat("\n=== TEST COMPLETE ===\n")
