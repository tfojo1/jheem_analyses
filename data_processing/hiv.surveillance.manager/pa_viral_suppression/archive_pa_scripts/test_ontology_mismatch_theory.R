# Test the detailed theory about ontology mismatch causing likelihood failure

library(jheem2)

manager <- load.data.manager("cached/surveillance.manager.rdata")

cat("=== DETAILED ONTOLOGY MISMATCH THEORY TEST ===\n")

# Theory: The pull method fails for age but not other dimensions because
# age values differ between cdc and cdc.new ontologies, while other dimensions are the same

cat("1. CHECKING DIMENSION VALUES ACROSS ONTOLOGIES\n")
cat(paste(rep("=", 50), collapse=""), "\n")

suppression_data <- manager$data$suppression$estimate$cdc.hiv
ontologies <- names(suppression_data)

# Check all dimension values across ontologies
dimensions_to_check <- c("age", "race", "risk", "sex")

for (dim in dimensions_to_check) {
  cat(sprintf("\n--- %s dimension ---\n", toupper(dim)))
  
  for (ont in ontologies) {
    # Find a stratification that includes this dimension
    strat_names <- names(suppression_data[[ont]])
    strat_with_dim <- strat_names[grepl(paste0("__", dim), strat_names)][1]
    
    if (!is.na(strat_with_dim)) {
      strat_data <- suppression_data[[ont]][[strat_with_dim]]
      if (!is.null(strat_data)) {
        dim_values <- dimnames(strat_data)[[dim]]
        cat(sprintf("%s: %s\n", ont, paste(dim_values, collapse=", ")))
      }
    }
  }
  
  # Test if this dimension causes pull failure
  cat(sprintf("Testing pull with %s dimension:\n", dim))
  tryCatch({
    result <- manager$pull(
      outcome = "suppression",
      keep.dimensions = c("year", "location", dim),
      sources = "cdc.hiv",
      year = as.character(2008:2030),
      location = "PA",
      na.rm = TRUE
    )
    
    if (is.null(result)) {
      cat(sprintf("❌ %s pull returns NULL\n", dim))
    } else {
      cat(sprintf("✅ %s pull succeeds\n", dim))
    }
  }, error = function(e) {
    cat(sprintf("❌ %s pull fails: %s\n", dim, e$message))
  })
}

cat("\n2. TESTING SPECIFIC ONTOLOGY COMPATIBILITY\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Theory: The pull method tries to combine data from both ontologies
# and fails when dimension values don't match

cat("Checking what happens when we have mismatched dimension values...\n")

# Check if the pull method is trying to combine data from multiple ontologies
cat("\nTesting pull without specifying ontology (default behavior):\n")
tryCatch({
  result <- manager$pull(
    outcome = "suppression", 
    keep.dimensions = c("year", "location"),
    sources = "cdc.hiv",
    year = "2022",
    location = "PA",
    na.rm = TRUE
  )
  
  if (!is.null(result)) {
    cat("✅ Total pull succeeds\n")
    cat("Result structure:\n")
    print(str(result))
  }
}, error = function(e) {
  cat("❌ Total pull fails:", e$message, "\n")
})

cat("\n3. INVESTIGATING PULL METHOD BEHAVIOR\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Test what happens when we have data in multiple ontologies for the same outcome
cat("Checking available stratifications by ontology:\n")

for (ont in ontologies) {
  cat(sprintf("\n%s ontology:\n", ont))
  strats <- names(suppression_data[[ont]])
  
  # Check which have PA data
  pa_strats <- c()
  for (strat_name in strats) {
    strat_data <- suppression_data[[ont]][[strat_name]]
    if (!is.null(strat_data)) {
      dims <- dimnames(strat_data)
      if ("location" %in% names(dims) && "PA" %in% dims$location) {
        pa_strats <- c(pa_strats, strat_name)
      }
    }
  }
  cat("Stratifications with PA data:", paste(pa_strats, collapse=", "), "\n")
}

cat("\n4. SPECIFIC AGE MISMATCH TEST\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Test the specific theory: age dimension mismatch causes the failure
cat("Testing if age dimension mismatch is the specific issue:\n")

# Check exactly what the pull method does with age
cat("\nDetailed age pull investigation:\n")

# Get the actual data for age stratification from each ontology
for (ont in ontologies) {
  cat(sprintf("\n--- %s ontology age data ---\n", ont))
  age_data <- suppression_data[[ont]]$year__location__age
  
  if (!is.null(age_data)) {
    dims <- dimnames(age_data)
    cat("Available years:", paste(dims$year, collapse=", "), "\n")
    
    if ("PA" %in% dims$location) {
      pa_idx <- which(dims$location == "PA")
      
      # Check 2022 data specifically
      if ("2022" %in% dims$year) {
        year_2022_idx <- which(dims$year == "2022")
        pa_2022_data <- age_data[year_2022_idx, pa_idx, ]
        
        cat("PA 2022 age data availability:\n")
        for (i in seq_along(dims$age)) {
          age_group <- dims$age[i]
          value <- pa_2022_data[i]
          status <- if (is.na(value)) "NA" else sprintf("%.3f", value)
          cat(sprintf("  %s: %s\n", age_group, status))
        }
      }
    }
  }
}

cat("\n5. THEORY SUMMARY AND NEXT STEPS\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat("THEORY: The pull method fails for age because:\n")
cat("1. PA data exists only in cdc.new ontology (with 6 age groups)\n")
cat("2. Other data exists in cdc ontology (with 5 age groups)\n") 
cat("3. Pull method cannot reconcile different age group structures\n")
cat("4. This returns NULL, causing the array() error in likelihood\n")
cat("\nTEST RESULTS ABOVE show whether this theory is correct.\n")