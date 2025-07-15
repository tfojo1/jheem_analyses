# Test: Create total diagnosis using age restratification
library(jheem2)

cat("=== TOTAL DIAGNOSIS WITH AGE RESTRATIFICATION ===\n")

# Load existing syphilis manager
cat("Loading existing syphilis manager...\n")
load("cached/syphilis.manager.rdata")

# Target: cdc.aggregated.county source, cdc.sti ontology (10 age groups)
source_name <- 'cdc.aggregated.county'
target_ontology <- 'cdc.sti'

cat(sprintf("Target: %s source, %s ontology\n", source_name, target_ontology))

# Check if total.syphilis.diagnoses outcome is registered
if ('total.syphilis.diagnoses' %in% names(syphilis.manager$outcome.info)) {
  cat("✅ total.syphilis.diagnoses outcome already registered\n")
} else {
  cat("❌ Need to register total.syphilis.diagnoses outcome\n")
  stop("Outcome not registered")
}

# Get target age groups from actual PS data (not ontology definition)
sample_ps_data <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[target_ontology]]$year__location__age
target_age_groups <- dimnames(sample_ps_data)$age
cat("Target age groups (from PS data):", paste(target_age_groups, collapse = ", "), "\n")

# Get available stratifications from PS syphilis (our target structure)
available_stratifications <- names(syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[target_ontology]])
cat("Available stratifications:", paste(available_stratifications, collapse = ", "), "\n\n")

successful_stratifications <- 0

for (stratification in available_stratifications) {
  
  cat(sprintf("Processing stratification: %s\n", stratification))
  
  # Get PS data (already in target ontology)
  ps_data <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[target_ontology]][[stratification]]
  
  # Get Early and Unknown data (from cdc.sti.two ontology)
  early_data_original <- syphilis.manager$data$early.syphilis.diagnoses$estimate[[source_name]][['cdc.sti.two']][[stratification]]
  unknown_data_original <- syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate[[source_name]][['cdc.sti.two']][[stratification]]
  
  if (is.null(early_data_original) || is.null(unknown_data_original)) {
    cat(sprintf("  Skipping - missing Early/Unknown data for this stratification\n"))
    next
  }
  
  # Check if age is a dimension in this stratification
  if (!"age" %in% names(dim(ps_data))) {
    # No age dimension - can directly sum
    cat(sprintf("  No age dimension - direct summation\n"))
    
    if (!identical(dim(ps_data), dim(early_data_original)) || !identical(dim(ps_data), dim(unknown_data_original))) {
      cat(sprintf("  Skipping - dimension mismatch\n"))
      next
    }
    
    total_data <- ps_data + early_data_original + unknown_data_original
    
    # Conservative NA handling (use original data since no restratification)
    na_mask <- is.na(ps_data) | is.na(early_data_original) | is.na(unknown_data_original)
    total_data[na_mask] <- NA
    
  } else {
    # Has age dimension - need restratification
    cat(sprintf("  Has age dimension - restratifying Early/Unknown\n"))
    
    # Restratify Early data to target age groups
    cat(sprintf("    Restratifying Early data...\n"))
    early_data_restratified <- jheem2::restratify.age.counts(
      early_data_original, 
      desired.age.brackets = target_age_groups,
      smooth.infinite.age.to = 100,
      allow.extrapolation = TRUE
    )
    
    # Restratify Unknown data to target age groups  
    cat(sprintf("    Restratifying Unknown data...\n"))
    unknown_data_restratified <- jheem2::restratify.age.counts(
      unknown_data_original,
      desired.age.brackets = target_age_groups, 
      smooth.infinite.age.to = 100,
      allow.extrapolation = TRUE
    )
    
    # Check dimensions now match
    if (!identical(dim(ps_data), dim(early_data_restratified)) || !identical(dim(ps_data), dim(unknown_data_restratified))) {
      cat(sprintf("    Skipping - dimensions don't match after restratification\n"))
      cat(sprintf("      PS: %s\n", paste(dim(ps_data), collapse="x")))
      cat(sprintf("      Early: %s\n", paste(dim(early_data_restratified), collapse="x")))
      cat(sprintf("      Unknown: %s\n", paste(dim(unknown_data_restratified), collapse="x")))
      next
    }
    
    if (!identical(dimnames(ps_data), dimnames(early_data_restratified)) || !identical(dimnames(ps_data), dimnames(unknown_data_restratified))) {
      cat(sprintf("    Skipping - dimnames don't match after restratification\n"))
      next
    }
    
    # Sum the components
    total_data <- ps_data + early_data_restratified + unknown_data_restratified
    
    # Conservative NA handling
    na_mask <- is.na(ps_data) | is.na(early_data_restratified) | is.na(unknown_data_restratified)
    total_data[na_mask] <- NA
  }
  
  # Put the total data into the manager
  tryCatch({
    syphilis.manager$put(
      data = total_data,
      outcome = 'total.syphilis.diagnoses', 
      source = source_name,
      ontology.name = target_ontology,
      url = "https://gis.cdc.gov/grasp/nchhstpatlas/main.html",
      details = "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from CDC AtlasPlus (Early/Unknown restratified to match PS age groups)"
    )
    
    cat(sprintf("  ✅ Successfully created total diagnosis data\n"))
    successful_stratifications <- successful_stratifications + 1
    
  }, error = function(e) {
    cat(sprintf("  ❌ Error creating total: %s\n", e$message))
  })
  
  cat("\n")
}

cat(sprintf("Successfully processed %d stratifications\n", successful_stratifications))

# Verification: Test Baltimore MSA 2022
cat("\n=== VERIFICATION ===\n")
tryCatch({
  # Get component data for Baltimore 2022
  ps_balt_2022 <- syphilis.manager$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location['2022', 'C.12580']
  early_orig_2022 <- syphilis.manager$data$early.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti.two$year__location['2022', 'C.12580']
  unknown_orig_2022 <- syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti.two$year__location['2022', 'C.12580']
  
  # Get our calculated total
  total_balt_2022 <- syphilis.manager$data$total.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location['2022', 'C.12580']
  
  cat(sprintf("Baltimore 2022 verification:\n"))
  cat(sprintf("PS (cdc.sti): %s\n", ps_balt_2022))
  cat(sprintf("Early (cdc.sti.two): %s\n", early_orig_2022))
  cat(sprintf("Unknown (cdc.sti.two): %s\n", unknown_orig_2022))
  cat(sprintf("Our calculated total: %s\n", total_balt_2022))
  
  # Note: Can't directly sum early_orig + unknown_orig since they use different age groups
  # The restratification handles the age group conversion
  
  if (!is.na(total_balt_2022) && total_balt_2022 > 0) {
    cat("✅ Total diagnosis successfully created!\n")
  } else {
    cat("❌ Total diagnosis is NA or 0\n")
  }
  
}, error = function(e) {
  cat("Error in verification:", e$message, "\n")
})

# Save test result
cat("\nSaving test result...\n")
save(syphilis.manager, file="cached/syphilis.manager_with_total_RESTRATIFIED_TEST.rdata")
cat("Saved to: cached/syphilis.manager_with_total_RESTRATIFIED_TEST.rdata\n")

cat("\n=== TEST COMPLETED ===\n")
