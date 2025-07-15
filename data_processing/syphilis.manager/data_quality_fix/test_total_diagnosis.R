# Standalone test: Add total diagnosis to existing syphilis manager
library(jheem2)

cat("=== TESTING TOTAL DIAGNOSIS ADDITION ===\n")

# Load existing syphilis manager (which has component data)
cat("Loading existing syphilis manager...\n")
load("cached/syphilis.manager.rdata")

# Check what we have to work with
cat("Checking available data for components...\n")
components <- c('ps.syphilis.diagnoses', 'early.syphilis.diagnoses', 'unknown.duration.or.late.syphilis.diagnoses')

for (component in components) {
  if (component %in% names(syphilis.manager$data)) {
    sources <- names(syphilis.manager$data[[component]]$estimate)
    cat(sprintf("%s has sources: %s\n", component, paste(sources, collapse = ", ")))
  } else {
    cat(sprintf("❌ %s missing\n", component))
  }
}

# Check if total.syphilis.diagnoses outcome is already registered
if ('total.syphilis.diagnoses' %in% names(syphilis.manager$outcome.info)) {
  cat("✅ total.syphilis.diagnoses outcome already registered\n")
} else {
  cat("❌ Need to register total.syphilis.diagnoses outcome\n")
  # Register it
  syphilis.manager$register.outcome(
    'total.syphilis.diagnoses',
    metadata = create.outcome.metadata(
      scale = 'non.negative.number',
      display.name = 'Total Syphilis Diagnoses',
      axis.name = 'Total Syphilis Diagnoses', 
      units = 'cases',
      description = "Total Syphilis Diagnoses (sum of PS + Early + Unknown Duration/Late)"))
  cat("✅ Registered total.syphilis.diagnoses outcome\n")
}

# Focus on cdc.aggregated.county source (the one with problems)
cat("\nFocusing on cdc.aggregated.county source...\n")
source_name <- 'cdc.aggregated.county'

# Check what ontologies each component uses
ps_ontologies <- names(syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]])
early_ontologies <- names(syphilis.manager$data$early.syphilis.diagnoses$estimate[[source_name]])
unknown_ontologies <- names(syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate[[source_name]])

cat(sprintf("PS ontologies: %s\n", paste(ps_ontologies, collapse = ", ")))
cat(sprintf("Early ontologies: %s\n", paste(early_ontologies, collapse = ", ")))
cat(sprintf("Unknown ontologies: %s\n", paste(unknown_ontologies, collapse = ", ")))

# Find common ontologies (where we can create totals)
common_ontologies <- intersect(intersect(ps_ontologies, early_ontologies), unknown_ontologies)
cat(sprintf("Common ontologies: %s\n", paste(common_ontologies, collapse = ", ")))

if (length(common_ontologies) == 0) {
  cat("❌ No common ontologies - cannot create totals\n")
  cat("This suggests PS syphilis uses different age groups than Early/Unknown\n")
  quit()
}

# Process each common ontology
for (ontology_name in common_ontologies) {
  cat(sprintf("\nProcessing ontology: %s\n", ontology_name))
  # Get available stratifications from PS syphilis (use as template)
  available_stratifications <- names(syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[ontology_name]])
  cat(sprintf("Available stratifications for %s: %s\n", ontology_name, paste(available_stratifications, collapse = ", "))))

  # Create total diagnosis for each stratification
  successful_stratifications <- 0
  
  for (stratification in available_stratifications) {
    
    cat(sprintf("  Processing stratification: %s\n", stratification))
    
    # Get data arrays for all three components
    ps_data <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    early_data <- syphilis.manager$data$early.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    unknown_data <- syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    
    # Check dimensions match
    if (!identical(dim(ps_data), dim(early_data)) || !identical(dim(ps_data), dim(unknown_data))) {
      cat(sprintf("    Skipping - dimension mismatch\n"))
      next
    }
    
    if (!identical(dimnames(ps_data), dimnames(early_data)) || !identical(dimnames(ps_data), dimnames(unknown_data))) {
      cat(sprintf("    Skipping - dimnames mismatch\n"))
      next  
    }
    
    # Create total by summing components
    total_data <- ps_data + early_data + unknown_data
    
    # Conservative NA handling: if any component is NA, total is NA
    na_mask <- is.na(ps_data) | is.na(early_data) | is.na(unknown_data)
    total_data[na_mask] <- NA
    
    # Put the total data into the manager
    tryCatch({
      syphilis.manager$put(
        data = total_data,
        outcome = 'total.syphilis.diagnoses',
        source = source_name,
        ontology.name = ontology_name,
        url = "https://gis.cdc.gov/grasp/nchhstpatlas/main.html",
        details = "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from CDC AtlasPlus"
      )
      
      cat(sprintf("    ✅ Successfully created total diagnosis data for %s\n", stratification))
      successful_stratifications <- successful_stratifications + 1
      
    }, error = function(e) {
      cat(sprintf("    ❌ Error creating total for %s: %s\n", stratification, e$message))
    })
  }

}

cat(sprintf("\nSuccessfully processed %d stratifications\n", successful_stratifications))

# Quick verification: Test Baltimore MSA
cat("\n=== VERIFICATION ===\n")
tryCatch({
  # Get component data for Baltimore 2022
  ps_balt_2022 <- syphilis.manager$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location['2022', 'C.12580']
  early_balt_2022 <- syphilis.manager$data$early.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location['2022', 'C.12580']
  unknown_balt_2022 <- syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location['2022', 'C.12580']
  
  # Get our calculated total
  total_balt_2022 <- syphilis.manager$data$total.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location['2022', 'C.12580']
  
  # Check the math
  expected_total <- ps_balt_2022 + early_balt_2022 + unknown_balt_2022
  
  cat(sprintf("Baltimore 2022 verification:\n"))
  cat(sprintf("PS: %s, Early: %s, Unknown: %s\n", ps_balt_2022, early_balt_2022, unknown_balt_2022))
  cat(sprintf("Expected total: %s\n", expected_total))
  cat(sprintf("Calculated total: %s\n", total_balt_2022))
  
  if (abs(as.numeric(total_balt_2022) - as.numeric(expected_total)) < 0.001) {
    cat("✅ Calculation verified!\n")
  } else {
    cat("❌ Calculation mismatch!\n")
  }
  
}, error = function(e) {
  cat("Error in verification:", e$message, "\n")
})

# Save test result
cat("\nSaving test result...\n")
save(syphilis.manager, file="cached/syphilis.manager_with_total_TEST.rdata")
cat("Saved to: cached/syphilis.manager_with_total_TEST.rdata\n")

cat("\n=== TEST COMPLETED ===\n")
