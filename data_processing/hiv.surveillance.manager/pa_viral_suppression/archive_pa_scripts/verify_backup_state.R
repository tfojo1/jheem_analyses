#!/usr/bin/env Rscript
# Quick check to verify the backup manager state

library(jheem2)

cat("=== VERIFYING BACKUP MANAGER STATE ===\n\n")

# Load the backup manager
manager <- load.data.manager("cached/surveillance.manager_backup_2025-08-14.rdata")

cat("Checking for PA data in backup manager:\n")
cat("-" , rep("", 40), "\n")

# Check if suppression data exists
if ("suppression" %in% manager$outcomes || "suppression" %in% names(manager$data)) {
  supp_data <- manager$data$suppression$estimate$cdc.hiv
  
  # Check each ontology
  for (ont in names(supp_data)) {
    cat(sprintf("\n%s ontology:\n", ont))
    
    if ("year__location" %in% names(supp_data[[ont]])) {
      data <- supp_data[[ont]][["year__location"]]
      dims <- dimnames(data)
      
      # Check for PA
      if ("PA" %in% dims$location) {
        pa_years <- dims$year[!is.na(data[, "PA"])]
        if (length(pa_years) > 0) {
          cat(sprintf("  ⚠️  PA data found for years: %s\n", paste(pa_years, collapse=", ")))
        } else {
          cat("  ✓ PA location exists but no data values\n")
        }
      } else {
        cat("  ✓ PA not in location list\n")
      }
      
      # Count locations with data
      locs_with_data <- sum(apply(data, 2, function(x) any(!is.na(x))))
      cat(sprintf("  Total locations with data: %d\n", locs_with_data))
    }
  }
} else {
  cat("Suppression outcome not found in manager\n")
}

cat("\n=== VERIFICATION COMPLETE ===\n")
cat("\nThis backup should be clean (no PA data in cdc.new ontology)\n")
cat("Ready to run the corrected integration script.\n")
