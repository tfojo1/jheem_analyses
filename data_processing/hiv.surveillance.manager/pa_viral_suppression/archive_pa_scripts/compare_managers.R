#!/usr/bin/env Rscript
# Compare both managers to understand the difference

library(jheem2)

cat("=== COMPARING MANAGERS ===\n\n")

# Load both managers
cat("Loading managers...\n")
backup_manager <- load.data.manager("cached/surveillance.manager_backup_2025-08-14.rdata")
current_manager <- load.data.manager("cached/surveillance.manager.rdata")

cat("\n1. BACKUP MANAGER (2025-08-14):\n")
cat("   Name:", backup_manager$name, "\n")
cat("   Outcomes:", length(backup_manager$outcomes), "\n")
cat("   Sources:", length(backup_manager$sources), "\n")
cat("   Ontologies:", length(backup_manager$ontologies), "\n")

# Check for suppression
if ("suppression" %in% names(backup_manager$outcomes)) {
  cat("   ✓ Has suppression outcome\n")
} else if ("suppression" %in% names(backup_manager$data)) {
  cat("   ⚠️ Has suppression in data but not in outcomes\n")
} else {
  cat("   ✗ No suppression outcome\n")
}

cat("\n2. CURRENT MANAGER:\n")
cat("   Name:", current_manager$name, "\n")
cat("   Outcomes:", length(current_manager$outcomes), "\n")
cat("   Sources:", length(current_manager$sources), "\n")
cat("   Ontologies:", length(current_manager$ontologies), "\n")

# Check for suppression
if ("suppression" %in% names(current_manager$outcomes)) {
  cat("   ✓ Has suppression outcome\n")
  
  # Check PA data
  if (!is.null(current_manager$data$suppression$estimate$cdc.hiv)) {
    supp_data <- current_manager$data$suppression$estimate$cdc.hiv
    
    for (ont in names(supp_data)) {
      if ("year__location" %in% names(supp_data[[ont]])) {
        data <- supp_data[[ont]][["year__location"]]
        dims <- dimnames(data)
        
        if ("PA" %in% dims$location) {
          pa_years <- dims$year[!is.na(data[, "PA"])]
          if (length(pa_years) > 0) {
            cat(sprintf("   PA data in %s ontology: years %s\n", 
                       ont, paste(pa_years, collapse=", ")))
          }
        }
      }
    }
  }
} else {
  cat("   ✗ No suppression outcome\n")
}

cat("\n3. DIFFERENCES:\n")

# Compare outcomes
backup_outcomes <- names(backup_manager$outcomes)
current_outcomes <- names(current_manager$outcomes)

new_outcomes <- setdiff(current_outcomes, backup_outcomes)
if (length(new_outcomes) > 0) {
  cat("   New outcomes in current:\n")
  for (o in new_outcomes) {
    cat("     +", o, "\n")
  }
}

removed_outcomes <- setdiff(backup_outcomes, current_outcomes)
if (length(removed_outcomes) > 0) {
  cat("   Outcomes in backup but not current:\n")
  for (o in removed_outcomes) {
    cat("     -", o, "\n")
  }
}

cat("\n4. RECOMMENDATION:\n")
if ("suppression" %in% names(current_manager$outcomes) && 
    !"suppression" %in% names(backup_manager$outcomes)) {
  cat("   The suppression outcome was added after the backup was created.\n")
  cat("   We should use the current manager and just fix the PA data.\n")
} else if (!"suppression" %in% names(current_manager$outcomes)) {
  cat("   Neither manager has suppression outcome registered.\n")
  cat("   Need to register the outcome first.\n")
} else {
  cat("   Both managers have suppression. Backup is suitable.\n")
}

cat("\n=== COMPARISON COMPLETE ===\n")
