#!/usr/bin/env Rscript
# Explore what's in the backup manager

library(jheem2)

cat("=== EXPLORING BACKUP MANAGER CONTENTS ===\n\n")

# Load the backup manager
manager <- load.data.manager("cached/surveillance.manager_backup_2025-08-14.rdata")

cat("1. Manager basic info:\n")
cat("   Name:", manager$name, "\n")
cat("   Description:", manager$description, "\n")
cat("   Created:", as.character(manager$creation.date), "\n")
cat("   Modified:", as.character(manager$last.modified.date), "\n")

cat("\n2. Registered Outcomes:\n")
if (length(manager$outcomes) > 0) {
  outcomes <- names(manager$outcomes)
  cat("   Found", length(outcomes), "outcomes:\n")
  
  # Look for suppression-related outcomes
  suppression_related <- outcomes[grep("supp", outcomes, ignore.case = TRUE)]
  if (length(suppression_related) > 0) {
    cat("   Suppression-related outcomes:\n")
    for (o in suppression_related) {
      cat("     -", o, "\n")
    }
  }
  
  # Show first 10 outcomes
  cat("   First 10 outcomes:\n")
  for (i in 1:min(10, length(outcomes))) {
    cat("     -", outcomes[i], "\n")
  }
  
  if (length(outcomes) > 10) {
    cat("   ... and", length(outcomes) - 10, "more\n")
  }
} else {
  cat("   No outcomes registered\n")
}

cat("\n3. Registered Sources:\n")
if (length(manager$sources) > 0) {
  sources <- names(manager$sources)
  cat("   Found", length(sources), "sources:\n")
  
  # Look for CDC sources
  cdc_sources <- sources[grep("cdc", sources, ignore.case = TRUE)]
  if (length(cdc_sources) > 0) {
    cat("   CDC sources:\n")
    for (s in cdc_sources) {
      cat("     -", s, "\n")
    }
  }
} else {
  cat("   No sources registered\n")
}

cat("\n4. Registered Ontologies:\n")
if (length(manager$ontologies) > 0) {
  ontologies <- names(manager$ontologies)
  cat("   Found", length(ontologies), "ontologies:\n")
  for (ont in ontologies) {
    cat("     -", ont, "\n")
    # Check dimensions
    if (!is.null(manager$ontologies[[ont]])) {
      dims <- names(manager$ontologies[[ont]])
      cat("       Dimensions:", paste(dims, collapse = ", "), "\n")
    }
  }
} else {
  cat("   No ontologies registered\n")
}

cat("\n5. Data structure:\n")
if (!is.null(manager$data) && length(manager$data) > 0) {
  cat("   Outcomes with data:\n")
  data_outcomes <- names(manager$data)
  
  # Check for suppression in data even if not in outcomes
  for (outcome in data_outcomes) {
    if (grepl("supp", outcome, ignore.case = TRUE)) {
      cat("   ⚠️ Found suppression-like outcome in data:", outcome, "\n")
      
      # Check what's in it
      if (!is.null(manager$data[[outcome]]$estimate$cdc.hiv)) {
        cat("      Has cdc.hiv source data\n")
        ontologies_with_data <- names(manager$data[[outcome]]$estimate$cdc.hiv)
        cat("      Ontologies:", paste(ontologies_with_data, collapse = ", "), "\n")
      }
    }
  }
  
  # Show first few
  cat("\n   First 5 outcomes in data:\n")
  for (i in 1:min(5, length(data_outcomes))) {
    cat("     -", data_outcomes[i], "\n")
  }
} else {
  cat("   No data stored\n")
}

cat("\n=== EXPLORATION COMPLETE ===\n")

# Try alternative names for suppression
cat("\nSearching for alternative suppression outcome names:\n")
possible_names <- c("suppression", "viral.suppression", "hiv.suppression", 
                    "viral_suppression", "hiv_suppression", "vs", "vl.suppression")

for (name in possible_names) {
  if (name %in% names(manager$outcomes)) {
    cat("  ✓ Found:", name, "\n")
  }
  if (name %in% names(manager$data)) {
    cat("  ✓ Found in data:", name, "\n")
  }
}
