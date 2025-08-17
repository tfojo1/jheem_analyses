# Replicate the exact data pull that causes the likelihood failure
# Based on suppression.basic.likelihood.instructions parameters

library(jheem2)

# Load the manager with our integrated PA data
manager <- load.data.manager("cached/surveillance.manager.rdata")

cat("=== REPLICATING LIKELIHOOD DATA PULL FOR PA SUPPRESSION ===\n")

# These are the exact parameters from suppression.basic.likelihood.instructions
outcome <- "suppression"
dimensions <- c("age", "sex", "race", "risk")
levels.of.stratification <- c(0, 1)  # totals + 1-way stratifications
from.year <- 2008
location <- "PA"

cat("Parameters:\n")
cat("- outcome:", outcome, "\n")
cat("- dimensions:", paste(dimensions, collapse=", "), "\n") 
cat("- levels.of.stratification:", paste(levels.of.stratification, collapse=", "), "\n")
cat("- from.year:", from.year, "\n")
cat("- location:", location, "\n")

# Generate stratifications like the likelihood system does
generate.stratifications <- function(dimensions, levels.of.stratification) {
  output.stratifications <- list()
  for (level in sort(levels.of.stratification)) {
    if (level == 0) {
      output.stratifications <- c(output.stratifications, "")
    } else {
      output.stratifications <- c(output.stratifications, combn(sort(dimensions), level, simplify = F))
    }
  }
  output.stratifications
}

stratifications <- generate.stratifications(dimensions, levels.of.stratification)
cat("\nGenerated stratifications:\n")
for (i in seq_along(stratifications)) {
  strat <- stratifications[[i]]
  if (identical(strat, "")) {
    cat(sprintf("  %d: total (no stratification)\n", i))
  } else {
    cat(sprintf("  %d: %s\n", i, paste(strat, collapse=" + ")))
  }
}

cat("\n=== TESTING DATA PULLS ===\n")

# Test each stratification that the likelihood system would try
for (i in seq_along(stratifications)) {
  strat <- stratifications[[i]]
  
  if (identical(strat, "")) {
    keep.dimensions <- c("year", "location")
    strat_name <- "total"
  } else {
    keep.dimensions <- c("year", "location", strat)
    strat_name <- paste(strat, collapse=" + ")
  }
  
  cat(sprintf("\n--- Stratification %d: %s ---\n", i, strat_name))
  cat("keep.dimensions:", paste(keep.dimensions, collapse=", "), "\n")
  
  # Try the exact pull the likelihood would do
  tryCatch({
    # The likelihood system would use the default ontology and include all dimension values
    result <- manager$pull(
      outcome = outcome,
      keep.dimensions = keep.dimensions,
      sources = "cdc.hiv",  # Specify the source that has our PA data
      year = as.character(from.year:2030),  # Use argument form instead of dimension.values list
      location = location,
      na.rm = TRUE
    )
    
    if (is.null(result)) {
      cat("❌ PULL RETURNED NULL\n")
    } else {
      cat("✅ Pull succeeded\n")
      cat("Result class:", class(result), "\n")
      cat("Result dimensions:", paste(dim(result), collapse=" x "), "\n")
      
      # Check for PA data specifically
      if (is.array(result) && "PA" %in% dimnames(result)$location) {
        pa_data <- result[, "PA"]
        if (length(strat) == 0) {
          # Total data
          non_na_count <- sum(!is.na(pa_data))
          cat("PA data points (non-NA):", non_na_count, "\n")
          if (non_na_count > 0) {
            years_with_data <- names(pa_data)[!is.na(pa_data)]
            cat("Years with PA data:", paste(years_with_data, collapse=", "), "\n")
          }
        } else {
          # Stratified data
          non_na_count <- sum(!is.na(pa_data))
          cat("PA data points (non-NA):", non_na_count, "\n")
        }
      } else {
        cat("❌ No PA data found in result\n")
      }
    }
    
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n")
  })
}

cat("\n=== INVESTIGATING NULL RESULTS ===\n")

# Check what suppression data exists in the manager
cat("Checking manager data structure:\n")
if ("suppression" %in% names(manager$data)) {
  cat("✅ Suppression outcome exists\n")
  
  if ("estimate" %in% names(manager$data$suppression)) {
    cat("✅ Estimate metric exists\n")
    sources_with_suppression <- names(manager$data$suppression$estimate)
    cat("Sources with suppression data:", paste(sources_with_suppression, collapse=", "), "\n")
    
    for (source_name in sources_with_suppression) {
      cat(sprintf("\n--- Source: %s ---\n", source_name))
      source_data <- manager$data$suppression$estimate[[source_name]]
      ontologies <- names(source_data)
      cat("Ontologies:", paste(ontologies, collapse=", "), "\n")
      
      for (ont_name in ontologies) {
        cat(sprintf("  Ontology: %s\n", ont_name))
        ont_data <- source_data[[ont_name]]
        stratifications <- names(ont_data)
        cat("    Stratifications:", paste(head(stratifications, 5), collapse=", "), "\n")
        
        # Check if PA is in any stratification
        if (length(stratifications) > 0) {
          first_strat_data <- ont_data[[stratifications[1]]]
          if (!is.null(first_strat_data) && is.array(first_strat_data)) {
            dims <- dimnames(first_strat_data)
            if ("location" %in% names(dims)) {
              if ("PA" %in% dims$location) {
                cat("    ✅ PA found in location dimension\n")
              } else {
                cat("    ❌ PA not found in location dimension\n")
                cat("    Available locations:", paste(head(dims$location, 10), collapse=", "), "\n")
              }
            }
          }
        }
      }
    }
  } else {
    cat("❌ No estimate metric found\n")
  }
} else {
  cat("❌ No suppression outcome found\n")
}

cat("\n=== SUMMARY ===\n")
cat("This script replicates the exact data pull parameters\n")
cat("that the likelihood system uses for PA suppression data.\n")
cat("Any NULL results indicate why the likelihood instantiation fails.\n")