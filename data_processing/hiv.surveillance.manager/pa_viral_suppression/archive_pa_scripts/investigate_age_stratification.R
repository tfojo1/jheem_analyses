# Investigate why age stratification returns NULL for PA data

library(jheem2)

manager <- load.data.manager("cached/surveillance.manager.rdata")

cat("=== INVESTIGATING AGE STRATIFICATION FAILURE ===\n")

# Check what age stratifications exist in our PA data
cat("Checking available age stratifications in cdc.hiv source:\n")

suppression_data <- manager$data$suppression$estimate$cdc.hiv

for (ont_name in names(suppression_data)) {
  cat(sprintf("\n--- Ontology: %s ---\n", ont_name))
  ont_data <- suppression_data[[ont_name]]
  age_strats <- names(ont_data)[grepl("age", names(ont_data))]
  cat("Age-related stratifications:", paste(age_strats, collapse=", "), "\n")
  
  # Check year__location__age specifically
  if ("year__location__age" %in% names(ont_data)) {
    age_data <- ont_data$year__location__age
    if (!is.null(age_data)) {
      dims <- dimnames(age_data)
      cat("year__location__age dimensions:", paste(names(dims), collapse=", "), "\n")
      cat("Years:", paste(dims$year, collapse=", "), "\n")
      cat("Locations (first 10):", paste(head(dims$location, 10), collapse=", "), "\n")
      cat("Ages:", paste(dims$age, collapse=", "), "\n")
      
      # Check if PA data exists
      if ("PA" %in% dims$location) {
        pa_idx <- which(dims$location == "PA")
        cat("✅ PA found at index", pa_idx, "\n")
        
        # Check 2022-2023 data
        for (year in c("2022", "2023")) {
          if (year %in% dims$year) {
            year_idx <- which(dims$year == year)
            pa_year_data <- age_data[year_idx, pa_idx, ]
            non_na_count <- sum(!is.na(pa_year_data))
            cat(sprintf("%s: %d non-NA age groups\n", year, non_na_count))
            if (non_na_count > 0) {
              ages_with_data <- dims$age[!is.na(pa_year_data)]
              cat(sprintf("  Ages with data: %s\n", paste(ages_with_data, collapse=", ")))
            }
          }
        }
      } else {
        cat("❌ PA not found in location dimension\n")
      }
    } else {
      cat("❌ year__location__age data is NULL\n")
    }
  } else {
    cat("❌ No year__location__age stratification found\n")
  }
}

cat("\n=== TESTING SPECIFIC AGE PULL ===\n")

# Test the exact pull that fails
cat("Testing pull with age dimension...\n")
tryCatch({
  result <- manager$pull(
    outcome = "suppression",
    keep.dimensions = c("year", "location", "age"),
    sources = "cdc.hiv",
    year = as.character(2008:2030),
    location = "PA",
    na.rm = TRUE
  )
  
  if (is.null(result)) {
    cat("❌ Result is NULL\n")
  } else {
    cat("✅ Result returned\n")
    cat("Class:", class(result), "\n")
    cat("Dimensions:", paste(dim(result), collapse=" x "), "\n")
    cat("Dimension names:", paste(names(dimnames(result)), collapse=", "), "\n")
  }
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
})

# Test with different ontology specification
cat("\n=== TESTING WITH EXPLICIT ONTOLOGY ===\n")

for (ont_name in c("cdc", "cdc.new")) {
  cat(sprintf("Testing with target.ontology = '%s':\n", ont_name))
  tryCatch({
    result <- manager$pull(
      outcome = "suppression",
      keep.dimensions = c("year", "location", "age"),
      sources = "cdc.hiv",
      target.ontology = ont_name,
      year = as.character(2008:2030),
      location = "PA",
      na.rm = TRUE
    )
    
    if (is.null(result)) {
      cat(sprintf("❌ Result is NULL for %s ontology\n", ont_name))
    } else {
      cat(sprintf("✅ Result returned for %s ontology\n", ont_name))
      cat("  Dimensions:", paste(dim(result), collapse=" x "), "\n")
    }
  }, error = function(e) {
    cat(sprintf("❌ Error with %s: %s\n", ont_name, e$message))
  })
}

cat("\n=== COMPARING AGE VALUES ACROSS ONTOLOGIES ===\n")

# Check if age values are compatible between what we have and what's expected
for (ont_name in names(suppression_data)) {
  cat(sprintf("\n--- %s ontology age values ---\n", ont_name))
  ont_data <- suppression_data[[ont_name]]
  if ("year__location__age" %in% names(ont_data)) {
    age_data <- ont_data$year__location__age
    if (!is.null(age_data)) {
      age_values <- dimnames(age_data)$age
      cat("Age values:", paste(age_values, collapse=", "), "\n")
    }
  }
}

# Check the ontology metadata to see what age values are expected
cat("\n=== CHECKING ONTOLOGY METADATA ===\n")
tryCatch({
  ont_meta <- manager$ontology.metadata
  if (length(ont_meta) > 0) {
    for (ont_name in names(ont_meta)) {
      cat(sprintf("Ontology: %s\n", ont_name))
      ont_info <- ont_meta[[ont_name]]
      if ("age" %in% names(ont_info)) {
        age_values <- ont_info$age
        cat("  Expected age values:", paste(age_values, collapse=", "), "\n")
      }
    }
  } else {
    cat("No ontology metadata found\n")
  }
}, error = function(e) {
  cat("Error accessing ontology metadata:", e$message, "\n")
})

cat("\n=== SUMMARY ===\n")
cat("Age stratification failure could be due to:\n")
cat("1. Missing age values in our integrated data\n") 
cat("2. Ontology mismatch between age values\n")
cat("3. Pull method not finding compatible age stratification\n")