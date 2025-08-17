# Investigation: Why is PA missing from the location ontology?
# This could be the root cause of the likelihood failure

library(jheem2)

# Load the integrated manager
manager <- load.data.manager("cached/surveillance.manager.rdata")

cat("=== ONTOLOGY INVESTIGATION ===\n")

# Check all ontology metadata
ont_metadata <- manager$ontology.metadata
cat("Available ontologies:", paste(names(ont_metadata), collapse=", "), "\n")

for (ont_name in names(ont_metadata)) {
  cat(sprintf("\n--- %s ontology ---\n", ont_name))
  ont_info <- ont_metadata[[ont_name]]
  cat("Dimensions:", paste(names(ont_info), collapse=", "), "\n")
  
  if ("location" %in% names(ont_info)) {
    location_values <- ont_info$location
    cat("Location values count:", length(location_values), "\n")
    
    # Check for PA specifically
    if ("PA" %in% location_values) {
      cat("✅ PA found in location ontology\n")
    } else {
      cat("❌ PA NOT found in location ontology\n")
    }
    
    # Show sample locations
    cat("Sample locations:", paste(head(location_values, 10), collapse=", "), "\n")
    
    # Check for PA counties
    pa_counties <- location_values[grepl("^42[0-9]+$", location_values)]
    cat("PA counties:", length(pa_counties), "\n")
    if (length(pa_counties) > 0) {
      cat("Sample PA counties:", paste(head(pa_counties, 5), collapse=", "), "\n")
    }
  }
}

# Check the actual data arrays to see what locations they contain
cat("\n=== DATA ARRAY LOCATIONS vs ONTOLOGY ===\n")

suppression_data <- manager$data$suppression$estimate$cdc.hiv$cdc.new
total_data <- suppression_data$year__location
data_locations <- dimnames(total_data)$location

cat("Locations in data arrays:", length(data_locations), "\n")
cat("Sample data locations:", paste(head(data_locations, 10), collapse=", "), "\n")

if ("PA" %in% data_locations) {
  cat("✅ PA found in data arrays\n")
} else {
  cat("❌ PA NOT found in data arrays\n")
}

# Compare ontology vs data locations
for (ont_name in names(ont_metadata)) {
  ont_info <- ont_metadata[[ont_name]]
  if ("location" %in% names(ont_info)) {
    ont_locations <- ont_info$location
    
    cat(sprintf("\n--- %s ontology vs data comparison ---\n", ont_name))
    
    # Find locations in data but not in ontology
    missing_from_ont <- setdiff(data_locations, ont_locations)
    if (length(missing_from_ont) > 0) {
      cat("❌ Locations in data but missing from ontology:", length(missing_from_ont), "\n")
      if ("PA" %in% missing_from_ont) {
        cat("  ❌ PA is missing from ontology!\n")
      }
      cat("  Missing:", paste(head(missing_from_ont, 10), collapse=", "), "\n")
    } else {
      cat("✅ All data locations found in ontology\n")
    }
    
    # Find locations in ontology but not in data
    missing_from_data <- setdiff(ont_locations, data_locations)
    if (length(missing_from_data) > 0) {
      cat("⚠️  Locations in ontology but missing from data:", length(missing_from_data), "\n")
      cat("  Extra:", paste(head(missing_from_data, 10), collapse=", "), "\n")
    }
  }
}

# Check if this is related to the data integration process
cat("\n=== INTEGRATION HISTORY CHECK ===\n")

# Look at sources to see if PA data was properly registered
sources <- manager$sources
cat("Available sources:", paste(names(sources), collapse=", "), "\n")

# Check if cdc.hiv source includes PA in its locations
if ("cdc.hiv" %in% names(sources)) {
  cdc_source <- sources$cdc.hiv
  cat("cdc.hiv source metadata available\n")
  
  # Check the source's location specification if it exists
  if (!is.null(cdc_source$locations)) {
    source_locations <- cdc_source$locations
    if ("PA" %in% source_locations) {
      cat("✅ PA found in cdc.hiv source locations\n")
    } else {
      cat("❌ PA NOT found in cdc.hiv source locations\n")
    }
  } else {
    cat("No location specification in cdc.hiv source\n")
  }
}

# Check outcomes metadata  
outcomes <- manager$outcomes
cat("\nAvailable outcomes:", paste(names(outcomes), collapse=", "), "\n")

if ("suppression" %in% names(outcomes)) {
  supp_outcome <- outcomes$suppression
  cat("Suppression outcome metadata available\n")
}

cat("\n=== POTENTIAL SOLUTIONS ===\n")
cat("1. The ontology needs to be updated to include PA and PA counties\n")
cat("2. This likely requires re-registering the location ontology\n")
cat("3. Or updating the existing ontology to include the new locations\n")
cat("4. The likelihood system probably validates against the ontology\n")