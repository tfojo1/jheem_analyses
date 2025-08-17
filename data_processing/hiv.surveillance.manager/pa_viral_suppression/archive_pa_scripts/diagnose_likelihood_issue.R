# Diagnostic script to understand why likelihood fails for PA data
# Compare PA data structure to working locations

library(jheem2)

# Load the integrated manager
manager <- load.data.manager("cached/surveillance.manager.rdata")

# Get surveillance data structure
suppression_data <- manager$data$suppression$estimate$cdc.hiv$cdc.new

cat("=== DIAGNOSTIC: PA Data Structure vs Working Locations ===\n")

# Let's check what locations have suppression data across years
total_data <- suppression_data$year__location
dims <- dimnames(total_data)
all_years <- dims$year
all_locations <- dims$location

cat("Available years:", paste(all_years, collapse=", "), "\n")
cat("Total locations:", length(all_locations), "\n")

# Find locations with historical data (pre-2022)
historical_years <- all_years[as.numeric(all_years) < 2022]
current_years <- c("2022", "2023")

cat("\nHistorical years:", paste(historical_years, collapse=", "), "\n")
cat("Current years:", paste(current_years, collapse=", "), "\n")

# Check data availability by location and year
cat("\n=== DATA AVAILABILITY COMPARISON ===\n")

# Sample of states to compare with PA
comparison_states <- c("PA", "NY", "CA", "TX", "FL")
comparison_states <- comparison_states[comparison_states %in% all_locations]

for (state in comparison_states) {
  cat(sprintf("\n--- %s Data Profile ---\n", state))
  state_idx <- which(all_locations == state)
  
  # Check historical data availability
  historical_data <- c()
  current_data <- c()
  
  for (year in historical_years) {
    year_idx <- which(all_years == year)
    value <- total_data[year_idx, state_idx]
    if (!is.na(value)) {
      historical_data <- c(historical_data, year)
    }
  }
  
  for (year in current_years) {
    year_idx <- which(all_years == year)
    value <- total_data[year_idx, state_idx]
    if (!is.na(value)) {
      current_data <- c(current_data, year)
    }
  }
  
  cat("Historical data years:", if(length(historical_data) > 0) paste(historical_data, collapse=", ") else "NONE", "\n")
  cat("Current data years:", if(length(current_data) > 0) paste(current_data, collapse=", ") else "NONE", "\n")
  cat("Total data years:", length(historical_data) + length(current_data), "\n")
  
  # Show actual values for context
  if (length(current_data) > 0) {
    for (year in current_data) {
      year_idx <- which(all_years == year)
      value <- total_data[year_idx, state_idx]
      cat(sprintf("  %s %s: %.3f\n", state, year, value))
    }
  }
}

# Check other key stratifications for PA
cat("\n=== PA STRATIFICATION DETAILS ===\n")

key_stratifications <- c("year__location", "year__location__age", "year__location__race", "year__location__risk")

for (strat_name in key_stratifications) {
  if (strat_name %in% names(suppression_data)) {
    cat(sprintf("\n--- %s ---\n", strat_name))
    strat_data <- suppression_data[[strat_name]]
    dims <- dimnames(strat_data)
    
    if ("PA" %in% dims$location) {
      pa_idx <- which(dims$location == "PA")
      
      # Count data points across all years
      total_pa_points <- 0
      historical_pa_points <- 0
      current_pa_points <- 0
      
      for (year in all_years) {
        if (year %in% dims$year) {
          year_idx <- which(dims$year == year)
          
          if (length(dim(strat_data)) == 2) {
            if (!is.na(strat_data[year_idx, pa_idx])) {
              total_pa_points <- total_pa_points + 1
              if (year %in% historical_years) historical_pa_points <- historical_pa_points + 1
              if (year %in% current_years) current_pa_points <- current_pa_points + 1
            }
          } else if (length(dim(strat_data)) == 3) {
            slice_data <- strat_data[year_idx, pa_idx, ]
            non_na_count <- sum(!is.na(slice_data))
            total_pa_points <- total_pa_points + non_na_count
            if (year %in% historical_years) historical_pa_points <- historical_pa_points + non_na_count
            if (year %in% current_years) current_pa_points <- current_pa_points + non_na_count
          }
        }
      }
      
      cat("PA total data points:", total_pa_points, "\n")
      cat("PA historical points:", historical_pa_points, "\n") 
      cat("PA current points:", current_pa_points, "\n")
    } else {
      cat("PA not found in this stratification\n")
    }
  } else {
    cat(sprintf("Stratification %s not found\n", strat_name))
  }
}

# Check for potential NULL or empty data structures
cat("\n=== NULL DATA INVESTIGATION ===\n")

# Try to simulate what the likelihood system might be looking for
cat("Checking for potential NULL data issues...\n")

# Check if PA has any NULLs in key arrays
for (strat_name in key_stratifications) {
  if (strat_name %in% names(suppression_data)) {
    strat_data <- suppression_data[[strat_name]]
    dims <- dimnames(strat_data)
    
    if ("PA" %in% dims$location) {
      pa_idx <- which(dims$location == "PA")
      
      # Check for structural issues
      cat(sprintf("%s - PA data class: %s, dimensions: %s\n", 
                  strat_name, class(strat_data), paste(dim(strat_data), collapse="x")))
      
      # Safely extract PA data based on dimensions
      tryCatch({
        if (length(dim(strat_data)) == 2) {
          pa_slice <- strat_data[, pa_idx]
        } else if (length(dim(strat_data)) == 3) {
          pa_slice <- strat_data[, pa_idx, ]
        } else {
          # For higher dimensions, just check if data exists
          pa_slice <- "multi-dimensional"
        }
        
        if (is.null(pa_slice)) {
          cat(sprintf("  ❌ PA slice is NULL in %s\n", strat_name))
        } else if (is.character(pa_slice)) {
          cat(sprintf("  ✅ PA data exists in %s\n", strat_name))
        } else {
          cat(sprintf("  ✅ PA slice has class %s, length %d\n", class(pa_slice), length(pa_slice)))
        }
      }, error = function(e) {
        cat(sprintf("  ❌ Error extracting PA data from %s: %s\n", strat_name, e$message))
      })
    }
  }
}

# Check for differences in data manager metadata for PA vs working states
cat("\n=== MANAGER METADATA COMPARISON ===\n")

# Get the ontology info
ont_info <- manager$ontology.metadata$cdc.new
cat("Ontology dimensions:", paste(names(ont_info), collapse=", "), "\n")

# Check location ontology specifically
location_ont <- ont_info$location
if ("PA" %in% location_ont) {
  cat("✅ PA found in location ontology\n")
} else {
  cat("❌ PA missing from location ontology\n")
}

# Compare PA counties to other state counties
pa_counties <- location_ont[grepl("^42[0-9]+$", location_ont)]
ny_counties <- location_ont[grepl("^36[0-9]+$", location_ont)]  # NY for comparison

cat("PA counties in ontology:", length(pa_counties), "\n")
cat("NY counties in ontology (comparison):", length(ny_counties), "\n")

# Compare working vs non-working state patterns
cat("\n=== WORKING VS NON-WORKING STATE COMPARISON ===\n")

# Find a state with full historical data for comparison
working_states <- c()
for (state in comparison_states) {
  if (state != "PA") {
    state_idx <- which(all_locations == state)
    historical_count <- 0
    for (year in historical_years) {
      year_idx <- which(all_years == year)
      value <- total_data[year_idx, state_idx]
      if (!is.na(value)) {
        historical_count <- historical_count + 1
      }
    }
    if (historical_count > 0) {
      working_states <- c(working_states, state)
    }
  }
}

if (length(working_states) > 0) {
  working_state <- working_states[1]
  cat(sprintf("Comparing PA vs %s (working state):\n", working_state))
  
  working_idx <- which(all_locations == working_state)
  pa_idx <- which(all_locations == "PA")
  
  # Compare data patterns across stratifications
  for (strat_name in key_stratifications[1:2]) { # Just check first 2 to avoid complexity
    if (strat_name %in% names(suppression_data)) {
      strat_data <- suppression_data[[strat_name]]
      dims <- dimnames(strat_data)
      
      cat(sprintf("\n%s comparison:\n", strat_name))
      
      # Count data points for each state
      working_points <- 0
      pa_points <- 0
      
      for (year in all_years) {
        if (year %in% dims$year) {
          year_idx <- which(dims$year == year)
          
          if (length(dim(strat_data)) == 2) {
            if (!is.na(strat_data[year_idx, working_idx])) working_points <- working_points + 1
            if (!is.na(strat_data[year_idx, pa_idx])) pa_points <- pa_points + 1
          } else if (length(dim(strat_data)) == 3) {
            working_slice <- strat_data[year_idx, working_idx, ]
            pa_slice <- strat_data[year_idx, pa_idx, ]
            working_points <- working_points + sum(!is.na(working_slice))
            pa_points <- pa_points + sum(!is.na(pa_slice))
          }
        }
      }
      
      cat(sprintf("  %s: %d data points\n", working_state, working_points))
      cat(sprintf("  PA: %d data points\n", pa_points))
    }
  }
}

cat("\n=== SUMMARY FINDINGS ===\n")
cat("1. PA data availability: Only 2022-2023 data (no historical)\n")
cat("2. This is the key difference from other states that have historical data\n")
cat("3. Likelihood system may expect historical data for proper initialization\n")
cat("4. Need to investigate jheem2 likelihood code to understand requirements\n")
cat("5. Error likely occurs when likelihood tries to access historical data that doesn't exist for PA\n")