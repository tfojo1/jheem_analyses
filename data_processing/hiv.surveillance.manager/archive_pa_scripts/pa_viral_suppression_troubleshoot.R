# Pennsylvania Viral Suppression - Troubleshoot Missing Stratifications
# Test individual stratifications to understand data availability

# Required libraries
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Set working directory and source required functions
setwd("/Users/nicholas/Documents/jheem/code/jheem_analyses")
source("data_processing/cdc_atlas/load_cdc.R")
source("data_processing/cdc_atlas/get_cdc_data_enhanced.R")

# Load the updated CDC mappings
cdc_mappings <- load_cdc_mappings("data_processing/cdc_atlas/varvals.json")

# Test individual calls for missing patterns
tests <- list(
  # Test 1: Check if "55+" age is available
  test_55_plus = list(
    age_groups = "55+",
    races = "All races/ethnicities",
    sex = "Both sexes"
  ),
  
  # Test 2: Check total level data (no stratifications)
  test_total = list(
    age_groups = "All age groups",
    races = "All races/ethnicities", 
    sex = "Both sexes"
  ),
  
  # Test 3: Check sex-only stratification
  test_sex_simple = list(
    age_groups = "All age groups",
    races = "All races/ethnicities",
    sex = "Male"
  ),
  
  # Test 4: Check race-only stratification 
  test_race_simple = list(
    age_groups = "All age groups",
    races = "Black/African American",
    sex = "Both sexes"
  ),
  
  # Test 5: Try different sex terms
  test_sex_alternative = list(
    age_groups = "All age groups", 
    races = "All races/ethnicities",
    sex = "All gender identities"
  ),
  
  # Test 6: Try a simple age-sex combination
  test_age_sex_simple = list(
    age_groups = "25-34",
    races = "All races/ethnicities",
    sex = "Male"
  )
)

# Function to test individual combinations
test_stratification <- function(test_name, params) {
  cat("\n=== TESTING:", test_name, "===\n")
  cat("Parameters:", 
      "Age:", params$age_groups, 
      "| Race:", params$races, 
      "| Sex:", params$sex, "\n")
  
  tryCatch({
    data <- get_cdc_data_enhanced(
      indicators = "HIV viral suppression",
      locations = "Pennsylvania",
      years = "2022",  # Just test one year for speed
      age_groups = params$age_groups,
      races = params$races,
      sex = params$sex,
      mappings = cdc_mappings
    )
    
    if (!is.null(data) && nrow(data) > 0) {
      cat("✅ SUCCESS:", nrow(data), "records\n")
      cat("Sample data:\n")
      print(head(data[, c("Age Group", "Race/Ethnicity", "Sex", "Cases")], 3))
      return(TRUE)
    } else {
      cat("❌ NO DATA returned\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n")
    return(FALSE)
  })
}

# Run all tests
cat("=== TROUBLESHOOTING PENNSYLVANIA VIRAL SUPPRESSION DATA ===\n")
cat("Testing individual stratifications to understand data availability\n")

results <- list()
for (test_name in names(tests)) {
  results[[test_name]] <- test_stratification(test_name, tests[[test_name]])
  Sys.sleep(1)  # Brief pause between requests
}

# Summary of results
cat("\n=== TEST SUMMARY ===\n")
for (test_name in names(results)) {
  status <- if(results[[test_name]]) "✅ PASS" else "❌ FAIL"
  cat(test_name, ":", status, "\n")
}

# Save test results
test_summary <- data.frame(
  test = names(results),
  success = unlist(results),
  stringsAsFactors = FALSE
)

write.csv(test_summary, "data_raw/hiv_surveillance/PA_suppression_tests.csv", row.names = FALSE)
cat("\nTest results saved to: data_raw/hiv_surveillance/PA_suppression_tests.csv\n")
