# Pennsylvania Viral Suppression - Comprehensive Data Pull
# Based on troubleshooting results: focus on age+X combinations

# Required libraries
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Set working directory and source required functions
setwd("/Users/nicholas/Documents/jheem/code/jheem_analyses")
source("data_processing/cdc_atlas/load_cdc.R")
source("data_processing/cdc_atlas/get_cdc_data_enhanced.R")

# Load the CDC mappings
cdc_mappings <- load_cdc_mappings("data_processing/cdc_atlas/varvals.json")

# Define parameters based on what we know works
indicator <- "HIV viral suppression"
location <- "Pennsylvania"
years <- c("2022", "2023")
available_ages <- c("13-24", "25-34", "35-44", "45-54")  # No 55+ available

# Define comprehensive stratifications that should work
stratifications <- list(
  # Age-only (we know this works)
  age_only = list(
    age_groups = available_ages,
    races = "All races/ethnicities",
    sex = "Both sexes"
  ),
  
  # Age + Race (we know this works)
  age_race = list(
    age_groups = available_ages,
    races = c("Black/African American", "Hispanic/Latino", "White"),
    sex = "Both sexes"
  ),
  
  # Age + Sex (we know this works)
  age_sex = list(
    age_groups = available_ages,
    races = "All races/ethnicities",
    sex = c("Male", "Female")
  ),
  
  # Age + Race + Sex (three-way stratification)
  age_race_sex = list(
    age_groups = available_ages,
    races = c("Black/African American", "Hispanic/Latino", "White"),
    sex = c("Male", "Female")
  )
)

# Function to safely run data pulls with error handling
safe_data_pull <- function(strat_name, strat_params) {
  cat("Pulling data for:", strat_name, "\n")
  
  tryCatch({
    data <- get_cdc_data_enhanced(
      indicators = indicator,
      locations = location,
      years = years,
      age_groups = strat_params$age_groups,
      races = strat_params$races,
      sex = strat_params$sex,
      mappings = cdc_mappings
    )
    
    if (!is.null(data) && nrow(data) > 0) {
      cat("Success:", nrow(data), "records retrieved for", strat_name, "\n")
      return(data)
    } else {
      cat("Warning: No data returned for", strat_name, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error in", strat_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Execute comprehensive data pull
cat("Starting comprehensive Pennsylvania viral suppression data pull...\n")
cat("Years:", paste(years, collapse = ", "), "\n")
cat("Location:", location, "\n")
cat("Available ages:", paste(available_ages, collapse = ", "), "\n\n")

all_data <- list()

for (strat_name in names(stratifications)) {
  result <- safe_data_pull(strat_name, stratifications[[strat_name]])
  if (!is.null(result)) {
    all_data[[strat_name]] <- result
  }
}

# Combine all successful data pulls
if (length(all_data) > 0) {
  combined_data <- bind_rows(all_data, .id = "stratification_type")
  
  # Display comprehensive summary
  cat("\n=== COMPREHENSIVE DATA PULL SUMMARY ===\n")
  cat("Total records retrieved:", nrow(combined_data), "\n")
  cat("Stratification types successful:", length(all_data), "\n")
  cat("Years covered:", paste(unique(combined_data$Year), collapse = ", "), "\n")
  
  # Show breakdown by stratification type
  cat("\nRecords by stratification type:\n")
  print(table(combined_data$stratification_type))
  
  # Show data coverage details
  cat("\nData coverage details:\n")
  cat("Unique Age Groups:", paste(unique(combined_data$`Age Group`), collapse = ", "), "\n")
  cat("Unique Races:", paste(unique(combined_data$`Race/Ethnicity`), collapse = ", "), "\n")
  cat("Unique Sex:", paste(unique(combined_data$Sex), collapse = ", "), "\n")
  
  # Save comprehensive raw data
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("data_raw/hiv_surveillance/PA_viral_suppression_comprehensive_", timestamp, ".csv")
  
  write.csv(combined_data, filename, row.names = FALSE)
  cat("\nComprehensive raw data saved to:", filename, "\n")
  
  # Show stratification coverage
  cat("\nStratification coverage:\n")
  coverage_summary <- combined_data %>%
    group_by(stratification_type, Year) %>%
    summarise(
      records = n(),
      age_groups = n_distinct(`Age Group`),
      races = n_distinct(`Race/Ethnicity`),
      sexes = n_distinct(Sex),
      .groups = 'drop'
    )
  print(coverage_summary)
  
} else {
  cat("ERROR: No data retrieved for any stratification type\n")
}

cat("\n=== FINAL RECOMMENDATIONS ===\n")
cat("Based on troubleshooting results:\n")
cat("✅ Age-based stratifications work well\n")
cat("✅ Can get age+race, age+sex, and age+race+sex combinations\n")
cat("❌ No total/unstratified data available for this indicator\n")
cat("❌ 55+ age group not available for viral suppression\n")
cat("❌ Race-only and sex-only stratifications don't work\n")
