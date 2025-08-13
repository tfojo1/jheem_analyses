# Pennsylvania Viral Suppression Data Pull for Surveillance Manager
# Script to pull 2022-2023 viral suppression data from Atlas Plus
# Using enhanced Atlas Plus API script

# Required libraries
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Set working directory and source required functions
setwd("/Users/nicholas/Documents/jheem/code/jheem_analyses")
source("data_processing/cdc_atlas/load_cdc.R")
source("data_processing/cdc_atlas/get_cdc_data_enhanced.R")

# Load the CDC mappings from varvals.json
cdc_mappings <- load_cdc_mappings("data_processing/cdc_atlas/varvals.json")

# Define data pull parameters
# Based on your request: Pennsylvania, 2022-2023, all available stratifications

# Base parameters
indicator <- "HIV viral suppression"
location <- "Pennsylvania"
years <- c("2022", "2023")

# Define stratification combinations
# 1. Total level (no additional stratifications)
# 2. One-way stratifications (age, race, sex, risk)
# 3. Two-way stratifications (all combinations)

stratifications <- list(
  # Total level - NOW WORKS!
  total = list(
    age_groups = "Ages 13 years and older",
    races = "All races/ethnicities",
    sex = "Both sexes"
  ),
  
  # One-way stratifications
  age_only = list(
    age_groups = c("13-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    races = "All races/ethnicities", 
    sex = "Both sexes"
  ),
  
  race_only = list(
    age_groups = "Ages 13 years and older",
    races = c("Black/African American", "Hispanic/Latino", "White", "American Indian/Alaska Native", "Asian"),
    sex = "Both sexes"
  ),
  
  sex_only = list(
    age_groups = "Ages 13 years and older", 
    races = "All races/ethnicities",
    sex = c("Male", "Female")
  ),
  
  # Two-way stratifications
  age_race = list(
    age_groups = c("13-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    races = c("Black/African American", "Hispanic/Latino", "White"),
    sex = "Both sexes"
  ),
  
  age_sex = list(
    age_groups = c("13-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    races = "All races/ethnicities",
    sex = c("Male", "Female")
  ),
  
  race_sex = list(
    age_groups = "Ages 13 years and older",
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

# Execute data pulls for all stratifications
cat("Starting Pennsylvania viral suppression data pull...\n")
cat("Years:", paste(years, collapse = ", "), "\n")
cat("Location:", location, "\n")
cat("Indicator:", indicator, "\n\n")

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
  
  # Display summary
  cat("\n=== DATA PULL SUMMARY ===\n")
  cat("Total records retrieved:", nrow(combined_data), "\n")
  cat("Stratification types successful:", length(all_data), "\n")
  cat("Years covered:", paste(unique(combined_data$Year), collapse = ", "), "\n")
  
  # Show breakdown by stratification type
  cat("\nRecords by stratification type:\n")
  print(table(combined_data$stratification_type))
  
  # Show unique age groups, races, and sex values
  cat("\nUnique Age Groups:", paste(unique(combined_data$`Age Group`), collapse = ", "), "\n")
  cat("Unique Races:", paste(unique(combined_data$`Race/Ethnicity`), collapse = ", "), "\n")
  cat("Unique Sex:", paste(unique(combined_data$Sex), collapse = ", "), "\n")
  
  # Save raw data to local data_raw directory
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("data_raw/hiv_surveillance/PA_viral_suppression_", timestamp, ".csv")
  
  write.csv(combined_data, filename, row.names = FALSE)
  cat("\nRaw data saved to:", filename, "\n")
  
  # Preview of data structure
  cat("\nFirst few rows of data:\n")
  print(head(combined_data))
  
} else {
  cat("ERROR: No data retrieved for any stratification type\n")
}

# Next steps message
cat("\n=== NEXT STEPS ===\n")
cat("1. Review the raw data file in Q drive\n")
cat("2. Create processing script to clean and format for data manager\n")
cat("3. Load surveillance manager and put data using put.long.form()\n")
cat("4. Use source = 'cdc.hiv' and ontology.name = 'cdc'\n")
cat("5. Set outcome = 'suppression' and convert percentages to proportions\n")
cat("6. Update manager cache and documentation\n")
