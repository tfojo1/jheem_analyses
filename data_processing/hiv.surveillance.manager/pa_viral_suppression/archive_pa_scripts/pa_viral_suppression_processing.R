# Pennsylvania Viral Suppression Data Processing Script
# Clean and format data for surveillance manager input
# Part 2: After raw data has been pulled from Atlas Plus

# Required libraries
library(dplyr)
library(stringr)

# Set working directory
setwd("/Users/nicholas/Documents/jheem/code/jheem_analyses")

# Source the surveillance manager (adjust path as needed)
# source("commoncode/load_surveillance_manager.R")  # Adjust this path

# Load the raw data file (update filename with actual timestamp)
# raw_data_file <- "data_raw/hiv_surveillance/PA_viral_suppression_YYYYMMDD_HHMMSS.csv"
# raw_data <- read.csv(raw_data_file)  # Uncomment when you have the actual file

# === DATA CLEANING AND FORMATTING ===

# Based on the existing atlas_plus_hiv_processing.R patterns
process_pa_suppression <- function(raw_data) {
  
  # Define mappings consistent with existing processing
  age.mappings <- c('13-24' = '13-24 years',
                   '25-34' = '25-34 years',
                   '35-44' = '35-44 years',
                   '45-54' = '45-54 years',
                   '55+' = '55+ years')
  
  processed_data <- raw_data %>%
    # Set outcome
    mutate(outcome = "suppression") %>%
    
    # Clean year 
    mutate(year = as.character(Year)) %>%
    
    # Set location (PA state code)
    mutate(location = "PA") %>%
    
    # Process percentage data (following SLE pattern from existing code)
    mutate(
      # Handle suppression indicators
      Percentage = case_when(
        `Data Status` == "Data suppressed" ~ NA_real_,
        `Data Status` == "Data not available" ~ NA_real_,
        TRUE ~ as.numeric(Percentage)
      ),
      # Convert percentage to proportion (divide by 100)
      value = case_when(
        is.na(Percentage) ~ NA_real_,
        TRUE ~ Percentage / 100
      )
    ) %>%
    
    # Process age groups
    mutate(
      age = case_when(
        `Age Group` == "All Ages" ~ NA_character_,
        TRUE ~ age.mappings[`Age Group`]
      )
    ) %>%
    
    # Process race/ethnicity
    mutate(
      race = case_when(
        `Race/Ethnicity` == "All Races" ~ NA_character_,
        `Race/Ethnicity` == "Black" ~ "black",
        `Race/Ethnicity` == "Hispanic" ~ "hispanic", 
        `Race/Ethnicity` == "White" ~ "white",
        `Race/Ethnicity` == "American Indian/Alaska Native" ~ "american indian/alaska native",
        `Race/Ethnicity` == "Asian" ~ "asian",
        TRUE ~ tolower(`Race/Ethnicity`)
      )
    ) %>%
    
    # Process sex
    mutate(
      sex = case_when(
        Sex == "Both sexes" ~ NA_character_,
        Sex == "Male" ~ "male",
        Sex == "Female" ~ "female",
        TRUE ~ tolower(Sex)
      )
    ) %>%
    
    # Select relevant columns for data manager
    select(year, location, outcome, value, age, race, sex) %>%
    
    # Remove rows where value is NA
    filter(!is.na(value)) %>%
    
    # Remove rows where all stratification columns are NA (this would be total)
    # but keep rows where some stratification columns are NA
    filter(!(is.na(age) & is.na(race) & is.na(sex))) %>%
    
    # Remove any duplicate rows
    distinct()
  
  return(processed_data)
}

# === SURVEILLANCE MANAGER INTEGRATION ===

# Function to put data into surveillance manager
put_pa_suppression_data <- function(processed_data, data_manager) {
  
  # Check if outcome is registered (it should be based on existing code)
  # If not, you may need to register it first
  
  # Split data by stratification type for cleaner putting
  
  # Total level data (no stratifications)
  total_data <- processed_data %>%
    filter(is.na(age) & is.na(race) & is.na(sex)) %>%
    select(year, location, outcome, value)
  
  # Age-only stratification
  age_data <- processed_data %>%
    filter(!is.na(age) & is.na(race) & is.na(sex)) %>%
    select(year, location, outcome, value, age)
  
  # Race-only stratification  
  race_data <- processed_data %>%
    filter(is.na(age) & !is.na(race) & is.na(sex)) %>%
    select(year, location, outcome, value, race)
  
  # Sex-only stratification
  sex_data <- processed_data %>%
    filter(is.na(age) & is.na(race) & !is.na(sex)) %>%
    select(year, location, outcome, value, sex)
  
  # Two-way stratifications
  age_race_data <- processed_data %>%
    filter(!is.na(age) & !is.na(race) & is.na(sex)) %>%
    select(year, location, outcome, value, age, race)
  
  age_sex_data <- processed_data %>%
    filter(!is.na(age) & is.na(race) & !is.na(sex)) %>%
    select(year, location, outcome, value, age, sex)
  
  race_sex_data <- processed_data %>%
    filter(is.na(age) & !is.na(race) & !is.na(sex)) %>%
    select(year, location, outcome, value, race, sex)
  
  # Put each dataset into the manager
  datasets <- list(
    "total" = total_data,
    "age" = age_data,
    "race" = race_data,
    "sex" = sex_data,
    "age_race" = age_race_data,
    "age_sex" = age_sex_data,
    "race_sex" = race_sex_data
  )
  
  for (dataset_name in names(datasets)) {
    dataset <- datasets[[dataset_name]]
    
    if (nrow(dataset) > 0) {
      cat("Putting", dataset_name, "data:", nrow(dataset), "records\n")
      
      tryCatch({
        data_manager$put.long.form(
          data = dataset,
          ontology.name = 'cdc',
          source = 'cdc.hiv',
          dimension.values = list(),
          url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
          details = 'Pennsylvania viral suppression data 2022-2023 from CDC Atlas Plus'
        )
        cat("Success:", dataset_name, "data added to manager\n")
      }, error = function(e) {
        cat("Error putting", dataset_name, "data:", e$message, "\n")
      })
    }
  }
}

# === VALIDATION AND SUMMARY ===

# Function to validate processed data
validate_processed_data <- function(processed_data) {
  cat("=== DATA VALIDATION ===\n")
  cat("Total records:", nrow(processed_data), "\n")
  cat("Years:", paste(unique(processed_data$year), collapse = ", "), "\n")
  cat("Locations:", paste(unique(processed_data$location), collapse = ", "), "\n")
  cat("Outcomes:", paste(unique(processed_data$outcome), collapse = ", "), "\n")
  
  # Check value ranges
  cat("Value range:", min(processed_data$value, na.rm = TRUE), "to", 
      max(processed_data$value, na.rm = TRUE), "\n")
  
  # Check for missing values
  cat("Missing values:", sum(is.na(processed_data$value)), "\n")
  
  # Stratification breakdown
  cat("\nStratification breakdown:\n")
  cat("Records with age:", sum(!is.na(processed_data$age)), "\n")
  cat("Records with race:", sum(!is.na(processed_data$race)), "\n")
  cat("Records with sex:", sum(!is.na(processed_data$sex)), "\n")
  
  # Show unique values
  cat("\nUnique age groups:", paste(unique(processed_data$age[!is.na(processed_data$age)]), collapse = ", "), "\n")
  cat("Unique races:", paste(unique(processed_data$race[!is.na(processed_data$race)]), collapse = ", "), "\n")
  cat("Unique sex:", paste(unique(processed_data$sex[!is.na(processed_data$sex)]), collapse = ", "), "\n")
}

# === EXECUTION TEMPLATE ===

# When ready to execute:
# 1. Update the raw_data_file path with actual timestamp
# 2. Load the raw data
# 3. Process the data
# 4. Validate the processed data
# 5. Load the surveillance manager
# 6. Put the data into the manager
# 7. Save the updated manager

cat("=== EXECUTION TEMPLATE ===\n")
cat("# Step 1: Load raw data\n")
cat("raw_data <- read.csv('data_raw/hiv_surveillance/PA_viral_suppression_TIMESTAMP.csv')\n")
cat("\n# Step 2: Process data\n")
cat("processed_data <- process_pa_suppression(raw_data)\n")
cat("\n# Step 3: Validate data\n")
cat("validate_processed_data(processed_data)\n")
cat("\n# Step 4: Load surveillance manager\n")
cat("data.manager <- load.data.manager('path/to/surveillance.manager.rdata')\n")
cat("\n# Step 5: Put data\n")
cat("put_pa_suppression_data(processed_data, data.manager)\n")
cat("\n# Step 6: Save updated manager\n")
cat("save(data.manager, file='path/to/updated_surveillance.manager.rdata')\n")
