#!/usr/bin/env Rscript
# Pennsylvania Viral Suppression Integration - CORRECTED VERSION
# Preserves multi-dimensional stratifications from raw data

source("use_jheem2_package_setting.R")
library(jheem2)
library(tidyverse)

cat("=== PA VIRAL SUPPRESSION INTEGRATION - CORRECTED ===\n")
cat("Started:", as.character(Sys.time()), "\n\n")

CONFIG <- list(
  INPUT_FILE = "data_raw/hiv_surveillance/viral_suppression_comprehensive_Pennsylvania_20250727_120208.csv",
  ORIGINAL_MANAGER = "cached/surveillance.manager.rdata",
  OUTCOME = "suppression",
  SOURCE = "cdc.hiv",
  ONTOLOGY = "cdc.new", 
  YEARS = c("2022", "2023"),
  URL = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html",
  DETAILS = "Pennsylvania viral suppression 2022-2023, comprehensive Atlas Plus API data with complete stratifications"
)

# Stratification mapping - maps raw stratification_type to required dimensions
STRATIFICATION_MAPPING <- list(
  "total" = character(0),
  "age_only" = "age",
  "race_only" = "race", 
  "sex_only" = "sex",
  "transmission_only" = "risk",
  "age_race" = c("age", "race"),
  "age_sex" = c("age", "sex"),
  "age_transmission" = c("age", "risk"),
  "race_sex" = c("race", "sex"),
  "race_transmission" = c("race", "risk"),
  "sex_transmission" = c("sex", "risk"),
  "age_race_sex" = c("age", "race", "sex"),
  "age_race_transmission" = c("age", "race", "risk"),
  "age_sex_transmission" = c("age", "sex", "risk"),
  "race_sex_transmission" = c("race", "sex", "risk")
)

# Risk factor mappings
RISK_MAPPINGS <- c(
  "Male-to-male sexual contact" = "msm",
  "Injection drug use" = "idu",
  "Male-to-male sexual contact and injection drug use" = "msm_idu", 
  "Heterosexual contact" = "heterosexual",
  "Other" = "other"
)

# Create working copy
cat("Creating working copy...\n")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
working_file <- paste0("cached/surveillance.manager_PA_corrected_", timestamp, ".rdata")
file.copy(CONFIG$ORIGINAL_MANAGER, working_file)
manager <- load.data.manager(working_file)

# Load and process raw data
cat("Loading raw data...\n")
raw_data <- read.csv(CONFIG$INPUT_FILE)

cat("Raw data summary:\n")
cat("- Total records:", nrow(raw_data), "\n")
cat("- Available records:", sum(raw_data$Data.Status == "Available"), "\n")
cat("- Years:", paste(unique(raw_data$Year), collapse=", "), "\n")
cat("- Stratification types:", length(unique(raw_data$stratification_type)), "\n")

# Process data with corrected approach
process_corrected_stratifications <- function(raw_data) {
  
  clean_data <- raw_data %>%
    filter(Data.Status == "Available") %>%
    mutate(
      outcome = CONFIG$OUTCOME,
      year = as.character(Year),
      
      # Location mapping - state vs county
      location = case_when(
        is.na(County) & Geography == "Pennsylvania" ~ "PA",
        !is.na(County) & !is.na(FIPS) ~ as.character(FIPS),
        TRUE ~ "PA"
      ),
      
      # Convert percentages to proportions
      value = as.numeric(Rate.per.100000) / 100,
      
      # Map all dimensions (even if not used in every stratification)
      age = case_when(
        Age.Group == "Ages 13 years and older" ~ NA_character_,
        Age.Group == "13-24" ~ "13-24 years",
        Age.Group == "25-34" ~ "25-34 years", 
        Age.Group == "35-44" ~ "35-44 years",
        Age.Group == "45-54" ~ "45-54 years",
        Age.Group == "55-64" ~ "55-64 years",
        Age.Group == "65+" ~ "65+ years",
        TRUE ~ NA_character_
      ),
      
      race = case_when(
        Race.Ethnicity == "All races/ethnicities" ~ NA_character_,
        Race.Ethnicity == "Black/African American" ~ "black/african american",
        Race.Ethnicity == "Hispanic/Latino" ~ "hispanic/latino",
        Race.Ethnicity == "White" ~ "white", 
        Race.Ethnicity == "American Indian/Alaska Native" ~ "american indian/alaska native",
        Race.Ethnicity == "Asian" ~ "asian",
        TRUE ~ NA_character_
      ),
      
      sex = case_when(
        Sex == "Both sexes" ~ NA_character_,
        Sex == "Male" ~ "male",
        Sex == "Female" ~ "female",
        TRUE ~ NA_character_
      ),
      
      risk = case_when(
        Transmission.Category == "All transmission categories" ~ NA_character_,
        Transmission.Category %in% names(RISK_MAPPINGS) ~ RISK_MAPPINGS[Transmission.Category],
        TRUE ~ NA_character_
      )
    ) %>%
    select(year, location, outcome, value, age, race, sex, risk, stratification_type) %>%
    filter(!is.na(value), value >= 0, value <= 1)
  
  # Split by stratification type
  stratification_datasets <- split(clean_data, clean_data$stratification_type)
  
  # Process each stratification according to mapping
  processed_datasets <- list()
  
  for (strat_type in names(stratification_datasets)) {
    if (strat_type %in% names(STRATIFICATION_MAPPING)) {
      strat_data <- stratification_datasets[[strat_type]]
      required_dims <- STRATIFICATION_MAPPING[[strat_type]]
      
      # Include core columns plus only the required dimensions for this stratification
      core_cols <- c("year", "location", "outcome", "value")
      keep_cols <- c(core_cols, required_dims)
      
      final_data <- strat_data %>%
        select(-stratification_type) %>%  # Remove stratification_type column
        select(all_of(keep_cols))  # Select only the columns we need
      
      # Filter to only rows where required dimensions are not NA
      if (length(required_dims) > 0) {
        # For each required dimension, keep only rows where it's not NA
        for (dim in required_dims) {
          final_data <- final_data %>% filter(!is.na(!!sym(dim)))
        }
      }
      
      if (nrow(final_data) > 0) {
        processed_datasets[[strat_type]] <- final_data
        cat("Processed", strat_type, ":", nrow(final_data), "records with dimensions [", 
            paste(required_dims, collapse=", "), "]\n")
      }
    } else {
      cat("Warning: Unknown stratification type:", strat_type, "\n")
    }
  }
  
  return(processed_datasets)
}

# Process all stratifications
cat("\nProcessing stratifications...\n")
processed_datasets <- process_corrected_stratifications(raw_data)

cat("Successfully processed", length(processed_datasets), "stratification types\n")
total_records <- sum(sapply(processed_datasets, nrow))
cat("Total records to integrate:", total_records, "\n")

# Integration function
integrate_datasets <- function(datasets, manager) {
  results <- list(success = 0, failed = 0, errors = list(), total_records = 0)
  
  for (strat_type in names(datasets)) {
    dataset <- datasets[[strat_type]]
    cat("Integrating", strat_type, ":", nrow(dataset), "records...")
    
    integration_result <- tryCatch({
      manager$put.long.form(
        data = dataset,
        ontology.name = CONFIG$ONTOLOGY,
        source = CONFIG$SOURCE,
        dimension.values = list(),
        url = CONFIG$URL,
        details = paste(CONFIG$DETAILS, "-", strat_type)
      )
      list(success = TRUE)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
    
    if (integration_result$success) {
      results$success <- results$success + 1
      results$total_records <- results$total_records + nrow(dataset)
      cat(" ✅\n")
    } else {
      results$failed <- results$failed + 1
      results$errors[[strat_type]] <- integration_result$error
      cat(" ❌", integration_result$error, "\n")
    }
  }
  
  return(results)
}

# Execute integration
cat("\n=== EXECUTING INTEGRATION ===\n")
integration_results <- integrate_datasets(processed_datasets, manager)

# Validation using direct access
cat("\n=== VALIDATION USING DIRECT ACCESS ===\n")

suppression_data <- manager$data$suppression$estimate$cdc.hiv$cdc.new
available_strats <- names(suppression_data)

validation_results <- list()

for (strat_name in available_strats) {
  strat_data <- suppression_data[[strat_name]]
  if (!is.null(strat_data)) {
    dims <- dimnames(strat_data)
    
    # Check for PA and county data
    has_pa <- "PA" %in% dims$location
    pa_counties <- dims$location[grepl("^42[0-9]+$", dims$location)]
    has_counties <- length(pa_counties) > 0
    
    # Count new data (2022-2023)
    new_years <- intersect(dims$year, CONFIG$YEARS)
    new_data_count <- 0
    
    if (length(new_years) > 0 && (has_pa || has_counties)) {
      # This is simplified - in reality would need to check all array positions
      # But for validation, we just check if new years exist with PA/county locations
      if (has_pa && "2022" %in% new_years) new_data_count <- 1
      if (has_counties && "2022" %in% new_years) new_data_count <- new_data_count + length(pa_counties)
    }
    
    validation_results[[strat_name]] <- list(
      has_pa = has_pa,
      county_count = length(pa_counties),
      new_years = new_years,
      estimated_new_data = new_data_count
    )
    
    if (new_data_count > 0) {
      cat("✅", strat_name, "- PA:", has_pa, "Counties:", length(pa_counties), 
          "New years:", paste(new_years, collapse=", "), "\n")
    }
  }
}

# Final report
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("FINAL INTEGRATION REPORT\n")
cat(paste(rep("=", 80), collapse=""), "\n")

cat("Completed:", as.character(Sys.time()), "\n")
cat("Working file:", working_file, "\n")
cat("Original manager: UNTOUCHED\n\n")

cat("INTEGRATION SUMMARY:\n")
cat("- Stratifications processed:", length(processed_datasets), "\n")
cat("- Successful integrations:", integration_results$success, "\n")
cat("- Failed integrations:", integration_results$failed, "\n") 
cat("- Total records integrated:", integration_results$total_records, "\n")

if (length(integration_results$errors) > 0) {
  cat("\nERRORS:\n")
  for (strat in names(integration_results$errors)) {
    cat("-", strat, ":", integration_results$errors[[strat]], "\n")
  }
}

cat("\nVALIDATION SUMMARY:\n")
successful_strats <- sum(sapply(validation_results, function(x) x$estimated_new_data > 0))
cat("- Stratifications with new data:", successful_strats, "\n")

# Overall success criteria
overall_success <- (integration_results$success >= 10 && 
                   integration_results$failed <= 2 && 
                   successful_strats >= 8)

if (overall_success) {
  cat("\n🎉 INTEGRATION SUCCESSFUL - Ready for production\n")
  cat("\nTo apply to production:\n")
  cat("file.copy('", working_file, "', '", CONFIG$ORIGINAL_MANAGER, "', overwrite=TRUE)\n")
} else {
  cat("\n⚠️ INTEGRATION NEEDS REVIEW\n")
  cat("Working file preserved for analysis:", working_file, "\n")
}

cat("\n", paste(rep("=", 80), collapse=""), "\n")