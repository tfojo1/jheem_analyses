#!/usr/bin/env Rscript
# Pennsylvania Viral Suppression Integration - CORRECTED FOR 5 AGE GROUPS
# Aligns with JHEEM standard processing pattern (cdc ontology)

source("use_jheem2_package_setting.R")
library(jheem2)
library(tidyverse)

cat("=== PA VIRAL SUPPRESSION INTEGRATION - 5 AGE GROUP VERSION ===\n")
cat("Started:", as.character(Sys.time()), "\n\n")

CONFIG <- list(
  INPUT_FILE = "data_raw/hiv_surveillance/viral_suppression_comprehensive_Pennsylvania_20250727_120208.csv",
  ORIGINAL_MANAGER = "cached/surveillance.manager_backup_2025-08-14.rdata",  # Using clean backup from before PA integration
  OUTCOME = "suppression",
  SOURCE = "cdc.hiv",
  ONTOLOGY = "cdc",  # CHANGED: Using 'cdc' ontology for 5 age groups
  YEARS = c("2022", "2023"),
  URL = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html",
  DETAILS = "Pennsylvania viral suppression 2022-2023, Atlas Plus data processed with standard 5-age-group structure per JHEEM conventions"
)

# Standard JHEEM age mappings (5 groups) - from atlas_plus_hiv_processing.R
AGE_MAPPINGS_5_GROUP <- c(
  '13-24' = '13-24 years',
  '25-34' = '25-34 years',
  '35-44' = '35-44 years',
  '45-54' = '45-54 years',
  '55-64' = '55+ years',    # KEY: Both 55-64 and 65+ map to 55+
  '65+' = '55+ years'
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

# Load and check manager
cat("1. Loading surveillance manager...\n")
manager <- load.data.manager(CONFIG$ORIGINAL_MANAGER)

# Check if outcome/source/ontology are registered
if (!CONFIG$OUTCOME %in% manager$outcomes) {
  stop(sprintf("Outcome '%s' not registered in manager", CONFIG$OUTCOME))
}
# Note: sources might be NULL or empty in this manager structure
if (!is.null(manager$sources) && length(manager$sources) > 0) {
  if (!CONFIG$SOURCE %in% manager$sources) {
    warning(sprintf("Source '%s' not in sources list, but continuing", CONFIG$SOURCE))
  }
}
if (!CONFIG$ONTOLOGY %in% names(manager$ontologies)) {
  stop(sprintf("Ontology '%s' not registered in manager", CONFIG$ONTOLOGY))
}

cat("2. Loading raw data...\n")
raw_data <- read.csv(CONFIG$INPUT_FILE, stringsAsFactors = FALSE)
cat(sprintf("   Loaded %d rows\n", nrow(raw_data)))

# Process the data
cat("3. Processing data with age group combination...\n")

process_data <- function(df) {
  # Clean location codes
  # For state-level data (County is NA), convert FIPS "42" to "PA"
  df$location <- ifelse(is.na(df$County), 
                        ifelse(df$FIPS == "42", "PA", df$FIPS),
                        df$FIPS)  # For counties, keep the 5-digit FIPS
  df$location <- gsub(" ", "", df$location)
  
  # Apply age mappings (5-group structure)
  df$age <- AGE_MAPPINGS_5_GROUP[df$Age.Group]
  
  # Map other dimensions
  df$race <- tolower(df$Race.Ethnicity)
  df$race[df$race == "all races/ethnicities"] <- NA
  # Keep the forward slashes as expected by cdc ontology
  # df$race <- gsub("/", "_", df$race)  # REMOVED - keep original slashes
  
  df$sex <- tolower(df$Sex)
  df$sex[df$sex == "both sexes"] <- NA
  
  df$risk <- RISK_MAPPINGS[df$Transmission.Category]
  
  # Handle data values
  # NOTE: "Rate per 100000" is actually percentage, "Cases" is numerator, "Population" is denominator
  df$numerator <- as.numeric(gsub(",", "", df$Cases))
  df$denominator <- as.numeric(gsub(",", "", df$Population))
  
  # For suppressed data
  df$numerator[df$Data.Status == "Data suppressed"] <- NA
  df$denominator[df$Data.Status == "Data suppressed"] <- NA
  
  return(df)
}

processed_data <- process_data(raw_data)

# Now aggregate rows that have the same dimensions after age mapping
cat("4. Aggregating age groups 55-64 and 65+ into 55+...\n")

aggregate_age_groups <- function(df) {
  # Group by all dimensions except the original Age.Group
  # Sum numerators and denominators for rows with same final dimensions
  
  aggregated <- df %>%
    filter(!is.na(numerator)) %>%  # Remove NA rows before aggregation
    group_by(Year, location, age, race, sex, risk, stratification_type) %>%
    summarise(
      numerator = sum(numerator, na.rm = TRUE),
      denominator = sum(denominator, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      value = numerator / denominator  # Calculate proportion
    ) %>%
    filter(!is.nan(value))  # Remove NaN values (0/0 cases)
  
  return(aggregated)
}

aggregated_data <- aggregate_age_groups(processed_data)

cat(sprintf("   After aggregation: %d rows\n", nrow(aggregated_data)))

# Check age groups
unique_ages <- unique(aggregated_data$age[!is.na(aggregated_data$age)])
cat("   Unique age groups after mapping:", paste(sort(unique_ages), collapse=", "), "\n")
if (length(unique_ages) > 5) {
  warning("More than 5 age groups detected after mapping!")
}

# Process each year separately
for (year in CONFIG$YEARS) {
  cat(sprintf("\n5. Processing year %s...\n", year))
  
  year_data <- aggregated_data %>%
    filter(Year == year)
  
  if (nrow(year_data) == 0) {
    cat(sprintf("   No data for year %s, skipping.\n", year))
    next
  }
  
  # Process each stratification type
  strat_types <- unique(year_data$stratification_type)
  
  for (strat_type in strat_types) {
    cat(sprintf("   Processing stratification: %s\n", strat_type))
    
    strat_data <- year_data %>%
      filter(stratification_type == strat_type)
    
    if (nrow(strat_data) == 0) next
    
    # Determine dimensions for this stratification
    dimensions <- STRATIFICATION_MAPPING[[strat_type]]
    
    # Create dimension.values list
    dimension.values <- list(year = year)
    
    # Build the data frame for put.long.form
    put_data <- data.frame(
      outcome = CONFIG$OUTCOME,
      year = year,
      location = strat_data$location,
      value = strat_data$value,
      stringsAsFactors = FALSE
    )
    
    # Add relevant dimensions based on stratification type
    # Only add dimensions that are actually part of this stratification
    if (length(dimensions) > 0) {
      # This is a stratified dataset
      if ("age" %in% dimensions) put_data$age <- strat_data$age
      if ("race" %in% dimensions) put_data$race <- strat_data$race  
      if ("sex" %in% dimensions) put_data$sex <- strat_data$sex
      if ("risk" %in% dimensions) put_data$risk <- strat_data$risk
      
      # Remove rows with NA values in the stratification dimensions
      # (these are subtotals that shouldn't be included)
      put_data <- put_data[complete.cases(put_data), ]
    }
    # For total stratification (no dimensions), keep all rows as is
    
    if (nrow(put_data) == 0) {
      cat(sprintf("     No valid data for %s, skipping.\n", strat_type))
      next
    }
    
    # Put the data
    tryCatch({
      manager$put.long.form(
        data = put_data,
        ontology.name = CONFIG$ONTOLOGY,
        source = CONFIG$SOURCE,
        dimension.values = dimension.values,
        url = CONFIG$URL,
        details = CONFIG$DETAILS
      )
      cat(sprintf("     Successfully added %d records\n", nrow(put_data)))
    }, error = function(e) {
      cat(sprintf("     ERROR: %s\n", e$message))
    })
  }
}

# Verify the data was added correctly
cat("\n6. Verifying integration...\n")

# Simple verification - just check if data exists
tryCatch({
  data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location"]]
  if ("PA" %in% dimnames(data)$location) {
    pa_2022 <- data["2022", "PA"]
    pa_2023 <- data["2023", "PA"]
    
    if (!is.na(pa_2022) && !is.nan(pa_2022)) {
      cat(sprintf("   ✓ PA 2022 total suppression: %.1f%%\n", pa_2022 * 100))
    } else {
      cat("   ⚠ PA 2022 data stored but value is NA/NaN\n")
    }
    
    if (!is.na(pa_2023) && !is.nan(pa_2023)) {
      cat(sprintf("   ✓ PA 2023 total suppression: %.1f%%\n", pa_2023 * 100))
    } else {
      cat("   ⚠ PA 2023 data stored but value is NA/NaN\n")
    }
  } else {
    cat("   ✗ PA not found in location dimension\n")
  }
}, error = function(e) {
  cat("   Error during verification:", e$message, "\n")
})

# Save the updated manager
cat("\n8. Saving updated manager...\n")
output_file <- "cached/surveillance.manager.pa_5age.rdata"
save(manager, file = output_file)
cat(sprintf("   Saved to: %s\n", output_file))

cat("\n=== INTEGRATION COMPLETE ===\n")
cat("Completed:", as.character(Sys.time()), "\n")

# Final summary
cat("\nSUMMARY:\n")
cat("- Processed PA viral suppression data for 2022-2023\n")
cat("- Used 'cdc' ontology with 5 age groups (55+ combined)\n")
cat("- Age groups 55-64 and 65+ properly weighted by population\n")
cat("- Data should now be compatible with likelihood system\n")
cat("\nNext step: Test with EHE calibration setup\n")
