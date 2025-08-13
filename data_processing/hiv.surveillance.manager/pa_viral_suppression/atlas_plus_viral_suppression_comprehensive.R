# Atlas Plus Viral Suppression Data Pull - Comprehensive & Scalable
# Pulls HIV viral suppression data with all stratifications for specified states/years
# Designed to handle: PA initially, then scale to all states
# 
# Covers ALL requested stratifications:
# - Total level
# - One-way: age, race, sex, transmission risk
# - Two-way: age+race, age+sex, age+risk, race+sex, race+risk, sex+risk
# - Three-way: age+race+sex (without risk), age+race+risk, age+sex+risk, race+sex+risk
#
# Author: Claude Code / JHEEM Team
# Created: 2025-07-27

# Required libraries
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Set working directory and source required functions
setwd("/Users/nicholas/Documents/jheem/code/jheem_analyses")
source("data_processing/cdc_atlas/load_cdc.R")

# Enhanced mappings loader that includes transmission risk (vtid 8)
load_cdc_mappings_comprehensive <- function(json_path) {
  mappings <- fromJSON(json_path)
  mappings_df <- as_tibble(mappings)
  
  list(
    indicators = filter(mappings_df, vtid == 1),
    years = filter(mappings_df, vtid == 2),
    geography = filter(mappings_df, vtid == 3),
    status = filter(mappings_df, vtid == 4),
    race = filter(mappings_df, vtid == 5),
    sex = filter(mappings_df, vtid == 6),
    age = filter(mappings_df, vtid == 7),
    transmission = filter(mappings_df, vtid == 8)
  )
}

# Enhanced ID lookup functions
get_transmission_ids <- function(transmission_categories, mappings) {
  ids <- mappings$transmission %>%
    filter(name %in% transmission_categories) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid transmission categories: ", paste(transmission_categories, collapse=", "))
  return(ids)
}

get_sex_ids <- function(sex_categories, mappings) {
  ids <- mappings$sex %>%
    filter(name %in% sex_categories) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid sex categories: ", paste(sex_categories, collapse=", "))
  return(ids)
}

# Core API call function with comprehensive parameter support
get_viral_suppression_data <- function(
    locations,
    years,
    age_groups = "Ages 13 years and older",
    races = "All races/ethnicities", 
    sex_categories = "Both sexes",
    transmission_categories = "All transmission categories",
    mappings
) {
  
  # Convert single values to vectors
  locations <- if(length(locations) == 1) c(locations) else locations
  years <- if(length(years) == 1) c(years) else as.character(years)
  age_groups <- if(length(age_groups) == 1) c(age_groups) else age_groups
  races <- if(length(races) == 1) c(races) else races
  sex_categories <- if(length(sex_categories) == 1) c(sex_categories) else sex_categories
  transmission_categories <- if(length(transmission_categories) == 1) c(transmission_categories) else transmission_categories
  
  # Create parameter combinations
  param_grid <- expand.grid(
    location = locations,
    year = years,
    sex = sex_categories,
    transmission = transmission_categories,
    stringsAsFactors = FALSE
  )
  
  # Initialize results
  all_results <- list()
  
  # Process each combination
  for (i in 1:nrow(param_grid)) {
    location <- param_grid$location[i]
    year <- param_grid$year[i]
    sex <- param_grid$sex[i]
    transmission <- param_grid$transmission[i]
    
    cat("Processing:", location, "/", year, "/", sex, "/", transmission, "\n")
    
    # Get IDs
    indicator_id <- get_indicator_ids("HIV viral suppression", mappings)
    location_ids <- get_location_ids(location, mappings)
    year_id <- get_year_ids(year, mappings)
    age_ids <- get_age_ids(age_groups, mappings)
    race_ids <- get_race_ids(races, mappings)
    sex_id <- get_sex_ids(sex, mappings)
    transmission_id <- get_transmission_ids(transmission, mappings)
    
    # Construct variable_ids string
    variable_ids <- paste(
      c(indicator_id, location_ids, year_id, age_ids, race_ids, sex_id, transmission_id),
      collapse = ","
    )
    
    # Make API request
    resp <- request("https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData") %>%
      req_headers(
        "content-type" = "application/json; charset=UTF-8",
        "origin" = "https://gis.cdc.gov",
        "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html"
      ) %>%
      req_body_json(list(VariableIDs = variable_ids)) %>%
      req_perform()
    
    if (resp_status(resp) != 200) {
      warning("Request failed for ", location, "/", year, "/", sex, "/", transmission, ": ", resp_status(resp))
      next
    }
    
    # Parse and process response
    tryCatch({
      raw_data <- jsonlite::fromJSON(resp_body_string(resp))$sourcedata
      
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        warning("No data returned for ", location, "/", year, "/", sex, "/", transmission)
        next
      }
      
      raw_data <- as.data.frame(raw_data) %>%
        setNames(paste0("sourcedata.", 1:ncol(.)))
      
      # Process into standardized format
      processed_data <- raw_data %>%
        mutate(
          # Geographic info
          location_name = mappings$geography$name[match(sourcedata.3, mappings$geography$id)],
          parent_id = mappings$geography$parentid[match(sourcedata.3, mappings$geography$id)],
          parent_name = mappings$geography$name[match(parent_id, mappings$geography$id)],
          grandparent_id = mappings$geography$parentid[match(parent_id, mappings$geography$id)],
          grandparent_name = mappings$geography$name[match(grandparent_id, mappings$geography$id)],
          
          location_type = case_when(
            is.na(parent_id) ~ "national",
            is.na(grandparent_id) ~ "state", 
            TRUE ~ "county"
          )
        ) %>%
        transmute(
          Indicator = "HIV viral suppression",
          Year = year,
          State = case_when(
            location_type == "national" ~ NA_character_,
            location_type == "state" ~ location_name,
            location_type == "county" ~ parent_name,
            TRUE ~ NA_character_
          ),
          County = case_when(
            location_type == "county" ~ location_name,
            TRUE ~ NA_character_
          ),
          Geography = case_when(
            location_type == "national" ~ location_name,
            location_type == "state" ~ location_name,
            location_type == "county" ~ paste0(location_name, ", ", parent_name),
            TRUE ~ NA_character_
          ),
          FIPS = mappings$geography$fips[match(sourcedata.3, mappings$geography$id)],
          `Age Group` = mappings$age$name[match(sourcedata.7, mappings$age$id)],
          `Race/Ethnicity` = mappings$race$name[match(sourcedata.5, mappings$race$id)],
          Sex = sex,
          `Transmission Category` = transmission,
          `Data Status` = case_when(
            sourcedata.4 == 405 ~ "Data suppressed",
            sourcedata.4 == 406 ~ "Data not available",
            TRUE ~ "Available"
          ),
          Cases = case_when(
            sourcedata.4 == 405 ~ "Data suppressed",
            sourcedata.4 == 406 ~ "Data not available", 
            is.na(sourcedata.10) ~ "Data suppressed",
            sourcedata.10 == 0 ~ "0",
            TRUE ~ as.character(sourcedata.10)
          ),
          `Rate per 100000` = case_when(
            sourcedata.4 == 405 ~ "Data suppressed",
            sourcedata.4 == 406 ~ "Data not available",
            is.na(sourcedata.9) ~ "Data suppressed",
            sourcedata.9 == 0 ~ "0.0",
            TRUE ~ format(sourcedata.9, nsmall = 1)
          ),
          Population = format(sourcedata.11, big.mark = ","),
          Percentage = case_when(
            sourcedata.4 == 405 ~ NA_real_,
            sourcedata.4 == 406 ~ NA_real_,
            is.na(sourcedata.12) ~ NA_real_,
            TRUE ~ sourcedata.12
          ),
          `CI Lower` = case_when(
            sourcedata.4 == 405 ~ NA_real_,
            sourcedata.4 == 406 ~ NA_real_,
            is.na(sourcedata.13) ~ NA_real_,
            TRUE ~ sourcedata.13
          ),
          `CI Upper` = case_when(
            sourcedata.4 == 405 ~ NA_real_,
            sourcedata.4 == 406 ~ NA_real_,
            is.na(sourcedata.14) ~ NA_real_,
            TRUE ~ sourcedata.14
          )
        )
      
      all_results[[length(all_results) + 1]] <- processed_data
    }, error = function(e) {
      warning("Error processing data for ", location, "/", year, "/", sex, "/", transmission, ": ", e$message)
    })
    
    # Be nice to the API
    Sys.sleep(0.1)
  }
  
  # Combine results
  if (length(all_results) > 0) {
    return(bind_rows(all_results))
  } else {
    return(NULL)
  }
}

# Main function to pull comprehensive viral suppression data
pull_comprehensive_viral_suppression <- function(
    states = "Pennsylvania",
    years = c("2022", "2023"),
    output_file = NULL
) {
  
  cat("=== COMPREHENSIVE VIRAL SUPPRESSION DATA PULL ===\n")
  cat("States:", paste(states, collapse = ", "), "\n")
  cat("Years:", paste(years, collapse = ", "), "\n")
  
  # Load mappings
  mappings <- load_cdc_mappings_comprehensive("data_processing/cdc_atlas/varvals.json")
  
  # Define all parameter values
  age_groups_all <- c("Ages 13 years and older", "13-24", "25-34", "35-44", "45-54", "55-64", "65+")
  races_all <- c("All races/ethnicities", "Black/African American", "Hispanic/Latino", "White", "American Indian/Alaska Native", "Asian")
  sex_all <- c("Both sexes", "Male", "Female")
  transmission_all <- c("All transmission categories", "Male-to-male sexual contact", "Injection drug use", 
                       "Male-to-male sexual contact and injection drug use", "Heterosexual contact", "Other")
  
  # Define comprehensive stratification combinations
  stratification_configs <- list(
    
    # === TOTAL LEVEL ===
    total = list(
      ages = "Ages 13 years and older",
      races = "All races/ethnicities", 
      sex = "Both sexes",
      transmission = "All transmission categories",
      description = "Total level"
    ),
    
    # === ONE-WAY STRATIFICATIONS ===
    age_only = list(
      ages = age_groups_all[-1],  # Exclude "Ages 13 years and older"
      races = "All races/ethnicities",
      sex = "Both sexes", 
      transmission = "All transmission categories",
      description = "Age stratification only"
    ),
    
    race_only = list(
      ages = "Ages 13 years and older",
      races = races_all[-1],  # Exclude "All races/ethnicities"
      sex = "Both sexes",
      transmission = "All transmission categories", 
      description = "Race stratification only"
    ),
    
    sex_only = list(
      ages = "Ages 13 years and older",
      races = "All races/ethnicities",
      sex = sex_all[-1],  # Exclude "Both sexes"
      transmission = "All transmission categories",
      description = "Sex stratification only"
    ),
    
    transmission_only = list(
      ages = "Ages 13 years and older",
      races = "All races/ethnicities", 
      sex = "Both sexes",
      transmission = transmission_all[-1],  # Exclude "All transmission categories"
      description = "Transmission risk stratification only"
    ),
    
    # === TWO-WAY STRATIFICATIONS ===
    age_race = list(
      ages = age_groups_all[-1],
      races = races_all[-1],
      sex = "Both sexes",
      transmission = "All transmission categories",
      description = "Age + Race stratification"
    ),
    
    age_sex = list(
      ages = age_groups_all[-1],
      races = "All races/ethnicities",
      sex = sex_all[-1],
      transmission = "All transmission categories", 
      description = "Age + Sex stratification"
    ),
    
    age_transmission = list(
      ages = age_groups_all[-1],
      races = "All races/ethnicities",
      sex = "Both sexes",
      transmission = transmission_all[-1],
      description = "Age + Transmission risk stratification"
    ),
    
    race_sex = list(
      ages = "Ages 13 years and older", 
      races = races_all[-1],
      sex = sex_all[-1],
      transmission = "All transmission categories",
      description = "Race + Sex stratification"
    ),
    
    race_transmission = list(
      ages = "Ages 13 years and older",
      races = races_all[-1], 
      sex = "Both sexes",
      transmission = transmission_all[-1],
      description = "Race + Transmission risk stratification"
    ),
    
    sex_transmission = list(
      ages = "Ages 13 years and older",
      races = "All races/ethnicities",
      sex = sex_all[-1],
      transmission = transmission_all[-1],
      description = "Sex + Transmission risk stratification"
    ),
    
    # === THREE-WAY STRATIFICATIONS ===
    age_race_sex = list(
      ages = age_groups_all[-1],
      races = races_all[-1],
      sex = sex_all[-1], 
      transmission = "All transmission categories",
      description = "Age + Race + Sex stratification"
    ),
    
    age_race_transmission = list(
      ages = age_groups_all[-1],
      races = races_all[-1],
      sex = "Both sexes",
      transmission = transmission_all[-1],
      description = "Age + Race + Transmission risk stratification"
    ),
    
    age_sex_transmission = list(
      ages = age_groups_all[-1],
      races = "All races/ethnicities",
      sex = sex_all[-1],
      transmission = transmission_all[-1],
      description = "Age + Sex + Transmission risk stratification"
    ),
    
    race_sex_transmission = list(
      ages = "Ages 13 years and older",
      races = races_all[-1],
      sex = sex_all[-1],
      transmission = transmission_all[-1],
      description = "Race + Sex + Transmission risk stratification"
    ),
    
    # === FOUR-WAY STRATIFICATION ===
    age_race_sex_transmission = list(
      ages = age_groups_all[-1],
      races = races_all[-1],
      sex = sex_all[-1],
      transmission = transmission_all[-1],
      description = "Age + Race + Sex + Transmission risk stratification (full intersection)"
    )
  )
  
  cat("Stratification configurations:", length(stratification_configs), "\n\n")
  
  # Execute data pulls
  all_stratification_results <- list()
  
  for (config_name in names(stratification_configs)) {
    config <- stratification_configs[[config_name]]
    cat("--- Processing:", config$description, "---\n")
    
    result <- get_viral_suppression_data(
      locations = states,
      years = years,
      age_groups = config$ages,
      races = config$races,
      sex_categories = config$sex,
      transmission_categories = config$transmission,
      mappings = mappings
    )
    
    if (!is.null(result)) {
      result$stratification_type <- config_name
      all_stratification_results[[config_name]] <- result
      cat("Success:", config_name, "returned", nrow(result), "records\n\n")
    } else {
      cat("Failed:", config_name, "returned no data\n\n")
    }
  }
  
  # Combine all results
  if (length(all_stratification_results) > 0) {
    final_data <- bind_rows(all_stratification_results)
    
    # Generate output filename if not provided
    if (is.null(output_file)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      state_suffix <- if(length(states) == 1) states[1] else "multi_state"
      output_file <- paste0("data_raw/hiv_surveillance/viral_suppression_comprehensive_", 
                           state_suffix, "_", timestamp, ".csv")
    }
    
    # Save results
    write.csv(final_data, output_file, row.names = FALSE)
    
    # Summary report
    cat("=== FINAL COMPREHENSIVE RESULTS ===\n")
    cat("Total records:", nrow(final_data), "\n")
    cat("States covered:", paste(unique(final_data$State[!is.na(final_data$State)]), collapse = ", "), "\n")
    cat("Years covered:", paste(unique(final_data$Year), collapse = ", "), "\n")
    cat("Stratification types:", paste(unique(final_data$stratification_type), collapse = ", "), "\n")
    cat("File saved:", output_file, "\n\n")
    
    # Detailed summary
    summary_table <- final_data %>%
      group_by(stratification_type) %>%
      summarise(
        records = n(),
        years = n_distinct(Year),
        states = n_distinct(State, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(stratification_type)
    
    cat("Records by stratification type:\n")
    print(summary_table)
    
    return(list(data = final_data, summary = summary_table, file = output_file))
    
  } else {
    cat("ERROR: No successful data pulls\n")
    return(NULL)
  }
}

# Usage examples:
# 
# # Pennsylvania only (current need)
# pa_result <- pull_comprehensive_viral_suppression(
#   states = "Pennsylvania", 
#   years = c("2022", "2023")
# )
#
# # Multiple states (future scalability)
# multi_result <- pull_comprehensive_viral_suppression(
#   states = c("Pennsylvania", "California", "Texas"),
#   years = c("2022", "2023")
# )
#
# # All states (ultimate goal)
# all_result <- pull_comprehensive_viral_suppression(
#   states = get_all_state_names(),  # Function to be created
#   years = c("2022", "2023")
# )