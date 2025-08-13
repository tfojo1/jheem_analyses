# Pennsylvania Viral Suppression - WITH RISK FACTOR STRATIFICATIONS
# Enhanced version that includes transmission risk category stratifications

# Required libraries
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Set working directory and source required functions
setwd("/Users/nicholas/Documents/jheem/code/jheem_analyses")
source("data_processing/cdc_atlas/load_cdc.R")
source("data_processing/cdc_atlas/get_cdc_data_enhanced.R")

# Enhanced load_cdc_mappings function to include transmission risk (vtid 8)
load_cdc_mappings_enhanced <- function(json_path) {
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
    transmission = filter(mappings_df, vtid == 8)  # ADD TRANSMISSION RISK
  )
}

# Load enhanced mappings
cdc_mappings_enhanced <- load_cdc_mappings_enhanced("data_processing/cdc_atlas/varvals.json")

# New function to get transmission risk IDs
get_transmission_ids <- function(transmission_categories, mappings = cdc_mappings_enhanced) {
  ids <- mappings$transmission %>%
    filter(name %in% transmission_categories) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid transmission categories: ", paste(transmission_categories, collapse=", "))
  return(ids)
}

# Enhanced get_cdc_data function that supports transmission risk stratification
get_cdc_data_with_transmission <- function(
    indicators,
    locations,
    years,
    age_groups,
    races,
    sex = "Both sexes",
    transmission_categories = "All transmission categories",
    mappings = cdc_mappings_enhanced
) {
  
  # Convert single values to vectors
  indicators <- if(length(indicators) == 1) c(indicators) else indicators
  locations <- if(length(locations) == 1) c(locations) else locations
  years <- if(length(years) == 1) c(years) else as.character(years)
  age_groups <- if(length(age_groups) == 1) c(age_groups) else age_groups
  races <- if(length(races) == 1) c(races) else races
  transmission_categories <- if(length(transmission_categories) == 1) c(transmission_categories) else transmission_categories
  
  # Get all combinations of parameters to fetch
  param_grid <- expand.grid(
    indicator = indicators,
    location = locations,
    year = years,
    transmission = transmission_categories,
    stringsAsFactors = FALSE
  )
  
  # Initialize results
  all_results <- list()
  
  # Process each combination
  for (i in 1:nrow(param_grid)) {
    indicator <- param_grid$indicator[i]
    location <- param_grid$location[i]
    year <- param_grid$year[i]
    transmission <- param_grid$transmission[i]
    
    cat("Processing:", indicator, "/", location, "/", year, "/", transmission, "\n")
    
    # Get IDs
    indicator_id <- get_indicator_ids(indicator, mappings)
    location_ids <- get_location_ids(location, mappings)
    year_id <- get_year_ids(year, mappings)
    age_ids <- get_age_ids(age_groups, mappings)
    race_ids <- get_race_ids(races, mappings)
    sex_id <- 601  # "Both sexes" - hardcoded for now
    transmission_id <- get_transmission_ids(transmission, mappings)
    
    # Construct variable_ids string
    variable_ids <- paste(
      c(
        indicator_id,
        location_ids,
        year_id,
        age_ids,
        race_ids,
        sex_id,
        transmission_id  # Use specific transmission risk instead of "unknown_id"
      ),
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
      warning("Request failed for ", indicator, "/", location, "/", year, "/", transmission, ": ", resp_status(resp))
      next
    }
    
    # Parse JSON response and convert to data frame
    tryCatch({
      raw_data <- jsonlite::fromJSON(resp_body_string(resp))$sourcedata
      
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        warning("No sourcedata returned for ", indicator, "/", location, "/", year, "/", transmission)
        next
      }
      
      raw_data <- as.data.frame(raw_data) %>%
        setNames(paste0("sourcedata.", 1:ncol(.)))
      
      # Process data into final format
      processed_data <- raw_data %>%
        mutate(
          # Get basic location info
          location_name = mappings$geography$name[match(sourcedata.3, mappings$geography$id)],
          parent_id = mappings$geography$parentid[match(sourcedata.3, mappings$geography$id)],
          parent_name = mappings$geography$name[match(parent_id, mappings$geography$id)],
          grandparent_id = mappings$geography$parentid[match(parent_id, mappings$geography$id)],
          grandparent_name = mappings$geography$name[match(grandparent_id, mappings$geography$id)],
          
          # Determine location type
          location_type = case_when(
            is.na(parent_id) ~ "national",
            is.na(grandparent_id) ~ "state", 
            TRUE ~ "county"
          )
        ) %>%
        transmute(
          Indicator = indicator,
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
          `Transmission Category` = transmission,  # ADD TRANSMISSION CATEGORY COLUMN
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
          # ADD PERCENTAGE COLUMN (if available)
          Percentage = case_when(
            sourcedata.4 == 405 ~ NA_real_,
            sourcedata.4 == 406 ~ NA_real_,
            is.na(sourcedata.12) ~ NA_real_,
            TRUE ~ sourcedata.12
          ),
          # ADD CONFIDENCE INTERVALS (if available)
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
      warning("Error processing data for ", indicator, "/", location, "/", year, "/", transmission, ": ", e$message)
    })
    
    # Small delay to be nice to the API
    Sys.sleep(0.1)
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    return(bind_rows(all_results))
  } else {
    warning("No valid data returned for any request")
    return(NULL)
  }
}

# Define parameters
indicator <- "HIV viral suppression"
location <- "Pennsylvania"
years <- c("2022", "2023")

# Define all transmission risk categories
transmission_categories <- c(
  "All transmission categories",
  "Male-to-male sexual contact",
  "Injection drug use", 
  "Male-to-male sexual contact and injection drug use",
  "Heterosexual contact",
  "Other"
)

# Define comprehensive stratification plan including risk factors
comprehensive_stratifications <- list(
  
  # === TOTAL LEVEL ===
  total = list(
    age_groups = "Ages 13 years and older",
    races = "All races/ethnicities",
    transmission = "All transmission categories"
  ),
  
  # === RISK FACTOR ONLY ===
  risk_only = list(
    age_groups = "Ages 13 years and older", 
    races = "All races/ethnicities",
    transmission = transmission_categories[-1]  # Exclude "All transmission categories"
  ),
  
  # === AGE + RISK COMBINATIONS ===
  age_risk = list(
    age_groups = c("13-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    races = "All races/ethnicities",
    transmission = transmission_categories[-1]  # Exclude "All transmission categories"
  ),
  
  # === RACE + RISK COMBINATIONS ===
  race_risk = list(
    age_groups = "Ages 13 years and older",
    races = c("Black/African American", "Hispanic/Latino", "White"),
    transmission = transmission_categories[-1]  # Exclude "All transmission categories"
  )
)

cat("=== STARTING COMPREHENSIVE DATA PULL WITH RISK FACTORS ===\n")
cat("Transmission categories to pull:", length(transmission_categories), "\n")
cat("Stratification types:", length(comprehensive_stratifications), "\n")

# Initialize combined results
all_stratification_results <- list()

# Execute data pulls for each stratification type
for (strat_name in names(comprehensive_stratifications)) {
  cat("\n--- Processing stratification:", strat_name, "---\n")
  
  strat_config <- comprehensive_stratifications[[strat_name]]
  
  # Call the enhanced function
  result <- get_cdc_data_with_transmission(
    indicators = indicator,
    locations = location,
    years = years,
    age_groups = strat_config$age_groups,
    races = strat_config$races,
    transmission_categories = strat_config$transmission,
    mappings = cdc_mappings_enhanced
  )
  
  if (!is.null(result)) {
    # Add stratification type column
    result$stratification_type <- strat_name
    all_stratification_results[[strat_name]] <- result
    cat("Success:", strat_name, "returned", nrow(result), "records\n")
  } else {
    cat("Failed:", strat_name, "returned no data\n")
  }
}

# Combine all results
if (length(all_stratification_results) > 0) {
  final_comprehensive_data <- bind_rows(all_stratification_results)
  
  # Add timestamp and save
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0("data_raw/hiv_surveillance/PA_viral_suppression_WITH_RISK_", timestamp, ".csv")
  
  write.csv(final_comprehensive_data, output_file, row.names = FALSE)
  
  cat("\n=== FINAL RESULTS ===\n")
  cat("Total records:", nrow(final_comprehensive_data), "\n")
  cat("Stratification types:", paste(unique(final_comprehensive_data$stratification_type), collapse = ", "), "\n")
  cat("Transmission categories:", paste(unique(final_comprehensive_data$`Transmission Category`), collapse = ", "), "\n")
  cat("File saved:", output_file, "\n")
  
  # Summary by stratification type
  cat("\nRecords by stratification type:\n")
  summary_table <- final_comprehensive_data %>%
    group_by(stratification_type, `Transmission Category`) %>%
    summarise(records = n(), .groups = 'drop') %>%
    arrange(stratification_type, `Transmission Category`)
  
  print(summary_table)
  
} else {
  cat("ERROR: No successful data pulls\n")
}