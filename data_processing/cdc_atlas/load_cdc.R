library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

load_cdc_mappings <- function(json_path) {
  # Read the JSON file
  mappings <- fromJSON(json_path)
  
  # Convert to tibble and organize by vtid
  mappings_df <- as_tibble(mappings)
  
  # Create separate lookup tables by vtid
  list(
    indicators = filter(mappings_df, vtid == 1),
    years = filter(mappings_df, vtid == 2),
    geography = filter(mappings_df, vtid == 3),
    status = filter(mappings_df, vtid == 4),
    race = filter(mappings_df, vtid == 5),
    sex = filter(mappings_df, vtid == 6),
    age = filter(mappings_df, vtid == 7)
  )
}

# Assume mappings are loaded
cdc_mappings <- load_cdc_mappings("data_processing/cdc_atlas/varvals.json")

# Flexible location lookup
get_location_ids <- function(locations, mappings = cdc_mappings) {
  # Try to find locations directly in the mapping
  direct_matches <- mappings$geography %>%
    filter(name %in% locations)
  
  # For each location, also get any child locations
  all_ids <- c()
  
  for (loc_id in direct_matches$id) {
    # Add the location itself
    all_ids <- c(all_ids, loc_id)
    
    # Check if it has children
    children <- mappings$geography %>%
      filter(parentid == loc_id) %>%
      pull(id)
    
    if (length(children) > 0) {
      all_ids <- c(all_ids, children)
    }
  }
  
  if (length(all_ids) == 0) {
    stop("No valid locations found: ", paste(locations, collapse=", "))
  }
  
  return(unique(all_ids))
}

# Helper functions for other parameter types
get_indicator_ids <- function(indicators, mappings = cdc_mappings) {
  ids <- mappings$indicators %>%
    filter(name %in% indicators) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid indicators: ", paste(indicators, collapse=", "))
  return(ids)
}

get_year_ids <- function(years, mappings = cdc_mappings) {
  years <- as.character(years)
  ids <- mappings$years %>%
    filter(name %in% years) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid years: ", paste(years, collapse=", "))
  return(ids)
}

get_age_ids <- function(age_groups, mappings = cdc_mappings) {
  ids <- mappings$age %>%
    filter(name %in% age_groups) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid age groups: ", paste(age_groups, collapse=", "))
  return(ids)
}

get_race_ids <- function(races, mappings = cdc_mappings) {
  ids <- mappings$race %>%
    filter(name %in% races) %>%
    pull(id)
  
  if(length(ids) == 0) stop("Invalid races: ", paste(races, collapse=", "))
  return(ids)
}

# Main function that supports multiple parameters
get_cdc_data <- function(
    indicators,
    locations,
    years,
    age_groups,
    races,
    sex = "Both sexes",
    mappings = cdc_mappings
) {
  # Convert single values to vectors
  indicators <- if(length(indicators) == 1) c(indicators) else indicators
  locations <- if(length(locations) == 1) c(locations) else locations
  years <- if(length(years) == 1) c(years) else as.character(years)
  age_groups <- if(length(age_groups) == 1) c(age_groups) else age_groups
  races <- if(length(races) == 1) c(races) else races
  
  # Get all combinations of parameters to fetch
  param_grid <- expand.grid(
    indicator = indicators,
    location = locations,
    year = years,
    stringsAsFactors = FALSE
  )
  
  # Initialize results
  all_results <- list()
  
  # Process each combination
  for (i in 1:nrow(param_grid)) {
    indicator <- param_grid$indicator[i]
    location <- param_grid$location[i]
    year <- param_grid$year[i]
    
    # Get IDs
    indicator_id <- get_indicator_ids(indicator)
    location_ids <- get_location_ids(location)
    year_id <- get_year_ids(year)
    age_ids <- get_age_ids(age_groups)
    race_ids <- get_race_ids(races)
    
    # Constants
    sex_id <- 601  # "Both sexes" - could make this dynamic later
    unknown_id <- 801  # Required by API
    
    # Construct variable_ids string
    variable_ids <- paste(
      c(
        indicator_id,
        location_ids,
        year_id,
        age_ids,
        race_ids,
        sex_id,
        unknown_id
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
      warning("Request failed for ", indicator, "/", location, "/", year, ": ", resp_status(resp))
      next
    }
    
    # Parse JSON response and convert to data frame
    tryCatch({
      raw_data <- jsonlite::fromJSON(resp_body_string(resp))$sourcedata %>%
        as.data.frame() %>%
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
          Cases = case_when(
            sourcedata.4 == 405 ~ "Data suppressed",
            is.na(sourcedata.10) ~ "Data suppressed",
            sourcedata.10 == 0 ~ "0",
            TRUE ~ as.character(sourcedata.10)
          ),
          `Rate per 100000` = case_when(
            sourcedata.4 == 405 ~ "Data suppressed",
            is.na(sourcedata.9) ~ "Data suppressed",
            sourcedata.9 == 0 ~ "0.0",
            TRUE ~ format(sourcedata.9, nsmall = 1)
          ),
          Population = format(sourcedata.11, big.mark = ",")
        )
      
      all_results[[length(all_results) + 1]] <- processed_data
    }, error = function(e) {
      warning("Error processing data for ", indicator, "/", location, "/", year, ": ", e$message)
    })
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    return(bind_rows(all_results))
  } else {
    warning("No valid data returned for any request")
    return(NULL)
  }
}


dat <- get_cdc_data(
  indicators = "Early Non-Primary, Non-Secondary Syphilis",
  locations = "Arizona",
  years = c("2023"),
  age_groups = c("0-14", "15-24"),
  races = c("Asian", "Hispanic/Latino")
)

# Example 1: Multiple years for one location
data1 <- get_cdc_data(
  indicators = "Early Non-Primary, Non-Secondary Syphilis",
  locations = "Arizona",
  years = c("2022", "2023"),
  age_groups = c("0-14", "15-24"),
  races = c("Asian", "Hispanic/Latino")
)

# Example 2: Multiple locations
data2 <- get_cdc_data(
  indicators = "Primary and Secondary Syphilis",
  locations = c("Arizona", "California"),
  years = "2023",
  age_groups = c("0-14", "15-24"),
  races = "All races/ethnicities"
)

# Example 3: Direct county selection
data3 <- get_cdc_data(
  indicators = "Gonorrhea",
  locations = c("Maricopa County, AZ", "Pima County, AZ"),
  years = "2023",
  age_groups = "Ages 13 years and older",
  races = "All races/ethnicities"
)
