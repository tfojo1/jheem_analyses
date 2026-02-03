# Enhanced get_cdc_data function to handle both surveillance and QoL data

# Modified main function that handles both surveillance and QoL data
get_cdc_data_enhanced <- function(
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
      raw_data <- jsonlite::fromJSON(resp_body_string(resp))$sourcedata
      
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        warning("No sourcedata returned for ", indicator, "/", location, "/", year)
        next
      }
      
      # Convert to data frame and add column names
      raw_data <- as.data.frame(raw_data) %>%
        setNames(paste0("sourcedata.", 1:ncol(.)))
      
      # Check if this is a QoL indicator by looking for QoL indicators
      qol_indicators <- c("HIV Stigma", "Unstable Housing or Homelessness", 
                         "Good or better self-rated health", "Unmet needs for mental health services",
                         "Hunger or food insecurity", "Unemployment")
      
      is_qol_indicator <- indicator %in% qol_indicators
      
      # Process data based on whether it's QoL or surveillance
      if (is_qol_indicator) {
        # QoL data processing
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
            # QoL-specific columns
            `Data Status` = case_when(
              sourcedata.4 == 404 ~ "Available",
              sourcedata.4 == 408 ~ "Not available", 
              TRUE ~ as.character(sourcedata.4)
            ),
            `Percentage` = case_when(
              sourcedata.4 == 404 ~ sourcedata.9,
              sourcedata.4 == 408 ~ NA_real_,
              TRUE ~ sourcedata.9
            ),
            `CI Lower` = case_when(
              sourcedata.4 == 404 ~ sourcedata.12,
              TRUE ~ NA_real_
            ),
            `CI Upper` = case_when(
              sourcedata.4 == 404 ~ sourcedata.13,
              TRUE ~ NA_real_
            ),
            # Keep empty columns for consistency with surveillance format
            Cases = NA_character_,
            `Rate per 100000` = NA_character_,
            Population = NA_character_
          )
      } else {
        # Original surveillance data processing
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
            # Surveillance-specific columns  
            `Data Status` = case_when(
              sourcedata.4 == 400 ~ "Not suppressed",
              sourcedata.4 == 405 ~ "Data suppressed",
              TRUE ~ as.character(sourcedata.4)
            ),
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
            Population = format(sourcedata.11, big.mark = ","),
            # Keep empty QoL columns for consistency
            `Percentage` = NA_real_,
            `CI Lower` = NA_real_,
            `CI Upper` = NA_real_
          )
      }
      
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
