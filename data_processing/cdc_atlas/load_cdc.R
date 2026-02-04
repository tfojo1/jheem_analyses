library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Function to fetch updated mappings from CDC API
update_cdc_mappings <- function(save_path = "data_processing/cdc_atlas/varvals.json", force_update = FALSE) {
  # Check if file exists and is recent (less than 30 days old)
  if (file.exists(save_path) && !force_update) {
    file_age <- difftime(Sys.time(), file.mtime(save_path), units = "days")
    if (file_age < 30) {
      message("Using existing mappings file (created ", round(file_age, 1), " days ago). Use force_update=TRUE to refresh.")
      return(invisible(NULL))
    }
  }
  
  message("Fetching updated mappings from CDC API...")
  
  # Make request to CDC API
  resp <- request("https://gis.cdc.gov/grasp/AtlasPlus/getInitData/00") %>%
    req_headers(
      "accept" = "application/json, text/javascript, */*; q=0.01",
      "content-type" = "application/json; charset=utf-8",
      "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html",
      "x-requested-with" = "XMLHttpRequest"
    ) %>%
    req_perform()
  
  if (resp_status(resp) != 200) {
    stop("Failed to fetch mappings from CDC API: ", resp_status(resp))
  }
  
  # Parse response
  full_response <- jsonlite::fromJSON(resp_body_string(resp))
  
  # Extract just the varvals portion for compatibility with existing code
  varvals_data <- full_response$varvals
  
  # Save to file
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  write(jsonlite::toJSON(varvals_data, pretty = TRUE), save_path)
  
  message("Updated mappings saved to: ", save_path)
  
  # Also save the full response for potential future use
  full_save_path <- gsub("\\.json$", "_full.json", save_path)
  write(jsonlite::toJSON(full_response, pretty = TRUE), full_save_path)
  message("Full response saved to: ", full_save_path)
  
  return(invisible(varvals_data))
}

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

# Update mappings if needed (checks if file is older than 30 days)
update_cdc_mappings()

# Load mappings
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

get_sex_ids <- function(sexes, mappings = cdc_mappings) {
  ids <- mappings$sex %>%
    filter(name %in% sexes) %>%
    pull(id)

  if(length(ids) == 0) stop("Invalid sexes: ", paste(sexes, collapse=", "))
  return(ids)
}

# Main function that supports multiple parameters
# Handles both surveillance data and QoL (Quality of Life) indicators
get_cdc_data <- function(
    indicators,
    locations,
    years,
    age_groups,
    races,
    sex = "Both sexes",
    mappings = cdc_mappings
) {
  # QoL indicators (handled differently from surveillance data)
  qol_indicators <- c("HIV Stigma", "Unstable Housing or Homelessness",
                      "Good or better self-rated health", "Unmet needs for mental health services",
                      "Hunger or food insecurity", "Unemployment")

  # Convert single values to vectors
  indicators <- if(length(indicators) == 1) c(indicators) else indicators
  locations <- if(length(locations) == 1) c(locations) else locations
  years <- if(length(years) == 1) c(years) else as.character(years)
  age_groups <- if(length(age_groups) == 1) c(age_groups) else age_groups
  races <- if(length(races) == 1) c(races) else races
  sex <- if(length(sex) == 1) c(sex) else sex

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
    sex_ids <- get_sex_ids(sex)

    unknown_id <- 801  # Required by API

    # Construct variable_ids string
    variable_ids <- paste(
      c(
        indicator_id,
        location_ids,
        year_id,
        age_ids,
        race_ids,
        sex_ids,
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

      raw_data <- as.data.frame(raw_data) %>%
        setNames(paste0("sourcedata.", 1:ncol(.)))

      is_qol_indicator <- indicator %in% qol_indicators

      # Process data based on whether it's QoL or surveillance
      if (is_qol_indicator) {
        # QoL data processing
        processed_data <- raw_data %>%
          mutate(
            location_name = mappings$geography$name[match(sourcedata.3, mappings$geography$id)],
            parent_id = mappings$geography$parentid[match(sourcedata.3, mappings$geography$id)],
            parent_name = mappings$geography$name[match(parent_id, mappings$geography$id)],
            grandparent_id = mappings$geography$parentid[match(parent_id, mappings$geography$id)],
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
            Sex = mappings$sex$name[match(sourcedata.6, mappings$sex$id)],
            `Data Status` = case_when(
              sourcedata.4 == 404 ~ "Available",
              sourcedata.4 == 408 ~ "Not available",
              TRUE ~ as.character(sourcedata.4)
            ),
            Percentage = case_when(
              sourcedata.4 == 404 ~ sourcedata.9,
              TRUE ~ NA_real_
            ),
            `CI Lower` = case_when(
              sourcedata.4 == 404 ~ sourcedata.12,
              TRUE ~ NA_real_
            ),
            `CI Upper` = case_when(
              sourcedata.4 == 404 ~ sourcedata.13,
              TRUE ~ NA_real_
            ),
            Cases = NA_character_,
            `Rate per 100000` = NA_character_,
            Population = NA_character_
          )
      } else {
        # Surveillance data processing
        processed_data <- raw_data %>%
          mutate(
            location_name = mappings$geography$name[match(sourcedata.3, mappings$geography$id)],
            parent_id = mappings$geography$parentid[match(sourcedata.3, mappings$geography$id)],
            parent_name = mappings$geography$name[match(parent_id, mappings$geography$id)],
            grandparent_id = mappings$geography$parentid[match(parent_id, mappings$geography$id)],
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
            Sex = mappings$sex$name[match(sourcedata.6, mappings$sex$id)],
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
            Percentage = NA_real_,
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
