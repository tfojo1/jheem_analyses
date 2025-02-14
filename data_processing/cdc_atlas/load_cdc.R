library(jsonlite)
library(httr2)
library(readr)
library(dplyr)

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
    status = filter(mappings_df, vtid == 4),  # Added this
    race = filter(mappings_df, vtid == 5),
    sex = filter(mappings_df, vtid == 6),
    age = filter(mappings_df, vtid == 7)
  )
}


# Assume mappings are loaded
cdc_mappings <- load_cdc_mappings("data_processing/cdc_atlas/varvals.json")

# Helper functions
get_indicator_id <- function(indicator_name, mappings = cdc_mappings) {
  id <- mappings$indicators %>%
    filter(name == indicator_name) %>%
    pull(id)
  
  if(length(id) == 0) stop("Invalid indicator: ", indicator_name)
  return(id)
}

get_year_id <- function(year, mappings = cdc_mappings) {
  id <- mappings$years %>%
    filter(name == as.character(year)) %>%
    pull(id)
  
  if(length(id) == 0) stop("Invalid year: ", year)
  return(id)
}

get_location_ids <- function(state, mappings = cdc_mappings) {
  # First find state id
  state_info <- mappings$geography %>%
    filter(grepl(paste0("^", state, "$"), name))
  
  if(nrow(state_info) == 0) stop("Invalid state: ", state)
  
  # Then find all counties for that state
  county_ids <- mappings$geography %>%
    filter(parentid == state_info$id) %>%
    pull(id)
  
  return(county_ids)
}

# Main function
get_cdc_data <- function(
    indicator,
    state,
    year,
    age_groups,
    races,
    mappings = cdc_mappings
) {
  # Get IDs
  indicator_id <- get_indicator_id(indicator)
  year_id <- get_year_id(year)
  location_ids <- get_location_ids(state)
  age_ids <- mappings$age %>% filter(name %in% age_groups) %>% pull(id)
  race_ids <- mappings$race %>% filter(name %in% races) %>% pull(id)
  
  # Constants we found earlier
  sex_id <- 601  # "Both sexes"
  unknown_id <- 801  # Unknown constant required by API
  
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
    stop("Request failed: ", resp_status(resp))
  }
  
  # Parse JSON response and convert to data frame
  raw_data <- jsonlite::fromJSON(resp_body_string(resp))$sourcedata %>%
    as.data.frame() %>%
    setNames(paste0("sourcedata.", 1:ncol(.)))
  
  # Process data into final format
  processed_data <- raw_data %>%
    transmute(
      Indicator = indicator,
      Year = year,
      State = state,
      County = mappings$geography$name[match(sourcedata.3, mappings$geography$id)],
      Geography = paste0(County, ", ", State),
      FIPS = mappings$geography$fips[match(sourcedata.3, mappings$geography$id)],
      `Age Group` = mappings$age$name[match(sourcedata.7, mappings$age$id)],
      `Race/Ethnicity` = mappings$race$name[match(sourcedata.5, mappings$race$id)],
      Sex = "Both sexes",
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
  
  return(processed_data)
}


data <- get_cdc_data(
  indicator = "Early Non-Primary, Non-Secondary Syphilis",
  state = "Arizona",
  year = "2023",
  age_groups = c("0-14", "15-24"),
  races = c("Asian", "Hispanic/Latino")
)
