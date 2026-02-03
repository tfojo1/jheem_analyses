library(httr2)
library(readr)
library(dplyr)

# Validation/lookup functions to document known codes
known_codes <- list(
  indicators = c(
    "Primary and Secondary Syphilis" = "208",
    "Early Non-Primary, Non-Secondary Syphilis" = "213"
  ),
  years = c(
    "2022" = "544",
    "2023" = "545"
  ),
  age_groups = c(
    "0-14" = "654",
    "15-19" = "657",
    "15-24" = "658"
  ),
  races = c(
    "Asian" = "553",
    "Hispanic/Latino" = "555"
  ),
  # Document the constant components we've observed
  constants = c(
    "sex_both" = "601",
    "unknown" = "801"
  )
)

# Core function to fetch raw data
get_cdc_raw_data <- function(variable_ids) {
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
  
  # Parse JSON response
  json_data <- jsonlite::fromJSON(resp_body_string(resp))
  
  # Convert to data frame with proper column names
  raw_data <- as.data.frame(json_data$sourcedata) %>%
    setNames(paste0("sourcedata.", 1:ncol(.)))
  
  return(raw_data)
}

# Process raw data into CDC format
process_cdc_data <- function(raw_data, known_mappings = known_codes) {
  processed_data <- raw_data %>%
    transmute(
      Indicator = case_when(
        sourcedata.1 %in% known_mappings$indicators ~ names(known_mappings$indicators)[match(sourcedata.1, known_mappings$indicators)],
        TRUE ~ paste("Code:", sourcedata.1)
      ),
      Year = case_when(
        sourcedata.2 %in% known_mappings$years ~ as.integer(names(known_mappings$years)[match(sourcedata.2, known_mappings$years)]),
        TRUE ~ NA_integer_
      ),
      State = NA_character_,  # To be determined from geography codes
      County = as.character(sourcedata.3),  # Keep raw code until we know what it means
      Geography = NA_character_,  # To be built from State/County
      FIPS = NA_character_,    # To be determined from geography codes
      `Age Group` = case_when(
        sourcedata.7 %in% known_mappings$age_groups ~ names(known_mappings$age_groups)[match(sourcedata.7, known_mappings$age_groups)],
        TRUE ~ paste("Code:", sourcedata.7)
      ),
      `Race/Ethnicity` = case_when(
        sourcedata.5 %in% known_mappings$races ~ names(known_mappings$races)[match(sourcedata.5, known_mappings$races)],
        TRUE ~ paste("Code:", sourcedata.5)
      ),
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

# Main function that combines components
get_cdc_data <- function(
    indicator,
    geography_codes,
    year,
    age_groups,
    races,
    known_mappings = known_codes
) {
  # Build variable_ids string, documenting each component
  components <- list(
    indicator = if(indicator %in% names(known_mappings$indicators)) 
      known_mappings$indicators[indicator] else indicator,
    geography = geography_codes,
    year = if(year %in% names(known_mappings$years)) 
      known_mappings$years[year] else year,
    age = if(all(age_groups %in% names(known_mappings$age_groups))) 
      known_mappings$age_groups[age_groups] else age_groups,
    race = if(all(races %in% names(known_mappings$races))) 
      known_mappings$races[races] else races,
    constants = unname(known_mappings$constants)
  )
  
  # Construct final string
  variable_ids <- paste(
    unlist(components),
    collapse = ","
  )
  
  # Get and process data
  raw_data <- get_cdc_raw_data(variable_ids)
  processed_data <- process_cdc_data(raw_data, known_mappings)
  
  return(processed_data)
}


data <- get_cdc_data(
  indicator = "Early Non-Primary, Non-Secondary Syphilis",
  geography_codes = 5158:5172,  # AZ counties
  year = "2023",
  age_groups = c("0-14", "15-24"),
  races = c("Asian", "Hispanic/Latino")
)
