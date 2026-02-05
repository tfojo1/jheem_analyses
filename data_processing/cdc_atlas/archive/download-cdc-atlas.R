library(httr2)
library(readr)
library(dplyr)

# Helper function to validate age groups
validate_age_group <- function(age_group) {
  valid_ages <- list(
    "0-14" = 654,
    "15-19" = 657,
    "15-24" = 658
    # Add more as we discover them
  )
  
  if (!age_group %in% names(valid_ages)) {
    stop("Invalid age group. Valid options are: ", paste(names(valid_ages), collapse = ", "))
  }
  return(valid_ages[[age_group]])
}

# Helper function to validate years
validate_year <- function(year) {
  valid_years <- list(
    "2022" = 544,
    "2023" = 545
    # Add more as we discover them
  )
  
  if (!as.character(year) %in% names(valid_years)) {
    stop("Invalid year. Valid options are: ", paste(names(valid_years), collapse = ", "))
  }
  return(valid_years[[as.character(year)]])
}

# Helper function to validate indicators
validate_indicator <- function(indicator) {
  valid_indicators <- list(
    "Primary and Secondary Syphilis" = 208,
    "Early Non-Primary, Non-Secondary Syphilis" = 213
    # Add more as we discover them
  )
  
  if (!indicator %in% names(valid_indicators)) {
    stop("Invalid indicator. Valid options are: ", paste(names(valid_indicators), collapse = ", "))
  }
  return(valid_indicators[[indicator]])
}

# Helper function to validate race/ethnicity
validate_race <- function(race) {
  valid_races <- list(
    "Asian" = 553,
    "Hispanic/Latino" = 555
    # Add more as we discover them
  )
  
  if (!race %in% names(valid_races)) {
    stop("Invalid race/ethnicity. Valid options are: ", paste(names(valid_races), collapse = ", "))
  }
  return(valid_races[[race]])
}

get_cdc_data <- function(
    indicator,
    year,
    age_groups,
    races,
    base_url_data = "https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData"
) {
  # Validate inputs
  indicator_id <- validate_indicator(indicator)
  year_id <- validate_year(year)
  age_group_ids <- sapply(age_groups, validate_age_group)
  race_ids <- sapply(races, validate_race)
  
  # Fixed county codes for Arizona
  county_codes <- paste(seq(5158, 5172), collapse = ",")
  
  # Construct variable IDs string
  variable_ids <- paste(
    c(
      indicator_id,
      strsplit(county_codes, ",")[[1]],
      year_id,
      age_group_ids,
      race_ids,
      601,
      801
    ),
    collapse = ","
  )
  
  # Construct cookie string
  cookie_str <- paste(
    "ASLBSA=000356b08ae5113cb419a171181983850bf6ce864959bde9aff09a9e104326787544;",
    "ASLBSACORS=000356b08ae5113cb419a171181983850bf6ce864959bde9aff09a9e104326787544;",
    "gpv_v45=AtlasPlus%20-%20Tables;",
    "gpv_c54=https%3A%2F%2Fgis.cdc.gov%2Fgrasp%2Fnchhstpatlas%2Ftables.html"
  )
  
  # Create request with all required headers
  resp <- request(base_url_data) %>%
    req_headers(
      "accept" = "application/json, text/javascript, */*; q=0.01",
      "accept-language" = "en-US,en;q=0.9",
      "content-type" = "application/json; charset=UTF-8",
      "cookie" = cookie_str,
      "origin" = "https://gis.cdc.gov",
      "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html",
      "x-requested-with" = "XMLHttpRequest",
      "sec-fetch-dest" = "empty",
      "sec-fetch-mode" = "cors",
      "sec-fetch-site" = "same-origin",
      "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36"
    ) %>%
    req_body_json(list(VariableIDs = variable_ids)) %>%
    req_error(is_error = function(resp) FALSE)
  
  # Perform request
  response <- resp %>% req_perform()
  
  # Check response
  if (resp_status(response) != 200) {
    stop("Request failed with status: ", resp_status(response),
         "\nError message: ", resp_body_string(response))
  }
  
  # Add debug output
  message("Response content:")
  message(resp_body_string(response))
  
  # Parse the JSON response
  data <- tryCatch({
    jsonlite::fromJSON(resp_body_string(response))
  }, error = function(e) {
    message("Failed to parse JSON: ", e$message)
    message("Raw response: ", resp_body_string(response))
    return(NULL)
  })
  
  # Convert to data frame
  if(is.null(data) || length(data) == 0) {
    stop("No data returned from the server")
  }
  
  return(as.data.frame(data))
}

process_cdc_data <- function(raw_data) {
  # County and FIPS code mappings
  county_fips_map <- tibble(
    code = c(5158, 5159, 5160, 5161, 5162, 5163, 5164, 5165, 5166, 5167, 5168, 5169, 5170, 5171, 5172),
    county = c("Apache County", "Cochise County", "Coconino County", "Gila County", "Graham County", 
               "Greenlee County", "La Paz County", "Maricopa County", "Mohave County", "Navajo County",
               "Pima County", "Pinal County", "Santa Cruz County", "Yavapai County", "Yuma County"),
    fips = c("04001", "04003", "04005", "04007", "04009", 
             "04011", "04012", "04013", "04015", "04017",
             "04019", "04021", "04023", "04025", "04027")
  )
  
  processed_data <- raw_data %>%
    transmute(
      Indicator = "Early Non-Primary, Non-Secondary Syphilis",  # From code 213
      Year = if_else(sourcedata.2 == 545, 2023, 2022),
      State = "AZ",
      County = county_fips_map$county[match(sourcedata.3, county_fips_map$code)],
      Geography = paste0(County, ", AZ"),
      FIPS = county_fips_map$fips[match(sourcedata.3, county_fips_map$code)],
      `Age Group` = if_else(sourcedata.7 == 654, "0-14", "15-24"),
      `Race/Ethnicity` = if_else(sourcedata.5 == 553, "Asian", "Hispanic/Latino"),
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

# Modified try_get_cdc_data function
try_get_cdc_data <- function(...) {
  tryCatch({
    raw_data <- get_cdc_data(...)
    if (!is.null(raw_data)) {
      return(process_cdc_data(raw_data))
    }
    return(NULL)
  }, error = function(e) {
    message("Error retrieving or processing data: ", e$message)
    return(NULL)
  })
}

# Example: Get data for both Asian and Hispanic/Latino populations, ages 0-14 and 15-24
data <- try_get_cdc_data(
  indicator = "Early Non-Primary, Non-Secondary Syphilis",
  year = 2023,
  age_groups = c("0-14", "15-24"),
  races = c("Asian", "Hispanic/Latino")
)

if (!is.null(data)) {
  # View the results
  print(data)
  
  # Optionally save to file
  #write_csv(data, paste0("cdc_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
}
