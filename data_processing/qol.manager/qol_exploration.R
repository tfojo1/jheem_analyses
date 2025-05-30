# QoL/SDOH Data Exploration - Working with 2022 Data
# Now that we found working data, let's explore its structure

# Source the main script
source("data_processing/cdc_atlas/load_cdc.R")

# =============================================================================
# STEP 1: GET THE WORKING 2022 HIV STIGMA DATA AND EXAMINE ITS STRUCTURE
# =============================================================================

cat("=== EXAMINING 2022 HIV STIGMA DATA STRUCTURE ===\n")

# Make the API call that we know works
indicator_id <- get_indicator_ids("HIV Stigma")
location_ids <- get_location_ids("United States") 
year_id <- get_year_ids("2022")
age_ids <- get_age_ids("Ages 13 years and older")
race_ids <- get_race_ids("All races/ethnicities")

variable_ids <- paste(c(indicator_id, location_ids, year_id, age_ids, race_ids, 601, 801), collapse = ",")

resp <- request("https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData") %>%
  req_headers(
    "content-type" = "application/json; charset=UTF-8",
    "origin" = "https://gis.cdc.gov",
    "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html"
  ) %>%
  req_body_json(list(VariableIDs = variable_ids)) %>%
  req_perform()

hiv_stigma_raw <- jsonlite::fromJSON(resp_body_string(resp))
sourcedata_2022 <- hiv_stigma_raw$sourcedata

cat("Raw sourcedata dimensions:", dim(sourcedata_2022), "\n")
cat("Raw sourcedata structure:\n")
str(sourcedata_2022)

cat("\n--- First few rows of raw sourcedata ---\n")
print(sourcedata_2022[1:5, ])

cat("\n--- Column-by-column analysis ---\n")
for(i in 1:ncol(sourcedata_2022)) {
  cat("Column", i, "unique values:", length(unique(sourcedata_2022[,i])), 
      "| Sample values:", paste(head(unique(sourcedata_2022[,i]), 3), collapse=", "), "\n")
}

# =============================================================================
# STEP 2: COMPARE WITH SURVEILLANCE DATA STRUCTURE
# =============================================================================

cat("\n=== COMPARING WITH SURVEILLANCE DATA STRUCTURE ===\n")

# Get surveillance data for comparison
surv_variable_ids <- paste(c(208, location_ids, get_year_ids("2023"), age_ids, race_ids, 601, 801), collapse = ",")

surv_resp <- request("https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData") %>%
  req_headers(
    "content-type" = "application/json; charset=UTF-8",
    "origin" = "https://gis.cdc.gov",
    "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html"
  ) %>%
  req_body_json(list(VariableIDs = surv_variable_ids)) %>%
  req_perform()

surv_raw <- jsonlite::fromJSON(resp_body_string(surv_resp))
surv_sourcedata <- surv_raw$sourcedata

cat("Surveillance sourcedata dimensions:", dim(surv_sourcedata), "\n")
cat("Surveillance sourcedata structure:\n")
str(surv_sourcedata)

cat("\n--- First few rows of surveillance sourcedata ---\n")
print(surv_sourcedata[1:5, ])

cat("\n--- Surveillance column-by-column analysis ---\n")
for(i in 1:ncol(surv_sourcedata)) {
  cat("Column", i, "unique values:", length(unique(surv_sourcedata[,i])), 
      "| Sample values:", paste(head(unique(surv_sourcedata[,i]), 3), collapse=", "), "\n")
}

# =============================================================================
# STEP 3: TEST ALL QOL INDICATORS FOR 2022
# =============================================================================

cat("\n=== TESTING ALL QOL INDICATORS FOR 2022 ===\n")

qol_indicators <- c("HIV Stigma", "Unstable Housing or Homelessness", 
                   "Good or better self-rated health", "Unmet needs for mental health services",
                   "Hunger or food insecurity", "Unemployment")

qol_results <- list()

for(indicator in qol_indicators) {
  cat("\n--- Testing", indicator, "for 2022 ---\n")
  
  tryCatch({
    indicator_id <- get_indicator_ids(indicator)
    variable_ids <- paste(c(indicator_id, location_ids, get_year_ids("2022"), 
                           age_ids, race_ids, 601, 801), collapse = ",")
    
    resp <- request("https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData") %>%
      req_headers(
        "content-type" = "application/json; charset=UTF-8",
        "origin" = "https://gis.cdc.gov",
        "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html"
      ) %>%
      req_body_json(list(VariableIDs = variable_ids)) %>%
      req_perform()
    
    if (resp_status(resp) == 200) {
      response_data <- jsonlite::fromJSON(resp_body_string(resp))
      if (!is.null(response_data$sourcedata) && nrow(response_data$sourcedata) > 0) {
        cat("SUCCESS! Got", nrow(response_data$sourcedata), "rows\n")
        qol_results[[indicator]] <- response_data$sourcedata
        
        # Show unique locations that have data
        location_col <- response_data$sourcedata[,3]  # Based on pattern, column 3 seems to be location
        unique_locations <- unique(location_col)
        location_names <- cdc_mappings$geography$name[match(unique_locations, cdc_mappings$geography$id)]
        cat("States/regions with data:", paste(location_names[!is.na(location_names)], collapse=", "), "\n")
        
      } else {
        cat("No data available\n")
      }
    } else {
      cat("API call failed\n")
    }
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}

# =============================================================================
# STEP 4: TEST WHAT YEARS ARE AVAILABLE FOR QOL DATA
# =============================================================================

cat("\n=== TESTING AVAILABLE YEARS FOR QOL INDICATORS ===\n")

test_years <- c("2022", "2021", "2020", "2019", "2018")
hiv_stigma_years <- list()

for(year in test_years) {
  cat("Testing HIV Stigma for", year, "...")
  
  tryCatch({
    variable_ids <- paste(c(289, location_ids, get_year_ids(year), 
                           age_ids, race_ids, 601, 801), collapse = ",")
    
    resp <- request("https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData") %>%
      req_headers(
        "content-type" = "application/json; charset=UTF-8",
        "origin" = "https://gis.cdc.gov",
        "referer" = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html"
      ) %>%
      req_body_json(list(VariableIDs = variable_ids)) %>%
      req_perform()
    
    if (resp_status(resp) == 200) {
      response_data <- jsonlite::fromJSON(resp_body_string(resp))
      if (!is.null(response_data$sourcedata) && nrow(response_data$sourcedata) > 0) {
        cat(" SUCCESS! Got", nrow(response_data$sourcedata), "rows\n")
        hiv_stigma_years[[year]] <- response_data$sourcedata
      } else {
        cat(" No data\n")
      }
    } else {
      cat(" API failed\n")
    }
  }, error = function(e) {
    cat(" Error:", e$message, "\n")
  })
}

# =============================================================================
# STEP 5: SUMMARY OF FINDINGS
# =============================================================================

cat("\n=== SUMMARY OF FINDINGS ===\n")
cat("QoL indicators with data available for 2022:\n")
for(indicator in names(qol_results)) {
  cat("- ", indicator, ": ", nrow(qol_results[[indicator]]), " rows\n")
}

cat("\nYears with HIV Stigma data available:\n")
for(year in names(hiv_stigma_years)) {
  cat("- ", year, ": ", nrow(hiv_stigma_years[[year]]), " rows\n")
}

cat("\n=== READY FOR NEXT STEPS ===\n")
cat("We now understand:\n")
cat("1. QoL data structure and availability\n") 
cat("2. Which indicators have data\n")
cat("3. Which years have data\n")
cat("4. How the data differs from surveillance data\n")
