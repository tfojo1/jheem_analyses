# Corrected PA viral suppression integration following established Atlas Plus SLE patterns
# This processes our downloaded PA data through the same workflow as other SLE data

library(jheem2)
library(tidyverse)

# Load manager
source("use_jheem2_package_setting.R")
manager <- load.data.manager("cached/surveillance.manager.rdata")

# Load our PA data (this is the script-downloaded data we want to use)
pa_data_file <- "data_raw/hiv_surveillance/viral_suppression_comprehensive_Pennsylvania_20250727_120208.csv"
pa_raw_data <- read.csv(pa_data_file, header = TRUE, stringsAsFactors = FALSE, colClasses = c(FIPS = "character"))

cat("Loaded PA data:", nrow(pa_raw_data), "rows\n")
cat("Columns:", paste(colnames(pa_raw_data), collapse = ", "), "\n")

# Create a data list structure like the established Atlas Plus processing
# This mimics the data.list.sle structure from atlas_plus_hiv_processing.R
pa_data_list <- list(
  list(filename = pa_data_file, data = pa_raw_data)
)

cat("\n=== Processing PA data through established SLE patterns ===\n")

# Apply the established SLE cleaning function (adapted from atlas_plus_hiv_processing.R lines 315-381)
# This uses the same outcome mappings, location mappings, etc.

# Define mappings (from atlas_plus_hiv_processing.R)
outcome.mappings = c('HIV diagnoses'='diagnoses',
                     'HIV deaths' = 'hiv.deaths',
                     'HIV prevalence' = 'diagnosed.prevalence',
                     'Linkage to HIV care' = 'linkage_1mo',
                     'Receipt of HIV medical care' = 'engagement',
                     'HIV viral suppression' = 'suppression',
                     'Knowledge of Status' = 'awareness')

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')

age.mappings = c('13-24' = '13-24 years',
                 '25-34' = '25-34 years',
                 '35-44' = '35-44 years',
                 '45-54' = '45-54 years',
                 '55+' = '55+ years')

# Process the PA data through the established SLE cleaning pattern
pa_data_clean_sle = lapply(pa_data_list, function(file){
  
  data <- file[["data"]]
  filename <- file[["filename"]]
  
  # Universal cleaning following exact SLE pattern from atlas_plus_hiv_processing.R
  data$outcome = outcome.mappings[data$Indicator]
  data = data[!is.na(data$outcome),]
  
  names(data)[names(data)=='Year'] = 'year'   
  data$year = substring(data$year, 1, 4)                                          
  data$year = as.character(data$year)
  
  # For our PA data, the percentages are in 'Rate.per.100000' column (not 'Percentage')
  names(data)[names(data)=='Rate.per.100000'] = 'Percent'
  data$Percent[data$Percent %in% c("Data suppressed")] = NA    
  data$Percent[data$Percent %in% c("Data not available")] = NA 
  data$Percent = as.numeric(data$Percent)
  data$value = (data$Percent/100)  # Convert to proportion
  
  # Location processing - check what location format we have
  if (grepl("state", filename, ignore.case = TRUE) || "Geography" %in% names(data)) {
    names(state.abb) <- state.name 
    data$Geography = gsub('[^[:alnum:] ]',"",data$Geography) # remove special chars
    names(data)[names(data)=='Geography'] = 'state'
    data$location = ifelse(data$state == "District of Columbia", "DC", state.abb[data$state])
  }
  
  # Check if we have FIPS codes for counties
  if ("FIPS" %in% names(data)) {
    data$location = as.character(data$FIPS)
  }
  
  # Demographic conditionals (following exact SLE pattern)
  if(grepl("age", filename, ignore.case = TRUE) || any(grepl("Age.Group", names(data)))) {
    data$age = age.mappings[data$Age.Group]
  }
  if(grepl("race", filename, ignore.case = TRUE) || any(grepl("Race.Ethnicity", names(data)))) {
    names(data)[names(data)=='Race.Ethnicity'] = 'race'
    data = subset(data, data$race != "Multiracial")  # SLE pattern removes multiracial
    data$race = tolower(data$race)
  }
  if(grepl("sex", filename, ignore.case = TRUE) || any(grepl("Sex", names(data)))) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  if(grepl("risk", filename, ignore.case = TRUE) || any(grepl("Transmission.Category", names(data)))) {
    data$risk = risk.mappings[data$Transmission.Category]
  }
  
  cat("Processed file with", nrow(data), "rows\n")
  cat("Unique outcomes:", paste(unique(data$outcome), collapse=", "), "\n")
  cat("Years:", paste(sort(unique(data$year)), collapse=", "), "\n")
  cat("Locations:", paste(sort(unique(data$location)), collapse=", "), "\n")
  
  list(filename, data) 
})

# Extract the cleaned data
pa_sle_data = lapply(pa_data_clean_sle, `[[`, 2)  

cat("\n=== Integrating into surveillance manager ===\n")

# Use the established SLE integration pattern (from lines 814-823 in atlas_plus_hiv_processing.R)
for (data in pa_sle_data) {
  
  cat("Integrating", nrow(data), "rows with outcome:", paste(unique(data$outcome), collapse=", "), "\n")
  
  manager$put.long.form(
    data = data,
    ontology.name = 'cdc',          # Use established ontology
    source = 'cdc.hiv',             # Use established source  
    dimension.values = list(),       # SLE pattern uses empty list
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data - Pennsylvania 2022-2023 script download')
}

cat("\n=== Integration completed ===\n")
cat("Saving updated manager...\n")

# Save the manager with timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- paste0("cached/surveillance.manager_PA_corrected_SLE_", timestamp, ".rdata")
save(manager, file = output_file)

cat("Saved to:", output_file, "\n")
cat("✅ PA viral suppression data integrated using established SLE patterns\n")