# Original Examples from load_cdc.R
# Source the main script
source("data_processing/cdc_atlas/load_cdc.R")

# Example: Basic query
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
