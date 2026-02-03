# CDC Atlas Plus Data Download

Download surveillance and QoL data from CDC Atlas Plus via their API.

## Usage

```r
source("data_processing/cdc_atlas/load_cdc.R")

# Basic query - single indicator, single state (includes counties)
data <- get_cdc_data(
  indicators = "Primary and Secondary Syphilis",
  locations = "Arizona",
  years = "2023",
  age_groups = "Ages 13 years and older",
  races = "All races/ethnicities"
)

# Multiple indicators
data <- get_cdc_data(
  indicators = c("Primary and Secondary Syphilis",
                 "Early Non-Primary, Non-Secondary Syphilis"),
  locations = "California",
  years = "2023",
  age_groups = "Ages 13 years and older",
  races = "All races/ethnicities"
)

# Multiple states (each state + its counties)
data <- get_cdc_data(
  indicators = "Primary and Secondary Syphilis",
  locations = c("Arizona", "California", "Texas"),
  years = c("2022", "2023"),
  age_groups = "Ages 13 years and older",
  races = "All races/ethnicities"
)

# Specific counties
data <- get_cdc_data(
  indicators = "Gonorrhea",
  locations = c("Maricopa County, AZ", "Pima County, AZ"),
  years = "2023",
  age_groups = "Ages 13 years and older",
  races = "All races/ethnicities"
)
```

## Available Syphilis Indicators

- "Primary and Secondary Syphilis"
- "Early Non-Primary, Non-Secondary Syphilis"
- "Congenital Syphilis"
- "Unknown Duration or Late Syphilis"

## List All Available Indicators

```r
source("data_processing/cdc_atlas/load_cdc.R")
print(cdc_mappings$indicators$name)
```

## Output Columns

**Note (Feb 2026):** `get_cdc_data_enhanced.R` was merged into `load_cdc.R`. The function now handles both surveillance and QoL indicators, and returns additional columns (`Data Status`, `Percentage`, `CI Lower`, `CI Upper`). For surveillance data these extra columns are NA. Existing code using named column access (e.g., `data$Cases`) is unaffected.

All queries return a data frame with these columns:

| Column | Surveillance Data | QoL Data |
|--------|-------------------|----------|
| Indicator | indicator name | indicator name |
| Year | year | year |
| State, County, Geography, FIPS | location info | location info |
| Age Group, Race/Ethnicity, Sex | demographics | demographics |
| Data Status | "Not suppressed" / "Data suppressed" | "Available" / "Not available" |
| Cases | case count or "Data suppressed" | NA |
| Rate per 100000 | rate or "Data suppressed" | NA |
| Population | population | NA |
| Percentage | NA | percentage value |
| CI Lower, CI Upper | NA | confidence interval |

## Notes

- When you pass a **state name**, the function returns state-level data AND all counties in that state
- The function makes separate API calls for each indicator/location/year combination, so large queries may be slow
- Mappings are cached locally (`varvals.json`) and auto-refresh after 30 days
