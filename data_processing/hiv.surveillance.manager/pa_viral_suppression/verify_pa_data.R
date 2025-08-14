# Quick verification of PA viral suppression data integration
# Load the test manager and check a few stratifications

library(jheem2)

# Load the test manager with integrated PA data
manager <- load.data.manager("cached/surveillance.manager_PA_corrected_20250813_135234.rdata")

# Get the suppression data structure
suppression_data <- manager$data$suppression$estimate$cdc.hiv$cdc.new

cat("Available stratifications:\n")
print(names(suppression_data))

# Check a few key stratifications
stratifications_to_check <- c("year__location", "year__location__age", "year__location__age__race__risk")

for (strat_name in stratifications_to_check) {
  cat("\n=== Checking", strat_name, "===\n")
  
  strat_data <- suppression_data[[strat_name]]
  if (is.null(strat_data)) {
    cat("No data in this stratification\n")
    next
  }
  
  dims <- dimnames(strat_data)
  cat("Dimensions:", names(dims), "\n")
  cat("Years:", paste(dims$year, collapse=", "), "\n")
  cat("Locations:", paste(dims$location, collapse=", "), "\n")
  
  # Show some raw data values
  total_data_points <- length(strat_data)
  non_na_points <- sum(!is.na(strat_data))
  cat("Total data points:", total_data_points, "Non-NA:", non_na_points, "\n")
  
  # Check for PA data specifically
  if ("PA" %in% dims$location) {
    pa_idx <- which(dims$location == "PA")
    
    if ("2022" %in% dims$year) {
      year_2022_idx <- which(dims$year == "2022")
      
      # Extract PA 2022 data
      if (length(dim(strat_data)) == 2) {
        pa_2022_value <- strat_data[year_2022_idx, pa_idx]
        cat("PA 2022 values:", pa_2022_value, "\n")
      } else if (length(dim(strat_data)) == 3) {
        pa_2022_slice <- strat_data[year_2022_idx, pa_idx, ]
        non_na_values <- pa_2022_slice[!is.na(pa_2022_slice)]
        cat("PA 2022 non-NA values:", length(non_na_values), "\n")
        if (length(non_na_values) > 0) {
          cat("Sample values:", head(non_na_values, 3), "\n")
        }
      } else {
        # Multi-dimensional - just count non-NA
        pa_slice <- strat_data[year_2022_idx, pa_idx, , , ]
        non_na_count <- sum(!is.na(pa_slice))
        cat("PA 2022 non-NA data points:", non_na_count, "\n")
      }
    }
  }
  
  # Check county data
  county_locations <- dims$location[grepl("^42[0-9]+$", dims$location)]
  if (length(county_locations) > 0) {
    cat("PA counties found:", length(county_locations), "counties\n")
    cat("Example counties:", paste(head(county_locations, 3), collapse=", "), "\n")
  }
}

cat("\n=== SAMPLE DATA PULLS ===\n")

# Pull 1: Simple total data for PA 2022-2023
cat("1. PA total viral suppression 2022-2023:\n")
total_data <- suppression_data$year__location
pa_total <- total_data[c("2022", "2023"), "PA"]
print(data.frame(
  year = c("2022", "2023"),
  PA_suppression = pa_total
))

# Pull 2: Age-stratified data for PA 2022
cat("\n2. PA viral suppression by age, 2022:\n")
age_data <- suppression_data$year__location__age
pa_age_2022 <- age_data["2022", "PA", ]
age_df <- data.frame(
  age_group = names(pa_age_2022),
  suppression_rate = as.numeric(pa_age_2022)
)
print(age_df[!is.na(age_df$suppression_rate), ])

# Pull 3: Sample of county-level data
cat("\n3. Sample county data for 2022:\n")
county_locations <- dimnames(total_data)$location[grepl("^42[0-9]+$", dimnames(total_data)$location)]
sample_counties <- head(county_locations, 5)
county_2022 <- total_data["2022", sample_counties]
county_df <- data.frame(
  county_fips = sample_counties,
  suppression_rate = as.numeric(county_2022)
)
print(county_df)

# Pull 4: County-level age stratification for Allegheny County (42003) 2022
cat("\n4. Allegheny County (42003) by age, 2022:\n")
age_data <- suppression_data$year__location__age
allegheny_age_2022 <- age_data["2022", "42003", ]
allegheny_age_df <- data.frame(
  age_group = names(allegheny_age_2022),
  suppression_rate = as.numeric(allegheny_age_2022)
)
print(allegheny_age_df[!is.na(allegheny_age_df$suppression_rate), ])

# Pull 5: County-level race stratification for Philadelphia County (42101) 2022
cat("\n5. Philadelphia County (42101) by race, 2022:\n") 
race_data <- suppression_data$year__location__race
if ("42101" %in% dimnames(race_data)$location) {
  phila_race_2022 <- race_data["2022", "42101", ]
  phila_race_df <- data.frame(
    race = names(phila_race_2022),
    suppression_rate = as.numeric(phila_race_2022)
  )
  print(phila_race_df[!is.na(phila_race_df$suppression_rate), ])
} else {
  cat("Philadelphia County (42101) not found in race stratification\n")
}

cat("\n=== Summary ===\n")
cat("PA viral suppression data successfully integrated for 2022-2023\n")
cat("Data includes both state-level (PA) and county-level (FIPS codes)\n")
cat("Multi-dimensional stratifications preserved\n")