# Proper verification: Count actual data points in manager vs expected
library(jheem2)

# Load manager and get expected counts from the integration script output
manager <- load.data.manager("cached/surveillance.manager_PA_corrected_20250813_135234.rdata")
suppression_data <- manager$data$suppression$estimate$cdc.hiv$cdc.new

# Expected counts from integration script
expected_counts <- list(
  "age_only" = 571,
  "age_race" = 1434, 
  "age_race_transmission" = 60,
  "age_sex" = 740,
  "age_transmission" = 1434,
  "race_only" = 526,
  "race_sex" = 647,
  "race_transmission" = 1274,
  "sex_only" = 216,
  "sex_transmission" = 492,
  "total" = 131,
  "transmission_only" = 480
)

# Map to manager stratification names
strat_mapping <- list(
  "age_only" = "year__location__age",
  "race_only" = "year__location__race", 
  "sex_only" = "year__location__sex",
  "transmission_only" = "year__location__risk",
  "total" = "year__location",
  "age_race" = "year__location__age__race",
  "age_sex" = "year__location__age__sex", 
  "age_transmission" = "year__location__age__risk",
  "race_sex" = "year__location__race__sex",
  "race_transmission" = "year__location__race__risk",
  "sex_transmission" = "year__location__sex__risk",
  "age_race_transmission" = "year__location__age__race__risk"
)

cat("=== ACTUAL vs EXPECTED DATA COUNTS ===\n")
total_expected <- sum(unlist(expected_counts))
total_actual <- 0

for (raw_strat in names(expected_counts)) {
  manager_strat <- strat_mapping[[raw_strat]]
  expected <- expected_counts[[raw_strat]]
  
  if (manager_strat %in% names(suppression_data)) {
    strat_data <- suppression_data[[manager_strat]]
    dims <- dimnames(strat_data)
    
    # Count only 2022-2023 PA data (our new data)
    pa_2022_2023_count <- 0
    
    if ("PA" %in% dims$location && all(c("2022", "2023") %in% dims$year)) {
      pa_idx <- which(dims$location == "PA")
      year_2022_idx <- which(dims$year == "2022")
      year_2023_idx <- which(dims$year == "2023")
      
      for (year_idx in c(year_2022_idx, year_2023_idx)) {
        if (length(dim(strat_data)) == 2) {
          # year x location
          if (!is.na(strat_data[year_idx, pa_idx])) {
            pa_2022_2023_count <- pa_2022_2023_count + 1
          }
        } else if (length(dim(strat_data)) == 3) {
          # year x location x [dimension]
          slice_data <- strat_data[year_idx, pa_idx, ]
          pa_2022_2023_count <- pa_2022_2023_count + sum(!is.na(slice_data))
        } else if (length(dim(strat_data)) == 4) {
          # year x location x dim1 x dim2
          slice_data <- strat_data[year_idx, pa_idx, , ]
          pa_2022_2023_count <- pa_2022_2023_count + sum(!is.na(slice_data))
        } else if (length(dim(strat_data)) == 5) {
          # year x location x dim1 x dim2 x dim3
          slice_data <- strat_data[year_idx, pa_idx, , , ]
          pa_2022_2023_count <- pa_2022_2023_count + sum(!is.na(slice_data))
        }
      }
    }
    
    # Also count county data
    county_2022_2023_count <- 0
    county_locations <- dims$location[grepl("^42[0-9]+$", dims$location)]
    
    if (length(county_locations) > 0 && all(c("2022", "2023") %in% dims$year)) {
      for (county in county_locations) {
        county_idx <- which(dims$location == county)
        for (year_idx in c(year_2022_idx, year_2023_idx)) {
          if (length(dim(strat_data)) == 2) {
            if (!is.na(strat_data[year_idx, county_idx])) {
              county_2022_2023_count <- county_2022_2023_count + 1
            }
          } else if (length(dim(strat_data)) == 3) {
            slice_data <- strat_data[year_idx, county_idx, ]
            county_2022_2023_count <- county_2022_2023_count + sum(!is.na(slice_data))
          } else if (length(dim(strat_data)) == 4) {
            slice_data <- strat_data[year_idx, county_idx, , ]
            county_2022_2023_count <- county_2022_2023_count + sum(!is.na(slice_data))
          } else if (length(dim(strat_data)) == 5) {
            slice_data <- strat_data[year_idx, county_idx, , , ]
            county_2022_2023_count <- county_2022_2023_count + sum(!is.na(slice_data))
          }
        }
      }
    }
    
    actual_total <- pa_2022_2023_count + county_2022_2023_count
    total_actual <- total_actual + actual_total
    
    status <- if (actual_total == expected) "✅" else "❌"
    cat(sprintf("%s %-20s Expected: %4d  Actual: %4d\n", 
                status, raw_strat, expected, actual_total))
    
  } else {
    cat(sprintf("❌ %-20s Expected: %4d  MISSING from manager\n", 
                raw_strat, expected))
  }
}

cat("\n=== TOTAL SUMMARY ===\n")
cat(sprintf("Expected total: %d\n", total_expected))
cat(sprintf("Actual total:   %d\n", total_actual))
cat(sprintf("Match: %s\n", if (total_actual == total_expected) "✅ YES" else "❌ NO"))

if (total_actual != total_expected) {
  cat("\n❌ INTEGRATION INCOMPLETE - Not all data was integrated\n")
} else {
  cat("\n✅ INTEGRATION VERIFIED - All expected data is present\n")
}