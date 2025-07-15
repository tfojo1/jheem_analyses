# Enhanced Problem Demonstration: Syphilis Data Quality Issues
# Shows the root cause: NaN values from incomplete county-level race data

library(locations)

# Load data
load("cached/syphilis.manager.rdata")

cat("=== SYPHILIS DATA QUALITY PROBLEM DEMONSTRATION ===\n\n")

# Focus on clear examples that show the pattern
examples <- list(
  list(msa = "C.12580", msa_name = "Baltimore", year = "2022", outcome = "ps.syphilis.diagnoses", ontology = "cdc.sti"),
  list(msa = "C.12060", msa_name = "Atlanta", year = "2022", outcome = "unknown.duration.or.late.syphilis.diagnoses", ontology = "cdc.sti.two")
)

for (example in examples) {
  msa <- example$msa
  msa_name <- example$msa_name  
  year <- example$year
  outcome <- example$outcome
  ontology <- example$ontology
  
  cat(sprintf("=== %s MSA %s: %s ===\n", msa_name, year, toupper(gsub("\\.", " ", outcome))))
  
  # 1. Show the MSA-level aggregated data with NaN values
  cat("\n1. MSA-LEVEL AGGREGATED RACE DATA (what models see):\n")
  msa_total <- syphilis.manager$data[[outcome]]$estimate$cdc.aggregated.county[[ontology]]$year__location[year, msa]
  msa_race_data <- syphilis.manager$data[[outcome]]$estimate$cdc.aggregated.county[[ontology]]$year__location__race[year, msa, ]
  
  cat(sprintf("   Total cases: %d\n", msa_total))
  cat("   Race breakdown:\n")
  for (i in seq_along(msa_race_data)) {
    race_name <- names(msa_race_data)[i]
    value <- msa_race_data[i]
    if (is.nan(value)) {
      cat(sprintf("     %s: NaN ⚠️\n", race_name))
    } else {
      cat(sprintf("     %s: %d\n", race_name, value))
    }
  }
  
  race_sum <- sum(msa_race_data, na.rm = TRUE)
  cat(sprintf("   Race sum (ignoring NaN): %d\n", race_sum))
  cat(sprintf("   Missing from race data: %d (%.1f%%)\n", 
              msa_total - race_sum, (msa_total - race_sum) / msa_total * 100))
  
  # 2. Show the county-level breakdown that explains WHY
  cat("\n2. COUNTY-LEVEL BREAKDOWN (why this happens):\n")
  all_counties <- locations::get.contained.locations(msa, "COUNTY")
  
  county_strat_data <- syphilis.manager$data[[outcome]]$estimate$cdc.sti[[ontology]]$year__location__race
  county_total_data <- syphilis.manager$data[[outcome]]$estimate$cdc.sti[[ontology]]$year__location
  
  cat("   County-by-county analysis:\n")
  total_county_cases <- 0
  total_county_race_cases <- 0
  problem_counties <- character(0)
  
  for (county in all_counties) {
    if (county %in% dimnames(county_strat_data)$location && 
        county %in% dimnames(county_total_data)$location) {
      
      county_total <- county_total_data[year, county]
      county_race <- county_strat_data[year, county, ]
      county_race_sum <- sum(county_race, na.rm = TRUE)
      
      if (!is.na(county_total) && county_total > 0) {
        coverage <- county_race_sum / county_total * 100
        total_county_cases <- total_county_cases + county_total
        total_county_race_cases <- total_county_race_cases + county_race_sum
        
        if (coverage < 80) {
          problem_counties <- c(problem_counties, county)
          cat(sprintf("     %s: %d total, %d race (%.1f%% coverage) ⚠️ PROBLEM\n", 
                     county, county_total, county_race_sum, coverage))
        } else {
          cat(sprintf("     %s: %d total, %d race (%.1f%% coverage) ✓\n", 
                     county, county_total, county_race_sum, coverage))
        }
      }
    }
  }
  
  # 3. Show the aggregation math
  cat("\n3. HOW AGGREGATION CREATES THE PROBLEM:\n")
  cat(sprintf("   Sum of county totals: %d\n", total_county_cases))
  cat(sprintf("   Sum of county race data: %d\n", total_county_race_cases))
  cat(sprintf("   MSA shows race sum as: %d (same as county race sum)\n", race_sum))
  cat(sprintf("   But MSA total is: %d (same as county total sum)\n", msa_total))
  cat("\n   ⚠️  The aggregation process treats missing race data as 0,\n")
  cat("       creating an inconsistency between totals and race sums!\n")
  
  if (length(problem_counties) > 0) {
    cat(sprintf("\n   Problem counties (%d): %s\n", 
               length(problem_counties), paste(problem_counties, collapse = ", ")))
  }
  
  cat(paste0("\n", strrep("=", 60), "\n\n"))
}

# 4. Show systematic nature across MSAs
cat("4. SYSTEMATIC NATURE ACROSS MSAs:\n")
cat("Race coverage for PS syphilis 2022:\n")

problem_msas <- c("C.12580", "C.35620", "C.33100", "C.12060")
msa_names <- c("Baltimore", "NYC", "Miami", "Atlanta")

for (i in seq_along(problem_msas)) {
  msa_code <- problem_msas[i]
  msa_name <- msa_names[i]
  
  ps_tot <- syphilis.manager$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location["2022", msa_code]
  ps_race <- syphilis.manager$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location__race["2022", msa_code, ]
  
  if (!is.na(ps_tot) && !is.null(ps_race)) {
    ps_race_sum <- sum(ps_race, na.rm = TRUE)
    coverage <- ps_race_sum / ps_tot * 100
    has_nan <- any(is.nan(ps_race))
    
    status <- if (coverage < 90 || has_nan) "⚠️ PROBLEMATIC" else "✓ OK"
    cat(sprintf("   %s: %.1f%% coverage, Has NaN: %s %s\n", 
               msa_name, coverage, has_nan, status))
  } else {
    cat(sprintf("   %s: No data\n", msa_name))
  }
}

cat("\n=== KEY INSIGHTS ===\n")
cat("🔍 ROOT CAUSE: Some counties have total case counts but incomplete race data\n")
cat("⚠️  AGGREGATION BUG: MSA aggregation treats missing race data as 0\n") 
cat("📊 RESULT: Race stratifications severely undercount compared to totals\n")
cat("🎯 SOLUTION: Remove stratifications where counties with good race data\n")
cat("             represent <90% of MSA population\n")

cat("\n=== END DEMONSTRATION ===\n")
