# Test the Enhanced Function
# Testing the new get_cdc_data_enhanced function that handles both QoL and surveillance data

# Source the main script and enhanced function
source("data_processing/cdc_atlas/load_cdc.R")
source("data_processing/cdc_atlas/get_cdc_data_enhanced.R")

cat("=== TESTING ENHANCED FUNCTION ===\n")

# Test 1: QoL indicator (HIV Stigma)
cat("\n--- Test 1: QoL Indicator (HIV Stigma) ---\n")
qol_data <- get_cdc_data_enhanced(
    indicators = "HIV Stigma",
    locations = "United States",
    years = "2022",
    age_groups = "Ages 13 years and older",
    races = "All races/ethnicities"
)

if (!is.null(qol_data)) {
    cat("SUCCESS! Got", nrow(qol_data), "rows\n")
    cat("Columns:", paste(names(qol_data), collapse=", "), "\n")
    cat("Sample QoL data:\n")
    print(head(qol_data %>% select(Geography, `Data Status`, Percentage, `CI Lower`, `CI Upper`), 3))
} else {
    cat("FAILED - No data returned\n")
}

# Test 2: Surveillance indicator 
cat("\n--- Test 2: Surveillance Indicator (Syphilis) ---\n")
surv_data <- get_cdc_data_enhanced(
    indicators = "Primary and Secondary Syphilis",
    locations = "United States", 
    years = "2023",
    age_groups = "Ages 13 years and older",
    races = "All races/ethnicities"
)

if (!is.null(surv_data)) {
    cat("SUCCESS! Got", nrow(surv_data), "rows\n")
    cat("Columns:", paste(names(surv_data), collapse=", "), "\n")
    cat("Sample surveillance data:\n")
    print(head(surv_data %>% select(Geography, `Data Status`, Cases, `Rate per 100000`, Population), 3))
} else {
    cat("FAILED - No data returned\n")
}

# Test 3: Multiple QoL indicators
cat("\n--- Test 3: Multiple QoL Indicators ---\n")
multi_qol <- get_cdc_data_enhanced(
    indicators = c("HIV Stigma", "Unstable Housing or Homelessness"),
    locations = c("United States", "California", "New York"),
    years = "2022",
    age_groups = "Ages 13 years and older", 
    races = "All races/ethnicities"
)

if (!is.null(multi_qol)) {
    cat("SUCCESS! Got", nrow(multi_qol), "rows\n")
    cat("Indicators:", paste(unique(multi_qol$Indicator), collapse=", "), "\n")
    cat("Locations:", paste(unique(multi_qol$Geography), collapse=", "), "\n")
    
    # Show some sample data for each indicator
    for(indicator in unique(multi_qol$Indicator)) {
        cat("\n", indicator, "sample:\n")
        sample_data <- multi_qol %>% 
            filter(Indicator == indicator, !is.na(Percentage)) %>%
            select(Geography, Percentage, `CI Lower`, `CI Upper`) %>%
            head(3)
        print(sample_data)
    }
} else {
    cat("FAILED - No data returned\n")
}

# Test 4: Show data that's available vs not available
cat("\n--- Test 4: Data Availability Status ---\n")
if (!is.null(multi_qol)) {
    available_count <- sum(multi_qol$`Data Status` == "Available", na.rm = TRUE)
    not_available_count <- sum(multi_qol$`Data Status` == "Not available", na.rm = TRUE)
    
    cat("Available data points:", available_count, "\n")
    cat("Not available data points:", not_available_count, "\n")
    
    # Show which locations have data vs don't
    availability_summary <- multi_qol %>%
        group_by(Geography, `Data Status`) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(Geography)
    
    cat("Availability by location:\n")
    print(availability_summary)
}

cat("\n=== ENHANCED FUNCTION TESTING COMPLETE ===\n")
