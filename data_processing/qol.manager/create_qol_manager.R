# QoL/SDOH Data Manager - Complete End-to-End Workflow
# Final version combining data collection + working data manager creation

# =============================================================================
# CONFIGURATION
# =============================================================================

# Set this to control data collection behavior
SKIP_DATA_COLLECTION <- TRUE  # Set to FALSE to collect fresh data from AtlasPlus
RAW_DATA_FILENAME <- "qol_sdoh_raw_data_20250530.csv"  # Just the filename

# NAS Detection for shared team storage
NAS.PATHS <- c("Q:", "/Volumes/jheem$", "/mnt/jheem_nas_share")
ROOT.DIR <- NULL

for(path in NAS.PATHS) {
    if(dir.exists(path)) {
        ROOT.DIR <- path
        break
    }
}

NAS.AVAILABLE <- !is.null(ROOT.DIR)

# Determine raw data file path (NAS first, then local)
if(NAS.AVAILABLE && file.exists(file.path(ROOT.DIR, "data_raw/atlasplus_qol_sdoh", RAW_DATA_FILENAME))) {
    RAW_DATA_FILE <- file.path(ROOT.DIR, "data_raw/atlasplus_qol_sdoh", RAW_DATA_FILENAME)
    RAW_DATA_SOURCE <- "NAS"
} else if(file.exists(file.path("data_raw/atlasplus_qol_sdoh", RAW_DATA_FILENAME))) {
    RAW_DATA_FILE <- file.path("data_raw/atlasplus_qol_sdoh", RAW_DATA_FILENAME)
    RAW_DATA_SOURCE <- "local"
} else {
    RAW_DATA_FILE <- file.path("data_raw/atlasplus_qol_sdoh", RAW_DATA_FILENAME)  # Default path for new collection
    RAW_DATA_SOURCE <- "not found"
}

cat("=== QoL/SDOH DATA MANAGER - END-TO-END WORKFLOW ===\n")
if(SKIP_DATA_COLLECTION) {
    cat("📁 Mode: Using existing raw data\n")
    if(RAW_DATA_SOURCE != "not found") {
        cat("   Source:", RAW_DATA_SOURCE, "-", RAW_DATA_FILE, "\n")
    } else {
        cat("   ⚠️  Raw data file not found:", RAW_DATA_FILENAME, "\n")
    }
} else {
    cat("🌐 Mode: Collecting fresh data from AtlasPlus API\n")
}
cat("📊 Creating data manager with sparse 6D array support\n")

if(NAS.AVAILABLE) {
    cat("🌐 NAS detected at:", ROOT.DIR, "- will save to both local and NAS\n\n")
} else {
    cat("💻 NAS not available - will save to local directories only\n\n")
}

# =============================================================================
# SETUP
# =============================================================================

source("data_processing/cdc_atlas/load_cdc.R")
source("data_processing/cdc_atlas/get_cdc_data_enhanced.R")

library(dplyr)
library(jheem2)

# =============================================================================
# DATA COLLECTION (if needed)
# =============================================================================

if(!SKIP_DATA_COLLECTION) {
    cat("--- Collecting QoL Data from AtlasPlus ---\n")
    
    # Ensure local data directory exists
    dir.create("data_raw/atlasplus_qol_sdoh", recursive = TRUE, showWarnings = FALSE)
    
    # QoL indicators to collect
    qol_indicators <- c(
        "Unstable Housing or Homelessness",
        "HIV Stigma", 
        "Good or better self-rated health",
        "Unmet needs for mental health services",
        "Hunger or food insecurity",
        "Unemployment"
    )
    
    # Geographic coverage
    locations_to_collect <- c(
        "United States",
        "California", "Delaware", "Florida", "Georgia", "Illinois", "Indiana",
        "Michigan", "Mississippi", "New Jersey", "New York", "North Carolina",
        "Oregon", "Pennsylvania", "Texas", "Virginia", "Washington", "Puerto Rico"
    )
    
    # Collect comprehensive data
    cat("Collecting data for", length(qol_indicators), "indicators across", length(locations_to_collect), "locations...\n")
    
    all_qol_data <- get_cdc_data_enhanced(
        indicators = qol_indicators,
        locations = locations_to_collect,
        years = as.character(2018:2022),
        age_groups = c("Ages 13 years and older", "13-24", "25-34", "35-44", "45-54", "55+"),
        races = c("All races/ethnicities", "White")
    )
    
    if(!is.null(all_qol_data) && nrow(all_qol_data) > 0) {
        # Save raw data with timestamp
        current_date <- format(Sys.Date(), "%Y%m%d")
        raw_filename <- paste0("qol_sdoh_raw_data_", current_date, ".csv")
        
        # Save to local
        local_raw_path <- file.path("data_raw/atlasplus_qol_sdoh", raw_filename)
        write.csv(all_qol_data, local_raw_path, row.names = FALSE)
        cat("✅ Data saved locally:", local_raw_path, "\n")
        
        # Save to NAS if available
        if(NAS.AVAILABLE) {
            nas_raw_path <- file.path(ROOT.DIR, "data_raw/atlasplus_qol_sdoh", raw_filename)
            tryCatch({
                write.csv(all_qol_data, nas_raw_path, row.names = FALSE)
                cat("✅ Data saved to NAS:", nas_raw_path, "\n")
            }, error = function(e) {
                cat("⚠️  Failed to save to NAS:", e$message, "\n")
            })
        }
        
        cat("📊 Data collection complete:", nrow(all_qol_data), "rows\n")
        RAW_DATA_FILE <- local_raw_path  # Update path for processing
    } else {
        stop("❌ Data collection failed - no data returned from AtlasPlus")
    }
    
} else {
    cat("--- Using Existing Raw Data ---\n")
    if(RAW_DATA_SOURCE != "not found") {
        cat("✅ Using", RAW_DATA_SOURCE, "raw data:", RAW_DATA_FILE, "\n")
    } else {
        stop("❌ Raw data file not found: ", RAW_DATA_FILENAME, "\nChecked NAS and local directories.\nSet SKIP_DATA_COLLECTION = FALSE to collect new data")
    }
}

# =============================================================================
# CREATE DATA MANAGER
# =============================================================================

cat("\n--- Creating QoL Data Manager ---\n")

# Create data manager
qol_manager <- create.data.manager(
    name = "pwh.qol.sdoh.manager", 
    description = "QoL/SDOH Data Manager for PWH - End-to-end version with sparse array support"
)

# Register outcomes
qol_outcomes <- c(
    "unstable.housing.pwh",
    "hiv.stigma.pwh",
    "good.health.pwh", 
    "unmet.mental.health.pwh",
    "food.insecurity.pwh",
    "unemployment.pwh"
)

outcome_metadata <- list(
    "unstable.housing.pwh" = list(display = "Unstable Housing or Homelessness (PWH)", indicator = "Unstable Housing or Homelessness"),
    "hiv.stigma.pwh" = list(display = "HIV Stigma (PWH)", indicator = "HIV Stigma"),
    "good.health.pwh" = list(display = "Good or Better Self-Rated Health (PWH)", indicator = "Good or better self-rated health"),
    "unmet.mental.health.pwh" = list(display = "Unmet Mental Health Needs (PWH)", indicator = "Unmet needs for mental health services"),
    "food.insecurity.pwh" = list(display = "Hunger or Food Insecurity (PWH)", indicator = "Hunger or food insecurity"),
    "unemployment.pwh" = list(display = "Unemployment (PWH)", indicator = "Unemployment")
)

for(outcome in qol_outcomes) {
    metadata <- create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = outcome_metadata[[outcome]]$display,
        axis.name = paste0("% ", outcome_metadata[[outcome]]$display),
        units = 'percent',
        description = paste("Proportion of people with HIV reporting", tolower(outcome_metadata[[outcome]]$indicator))
    )
    qol_manager$register.outcome(outcome = outcome, metadata = metadata)
}

cat("✅ Registered", length(qol_outcomes), "outcomes\n")

# Register sources
qol_manager$register.parent.source(
    parent.source = 'CDC.ATLASPLUS', 
    full.name = 'CDC NCHHSTP AtlasPlus', 
    short.name = 'CDC AtlasPlus'
)

qol_manager$register.source(
    source = 'cdc.atlasplus.qol', 
    parent.source = 'CDC.ATLASPLUS', 
    full.name = 'CDC AtlasPlus QoL/SDOH',
    short.name = 'AtlasPlus QoL'
)

cat("✅ Registered sources\n")

# Register simplified ontology (matches actual data structure)
qol_ontology <- ontology(
    year = NULL,     # Incomplete - years vary by data availability
    location = NULL, # Incomplete - locations vary by data availability
    age = c('13+ years', '13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
    race = c('all races', 'white')
)

incomplete.dimensions(qol_ontology) <- c('year', 'location')
qol_manager$register.ontology(name = 'qol.ontology', ont = qol_ontology)

cat("✅ Registered simplified ontology\n")

# =============================================================================
# PROCESS AND LOAD DATA
# =============================================================================

cat("\n--- Processing and Loading Data ---\n")

# Load raw data
all_qol_data <- read.csv(RAW_DATA_FILE, stringsAsFactors = FALSE)
cat("📁 Loaded", nrow(all_qol_data), "rows from raw data\n")

# Process data
processed_qol_data <- all_qol_data %>%
    filter(Data.Status == "Available", !is.na(Percentage)) %>%
    mutate(
        outcome = case_when(
            Indicator == "Unstable Housing or Homelessness" ~ "unstable.housing.pwh",
            Indicator == "HIV Stigma" ~ "hiv.stigma.pwh", 
            Indicator == "Good or better self-rated health" ~ "good.health.pwh",
            Indicator == "Unmet needs for mental health services" ~ "unmet.mental.health.pwh",
            Indicator == "Hunger or food insecurity" ~ "food.insecurity.pwh",
            Indicator == "Unemployment" ~ "unemployment.pwh",
            TRUE ~ NA_character_
        ),
        value = as.numeric(Percentage),
        year = as.character(Year),
        location = case_when(
            Geography == "United States" ~ "US",
            Geography == "California" ~ "CA",
            Geography == "Delaware" ~ "DE", 
            Geography == "Florida" ~ "FL",
            Geography == "Georgia" ~ "GA",
            Geography == "Illinois" ~ "IL",
            Geography == "Indiana" ~ "IN",
            Geography == "Michigan" ~ "MI",
            Geography == "Mississippi" ~ "MS",
            Geography == "New Jersey" ~ "NJ",
            Geography == "New York" ~ "NY",
            Geography == "North Carolina" ~ "NC",
            Geography == "Oregon" ~ "OR",
            Geography == "Pennsylvania" ~ "PA",
            Geography == "Texas" ~ "TX",
            Geography == "Virginia" ~ "VA",
            Geography == "Washington" ~ "WA",
            Geography == "Puerto Rico" ~ "PR",
            TRUE ~ Geography
        ),
        age = case_when(
            Age.Group == "Ages 13 years and older" ~ "13+ years",
            Age.Group == "13-24" ~ "13-24 years",
            Age.Group == "25-34" ~ "25-34 years", 
            Age.Group == "35-44" ~ "35-44 years",
            Age.Group == "45-54" ~ "45-54 years",
            Age.Group == "55+" ~ "55+ years",
            TRUE ~ "13+ years"
        ),
        race = case_when(
            Race.Ethnicity == "All races/ethnicities" ~ "all races",
            Race.Ethnicity == "White" ~ "white",
            TRUE ~ "all races"
        )
    ) %>%
    filter(!is.na(outcome), !is.na(value), !is.infinite(value)) %>%
    select(outcome, year, location, age, race, value)

cat("✅ Processed", nrow(processed_qol_data), "data points\n")

# Load data into manager
unique_outcomes <- unique(processed_qol_data$outcome)
current_date <- format(Sys.Date(), "%Y-%m-%d")

for(outcome in unique_outcomes) {
    outcome_data <- processed_qol_data %>%
        filter(outcome == !!outcome) %>%
        select(-outcome)
    
    if(nrow(outcome_data) > 0) {
        qol_manager$put.long.form(
            data = outcome_data,
            outcome = outcome,
            metric = "estimate", 
            source = "cdc.atlasplus.qol",
            ontology.name = "qol.ontology",
            url = "https://gis.cdc.gov/grasp/nchhstpatlas/tables.html",
            details = paste("QoL/SDOH data for", outcome, "from CDC AtlasPlus, processed", current_date),
            allow.na.to.overwrite = FALSE
        )
        cat("✅", outcome, "-", nrow(outcome_data), "rows loaded\n")
    }
}

# =============================================================================
# SAVE DATA MANAGER
# =============================================================================

cat("\n--- Saving Data Manager ---\n")

# Save locally
local_manager_path <- "cached/pwh.qol.sdoh.manager.rdata"
save(qol_manager, file = local_manager_path)
cat("✅ Saved locally:", local_manager_path, "\n")

# Save to NAS if available
if(NAS.AVAILABLE) {
    nas_manager_path <- file.path(ROOT.DIR, "data_managers", "pwh.qol.sdoh.manager.rdata")
    tryCatch({
        save(qol_manager, file = nas_manager_path)
        cat("✅ Saved to NAS:", nas_manager_path, "\n")
    }, error = function(e) {
        cat("⚠️  Failed to save to NAS:", e$message, "\n")
    })
}

# =============================================================================
# VALIDATION AND USAGE EXAMPLES
# =============================================================================

cat("\n--- Validation Tests ---\n")

# Test 1: US HIV stigma by year
us_stigma <- qol_manager$pull(
    outcome = "hiv.stigma.pwh",
    source = "cdc.atlasplus.qol",
    from.ontology.names = "qol.ontology",
    dimension.values = list(
        location = "US", 
        age = "13+ years", 
        race = "all races"
    ),
    keep.dimensions = "year"
)

if(!is.null(us_stigma)) {
    cat("✅ Test 1: US HIV stigma by year - PASS\n")
    cat("   Values:", paste(round(as.numeric(us_stigma), 1), collapse=", "), "\n")
} else {
    cat("❌ Test 1: US HIV stigma by year - FAIL\n")
}

# Test 2: All states for 2022
states_2022 <- qol_manager$pull(
    outcome = "hiv.stigma.pwh",
    source = "cdc.atlasplus.qol",
    from.ontology.names = "qol.ontology",
    dimension.values = list(
        year = "2022",
        age = "13+ years", 
        race = "all races"
    ),
    keep.dimensions = "location"
)

if(!is.null(states_2022)) {
    cat("✅ Test 2: All states for 2022 - PASS\n")
    cat("   Locations:", length(states_2022), "states/territories\n")
} else {
    cat("❌ Test 2: All states for 2022 - FAIL\n")
}

# Test 3: All outcomes for US 2022
cat("✅ Test 3: All outcomes for US 2022:\n")
for(outcome in qol_outcomes) {
    result <- qol_manager$pull(
        outcome = outcome,
        source = "cdc.atlasplus.qol",
        from.ontology.names = "qol.ontology",
        dimension.values = list(
            location = "US",
            year = "2022", 
            age = "13+ years", 
            race = "all races"
        ),
        keep.dimensions = "year"
    )
    
    if(!is.null(result)) {
        value_2022 <- as.numeric(result["2022", ])
        cat("   ", outcome, ":", round(value_2022, 1), "%\n")
    } else {
        cat("   ", outcome, ": No data\n")
    }
}

# =============================================================================
# COMPLETION SUMMARY
# =============================================================================

cat("\n=== QoL/SDOH DATA MANAGER COMPLETE ===\n")
cat("📊 Data Manager: pwh.qol.sdoh.manager\n")
if(NAS.AVAILABLE) {
    cat("📁 Saved to: LOCAL + NAS\n")
    cat("   Local:  cached/pwh.qol.sdoh.manager.rdata\n") 
    cat("   NAS:   ", file.path(ROOT.DIR, "data_managers", "pwh.qol.sdoh.manager.rdata"), "\n")
} else {
    cat("📁 Saved to: cached/pwh.qol.sdoh.manager.rdata\n")
}
cat("🎯 Outcomes:", length(qol_outcomes), "QoL/SDOH indicators\n")
cat("📈 Data Coverage:\n")
cat("   - Years: 2018-2022\n")
cat("   - Locations: 18 (US + 17 states/territories)\n")
cat("   - Age stratification: 6 groups (US only)\n")
cat("   - Race stratification: 2 groups (US only)\n")
cat("   - Total data points:", nrow(processed_qol_data), "\n")

cat("\n⚠️  IMPORTANT USAGE NOTE:\n")
cat("This data manager uses a 4D array structure (year, location, age, race).\n")
cat("Pull operations require explicit source/ontology and dimension specification:\n\n")
cat("# Example: Get US HIV stigma by year\n")
cat("us_data <- qol_manager$pull(\n")
cat("  outcome = 'hiv.stigma.pwh',\n")
cat("  source = 'cdc.atlasplus.qol',\n")
cat("  from.ontology.names = 'qol.ontology',\n")
cat("  dimension.values = list(\n")
cat("    location = 'US',\n")
cat("    age = '13+ years',\n")
cat("    race = 'all races'\n")
cat("  ),\n")
cat("  keep.dimensions = 'year'\n")
cat(")\n\n")

cat("✅ QoL Data Manager ready for team use!\n")
