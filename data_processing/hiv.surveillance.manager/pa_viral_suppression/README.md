# Pennsylvania Viral Suppression Data Integration

## Overview
This folder contains scripts and documentation for integrating Pennsylvania (PA) viral suppression data from CDC Atlas Plus into the JHEEM surveillance data manager. This integration was necessary because PA had no existing viral suppression data, preventing EHE model calibration.

## Problem Solved
The initial integration attempt caused likelihood instantiation failures due to an ontology mismatch. PA data was inadvertently added using the `cdc.new` ontology (6 age groups) while the likelihood system expected the `cdc` ontology (5 age groups). This incompatibility caused NULL returns when pulling age-stratified data, resulting in array construction errors.

## Solution Implemented
We re-integrated the PA data following JHEEM's established patterns:
- Used `cdc` ontology with 5 age groups (matching all other state-level data)
- Combined 55-64 and 65+ age groups into 55+ using population-weighted averages
- Converted state FIPS code "42" to standard abbreviation "PA"
- Preserved all county-level data with 5-digit FIPS codes

## Data Summary

### Source
- **File**: `viral_suppression_comprehensive_Pennsylvania_20250727_120208.csv`
- **Source**: CDC Atlas Plus API
- **Years**: 2022 and 2023
- **Records**: 18,820 total rows → 6,532 integrated data points

### Coverage
- **Geography**: PA state + 67 counties (42001-42133)
- **Stratifications**: Total, age, race, sex, risk, and all 2-way combinations
- **Integration Rate**: 100% of usable data (some 3-way/4-way stratifications had no numeric values in source)

### Key Values (2022)
- PA Total: 63.1%
- Philadelphia County: 62.3%
- Age 55+ (combined): 65.2%

## Scripts in This Folder

### Main Scripts (5 files)
1. **`pa_viral_suppression_integration_5age.R`** - Main integration script using 5 age groups
2. **`atlas_plus_viral_suppression_comprehensive.R`** - Atlas Plus data retrieval script
3. **`example_data_pulls.R`** - Examples of accessing PA data using $ syntax
4. **`verify_pa_integration_final.R`** - Verification tool to check PA data integrity
5. **`README.md`** - This documentation

### Archive Folder
**`archive_pa_scripts/`** - Contains diagnostic scripts and earlier attempts from the debugging process (21 files). These are kept for reference but are not needed for normal operations.

## How to Use

### Accessing PA Data
```r
# Load manager
manager <- load.data.manager("cached/surveillance.manager.rdata")

# Get PA 2022 total suppression
pa_total <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location"]]["2022", "PA"]
# Returns: 0.631 (63.1%)

# Get age-stratified data
age_data <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location__age"]]["2022", "PA", ]
# Returns values for all 5 age groups

# Get county data (e.g., Philadelphia)
phila <- manager$data$suppression$estimate$cdc.hiv$cdc[["year__location"]]["2022", "42101"]
# Returns: 0.623 (62.3%)
```

### Re-running the Integration (if needed)
```r
# Only needed if starting from scratch
source("data_processing/hiv.surveillance.manager/pa_viral_suppression/pa_viral_suppression_integration_5age.R")
```

### Example Data Pulls
```r
# For comprehensive examples, run:
source("data_processing/hiv.surveillance.manager/pa_viral_suppression/example_data_pulls.R")
```

### Verify PA Data Integrity
```r
# Check that PA data is properly integrated
source("data_processing/hiv.surveillance.manager/pa_viral_suppression/verify_pa_integration_final.R")
```

## Technical Details

### Age Group Mapping
The integration combines Atlas Plus's 6 age groups into JHEEM's standard 5:
```r
Atlas Plus → JHEEM
13-24     → 13-24 years
25-34     → 25-34 years  
35-44     → 35-44 years
45-54     → 45-54 years
55-64 ┐
      ├─→ 55+ years (weighted average)
65+   ┘
```

### Ontology Structure
PA data uses the `cdc` ontology with dimensions:
- year (incomplete - can expand)
- location (incomplete - can expand)
- age: 5 groups (complete)
- race: 6 categories (complete)
- sex: male, female (complete)
- risk: 5 transmission categories (complete)

### Data Processing Pipeline
1. Read raw CSV (18,820 rows)
2. Filter to available data (8,541 rows)
3. Extract numeric values (8,005 rows)
4. Aggregate age groups 6→5 (~7,298 rows)
5. Remove NaN values/0 denominators (6,497 rows)
6. Store in manager (6,532 data points)

## Troubleshooting

### If Likelihood Still Fails
1. Verify manager has PA data: Check `"PA" %in% dimnames(data)$location`
2. Confirm ontology: Data should be in `$cdc` not `$cdc.new`
3. Check age groups: Should see exactly 5 age groups ending with "55+ years"

### Common Issues
- **NaN values**: Some stratifications have 0 cases and 0 population (0/0 = NaN)
- **Missing stratifications**: 3-way/4-way combos often fully suppressed in source
- **Location codes**: State uses "PA", counties use 5-digit FIPS (e.g., "42001")

## Contact
For questions about this integration, refer to the session handoff document:
`session_handoff_pa_viral_suppression_2025_08_15.md`

## References
- [CDC Atlas Plus](https://gis.cdc.gov/grasp/nchhstpatlas/tables.html)
- JHEEM Data Manager Documentation (see parent directories)
- Original request: PA viral suppression for 2022-2023 to enable EHE calibration

---
*Last updated: 2025-08-15*
*Integration completed successfully with 6,532 PA data points across all available stratifications*
