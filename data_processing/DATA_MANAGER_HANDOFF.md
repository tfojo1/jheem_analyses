# Data Manager Changes - July/August 2025

Here are the scripts for changes I made to the syphilis and HIV surveillance managers that need to be integrated into your rebuild workflow.

---

## Syphilis Manager (3 changes)

### 1. Add Adult Population Data

**Script**: `data_processing/transfer_adult_population_to_syphilis_manager.R`

**What**: Transfers `adult.population` outcome from surveillance manager to syphilis manager.

---

### 2. Add Total Syphilis Diagnoses Outcome

**Script**: `data_processing/syphilis.manager/data_quality_fix/test_total_with_restratification.R`

**What**: Creates `total.syphilis.diagnoses` outcome by summing PS + Early + Unknown (with age restratification to handle different ontologies).

---

### 3. Remove Low-Quality Race Stratifications

**Scripts**:
1. `data_processing/syphilis.manager/data_quality_fix/identify_problematic_stratifications_county_level.R` (analysis)
2. `data_processing/syphilis.manager/data_quality_fix/implement_removals.R` (apply fixes)

**What**: Identifies and removes race stratifications where county-level race coverage < 90%. Affects 16 combinations across Baltimore, NYC, Miami, Atlanta.

---

## HIV Surveillance Manager (1 change)

### 4. Add Pennsylvania Viral Suppression Data

**Script**: `data_processing/hiv.surveillance.manager/pa_viral_suppression/pa_viral_suppression_integration_5age.R`

**What**: Integrates PA viral suppression data for 2022-2023 (6,532 data points) from CDC Atlas Plus.

**Critical**: Uses `cdc` ontology (5 age groups), NOT `cdc.new` (6 age groups).

**Data file**: `data_raw/hiv_surveillance/viral_suppression_comprehensive_Pennsylvania_20250727_120208.csv`

