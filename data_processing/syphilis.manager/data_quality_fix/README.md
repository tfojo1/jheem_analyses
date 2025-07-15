# Syphilis Data Quality Fix

This folder contains scripts to address **stratification data quality issues** in MSA-level syphilis diagnosis data. While discovered through race stratification analysis, this is a **generic problem** that can affect any stratification (age, sex, race, combinations).

## The Core Problem

### What We Found
Stratification data for key MSAs shows significant undercounting compared to total diagnoses, making stratified targets unreliable for model calibration.

**Example**: Baltimore MSA 2022 PS syphilis
- Total diagnoses: 525 cases ✅
- Race-stratified sum: 265 cases ❌ (only 50.5% coverage)
- **265 cases missing** from race stratification

### Root Cause: Incomplete County-Level Data + Aggregation Logic

**The Problem Chain**:
1. **County level**: Some counties have complete total counts but incomplete stratified data
   - Baltimore City: 306 total cases, but only 48 with race data (15.7% coverage)
   - Other counties: Complete race data (100% coverage)

2. **MSA aggregation**: The aggregation script treats missing stratified data as zeros
   - Sums available race data: 48 (Baltimore City) + 217 (other counties) = 265
   - **Ignores** the 258 cases with missing race data in Baltimore City
   - But correctly sums total data: 306 + 219 = 525

3. **Result**: MSA shows NaN values in race stratification, indicating the aggregation couldn't account for all cases

### Why This is Generic
This problem can occur with **any stratification** whenever:
- Counties have complete total data but incomplete stratified data (age, sex, race, etc.)
- The incomplete stratification represents a significant population/case load
- MSA aggregation treats the missing stratified data as negligible

**The race analysis revealed the pattern, but age, sex, or combined stratifications could have similar issues.**

## Our Solution: Population Coverage Analysis

### Approach
Two-level analysis to identify unreliable stratifications:

1. **County-level filter**: Only count counties with ≥80% stratification coverage as "having sufficient data"
   - Baltimore City (15.7% race coverage) → excluded
   - Other counties (100% race coverage) → included

2. **MSA-level check**: Only preserve stratifications where counties with sufficient data represent ≥90% of MSA population
   - Baltimore: 6 counties with good data represent 79.9% of MSA population
   - 79.9% < 90% threshold → **flag for removal**

### Why This Works
- **Catches significant data gaps**: When major population centers have poor stratification coverage
- **Preserves good data**: When NaN values represent normal suppression (small counts)
- **Generalizable**: Works for any stratification type (age, sex, race, combinations)
- **Conservative**: Better to remove unreliable data than propagate errors

### Data Flow
```
County Data (cdc.sti source)
├── Baltimore City: 306 total, 48 race (15.7% coverage) → EXCLUDED
├── Other 6 counties: 219 total, 217 race (99% coverage) → INCLUDED
│
└── Population Coverage Check:
    Counties with good data = 79.9% of MSA population
    79.9% < 90% threshold → FLAG MSA STRATIFICATION FOR REMOVAL

MSA Data (cdc.aggregated.county source)
├── Total: 525 cases → PRESERVE (accurate)
└── Race stratification → REMOVE (unreliable)
```

## Scripts

### Core Tools
- **`identify_problematic_stratifications.R`** - Main identification tool
  - Finds MSA/year/outcome/stratification combinations with insufficient population coverage
  - Uses county-level data to assess coverage, flags MSA-level data for removal
  - Configurable thresholds (80% county, 90% MSA)

- **`simple_problem_demo.R`** - Enhanced problem demonstration
  - Shows raw NaN values in MSA aggregated data
  - Explains county-level breakdown causing the problem
  - Demonstrates aggregation math that creates inconsistencies

### Total Diagnosis Creation
- **`test_total_with_restratification.R`** - **RECOMMENDED** total diagnosis script
  - Creates `total.syphilis.diagnoses` = PS + Early + Unknown duration
  - Handles ontology mismatch between PS (cdc.sti, 10 age groups) and Early/Unknown (cdc.sti.two, 7 age groups)
  - Uses `jheem2::restratify.age.counts()` for proper age harmonization

- **`test_total_diagnosis.R`** - Alternative total diagnosis approach
- **`total_diagnosis_addition.R`** - Another total diagnosis method  
- **`validate_total_diagnosis.R`** - Validation and quality checks

## Results: Widespread Issue

The analysis revealed this is **not just a Baltimore problem** - it affects multiple MSAs and outcomes:

**15 problematic stratification combinations identified**:
- **Baltimore**: 3/9 combinations (33.3%)
- **Atlanta**: 7/12 combinations (58.3%) - worst affected
- **NYC**: 3/12 combinations (25.0%)
- **Miami**: 2/12 combinations (16.7%)

**By outcome**:
- Unknown duration syphilis: 46.7% need removal (most problematic)
- PS syphilis: 26.7% need removal
- Early syphilis: 26.7% need removal

**Key insight**: This suggests the problem is systematic across the surveillance system, not isolated incidents.

## Usage

### 1. Identify Problematic Stratifications
```bash
cd /path/to/jheem_analyses
Rscript data_processing/syphilis.manager/data_quality_fix/identify_problematic_stratifications.R
```

### 2. Create Total Diagnosis Outcome
```bash
Rscript data_processing/syphilis.manager/data_quality_fix/test_total_with_restratification.R
```

### 3. Demonstrate Problem/Fix
```bash
Rscript data_processing/syphilis.manager/data_quality_fix/simple_problem_demo.R
```

## Implications & Future Work

### Immediate Impact
- **Race stratifications**: 15 combinations flagged for removal
- **Model calibration**: Use total targets instead of unreliable stratified targets
- **Data quality**: Clear methodology for identifying similar issues

### Future Applications
- **Other stratifications**: Apply same logic to age, sex, age×race, etc.
- **Other outcomes**: Extend beyond syphilis to HIV, other STIs
- **System-wide**: Use as template for surveillance data quality assessment

### Root Cause Considerations
- **Surveillance system**: Some counties may systematically under-report stratified data
- **Data collection**: Training/systems issues at county level?
- **Aggregation logic**: Could be enhanced to handle missing stratified data more intelligently

**This framework provides a systematic approach to detect and handle stratification data quality issues across any surveillance system.**

## Next Steps

1. Implement removal script to clean MSA-level data
2. Integrate total diagnosis calculations
3. Validate fix with before/after comparison
4. **Optional**: Extend to other stratifications (age, sex, age×race) and outcomes

## Technical Notes

- **Analysis uses**: County-level data (`cdc.sti` source) to assess coverage
- **Removal applies to**: MSA-level data (`cdc.aggregated.county` source)  
- **Thresholds**: 80% county stratification coverage, 90% MSA population coverage
- **Preserved data**: Total counts remain unchanged and accurate
