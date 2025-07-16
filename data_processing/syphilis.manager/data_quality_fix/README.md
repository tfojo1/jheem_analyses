# Syphilis Data Quality Fix

This folder contains scripts to address **stratification data quality issues** in MSA-level syphilis diagnosis data. The core problem: race stratification data shows significant undercounting compared to total diagnoses, making stratified targets unreliable for model calibration.

## The Core Problem

### What We Found
Stratification data for key MSAs shows significant undercounting compared to total diagnoses.

**Example**: Baltimore MSA 2022 PS syphilis
- Total diagnoses: 525 cases ✅
- Race-stratified sum: 265 cases ❌ (only 50.5% coverage)
- **260 cases missing** from race stratification

### Root Cause
Some MSAs have systematic gaps in race reporting, creating unreliable stratifications that produce NaN values during aggregation while total counts remain accurate.

## Our Solution: Simple Cases-Based Analysis

### Approach
Direct comparison at MSA level: **race_cases / total_cases < 90%** → flag for removal

### Why This Approach
1. **Epidemiologically appropriate**: Focus on where disease cases are, not population distribution
2. **Simple and defensible**: Direct comparison is intuitive and easy to explain  
3. **Conservative for model quality**: Better to have fewer, reliable targets than questionable ones
4. **Addresses the core issue**: Removes stratifications that don't sum to totals

## Scripts

### Core Tools
- **`identify_problematic_stratifications.R`** - Main identification tool using simple cases-based analysis
  - Flags MSA/year/outcome combinations where race_cases/total_cases < 90%
  - Simple, direct logic: focus on case coverage, not population distribution

- **`explore_flagged_combinations.R`** - Interactive exploration tool
  - Shows why each combination was flagged
  - Displays case coverage calculations and race breakdowns

### Implementation Tools  
- **`implement_removals.R`** - Removes flagged stratifications from manager
  - Creates clean manager with problematic race data removed
  - Preserves all total counts (unchanged and accurate)

- **`test_total_with_restratification.R`** - **RECOMMENDED** total diagnosis creation
  - Creates `total.syphilis.diagnoses` = PS + Early + Unknown duration
  - Handles ontology differences with proper age harmonization

### Archive
- **`archive/population_based_approach/`** - Original population-weighted approach
  - Preserved for reference but replaced with simpler cases-based method

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
