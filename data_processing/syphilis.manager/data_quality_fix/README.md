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
Some MSAs have systematic gaps in race reporting at the county level, creating unreliable stratifications that produce incomplete data during aggregation while total counts remain accurate.

## Our Solution: County-Level Data Quality Assessment

### Approach
Assess data quality at the source (county level): **county race coverage < 90%** → flag for removal

### Why This Approach
1. **Epidemiologically appropriate**: Assess quality where data is collected, not where it's aggregated
2. **Distinguishes real problems from aggregation artifacts**: County-level assessment vs MSA-level symptoms
3. **Conservative for model quality**: Better to have fewer, reliable targets than questionable ones
4. **Addresses the core issue**: Removes stratifications that don't represent complete surveillance

## Current Implementation

### Core Tools
- **`implement_removals.R`** - Main removal script with complete audit trail and rollback capability
- **`operations_manager.R`** - Operation management system providing backup, restore, and audit functions
- **`identify_problematic_stratifications_county_level.R`** - County-level data quality analysis
- **`explore_flagged_combinations_county_level.R`** - Interactive exploration of flagged combinations

### Workflow Tools  
- **`test_total_with_restratification.R`** - Creates total syphilis diagnosis outcome (PS + Early + Unknown)
- **`validate_total_diagnosis.R`** - Quality validation for total diagnosis calculations
- **`simple_problem_demo.R`** - Demonstrates the data quality problem for team discussions

### Operations System
- **`operations/`** - Complete audit trail for all data removal operations
  - Each operation gets its own timestamped directory
  - Includes summary, complete removed data CSV, metadata, and rollback backup
  - Organized chronologically with clear operation names

### Archive
- **`archive/msa_based_approach/`** - Original MSA-level analysis approach (superseded)
- **`archive/population_based_approach/`** - Population-weighted approach (alternative method)

## Results: Systematic Issue Identified

The county-level analysis revealed this is **not just a Baltimore problem** - it affects multiple MSAs and outcomes:

**16 problematic stratification combinations identified**:
- **Baltimore**: 3/12 combinations (25.0%)
- **NYC**: 5/12 combinations (41.7%)  
- **Miami**: 1/12 combinations (8.3%)
- **Atlanta**: 7/12 combinations (58.3%) - most affected

**By outcome**:
- Unknown duration syphilis: 46.7% need removal (most problematic)
- PS syphilis: 33.3% need removal
- Early syphilis: 26.7% need removal

**Key insight**: This suggests systematic county-level surveillance gaps across the system, not isolated incidents.

## Usage

### 1. Identify Problematic Stratifications
```bash
cd /path/to/jheem_analyses
Rscript data_processing/syphilis.manager/data_quality_fix/identify_problematic_stratifications_county_level.R
```

### 2. Apply Quality Fixes
```bash
Rscript data_processing/syphilis.manager/data_quality_fix/implement_removals.R
```

### 3. Create Total Diagnosis Outcome
```bash
Rscript data_processing/syphilis.manager/data_quality_fix/test_total_with_restratification.R
```

### 4. Validate Results
```bash
Rscript data_processing/syphilis.manager/data_quality_fix/simple_problem_demo.R
```

## Operation Management

### View Operations
```r
source("data_processing/syphilis.manager/data_quality_fix/operations_manager.R")
list_operations()
```

### Rollback Changes
```r
# Load operations system
source("data_processing/syphilis.manager/data_quality_fix/operations_manager.R")

# Load your manager
load("cached/syphilis.manager.rdata")

# Restore from specific operation
restore_operation(syphilis.manager, "operations/TIMESTAMP_OPERATION_NAME/backup.rdata")

# Save restored version
save(syphilis.manager, file="cached/syphilis.manager.rdata")
```

## Audit Trail

Each removal operation creates a complete audit trail in `operations/TIMESTAMP_OPERATION_NAME/`:
- **`summary.txt`** - Human-readable impact summary for team review
- **`removed_data.csv`** - Complete inventory of every removed data point
- **`metadata.json`** - Machine-readable operation metadata
- **`backup.rdata`** - Complete rollback capability
- **`manifest.json`** - Operation manifest and file inventory

## Implications & Future Work

### Immediate Impact
- **Race stratifications**: 16 combinations cleaned across 4 MSAs
- **Model calibration**: Use total targets when stratified targets are unreliable
- **Data quality**: Clear methodology for identifying similar issues

### Future Applications
- **Other stratifications**: Apply same logic to age, sex, age×race, etc.
- **Other outcomes**: Extend beyond syphilis to HIV, other STIs
- **System-wide**: Use as template for surveillance data quality assessment
- **Scaling up**: Framework supports expansion to all EHE MSAs and outcomes

### Root Cause Considerations
- **Surveillance system**: Some counties may systematically under-report stratified data
- **Data collection**: Training/systems issues at county level?
- **Aggregation logic**: Could be enhanced to handle missing stratified data more intelligently

**This framework provides a systematic approach to detect and handle stratification data quality issues across any surveillance system.**

## Implementation Notes

### Current Status
- **Adult population data**: Preserved throughout quality fix process
- **Total counts**: All MSA/year totals remain accurate and unchanged
- **Stratified data**: Only problematic race stratifications removed
- **Rollback capability**: Complete audit trail enables safe experimentation

### Database-Style Operations
The implementation provides transaction-like operations:
- **Backup before changes**: Automatic operation backup creation
- **Complete audit trail**: Every change tracked with full context
- **Rollback capability**: Can undo any operation safely
- **Impact assessment**: Quantified summary of all changes

### Team Collaboration
- **Shared audit files**: Complete removed data available for team review
- **Clear documentation**: Operation summaries for non-technical stakeholders  
- **Reproducible process**: All operations can be repeated or modified
- **Safe experimentation**: Rollback capability enables testing different approaches

## Next Steps

1. **Validate with team**: Review removed data CSV files for technical accuracy
2. **Expand scope**: Apply methodology to age and sex stratifications
3. **Scale up**: Extend to all EHE MSAs and STI outcomes
4. **Process refinement**: Adjust thresholds based on team feedback and model needs

## Technical Notes

- **County-level assessment**: Uses county source (`cdc.sti`) for coverage analysis
- **MSA-level removal**: Applies to aggregated source (`cdc.aggregated.county`)
- **Threshold**: 90% county-level race coverage required
- **Preserved data**: All total counts remain unchanged and accurate
- **Age handling**: Total diagnosis includes proper age restratification for ontology alignment
