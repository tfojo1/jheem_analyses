# Archive: Population-Based Approach

This folder contains the original population-based approach to identifying problematic stratifications. We decided to move to a simpler cases-based approach but preserved this work for reference.

## Population-Based Approach (Archived)

**Logic**: Two-level analysis:
1. County-level filter: Only count counties with ≥80% stratification coverage as "having sufficient data"
2. MSA-level check: Only preserve stratifications where counties with sufficient data represent ≥90% of MSA population

**Files**:
- `identify_problematic_stratifications_population_based.R` - Main identification script
- `explore_flagged_combinations_population_based.R` - Exploration tool

## Why We Moved Away From This Approach

After team discussion, we concluded that:
1. **Cases-based is more epidemiologically appropriate** - we care about where the disease cases are, not where the general population is
2. **Simpler to explain and defend** - direct comparison is more intuitive
3. **Avoids potential issues with health disparities** - population-weighting could preserve data from MSAs where disease burden is concentrated in areas with poor reporting
4. **Empirically equivalent** - both approaches flagged similar combinations in our dataset

## Current Approach (Simple Cases-Based)

**Logic**: Direct MSA-level comparison: `race_cases / total_cases < 90%` → flag for removal

See the main folder for the current implementation.

## Date

Archived: July 14, 2025
Original analysis period: July 2025
