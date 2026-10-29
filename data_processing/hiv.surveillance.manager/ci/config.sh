# What the shared manager build (.github/workflows/manager-build.yml) needs to know
# about the HIV surveillance manager. The steps that differ by manager are inputs.sh,
# merge.sh and consumers.sh, next to this file.

MANAGER_NAME="HIV Surveillance Manager"
MANAGER_FILE="surveillance.manager.rdata"
# Releases: hiv-surveillance-manager-vYYYY.MM.DD, promoted to hiv-surveillance-manager-latest
RELEASE_STEM="hiv-surveillance-manager"
# A baseline is a built manager or a recorded baseline snapshot
BASELINE_PATTERN='^hiv-surveillance-(manager-v|baseline-)[0-9][0-9.]*$'
SECTIONS_STEM="hiv-sections"
SECTION_FILES="surveillance.manager_section1.rdata surveillance.manager_section2.rdata surveillance.manager_section3.rdata surveillance.manager_section4.rdata surveillance.manager_section5.rdata"
R_PACKAGES="jsonlite digest httr2 readxl dplyr stringr tidyr purrr haven tidyverse"
CHECKS="data_processing/hiv.surveillance.manager/test_merge_paths.R data_processing/validation/test_manager_value_delta.R data_processing/validation/test_manager_reproduction.R data_processing/hiv.surveillance.manager/test_summarize_candidate.R data_processing/validation/test_marginal_consistency.R"
PROMOTE_WORKFLOW="Promote HIV Surveillance Manager"
NEEDS_REVIEW_TEXT="this build removes structure present in the promoted manager."
