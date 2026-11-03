# What the shared manager build (.github/workflows/manager-build.yml) needs to know
# about the census manager. The steps that differ by manager are inputs.sh, merge.sh
# and consumers.sh, next to this file.

MANAGER_NAME="Census Manager"
MANAGER_FILE="census.manager.rdata"
# Releases: census-manager-vYYYY.MM.DD, promoted to census-manager-latest
RELEASE_STEM="census-manager"
# A baseline must be a built census
BASELINE_PATTERN='^census-manager-v[0-9][0-9.]*$'
SECTIONS_STEM="census-sections"
SECTION_FILES="census.manager_section1.rdata census.manager_section2.rdata census.manager_section3.rdata"
R_PACKAGES="jsonlite digest httr2 openssl filelock ggplot2 reshape2 rlang logitnorm deSolve Rcpp RcppArmadillo mvtnorm tmvtnorm plotly dplyr"
CHECKS="data_processing/census.manager/test_merge_paths.R data_processing/validation/test_manager_value_delta.R data_processing/validation/test_manager_reproduction.R"
PROMOTE_WORKFLOW="Promote Census Manager"
NEEDS_REVIEW_TEXT="this build removes structure present in the baseline census, or the SHIELD model does not start with it."
