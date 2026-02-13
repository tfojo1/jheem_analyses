# Syphilis Manager Validation
# Run as the final build step to verify the manager is structurally complete
# and to produce a data quality summary for review.
#
# Usage:
#   Rscript data_processing/syphilis.manager/validation/run_validation.R path/to/syphilis.manager.rdata

library(jheem2)
source("data_processing/validation/manager_spec_tools.R")
source("data_processing/validation/data_quality_report.R")

# Load manager
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Usage: Rscript data_processing/syphilis.manager/validation/run_validation.R <path/to/syphilis.manager.rdata>")
}
manager_path <- args[1]

cat(sprintf("Loading manager from: %s\n", manager_path))
load(manager_path)

# Load spec
spec_path <- "data_processing/syphilis.manager/validation/syphilis_manager_spec.json"
cat(sprintf("Loading spec from: %s\n\n", spec_path))
spec <- jsonlite::read_json(spec_path, simplifyVector = TRUE)

cat(sprintf("Spec generated: %s\n", spec$metadata$generated_at))
cat(sprintf("Manager name: %s\n\n", syphilis.manager$name))

# --- Layer 1: Structural validation (pass/fail) ---
structural_results <- validate_manager_structure(syphilis.manager, spec)
print_validation_results(structural_results)

cat("\n")

# --- Layer 2: Data quality report (informational) ---
# Syphilis-specific component checks
syphilis_component_checks <- list(
  list(
    total = "total.syphilis.diagnoses",
    components = c("ps.syphilis.diagnoses",
                   "early.syphilis.diagnoses",
                   "unknown.duration.or.late.syphilis.diagnoses")
  )
)

quality_report <- report_data_quality(
  syphilis.manager,
  component.checks = syphilis_component_checks
)
print_quality_report(quality_report)

# Exit code based on structural validation only
if (!structural_results$passed) quit(status = 1)
