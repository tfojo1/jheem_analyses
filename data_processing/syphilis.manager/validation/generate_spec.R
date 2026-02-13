# Generate Syphilis Manager Spec
# Run this to create or regenerate the spec from a known-good manager.
# Review the git diff before committing.

library(jheem2)
source("data_processing/validation/manager_spec_tools.R")

# Load manager
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Usage: Rscript data_processing/syphilis.manager/validation/generate_spec.R <path/to/known-good-manager.rdata>")
}
manager_path <- args[1]

cat(sprintf("Loading manager from: %s\n", manager_path))
load(manager_path)

cat("Extracting spec...\n")
spec <- extract_manager_spec(syphilis.manager)

spec_path <- "data_processing/syphilis.manager/validation/syphilis_manager_spec.json"
jsonlite::write_json(spec, spec_path, pretty = TRUE, auto_unbox = TRUE)

cat(sprintf("Spec written to: %s\n", spec_path))
cat(sprintf("Outcomes: %d\n", length(spec$outcomes)))
cat("\nReview the diff before committing:\n")
cat(sprintf("  git diff %s\n", spec_path))
