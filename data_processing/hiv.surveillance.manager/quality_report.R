# Informational data-quality report for a built HIV surveillance manager, using
# the shared checks behind the syphilis build's report. It never gates a build.
# Usage: Rscript quality_report.R <manager.rdata> <report.txt>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !file.exists(args[[1]])) {
  stop("Usage: Rscript quality_report.R <manager.rdata> <report.txt>")
}

library(jheem2)
source("data_processing/validation/data_quality_report.R")

manager <- load.data.manager(file = args[[1]])
known.issues.file <- "data_processing/hiv.surveillance.manager/known_issues.json"
report <- report_data_quality(manager, known.issues.file = known.issues.file)
writeLines(capture.output(print_quality_report(report)), args[[2]])
cat("Data quality report written to", args[[2]], "\n")
