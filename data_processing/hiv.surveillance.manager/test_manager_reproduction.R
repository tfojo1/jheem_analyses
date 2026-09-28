source("data_processing/hiv.surveillance.manager/manager_reproduction.R")
baseline <- list(data = list(x = array(1:4, c(2, 2))), name = "manager",
                 description = "baseline", ontologies = list(cdc = "race"),
                 outcome.info = list(x = list(metadata = list(unit = "count"))),
                 creation.date = "2026-08-31")
stopifnot(compare.manager.reproduction(baseline, baseline)$equivalent)
candidate <- baseline
candidate$creation.date <- "2026-09-23"
candidate$last.modified.date <- "2026-09-23"
stopifnot(compare.manager.reproduction(baseline, candidate)$equivalent)
candidate$data$x[1] <- 5L
stopifnot(!compare.manager.reproduction(baseline, candidate)$equivalent)
for (field in c("name", "description", "ontologies", "outcomes", "ontology.names",
                "source.info", "parent.source.info", "details", "url",
                "details.list", "url.list", "outcome.info")) {
  candidate <- baseline
  candidate[[field]] <- "changed"
  result <- compare.manager.reproduction(baseline, candidate)
  stopifnot(result$data_identical, !result$equivalent,
            !result$metadata_matches[[field]])
}
message("Manager reproduction comparison checks passed")
