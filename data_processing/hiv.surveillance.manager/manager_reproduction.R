# Compare stored data and descriptive metadata, not serialization bytes or
# creation timestamps. This records equivalence, not scientific correctness.
compare.manager.reproduction <- function(baseline, candidate) {
  fields <- c("name", "description", "ontologies", "outcomes",
              "ontology.names", "source.info", "parent.source.info",
              "details", "url", "details.list", "url.list")
  matches <- vapply(fields, function(field) {
    identical(baseline[[field]], candidate[[field]])
  }, logical(1))
  # outcome.info includes R6 outcome definitions; their equivalent contents may
  # have different environment identities after loading separate files.
  matches <- c(matches, outcome.info = isTRUE(all.equal(
    baseline$outcome.info, candidate$outcome.info, tolerance = 0)))
  data.identical <- identical(baseline$data, candidate$data)
  list(data_identical = data.identical,
       metadata_matches = as.list(matches),
       equivalent = data.identical && all(matches),
       excluded_fields = c("creation.date", "last.modified.date", "runtime methods"))
}
