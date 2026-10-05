source("applications/SHIELD/R/shield_trace_checks.R")
error <- function(expr, pattern) {
    result <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(result, "error"), grepl(pattern, conditionMessage(result)))
}
x <- c(rate = 1, perturbed = 1 + .Machine$double.eps)
packed <- shield.trace.numeric(x)
stopifnot(identical(as.numeric(unlist(packed$values)), unname(x)),
          packed$values[[1L]] != packed$values[[2L]],
          identical(packed$dimensions$element, as.list(names(x))))
array <- array(seq_len(12), c(1L, 3L, 4L),
               dimnames = list(chain = "1", iteration = c("1", "2", "3"), variable = letters[1:4]))
stopifnot(identical(shield.trace.numeric(array)$values, as.list(as.character(seq_len(12)))))
for (bad in list(numeric(), c(1, NA), c(1, Inf), "1")) {
    error(shield.trace.numeric(bad), "finite.*numeric")
}
error(shield.trace.numeric(c(a = 1, a = 2)), "unique")
methods::setClass("shield_trace_test_state", slots = c(
    current.parameters = "numeric", first.step.for.iter = "integer", run.time = "numeric"))
state <- methods::new("shield_trace_test_state", current.parameters = c(rate = 1),
                      first.step.for.iter = NA_integer_, run.time = 2)
other <- state
other@run.time <- 500
stopifnot(identical(shield.trace.state(state), shield.trace.state(other)),
          identical(shield.trace.state(state)$first.step.for.iter, list(unset = TRUE)))
cat("Full-precision trace export checks passed\n")
