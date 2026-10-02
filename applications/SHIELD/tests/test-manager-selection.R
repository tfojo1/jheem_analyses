# Check the actual bootstrap's manager-selection block without loading SHIELD,
# contacting GitHub, or changing a calibration/cache.
# Rscript applications/SHIELD/tests/test-manager-selection.R
script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(script.argument) == 1L)
script.path <- normalizePath(sub("^--file=", "", script.argument))
bootstrap <- file.path(dirname(script.path), "../shield_source_code.R")
expressions <- parse(bootstrap)
assigns <- function(expression, name) {
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name(name))
}
start <- which(vapply(expressions, assigns, logical(1), "SYPHILIS.MANAGER.RELEASE.TAG"))
end <- which(vapply(expressions, assigns, logical(1), "PULL.GIT.UPDATES"))
stopifnot(length(start) == 1L, length(end) == 1L, end > start)
selection <- expressions[seq.int(start, end - 1L)]

select <- function(recorded = FALSE, follow.promoted = FALSE) {
    env <- new.env(parent = baseenv())
    env$SHIELD.RECORDED.RUN <- recorded
    env$SHIELD.RECORDED.CONFIG <- list(syphilis_tag = "syphilis-manager-v2026.07.27")
    output <- capture.output({
        eval(selection[[1L]], env)
        # Emulate editing the ordinary setting to NULL, before the recorded override.
        if (follow.promoted) env$SYPHILIS.MANAGER.RELEASE.TAG <- NULL
        for (expression in selection[-1L]) eval(expression, env)
    })
    list(tag = env$SYPHILIS.MANAGER.RELEASE.TAG, output = output)
}

ordinary <- select()
stopifnot(identical(ordinary$tag, "syphilis-manager-v2026.09.09"),
          identical(ordinary$output,
                    "1-Requesting syphilis manager release: syphilis-manager-v2026.09.09"))
recorded <- select(recorded = TRUE)
stopifnot(identical(recorded$tag, "syphilis-manager-v2026.07.27"),
          identical(recorded$output,
                    "1-Requesting syphilis manager release: syphilis-manager-v2026.07.27"))
promoted <- select(follow.promoted = TRUE)
stopifnot(is.null(promoted$tag),
          identical(promoted$output, "1-Requesting the promoted syphilis manager"),
          identical(select(recorded = TRUE, follow.promoted = TRUE), recorded))
cat("SHIELD manager selection tests passed\n")
