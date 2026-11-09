# Check SHIELD's declared data managers and the bootstrap's selection block
# without loading SHIELD, contacting GitHub, or changing a calibration/cache.
# Rscript applications/SHIELD/tests/test-manager-selection.R
script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(script.argument) == 1L)
script.path <- normalizePath(sub("^--file=", "", script.argument))
shield.dir <- normalizePath(file.path(dirname(script.path), ".."))
analyses.path <- normalizePath(file.path(shield.dir, "../.."))
helpers <- new.env(parent = globalenv())
sys.source(file.path(shield.dir, "R/shield_data_managers.R"), envir = helpers)

expect.error <- function(expr, pattern) {
    message <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(message) || !grepl(pattern, message, fixed = TRUE)) {
        stop("expected an error containing '", pattern, "', got: ",
             if (is.null(message)) "no error" else message)
    }
}
declare <- function(...) {
    path <- tempfile(fileext = ".csv")
    writeLines(c(...), path)
    helpers$shield.declared.data.managers(path)
}
header <- "name,file,release"
census <- "census,census.manager.rdata,unpinned"
syphilis <- "syphilis,syphilis.manager.rdata,syphilis-manager-v2026.10.09"

#-- The declaration file's rules (the container wrapper applies the same) --#
stopifnot(identical(declare("# a comment", "", header, census, syphilis),
                    list(census = NULL, syphilis = "syphilis-manager-v2026.10.09")),
          identical(declare(header, syphilis, "census, census.manager.rdata, census-manager-v2026.10.08"),
                    list(census = "census-manager-v2026.10.08",
                         syphilis = "syphilis-manager-v2026.10.09")))
expect.error(declare("name,release", census), "columns name,file,release")
expect.error(declare(header, census), "does not list syphilis")
expect.error(declare(header, census, syphilis, syphilis), "'syphilis' is listed twice")
expect.error(declare(header, census, "syphilis,syphilis.manager.rdata,unpinned"), "syphilis is unpinned")
expect.error(declare(header, census, "syphilis,other.rdata,syphilis-manager-v2026.10.09"),
             "'syphilis' must be syphilis.manager.rdata")
expect.error(declare(header, census, "syphilis,syphilis.manager.rdata,"), "'syphilis' has no release")
expect.error(helpers$shield.declared.data.managers(tempfile()), "does not exist")

#-- The committed declarations: their form, not their values (a scientific choice) --#
committed <- helpers$shield.declared.data.managers(file.path(shield.dir, "data_managers.csv"))
dated <- function(kind) paste0("^", kind, "-manager-v[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}([.-][0-9A-Za-z]+)*$")
stopifnot(grepl(dated("syphilis"), committed$syphilis),
          is.null(committed$census) || grepl(dated("census"), committed$census))

#-- The bootstrap's selection block, evaluated as written --#
expressions <- parse(file.path(shield.dir, "shield_source_code.R"))
assigns <- function(expression, name) {
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name(name))
}
is.library.call <- function(expression) is.call(expression) && identical(expression[[1L]], as.name("library"))
start <- which(vapply(expressions, assigns, logical(1), "SHIELD.DATA.MANAGERS")) - 1L
end <- which(vapply(expressions, is.library.call, logical(1)))
stopifnot(length(start) == 1L, length(end) > 0L, end[[1L]] > start,
          grepl("shield_data_managers.R", deparse(expressions[[start]]), fixed = TRUE))
selection <- expressions[seq.int(start, end[[1L]] - 1L)]
select <- function(recorded = FALSE, analyses = analyses.path) {
    env <- new.env(parent = globalenv())
    env$JHEEM.ANALYSES.PATH <- analyses
    env$SHIELD.RECORDED.RUN <- recorded
    env$SHIELD.RECORDED.CONFIG <- list(syphilis_tag = "syphilis-manager-v2026.07.27",
                                       census_tag = "census-manager-v2026.10.08")
    output <- capture.output(for (expression in selection) eval(expression, env))
    list(syphilis = env$SYPHILIS.MANAGER.RELEASE.TAG, census = env$CENSUS.MANAGER.RELEASE.TAG,
         output = output)
}
ordinary <- select()
stopifnot(identical(ordinary$syphilis, committed$syphilis),
          identical(ordinary$census, committed$census),
          identical(ordinary$output,
                    c(paste0("1-Requesting syphilis manager release: ", committed$syphilis),
                      paste0("1-Requesting census manager release: ",
                             if (is.null(committed$census)) "unpinned (the promoted census)" else committed$census))))
recorded <- select(recorded = TRUE)
stopifnot(identical(recorded$syphilis, "syphilis-manager-v2026.07.27"),
          identical(recorded$census, "census-manager-v2026.10.08"),
          identical(recorded$output,
                    c("1-Requesting syphilis manager release: syphilis-manager-v2026.07.27",
                      "1-Requesting census manager release: census-manager-v2026.10.08")))
# An unpinned census is still announced, so the log always says what was requested
unpinned.tree <- tempfile("analyses")
dir.create(file.path(unpinned.tree, "applications/SHIELD/R"), recursive = TRUE)
file.copy(file.path(shield.dir, "R/shield_data_managers.R"), file.path(unpinned.tree, "applications/SHIELD/R"))
writeLines(c(header, census, syphilis), file.path(unpinned.tree, "applications/SHIELD/data_managers.csv"))
unpinned <- select(analyses = unpinned.tree)
stopifnot(is.null(unpinned$census),
          identical(unpinned$output[[2L]], "1-Requesting census manager release: unpinned (the promoted census)"))

#-- A manager already in memory: reused, with a warning only when it is not the declared one --#
warnings.from <- function(release, modified = FALSE, declared = "syphilis-manager-v2026.10.09",
                          recorded = TRUE) {
    helpers$get.data.manager.provenance <- function(manager) {
        if (recorded) list(release = release, modified = modified)
    }
    on.exit(rm("get.data.manager.provenance", envir = helpers))
    seen <- character()
    withCallingHandlers(helpers$shield.check.loaded.data.manager("manager", "SURVEILLANCE.MANAGER", declared),
                        warning = function(w) { seen <<- c(seen, conditionMessage(w)); invokeRestart("muffleWarning") })
    seen
}
stopifnot(identical(warnings.from("syphilis-manager-v2026.10.09"), character()),
          identical(warnings.from("anything", declared = NULL), character()),
          grepl("loaded from syphilis-manager-v2026.09.09, not the declared syphilis-manager-v2026.10.09",
                warnings.from("syphilis-manager-v2026.09.09"), fixed = TRUE),
          grepl("(changed since it was loaded)", warnings.from("syphilis-manager-v2026.10.09", modified = TRUE),
                fixed = TRUE),
          grepl("loaded from an unrecorded release", warnings.from(NULL, recorded = FALSE), fixed = TRUE),
          grepl("rm(SURVEILLANCE.MANAGER)", warnings.from(NULL, recorded = FALSE), fixed = TRUE))
#-- The bootstrap's pull: it stops when the pull moves the checkout --#
# Files read before the pull (data_managers.csv among them) would otherwise come from
# the old commit and everything sourced after it from the new one.
sync.at <- which(vapply(expressions, function(expression) {
    any(grepl("3-Checking JHEEM_ANALYSES", deparse(expression), fixed = TRUE))
}, logical(1)))
stopifnot(length(sync.at) == 1L)
sync <- expressions[[sync.at + 1L]]
git <- function(...) {
    if (system2("git", c(...), stdout = FALSE, stderr = FALSE) != 0L)
        stop("git ", paste(c(...), collapse = " "), " failed")
}
commit.to <- function(repo, name) {
    writeLines(name, file.path(repo, name))
    git("-C", shQuote(repo), "add", name)
    git("-C", shQuote(repo), "-c", "user.name=test", "-c", "user.email=test@example.org",
        "commit", "-q", "-m", name)
}
upstream <- tempfile("upstream")
dir.create(upstream)
git("init", "-q", shQuote(upstream))
commit.to(upstream, "first")
checkout <- tempfile("checkout")
git("clone", "-q", shQuote(upstream), shQuote(checkout))
sync.in <- function(path, pull = TRUE) {
    env <- new.env(parent = globalenv())
    env$SHIELD.RECORDED.RUN <- FALSE
    env$PULL.GIT.UPDATES <- pull
    env$JHEEM.ANALYSES.PATH <- path
    tryCatch({ capture.output(eval(sync, env)); NULL }, error = function(e) conditionMessage(e))
}
stopifnot(is.null(sync.in(checkout)))                    # up to date: nothing moves
commit.to(upstream, "second")
moved <- sync.in(checkout)
stopifnot(!is.null(moved),
          grepl("git pull updated jheem_analyses", moved, fixed = TRUE),
          grepl("Source the specification again", moved, fixed = TRUE))
stopifnot(is.null(sync.in(checkout)))                    # sourcing again goes through
commit.to(upstream, "third")
stopifnot(is.null(sync.in(checkout, pull = FALSE)))      # pulls off: unchanged behavior
cat("SHIELD manager selection tests passed\n")
