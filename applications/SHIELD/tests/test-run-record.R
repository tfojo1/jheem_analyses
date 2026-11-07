# Check the run records SHIELD calibration steps write (R/shield_run_record.R) without
# loading SHIELD or jheem2: temporary git checkouts and stand-in managers.
# Rscript applications/SHIELD/tests/test-run-record.R
script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(script.argument) == 1L)
script.path <- normalizePath(sub("^--file=", "", script.argument))
shield.dir <- normalizePath(file.path(dirname(script.path), ".."))
source(file.path(shield.dir, "R/shield_run_record.R"))

root <- tempfile("run-record-")
dir.create(root)
make.checkout <- function(name) {
    path <- file.path(root, name)
    dir.create(path)
    git <- function(...) stopifnot(system2("git", c("-C", shQuote(path), ...), stdout = FALSE) == 0L)
    git("init", "-q", "-b", "dev")
    writeLines("a", file.path(path, "tracked.R"))
    git("add", "tracked.R")
    git("-c", "user.name=test", "-c", "user.email=test@example.com", "commit", "-q", "-m", "first")
    path
}
JHEEM.ANALYSES.PATH <- make.checkout("jheem_analyses")
JHEEM2.PATH <- make.checkout("jheem2")
writeLines("changed", file.path(JHEEM.ANALYSES.PATH, "tracked.R"))
writeLines("untracked", file.path(JHEEM.ANALYSES.PATH, "untracked.R"))
USE.JHEEM2.PACKAGE <- FALSE

# Stand-ins for the loader's and jheem2's identity functions
get.data.manager.resolution <- function(data.manager) attr(data.manager, "jheem.manager.resolution", exact = TRUE)
get.data.manager.provenance <- function(data.manager) attr(data.manager, "provenance", exact = TRUE)
get.calibration.dir <- function(version, location, calibration.code) {
    file.path(root, "mcmc_runs", version, calibration.code, location)
}
SHIELD.DATA.MANAGERS <- list(census = NULL, syphilis = "syphilis-manager-v2026.10.09")
CENSUS.MANAGER <- structure(list(), jheem.manager.resolution = list(
    resolved_tag = "census-manager-v2026.11.05", repository = "tfojo1/jheem_analyses",
    sha256 = strrep("e", 64)), provenance = list(md5 = strrep("a", 32), modified = TRUE))
SURVEILLANCE.MANAGER <- structure(list(), provenance = list(md5 = strrep("b", 32), modified = FALSE))

git.head <- function(path) system2("git", c("-C", shQuote(path), "rev-parse", "HEAD"), stdout = TRUE)
read.record <- function(path) {
    entries <- as.data.frame(read.dcf(path, all = TRUE), stringsAsFactors = FALSE)
    split(entries, entries$record)
}

#-- A chain's record --#
path <- shield.write.run.record("C.12580", "calib.test", "run", chain = 2L)
calibration.dir <- get.calibration.dir("shield", "C.12580", "calib.test")
stopifnot(identical(normalizePath(dirname(path)), normalizePath(file.path(calibration.dir, "run_records"))),
          grepl("^run-chain2-[0-9]{8}T[0-9]{6}Z-.+-[0-9]+\\.dcf$", basename(path)),
          length(list.files(dirname(path), pattern = "partial")) == 0L)
record <- read.record(path)
run <- record$run
stopifnot(nrow(run) == 1L, run$format == SHIELD.RUN.RECORD.FORMAT, run$step == "run",
          run$chain == "2", run$location == "C.12580", run$calibration.code == "calib.test",
          run$pid == as.character(Sys.getpid()), is.na(run$seed),
          run$r.version == paste(R.version$major, R.version$minor, sep = "."))

sources <- record$source
rownames(sources) <- sources$name
stopifnot(setequal(sources$name, c("jheem_analyses", "jheem2", "bayesian.simulations",
                                   "distributions", "locations")),
          sources["jheem_analyses", "commit"] == git.head(JHEEM.ANALYSES.PATH),
          sources["jheem_analyses", "branch"] == "dev",
          sources["jheem_analyses", "changed"] == "1",           # untracked files are not counted
          sources["jheem_analyses", "changed.files"] == "tracked.R",
          sources["jheem2", "kind"] == "checkout",
          sources["jheem2", "commit"] == git.head(JHEEM2.PATH),
          sources["jheem2", "changed"] == "0",
          is.na(sources["jheem2", "changed.files"]),
          all(sources[c("bayesian.simulations", "distributions", "locations"), "kind"] == "package"))

managers <- record$`data manager`
rownames(managers) <- managers$name
stopifnot(managers["census", "declared"] == "unpinned",
          managers["census", "loaded"] == "release",
          managers["census", "release"] == "census-manager-v2026.11.05",
          managers["census", "sha256"] == strrep("e", 64),
          managers["census", "modified"] == "TRUE",
          managers["syphilis", "declared"] == "syphilis-manager-v2026.10.09",
          managers["syphilis", "loaded"] == "unrecorded",
          is.na(managers["syphilis", "sha256"]),
          managers["syphilis", "md5"] == strrep("b", 32))

#-- Setup records its seed; each step writes its own file --#
setup <- read.record(shield.write.run.record("C.12580", "calib.test", "setup", seed = 0))$run
stopifnot(setup$step == "setup", setup$seed == "0", is.na(setup$chain),
          length(list.files(file.path(calibration.dir, "run_records"))) == 2L)

#-- A jheem2 package instead of a checkout --#
USE.JHEEM2.PACKAGE <- TRUE
jheem2 <- shield.run.record("C.12580", "calib.test", "run", 1L, NA)[[3L]]
stopifnot(jheem2$name == "jheem2", jheem2$kind == "package")
USE.JHEEM2.PACKAGE <- FALSE

#-- A record that cannot be written warns and lets the step continue --#
get.calibration.dir <- function(version, location, calibration.code) file.path(JHEEM2.PATH, "tracked.R", "x")
warned <- NULL
result <- withCallingHandlers(shield.write.run.record("C.12580", "calib.test", "assemble"),
                              warning = function(w) {
                                  warned <<- conditionMessage(w)
                                  invokeRestart("muffleWarning")
                              })
stopifnot(is.null(result), grepl("Could not write the run record", warned, fixed = TRUE))

#-- The modular launcher records each step, and the SHIELD bootstrap sources the writer --#
modular <- readLines(file.path(shield.dir, "shield_calib_setup_and_run_modular.R"))
calls <- grep("shield.write.run.record(", modular, fixed = TRUE, value = TRUE)
stopifnot(length(calls) == 3L,
          any(grepl('"setup"', calls)), any(grepl('"run", chain = CHAIN', calls)),
          any(grepl('"assemble"', calls)),
          any(grepl('"R/shield_run_record.R"', readLines(file.path(shield.dir, "shield_source_code.R")),
                    fixed = TRUE)))

unlink(root, recursive = TRUE)
cat("Run record tests passed\n")
