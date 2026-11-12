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
# jheem2 is pushed to a remote; jheem_analyses is pushed, then has one local commit on top.
remote <- file.path(root, "remote.git")
stopifnot(system2("git", c("init", "-q", "--bare", shQuote(remote)), stdout = FALSE) == 0L)
for (checkout in c(JHEEM2.PATH, JHEEM.ANALYSES.PATH)) {
    stopifnot(system2("git", c("-C", shQuote(checkout), "remote", "add", "origin", shQuote(remote)), stdout = FALSE) == 0L,
              system2("git", c("-C", shQuote(checkout), "push", "-q", "-u", "origin",
                               paste0("dev:", basename(checkout))), stdout = FALSE, stderr = FALSE) == 0L)
}
writeLines("b", file.path(JHEEM.ANALYSES.PATH, "local.R"))
stopifnot(system2("git", c("-C", shQuote(JHEEM.ANALYSES.PATH), "add", "local.R"), stdout = FALSE) == 0L,
          system2("git", c("-C", shQuote(JHEEM.ANALYSES.PATH), "-c", "user.name=test", "-c",
                           "user.email=test@example.com", "commit", "-q", "-m", "local"), stdout = FALSE) == 0L)
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
          sources["jheem2", "on.remote"] == "yes", sources["jheem2", "ahead"] == "0",
          sources["jheem_analyses", "on.remote"] == "no",       # the local commit is on no remote
          sources["jheem_analyses", "ahead"] == "1",
          all(sources[c("bayesian.simulations", "distributions", "locations"), "kind"] == "package"))

# The platform, and every non-base package loaded in the session
stopifnot(nrow(record$platform) == 1L, !is.na(record$platform$os), !is.na(record$platform$lapack.version))
loaded <- record$`loaded package`
expected <- setdiff(loadedNamespaces(), c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
                                          "methods", "parallel", "splines", "stats", "stats4", "tcltk",
                                          "tools", "utils"))
stopifnot(setequal(if (is.null(loaded)) character() else loaded$name, expected))
if (requireNamespace("MASS", quietly = TRUE)) {
    entries <- shield.run.record("C.12580", "calib.test", "run", 3L, NA)
    mass <- Filter(function(e) e$record == "loaded package" && e$name == "MASS", entries)
    stopifnot(length(mass) == 1L, mass[[1L]]$version == utils::packageDescription("MASS")$Version)
}

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

#-- Finishing a step: an end entry, then the files it saved --#
small <- file.path(root, "small.Rdata"); writeBin(charToRaw("abc\n"), small)                  # 4 bytes
large <- file.path(root, "large.Rdata"); writeBin(charToRaw(paste0(strrep("x", 20), "\n")), large)  # 21 bytes
Sys.setenv(SHIELD_RUN_RECORD_HASH_LIMIT_GB = "1e-8")                         # 10 bytes
started <- shield.write.run.record("C.12580", "calib.test", "assemble")
stopifnot(is.null(read.record(started)$end))                                 # not finished yet
finished <- shield.finish.run.record(started, c(small, large, file.path(root, "missing.Rdata")))
Sys.unsetenv("SHIELD_RUN_RECORD_HASH_LIMIT_GB")
record <- read.record(finished)
outputs <- record$output
rownames(outputs) <- basename(outputs$path)
stopifnot(identical(finished, started), nrow(record$run) == 1L,
          nrow(record$end) == 1L, record$end$outcome == "completed",
          setequal(rownames(outputs), c("small.Rdata", "large.Rdata")),          # the missing file is skipped
          outputs["small.Rdata", "size"] == "4", outputs["large.Rdata", "size"] == "21",
          is.na(outputs["large.Rdata", "sha256"]),                               # over the hashing limit
          length(list.files(dirname(started), pattern = "partial")) == 0L,
          is.null(shield.finish.run.record(NULL, small)))
stopifnot(outputs["small.Rdata", "sha256"] == "edeaaff3f1774ad2888673770c6d64097e391bc362d7d6fb34982ddf0efd18cb")

#-- The files each step saves --#
cache <- file.path(calibration.dir, "cache")
dir.create(cache)
invisible(file.create(file.path(cache, c("global_control.Rdata", "chain1_control.Rdata", "chain1_chunk1.Rdata"))))
get.mcmc.summary.file <- function(version, location, calibration.code) file.path(root, "summary.Rdata")
stopifnot(setequal(basename(shield.step.outputs("setup", "C.12580", "calib.test")),
                   c("global_control.Rdata", "chain1_control.Rdata")),
          basename(shield.step.outputs("run", "C.12580", "calib.test", chain = 2L)) == "chain2_control.Rdata",
          basename(shield.step.outputs("assemble", "C.12580", "calib.test")) == "summary.Rdata")
rm(get.mcmc.summary.file)
stopifnot(identical(shield.step.outputs("assemble", "C.12580", "calib.test"), character()))  # jheem2 can't say

#-- A record that cannot be written warns and lets the step continue --#
get.calibration.dir <- function(version, location, calibration.code) file.path(JHEEM2.PATH, "tracked.R", "x")
warned <- NULL
result <- withCallingHandlers(shield.write.run.record("C.12580", "calib.test", "assemble"),
                              warning = function(w) {
                                  warned <<- conditionMessage(w)
                                  invokeRestart("muffleWarning")
                              })
stopifnot(is.null(result), grepl("Could not write the run record", warned, fixed = TRUE))

#-- The modular launcher sources the writer and records each step --#
modular <- readLines(file.path(shield.dir, "shield_calib_setup_and_run_modular.R"))
calls <- grep("shield.write.run.record(", modular, fixed = TRUE, value = TRUE)
finishes <- grep("shield.finish.run.record(RUN.RECORD, shield.step.outputs(", modular, fixed = TRUE, value = TRUE)
stopifnot(length(finishes) == 3L, length(grep("RUN.RECORD <- shield.write.run.record(", modular, fixed = TRUE)) == 3L,
          any(grepl('"setup"', finishes)), any(grepl('"run"', finishes)), any(grepl('"assemble"', finishes)))
stopifnot(length(calls) == 3L,
          any(grepl('"setup"', calls)), any(grepl('"run", chain = CHAIN', calls)),
          any(grepl('"assemble"', calls)),
          any(grepl('source(file.path(SHIELD.DIR, "R", "shield_run_record.R"))', modular, fixed = TRUE)))

unlink(root, recursive = TRUE)
cat("Run record tests passed\n")
