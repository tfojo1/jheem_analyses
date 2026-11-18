# SHIELD sensitivity analyses: shared session setup (used by prcc/ and gam/)
#
# Sourced by the first line of each analysis's settings file (prcc/00_prcc_settings.R,
# gam/gam_settings.R), so every script runs it. It:
#   1. checks that every package the analysis needs is installed, then attaches them
#      (the settings file sets SENS.PACKAGES before sourcing this file);
#   2. sources the SHIELD base files ONLY when their objects are not already in the
#      session, or when a file has changed since it was last loaded here;
#   3. defines get_param_matrix(), which reads the calibrated parameters saved by
#      intervention/01a_generate_raw_results_arrays.R.
# Re-sourcing it in the same session costs almost nothing (it only checks).
# Run from the jheem_analyses folder: every source() path is relative to it.
#
# To reload everything once (e.g., after editing a file outside the list below):
#   FORCE.RELOAD <- TRUE    # in the console, then run any script; it resets to FALSE

# PACKAGES ----
if (!exists("SENS.PACKAGES")) SENS.PACKAGES <- "tidyverse"
missing.packages <- SENS.PACKAGES[!vapply(SENS.PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing.packages) > 0)
    stop("Missing R package(s): ", paste(missing.packages, collapse = ", "),
         "\n  Install with: install.packages(c(", paste0('"', missing.packages, '"', collapse = ", "), "))")
for (pkg in SENS.PACKAGES) suppressPackageStartupMessages(library(pkg, character.only = TRUE))   # no-op if attached

# SHIELD BASE FILES ----
# Each file is sourced if any of its objects is missing from the session, if the file
# changed since it was loaded here, or if FORCE.RELOAD is TRUE.
SETUP.FILES <- list(
    list(file    = '../jheem_analyses/commoncode/locations_of_interest.R',
         objects = c("SHIELD.TEN.MSAS")),
    list(file    = '../jheem_analyses/applications/SHIELD/shield_specification.R',
         objects = c("SHIELD.SPECIFICATION", "ROOT.DIR"))
    # list(file    = '../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R',
    #      objects = c("load.calib.simsets", "extract.calib.simsets"))    # not needed: parameters come from 01a
)

if (!exists(".SENS.SETUP.MTIMES")) .SENS.SETUP.MTIMES <- list()   # file -> modification time when loaded
force.reload <- exists("FORCE.RELOAD") && isTRUE(FORCE.RELOAD)

for (sf in SETUP.FILES) {
    if (!file.exists(sf$file)) stop("Setup file not found: ", sf$file,
                                    "\n  Run from the jheem_analyses folder.")
    mtime   <- file.mtime(sf$file)
    in.mem  <- all(vapply(sf$objects, exists, logical(1)))
    changed <- !is.null(.SENS.SETUP.MTIMES[[sf$file]]) && mtime > .SENS.SETUP.MTIMES[[sf$file]]

    if (force.reload || !in.mem || changed) {
        reason <- if (force.reload) "FORCE.RELOAD" else if (!in.mem) "not in session" else "file changed since last load"
        cat("[setup] ", basename(sf$file), ": loading (", reason, ")\n", sep = "")
        source(sf$file)
    } else {
        cat("[setup] ", basename(sf$file), ": already loaded - skipped\n", sep = "")
    }
    # Loaded earlier in the session by hand: record the current time stamp from now on
    .SENS.SETUP.MTIMES[[sf$file]] <- mtime
}
FORCE.RELOAD <- FALSE    # a forced reload applies once
rm(sf, mtime, in.mem, changed, force.reload, missing.packages)

# CALIBRATED PARAMETERS ----
# n.sim x n.params matrix for one MSA (rows = sims, in the same order as the outcome
# arrays; colnames = parameter names), from the no-intervention simset. Written by
# intervention/01a_generate_raw_results_arrays.R to
#   <ROOT.DIR>/shield/outputs/<calibration>/extracts/params/<MSA code>.Rdata  (object x)
# Stops if the file is missing: run 01a first (RUN.STRATA <- character(0) = parameters only).
get_param_matrix <- function(city, base.path = BASE.PATH, n.sim = N.SIM) {
    param.file <- paste0(base.path, "extracts/params/", city, ".Rdata")
    if (!file.exists(param.file))
        stop("Parameter file not found for ", city, ":\n  ", param.file,
             "\n  Run intervention/01a_generate_raw_results_arrays.R first ",
             "(RUN.STRATA <- character(0) extracts the parameters only).")
    x <- get(load(param.file))
    if (!is.matrix(x) || nrow(x) != n.sim)
        stop("Parameter file for ", city, " has ", NROW(x), " sims, expected ", n.sim, ": ", param.file)
    x
}
