# Sensitivity analysis (PRCC, v3): session setup
#
# Sourced by the first line of 00_sensitivity_settings.R, so every step runs it.
# Loads the required packages and SHIELD base files ONLY when they are not already
# in the R session, or when a file has changed since it was last loaded here.
# Re-sourcing it in the same session costs almost nothing (it only checks).
# Run from the jheem_analyses folder: every source() path is relative to it.
#
# To reload everything once (e.g., after editing a file outside the list below):
#   FORCE.RELOAD <- TRUE    # in the console, then run any step; it resets to FALSE

# PACKAGES (all steps 1-4) ----
#   tidyverse : dplyr, tidyr, purrr, ggplot2
#   PRCC (step 3) uses calc_prcc_base() in 00_sensitivity_settings.R: base R only
REQUIRED.PACKAGES <- c("tidyverse")
missing.packages  <- REQUIRED.PACKAGES[!vapply(REQUIRED.PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing.packages) > 0)
    stop("Missing R package(s): ", paste(missing.packages, collapse = ", "),
         "\n  Install with: install.packages(c(", paste0('"', missing.packages, '"', collapse = ", "), "))")
for (pkg in REQUIRED.PACKAGES) suppressPackageStartupMessages(library(pkg, character.only = TRUE))   # no-op if attached

# SHIELD BASE FILES ----
# Each file is sourced if any of its objects is missing from the session, if the file
# changed since it was loaded here, or if FORCE.RELOAD is TRUE.
SETUP.FILES <- list(
    list(file    = '../jheem_analyses/commoncode/locations_of_interest.R',
         objects = c("SHIELD.TEN.MSAS")),
    list(file    = '../jheem_analyses/applications/SHIELD/shield_specification.R',
         objects = c("SHIELD.SPECIFICATION", "ROOT.DIR")),
    list(file    = '../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R',
         objects = c("load.calib.simsets", "extract.calib.simsets")),
    list(file    = '../jheem_analyses/applications/SHIELD/analysis/shield_param_cache.R',    # shared with the GAM analysis
         objects = c("get_param_matrix"))
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
