# ****************************************************************************************************
# SHIELD OUTPUT PATHS ----
# ****************************************************************************************************
# Where the analysis output for one calibration lives. Give these functions a calibration name
# and they give you back the folder.
#
# THE LAYOUT
#   <root>/shield/outputs/calib.8.21.stage3.az/
#       figures/                    png written by the figure scripts
#       tables/                     csv written by the table scripts
#       total_raw_results.Rdata     the four result arrays, written by
#       total_calc_results.Rdata    intervention/01_generate_raw_results_arrays.R and
#       sex_raw_results.Rdata       intervention/02_generate_custom_outcomes.R
#       sex_calc_results.Rdata
#
# HOW TO USE IT
#   Set the calibration name once at the top of a script, then build the three folders from it:
#
#     CALIB.NAME <- "calib.8.21.stage3.az"
#     OUT.DIR    <- shield.output.path(CALIB.NAME, create = TRUE)
#     FIG.DIR    <- shield.fig.path(CALIB.NAME,    create = TRUE)
#     TABLE.DIR  <- shield.table.path(CALIB.NAME,  create = TRUE)
#
#   Switching to a different calibration is now one edit per script instead of four.
#
# WHY NOT A BASE.PATH CONSTANT
#   BASE.PATH was pasted together in six scripts, with "calib.8.21.stage3.az" typed out in each
#   one, so moving to a new calibration meant editing six files and hoping none was missed.
#   A constant is also frozen the moment the script runs, which leaves no way to ask for a
#   second calibration's folder in the same session. These are functions, so you can:
#
#     shield.fig.path("calib.8.21.stage3.az")   # old run
#     shield.fig.path("calib.9.19.stage3")      # new run, same session
#
# ROOT DIRECTORY
#   root.dir = NULL (the default) uses get.jheem.root.directory(), which is ROOT.DIR from
#   commoncode/file_paths.R: "Q:" on the desktop, "/Volumes/jheem$" on the Mac. Pass root.dir
#   to read or write somewhere else. Never hard-code "Q:/..." -- that path only exists on the
#   desktop, and the scripts that read it back run on the Mac too.
# ****************************************************************************************************


## .check.calibration.code ----
.check.calibration.code <- function(calibration.code) {

    if (missing(calibration.code) || is.null(calibration.code) ||
        !is.character(calibration.code) || length(calibration.code) != 1 ||
        is.na(calibration.code) || !nzchar(calibration.code))
        stop("'calibration.code' must be a single calibration name, ",
             'for example "calib.8.21.stage3.az"')

    invisible(calibration.code)
}


## shield.output.path ----
# The folder holding everything for one calibration.
#
#   shield.output.path("calib.8.21.stage3.az")
#   -> "/Volumes/jheem$/shield/outputs/calib.8.21.stage3.az"
#
# create = TRUE makes the folder if it is not there yet. Leave it FALSE when you are only
# reading, so a typo in the calibration name fails instead of quietly creating an empty folder.
shield.output.path <- function(calibration.code, root.dir = NULL, create = FALSE) {

    .check.calibration.code(calibration.code)
    if (is.null(root.dir)) root.dir <- get.jheem.root.directory()

    path <- file.path(root.dir, "shield", "outputs", calibration.code)
    if (create) ensure.shield.dir(path)
    path
}


## shield.fig.path ----
# The figures/ folder for one calibration. Assign it to FIG.DIR at the top of a figure script.
shield.fig.path <- function(calibration.code, root.dir = NULL, create = FALSE) {

    path <- file.path(shield.output.path(calibration.code, root.dir = root.dir), "figures")
    if (create) ensure.shield.dir(path)
    path
}


## shield.table.path ----
# The tables/ folder for one calibration. Assign it to TABLE.DIR at the top of a table script.
shield.table.path <- function(calibration.code, root.dir = NULL, create = FALSE) {

    path <- file.path(shield.output.path(calibration.code, root.dir = root.dir), "tables")
    if (create) ensure.shield.dir(path)
    path
}


## ensure.shield.dir ----
# Creates a folder if it is missing and says so. Kept here, rather than using ensure.plot.dir()
# from shield_plot_core.R, so that a table-only script can source this file on its own.
ensure.shield.dir <- function(path) {

    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        message("Created directory: ", path)
    }
    invisible(path)
}


## load.shield.results ----
# Reads the result arrays for one calibration and returns them in a list, ready to pass
# straight to the 'data' argument of the table and figure functions.
#
#   intervention.results <- load.shield.results(CALIB.NAME)
#   make_multi_location_table(data = intervention.results, ...)
#
# 1. THE ARRAYS
#    Each array is one stratification level x one kind, saved as <level>_<kind>_results.Rdata:
#
#                      raw outcomes (step 01)   calculated outcomes (step 02)
#      total level     total_raw                total_calc
#      sex level       sex_raw                  sex_calc
#      race level      race_raw                 race_calc       (only if steps 01-02 built them)
#
#    By default the four total and sex arrays are loaded, in that order. Ask for others with
#    `which`, e.g. which = c("total_raw", "total_calc", "race_raw", "race_calc").
#
# 2. REUSING WHAT IS ALREADY LOADED
#    Loading takes a while, so the arrays are read from disk only when needed. If an object
#    called `cache.name` (default "intervention.results") is already in your workspace, it is
#    reused when all three hold:
#      1. it was loaded for the same calibration
#      2. it holds every array you asked for
#      3. none of its files has changed on disk since it was loaded (e.g. step 02 was re-run)
#    Otherwise the arrays are read from disk again. This only works if you assign the result to
#    that same name, as in the example above.
#
# 3. FORCING A RELOAD
#    force.reload = TRUE always reads from disk.
#
# 4. A MISSING FILE IS AN ERROR
#    It names the file and the script that writes it, rather than returning a short list that
#    would silently shift every data[[i]] index.
load.shield.results <- function(calibration.code,
                                which        = c("total_raw", "total_calc", "sex_raw", "sex_calc"),
                                cache.name   = "intervention.results",
                                force.reload = FALSE,
                                root.dir     = NULL) {

    # 1. Where the files are
    bad <- which[!grepl("^[a-z]+_(raw|calc)$", which)]
    if (length(bad) > 0)
        stop("'which' entries must look like \"total_raw\" or \"sex_calc\", not: ",
             paste(bad, collapse = ", "))

    out.dir <- shield.output.path(calibration.code, root.dir = root.dir)
    if (!dir.exists(out.dir))
        stop("No output folder for '", calibration.code, "':\n  ", out.dir,
             "\nCheck the calibration name, or run intervention/01_generate_raw_results_arrays.R first.")

    paths   <- setNames(file.path(out.dir, paste0(which, "_results.Rdata")), which)
    missing <- which[!file.exists(paths)]
    if (length(missing) > 0)
        stop("Missing result file(s) in ", out.dir, ":\n  ",
             paste(basename(paths[missing]), collapse = "\n  "),
             "\nraw arrays come from intervention/01_generate_raw_results_arrays.R,",
             " calc arrays from intervention/02_generate_custom_outcomes.R.")
    mtimes <- setNames(file.mtime(paths), which)   # file.mtime() drops the names

    # 2. Reuse the copy in the workspace if it is still current
    if (!force.reload && exists(cache.name, envir = globalenv(), inherits = FALSE)) {
        cached   <- get(cache.name, envir = globalenv())
        same.cal <- identical(attr(cached, "calibration.code"), calibration.code)
        has.all  <- all(which %in% names(cached))
        old.time <- attr(cached, "file.mtimes")
        same.files <- has.all && !is.null(old.time) && all(which %in% names(old.time)) &&
            all(old.time[which] == mtimes[which])

        if (same.cal && has.all && same.files) {
            message("Using '", cache.name, "' already in memory (", calibration.code, "). ",
                    "force.reload = TRUE to read from disk.")
            out <- cached[which]
            attr(out, "calibration.code") <- calibration.code
            attr(out, "file.mtimes")      <- old.time[which]
            return(out)
        }
        message("Reloading: '", cache.name, "' in memory is ",
                if (!same.cal) paste0("for ", if (is.null(attr(cached, "calibration.code"))) "an unknown calibration"
                                              else attr(cached, "calibration.code"))
                else if (!has.all) "missing some of the arrays asked for"
                else "older than the files on disk")
    }

    # 3. Read from disk
    out <- lapply(which, function(w) {
        message("Loading ", w, " ...")
        get(load(paths[[w]]))
    })
    names(out) <- which
    attr(out, "calibration.code") <- calibration.code
    attr(out, "file.mtimes")      <- mtimes
    out
}
