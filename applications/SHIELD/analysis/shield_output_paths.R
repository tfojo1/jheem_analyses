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
#       total_calc_results.Rdata    results/generate_total_results_array.R and
#       sex_raw_results.Rdata       results/generate_custom_outcomes.R
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
#   RESULTS <- load.shield.results("calib.8.21.stage3.az")
#   make_multi_location_table(data = RESULTS, ...)
#
# 1. THE FOUR ARRAYS
#    Two stratification levels x two kinds:
#
#                      raw outcomes       calculated outcomes
#      total level     total_raw          total_calc
#      sex level       sex_raw            sex_calc
#
#    The list keeps that order, which is the order the scripts used when they built
#    'results' by hand, so figures that index data[[1]] behave as before.
#
# 2. ASKING FOR A SUBSET
#    generate_custom_outcomes.R runs before the calculated arrays exist, so it asks for the
#    raw ones only:
#
#      RAW <- load.shield.results(CALIB.NAME, which = c("total_raw", "sex_raw"))
#
# 3. A MISSING FILE IS AN ERROR
#    It names the file and the script that writes it, rather than returning a short list that
#    would silently shift every data[[i]] index.
load.shield.results <- function(calibration.code,
                                which    = c("total_raw", "total_calc", "sex_raw", "sex_calc"),
                                root.dir = NULL) {

    files <- c(total_raw  = "total_raw_results.Rdata",
               total_calc = "total_calc_results.Rdata",
               sex_raw    = "sex_raw_results.Rdata",
               sex_calc   = "sex_calc_results.Rdata")

    which <- match.arg(which, choices = names(files), several.ok = TRUE)

    out.dir <- shield.output.path(calibration.code, root.dir = root.dir)
    if (!dir.exists(out.dir))
        stop("No output folder for '", calibration.code, "':\n  ", out.dir,
             "\nCheck the calibration name, or run results/generate_total_results_array.R first.")

    paths   <- file.path(out.dir, files[which])
    missing <- which[!file.exists(paths)]
    if (length(missing) > 0)
        stop("Missing result file(s) in ", out.dir, ":\n  ",
             paste(files[missing], collapse = "\n  "),
             "\nraw arrays come from results/generate_total_results_array.R,",
             " calc arrays from results/generate_custom_outcomes.R.")

    setNames(lapply(paths, function(f) get(load(f))), which)
}
