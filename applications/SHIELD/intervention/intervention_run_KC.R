# Get location and calibration stage from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) stop("Usage: Rscript script.R <location> <calibration.stage> <n.sim> <first.year> <last.year> [intervention.code]")

LOCATION         <- as.character(args[1])
CALIBRATION.CODE <- as.character(args[2])
N.SIM <- as.numeric(args[3])
FIRST.YEAR <- as.numeric(args[4])
LAST.YEAR <- as.numeric(args[5])
# Optional 6th argument: run ONE scenario per process, so the launcher can run them in parallel.
# Leave it out to run all four scenarios in turn in this process.
RUN.ONE <- if (length(args) >= 6) as.character(args[6]) else NA_character_

# These scenarios are calibrated to King County uptake: Seattle MSA only
if (LOCATION != "C.42660") stop("intervention_run_KC.R is for Seattle (C.42660) only; got ", LOCATION)

# ****************************************************************************
# DoxyPEP Intervention Analysis
# ****************************************************************************
# Locate the repo ----
# Resolved from this script's own path, so the run depends on neither the
# working directory nor the checkout being named "jheem_analyses".
if (!exists("JHEEM.ANALYSES.PATH")) {
    .arg   <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    .ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    .d <- if (length(.arg))          dirname(normalizePath(sub("^--file=", "", .arg[1])))
          else if (!is.null(.ofile)) dirname(normalizePath(.ofile))
          else                       normalizePath(getwd())
    while (!dir.exists(file.path(.d, "commoncode")) ||
           !dir.exists(file.path(.d, "applications", "SHIELD"))) {
        if (identical(dirname(.d), .d))
            stop("Could not locate the jheem_analyses repo root above ", .d)
        .d <- dirname(.d)
    }
    JHEEM.ANALYSES.PATH <- .d
    rm(.arg, .ofile, .d)
}

cat("Repo root:", JHEEM.ANALYSES.PATH, "\n")
source(file.path(JHEEM.ANALYSES.PATH, "commoncode", "locations_of_interest.R"))
source(file.path(JHEEM.ANALYSES.PATH, "applications", "SHIELD", "shield_specification.R"))
# King County-aligned doxy-PEP scenarios (Menza et al. 2026, PMID 41885610)
# source(file.path(JHEEM.ANALYSES.PATH, "applications", "SHIELD", "intervention", "intervention_definitions.R"))
source(file.path(JHEEM.ANALYSES.PATH, "applications", "SHIELD", "intervention", "intervention_definitions-kc.R"))


# source("../jheem_analyses/applications/SHIELD/intervention/intervention_helper_functions.R")

# *****************************************************************************
# SECTION 1: Configuration
# *****************************************************************************
# King County-aligned scenarios (defined in intervention_definitions-kc.R).
# noint and doxy.cov.5-50 already exist for Seattle (stage 4 run), so they are not rerun here.
INTERVENTION.LABELS <- c(
    doxy.kc.30.flat = "KC uptake: 15% mid-2023, 30% May 2024, flat",
    doxy.kc.30.to50 = "KC uptake: 15% mid-2023, 30% May 2024, 50% by end 2025",
    doxy.kc.45.flat = "KC uptake: 15% mid-2023, 45% May 2024, flat",
    doxy.kc.45.to50 = "KC uptake: 15% mid-2023, 45% May 2024, 50% by end 2025"
)

INTERVENTION.CODES <- names(INTERVENTION.LABELS)
if (!is.na(RUN.ONE)) {
    if (!RUN.ONE %in% INTERVENTION.CODES)
        stop("Unknown intervention code '", RUN.ONE, "'. Use one of: ", paste(INTERVENTION.CODES, collapse = ", "))
    INTERVENTION.CODES <- RUN.ONE
}
cat("Running intervention(s):", paste(INTERVENTION.CODES, collapse = ", "), "\n")

# *****************************************************************************
# SECTION 2: Run Interventions
# *****************************************************************************
# --- Create and Run Simulation Collection ---
if (1==1){
    sim.collection <- create.simset.collection(
        version = "shield",
        calibration.code = CALIBRATION.CODE,
        locations = LOCATION,
        interventions = INTERVENTION.CODES,
        n.sim = N.SIM
    )
    #
    FORCE.OVERWRITE<- FALSE
    #
    sim.collection$run(
        FIRST.YEAR,
        LAST.YEAR,
        verbose = T,
        stop.for.errors = FALSE,
        overwrite.prior = FORCE.OVERWRITE,
        keep.from.year = FIRST.YEAR
    )
}
