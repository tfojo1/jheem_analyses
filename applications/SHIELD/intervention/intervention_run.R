# Get location and calibration stage from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) stop("Usage: Rscript script.R <location> <calibration.stage> <n.sim> <first.year> <last.year>")

LOCATION         <- as.character(args[1])
CALIBRATION.CODE <- as.character(args[2])
N.SIM <- as.numeric(args[3])
FIRST.YEAR <- as.numeric(args[4])
LAST.YEAR <- as.numeric(args[5])

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
SHIELD.DIR <- file.path(JHEEM.ANALYSES.PATH, "applications", "SHIELD")
cat("Repo root:", JHEEM.ANALYSES.PATH, "\n")

source(file.path(SHIELD.DIR, "shield_specification.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode", "locations_of_interest.R"))
source(file.path(SHIELD.DIR, "intervention", "intervention_definitions.R"))
# source("../jheem_analyses/applications/SHIELD/intervention/intervention_helper_functions.R")

# *****************************************************************************
# SECTION 1: Configuration
# *****************************************************************************
# LOCATIONS        <- SHIELD.TEN.MSAS    # Named vector: names = city, values = codes


# CALIBRATION.CODE <- "calib.6.16.stage3.az"  
# N.SIM <- 300
# FIRST.YEAR <- 2000
# LAST.YEAR <- 2040

INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxy.cov.10   = "coverage 10%",
    doxy.cov.20   = "coverage 20%",
    doxy.cov.30   = "coverage 30%",
    doxy.cov.40   = "coverage 40%",
    doxy.cov.50   = "coverage 50%",
    doxy.cov.60   = "coverage 60%",
    doxy.cov.70   = "coverage 70%",
    doxy.cov.80   = "coverage 80%",
    doxy.cov.90   = "coverage 90%",
    doxy.cov.100  = "coverage 100%"
)
INTERVENTION.CODES <- names(INTERVENTION.LABELS)

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
