# ****************************************************************************************************
# ASSEMBLE ONE CITY AND SAVE THREE SIMSETS ----
# ****************************************************************************************************
# 1. full:    every saved simulation (stage 4: 4 chains x 5,000 = 20,000)
# 2. thinned: no burn, one simulation every THIN.EVERY iterations (stage 4: 2,000)
# 3. burned:  second half of each chain of (2), so the same spacing (stage 4: 1,000)
#
# USAGE (normally through launchers/launch_assemble_and_thin.sh)
#   Rscript shield_assemble_and_thin.R <location> <calibration.code> [thin.every=500] [burn.keep=0.5]
#
# NOTES
# 1. Only reads the MCMC cache. It never runs setup, so it cannot clear the chains.
# 2. allow.incomplete = F: a city with an unfinished chain stops with an error, not a partial simset.
# 3. A simset already on disk is not redone or overwritten, so a rerun picks up where the last one
#    stopped. To remake one, delete its file first.
# 4. Assembling the full simset is the memory-heavy step (stage 4: about 150 GB, maybe double).
# 5. Each file is written as <name>.tmp and renamed when complete, so a job killed during a save
#    never leaves a partial file that a rerun would mistake for a finished one.
# ****************************************************************************************************

# Get arguments from the command line ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript shield_assemble_and_thin.R <location> <calibration.code> [thin.every] [burn.keep]")

LOCATION         <- as.character(args[1])
CALIBRATION.NAME <- as.character(args[2])
THIN.EVERY       <- if (length(args) >= 3) as.numeric(args[3]) else 500   # iterations between kept simulations
BURN.KEEP        <- if (length(args) >= 4) as.numeric(args[4]) else 0.5   # fraction of each chain to keep

cat("Location:", LOCATION, "\n")
cat("Calibration stage:", CALIBRATION.NAME, "\n")
cat("Thin every:", THIN.EVERY, "iterations | Burn keep:", BURN.KEEP, "\n")

# Locate the repo ----
# Same search as shield_calib_setup_and_run_modular.R
.arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.d   <- if (length(.arg)) dirname(normalizePath(sub("^--file=", "", .arg[1]))) else normalizePath(getwd())
while (!dir.exists(file.path(.d, "commoncode")) ||
       !dir.exists(file.path(.d, "applications", "SHIELD"))) {
    if (identical(dirname(.d), .d))
        stop("Could not locate the jheem_analyses repo root above ", .d)
    .d <- dirname(.d)
}
JHEEM.ANALYSES.PATH <- .d
rm(.arg, .d)
SHIELD.DIR <- file.path(JHEEM.ANALYSES.PATH, "applications", "SHIELD")
cat("Repo root:", JHEEM.ANALYSES.PATH, "\n")

# Source requirements ----
source(file.path(SHIELD.DIR, "shield_specification.R"))
source(file.path(SHIELD.DIR, "shield_likelihoods.R"))
source(file.path(SHIELD.DIR, "shield_calib_register.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode", "locations_of_interest.R"))

VERSION <- 'shield'

# Work out the three sizes from the calibration ----
info <- get.calibration.info(CALIBRATION.NAME)
if (THIN.EVERY %% info$thin != 0)
    stop("thin.every (", THIN.EVERY, ") must be a multiple of the calibration's thin (", info$thin, ")")

N.FULL    <- info$n.chains * floor((info$n.iter - info$n.burn) / info$thin)
N.THINNED <- N.FULL / (THIN.EVERY / info$thin)
N.BURNED  <- N.THINNED * BURN.KEEP
if (N.THINNED != round(N.THINNED) || N.BURNED != round(N.BURNED))
    stop("These settings do not give a whole number of simulations: thinned = ", N.THINNED, ", burned = ", N.BURNED)
cat("Sizes: full", N.FULL, "| thinned", N.THINNED, "| burned", N.BURNED, "\n")

# Helpers ----
simset.file <- function(n.sim) {
    get.simset.filename(version = VERSION, location = LOCATION,
                        calibration.code = CALIBRATION.NAME, n.sim = n.sim)
}

load.simset <- function(n.sim) {
    print(paste0("Loading the ", n.sim, " simset for ", LOCATION))
    retrieve.simulation.set(version = VERSION, location = LOCATION,
                            calibration.code = CALIBRATION.NAME, n.sim = n.sim)
}

save.simset <- function(simset, expected.n.sim) {
    if (simset$n.sim != expected.n.sim)
        stop("Expected ", expected.n.sim, " simulations for ", LOCATION, " but have ", simset$n.sim, " - not saving")
    file <- simset.file(expected.n.sim)
    tmp  <- paste0(file, ".tmp")
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    save(simset, file = tmp)
    if (!file.rename(tmp, file)) stop("Could not rename ", tmp, " to ", file)
    print(paste0("Saved ", file))
}

need.full    <- !file.exists(simset.file(N.FULL))
need.thinned <- !file.exists(simset.file(N.THINNED))
need.burned  <- !file.exists(simset.file(N.BURNED))
full <- NULL
thinned <- NULL

# 1. Full simset ----
if (need.full) {
    print(paste0("Assembling all chains from ", CALIBRATION.NAME, " code for ", LOCATION,
                 " (", locations::get.location.name(LOCATION), ")"))
    full <- assemble.simulations.from.calibration(version = VERSION,
                                                  location = LOCATION,
                                                  calibration.code = CALIBRATION.NAME,
                                                  allow.incomplete = F)
    save.simset(full, N.FULL)
} else print(paste0("Full simset already saved: ", simset.file(N.FULL)))

# 2. Thinned, no burn ----
if (need.thinned) {
    if (is.null(full)) full <- load.simset(N.FULL)
    print(paste0("Thinning to ", N.THINNED, " simulations"))
    thinned <- full$thin(keep = N.THINNED)
    save.simset(thinned, N.THINNED)
} else print(paste0("Thinned simset already saved: ", simset.file(N.THINNED)))

# Free the full simset before the burn; the burn only needs the thinned one
rm(full)
gc()

# 3. Burned and thinned ----
# Burning the thinned simset picks the same simulations as burning first and thinning after
if (need.burned) {
    if (is.null(thinned)) thinned <- load.simset(N.THINNED)
    print(paste0("Keeping the last ", BURN.KEEP, " of each chain: ", N.BURNED, " simulations"))
    burned <- thinned$burn(keep = BURN.KEEP)
    save.simset(burned, N.BURNED)
} else {
    print(paste0("Burned simset already saved: ", simset.file(N.BURNED)))
    # 4. Consistency check, when the thinned simset was made after the burned one ----
    # The burned simset on disk is not overwritten (interventions may be built on it), so check that
    # burning the new thinned simset gives the same simulations: MATCH = same parameters, in the same order
    if (need.thinned) {
        saved <- load.simset(N.BURNED)
        new   <- thinned$burn(keep = BURN.KEEP)
        same  <- saved$n.sim == new$n.sim && isTRUE(all.equal(saved$parameters, new$parameters, check.attributes = FALSE))
        print(paste0("Burned simset on disk vs burn of the new thinned simset: ", if (same) "MATCH" else "DIFFERENT"))
        if (!same) print("The burned simset on disk was not made from this thinned simset. Check before using the thinned one with it.")
    }
}

print(paste0("Assemble and thin complete for ", LOCATION))
