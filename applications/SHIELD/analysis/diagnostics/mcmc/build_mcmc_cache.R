# ****************************************************************************************************
# BUILD THE MCMC CACHE ----
# ****************************************************************************************************
# Reads each city's raw MCMC once, drops the simulations, and saves the slimmed MCMC (about 1 MB) to
#   <jheem root>/shield/mcmc_diagnostics/mcmc_cache/<calibration code>_<location>.rds
# mixing_check.R and mixing_correlated_parameter_pairs.R then read the cache instead of the chunks.
#
# Why a separate script:
# 1. stage 4 has 500 chunks per chain of about 44 MB: about 84 GB per city, 840 GB for 10 cities,
#    all read from the shared drive. That takes hours, so run it unattended.
# 2. each city's cache is saved as soon as that city is done, so a crash loses only the cities in progress
# 3. a city that is already cached is read from the cache (quick), so a rerun picks up where it stopped
# 4. a city with an unfinished chain is not saved to the cache (see load.mcmc in mcmc/mcmc_helpers.R)
#
# Settings: CALIBRATION.CODE and LOCATIONS come from diagnostics_setup.R
#
# USAGE (from jheem_analyses). The log goes to the shared logs folder, like the launchers' logs
# (JHEEM_LOG_DIR if set, else /home/jheem-shared/logs), not into jheem_analyses:
#   LOG=${JHEEM_LOG_DIR:-/home/jheem-shared/logs}/build_mcmc_cache_$(date +%Y%m%d_%H%M).out
#   nohup Rscript applications/SHIELD/analysis/diagnostics/mcmc/build_mcmc_cache.R 4 > "$LOG" 2>&1 &
#   The number is how many cities to read at the same time (default 4). Memory per city is small;
#   the limit is how fast the drive can be read.
#   Monitor: tail -f "$LOG"
# The cache files themselves are saved on the shared drive: <jheem root>/shield/mcmc_diagnostics/mcmc_cache/
# ****************************************************************************************************

## SETTINGS ----
args    <- commandArgs(trailingOnly = TRUE)
N.CORES <- if (length(args) >= 1) as.integer(args[1]) else 4

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
cat("Building the MCMC cache for", CALIBRATION.CODE, "|", length(LOCATIONS), "cities |", N.CORES, "at a time\n")
cat("Cache folder:", .cache.dir(), "\n")

## one.city ----
# loads (and caches) one city; returns one row for the summary
one.city <- function(loc) {
    start <- Sys.time()
    cat(format(start, "%F %T"), "START", .city.name(loc), "\n")
    # load.mcmc() prints why a city failed and returns an empty list
    m <- tryCatch(load.mcmc(loc, CALIBRATION.CODE, n.cores = 1),
                  error = function(e) { cat(.city.name(loc), "FAILED:", conditionMessage(e), "\n"); NULL })
    m <- if (length(m)) m[[1]] else NULL
    minutes <- round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1)
    cat(format(Sys.time(), "%F %T"), "DONE ", .city.name(loc), "|", minutes, "min\n")
    file <- file.path(.cache.dir(), paste0(CALIBRATION.CODE, "_", loc, ".rds"))
    data.frame(city            = .city.name(loc),
               location        = loc,
               loaded          = !is.null(m),
               n.chains        = if (is.null(m)) NA else m@n.chains,
               saved.per.chain = if (is.null(m)) NA else m@n.iter,
               cached          = file.exists(file),
               minutes         = minutes)
}

## RUN ----
# several cities at a time; a city that fails is reported and the rest continue
rows <- parallel::mclapply(unname(LOCATIONS), one.city, mc.cores = min(N.CORES, length(LOCATIONS)),
                           mc.preschedule = FALSE)
ok   <- sapply(rows, is.data.frame)
if (any(!ok)) cat("Worker crashed for:", paste(.city.name(unname(LOCATIONS)[!ok]), collapse = ", "),
                  "(if out of memory, rerun with fewer at a time)\n")

## SUMMARY ----
# cached = TRUE for every city means mixing_check.R can run without reading the chunks
cat("\n== MCMC cache for", CALIBRATION.CODE, "==\n")
print(do.call(rbind, rows[ok]), row.names = FALSE)
