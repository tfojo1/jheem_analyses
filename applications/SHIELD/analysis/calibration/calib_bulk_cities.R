# ****************************************************************************************************
# REVIEW A CALIBRATION ACROSS MANY CITIES ----
# ****************************************************************************************************
# Loading every city's full simset at once does not fit in memory (each file is about 1.3 GB).
# This script works in two steps:
#   1. EXTRACT: open each city's file once, keep a small piece, save it, free the memory.
#      Cities run in parallel (N.CORES at a time). Finished cities are skipped, so a rerun
#      picks up where the last one stopped.
#   2. REVIEW: everything after that reads only the small pieces, so it takes seconds.
#
# WHAT EACH PIECE HOLDS (one .rds per city)
#   1. ps:     PS diagnoses by year for every simulation (year x sim), 2000-2030
#   2. last20: the last 20 simulations, as a simset (works with simplot and the plot helpers)
#   3. last:   the last simulation
#
# USAGE
#   1. Set ROOT, CALIB.CODE, N.SIM and N.CORES in the settings block.
#   2. Run from the folder that holds jheem_analyses (same as the other calibration scripts).
#   3. Step 1 is the slow part (network copy + loading). Run it once, then rerun step 2 as needed.
#
# OUTPUT
#   <ROOT>/shield/calibrationPlots/<CALIB.CODE>/
#     1. extracts/  the small pieces, one per city
#     2. ps_diagnoses_2022_check.csv
#
# NOTES ON PARALLEL
#   1. Uses parallel::mclapply (forked workers): works on the Mac and the shield servers, not Windows.
#   2. Each worker holds one full simset in memory, so peak memory is about N.CORES x one simset.
#      Check free memory before raising N.CORES.
#   3. On the Mac the network share is usually the bottleneck, so more than 3-4 cores helps little.
#      On a server with the share mounted, more cores help more.
#   4. Each piece is written as <name>.tmp and renamed when complete, so a killed run never leaves
#      a partial file that a rerun would mistake for a finished one.
# ****************************************************************************************************

source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source("../jheem_analyses/commoncode/locations_of_interest.R")
library(parallel)

# Settings ----
ROOT    <- get.jheem.root.directory()                    # /Volumes/jheem$ on the Mac
CALIB.CODE <- "calib.v2.11.25.s0"
N.SIM      <- 300
RUN        <- paste0(CALIB.CODE, "-", N.SIM)             # folder name on disk: <calibration.code>-<n.sim>
N.CORES    <- 4                                          # cities loaded at the same time
YEARS      <- as.character(2000:2030)                    # years kept in the ps piece
REVIEW.DIR  <- file.path(ROOT, "shield", "calibrationPlots", CALIB.CODE)
EXTRACT.DIR <- file.path(REVIEW.DIR, "extracts")
dir.create(EXTRACT.DIR, recursive = TRUE, showWarnings = FALSE)


# ****************************************************************************************************
# STEP 1: EXTRACT (slow, run once) ----
# ****************************************************************************************************

## helpers ----
extract.file <- function(loc) file.path(EXTRACT.DIR, paste0(loc, ".rds"))

simset.file <- function(loc) {
    file.path(ROOT, "simulations", "shield", RUN, loc,
              paste0("shield_", RUN, "_", loc, "_baseline.Rdata"))
}

## extract.one ----
# returns "done", "skipped" or the error message, so one bad city does not stop the others
extract.one <- function(loc) {
    out.f <- extract.file(loc)
    if (file.exists(out.f)) return("skipped")
    tryCatch({
        ss <- load.simulation.set(simset.file(loc))
        n  <- ss$n.sim
        piece <- list(ps     = ss$get("diagnosis.ps", keep.dimensions = "year",
                                      dimension.values = list(year = YEARS)),   # year x sim
                      last20 = ss$subset((n - 19):n),
                      last   = ss$last.sim())
        rm(ss); gc()
        tmp.f <- paste0(out.f, ".tmp")
        saveRDS(piece, tmp.f)
        file.rename(tmp.f, out.f)
        "done"
    }, error = function(e) conditionMessage(e))
}

## build the job list: every city folder found on disk that is not extracted yet ----
all.locs <- list.dirs(file.path(ROOT, "simulations", "shield", RUN), full.names = FALSE, recursive = FALSE)
jobs     <- data.frame(loc = all.locs[!file.exists(extract.file(all.locs))], stringsAsFactors = FALSE)
cat(length(all.locs), "cities on disk,", nrow(jobs), "left to extract, using", N.CORES, "cores\n")

## run in parallel ----
# mc.preschedule = FALSE: each worker takes the next city as soon as it finishes one
if (nrow(jobs) > 0) {
    jobs$status <- unlist(mclapply(jobs$loc, extract.one,
                                   mc.cores = N.CORES, mc.preschedule = FALSE))
    print(table(ok = jobs$status %in% c("done", "skipped")))
    print(jobs[!jobs$status %in% c("done", "skipped"), ])   # failed cities and why
}


# ****************************************************************************************************
# STEP 2: 2022 PS DIAGNOSES CHECK (fast) ----
# ****************************************************************************************************
# One row per city: 2022 data, the last simulation, median and range over all simulations,
# and the ratio last simulation / data. Flagged when the ratio is outside 0.67-1.5.

CHECK.YEAR  <- "2022"
RATIO.RANGE <- c(0.67, 1.5)

## pull.ps.data ----
# averages over sources; NA when there is no data for that city and year
pull.ps.data <- function(loc, year) {
    d <- tryCatch(SURVEILLANCE.MANAGER$pull("ps.syphilis.diagnoses", keep.dimensions = "year",
                                            dimension.values = list(location = loc, year = year)),
                  error = function(e) NULL)
    if (is.null(d) || all(is.na(d))) NA_real_ else mean(d, na.rm = TRUE)
}

## make.year.table ----
make.year.table <- function(year = CHECK.YEAR) {
    locs <- sub("\\.rds$", "", list.files(EXTRACT.DIR, pattern = "\\.rds$"))
    do.call(rbind, lapply(locs, function(loc) {
        x   <- readRDS(extract.file(loc))
        sim <- x$ps[year, ]
        dat <- pull.ps.data(loc, year)
        data.frame(loc      = loc,
                   city     = names(SHIELD.MSAS.OF.INTEREST)[match(loc, SHIELD.MSAS.OF.INTEREST)],
                   data     = round(dat),
                   last.sim = round(tail(sim, 1)),
                   median   = round(median(sim)),
                   lo       = round(min(sim)),
                   hi       = round(max(sim)),
                   ratio    = round(tail(sim, 1) / dat, 2),
                   stringsAsFactors = FALSE)
    }))
}

## build, sort and save ----
tab <- make.year.table()
tab$flag <- is.na(tab$ratio) | tab$ratio < RATIO.RANGE[1] | tab$ratio > RATIO.RANGE[2]
tab <- tab[order(-tab$flag, -abs(log(tab$ratio))), ]   # flagged first, then worst fit first

print(tab, row.names = FALSE)
cat("\nFlagged:", sum(tab$flag), "of", nrow(tab), "cities\n")
write.csv(tab, file.path(REVIEW.DIR, paste0("ps_diagnoses_", CHECK.YEAR, "_check.csv")), row.names = FALSE)
