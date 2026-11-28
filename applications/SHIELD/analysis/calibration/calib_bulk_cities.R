# ****************************************************************************************************
# REVIEW A CALIBRATION ACROSS MANY CITIES ----
# ****************************************************************************************************
# Loading every city's full simset at once does not fit in memory (each file is about 1.3 GB).
# This script works in three steps:
#   1. EXTRACT: open each city's file once, keep small pieces, save them, free the memory.
#      While the city is loaded, it also makes the full set of stage-0 plots for that city
#      (plot.calib.stages, stage = 0: population, deaths, immigration, emigration, fertility,
#      PS diagnoses, by sex / race / age). Cities run in parallel (N.CORES at a time).
#      Finished cities are skipped, so a rerun picks up where the last one stopped.
#   2. CHECK:   table of 2022 PS diagnoses, model vs data, one row per city.
#   3. PLOTS:   PS diagnoses over time in each city against the data points.
#   Steps 2 and 3 read only the small ps pieces, so they take seconds to a minute.
#
# WHAT IS SAVED PER CITY
#   1. <loc>_ps.rds:     PS diagnoses by year for every simulation (year x sim). Small.
#   2. <loc>_last20.rds: the last 20 simulations as a simset, for simplot and the plot helpers.
#                        About 85 MB. Steps 2 and 3 do not read it.
#
# HOW LONG STEP 1 TAKES
#   1. Each city file has to be read whole (about 1.3 GB), so all 38 come to about 49 GB.
#   2. From the Mac the share read at about 18 MB/s (measured 2026-11-27), so step 1 takes
#      about 45-60 minutes. Extra cores barely help here: they share the same connection.
#   3. On a server with the share mounted locally it should be several times faster, and there
#      more cores do help. Change ROOT to that server's path.
#   4. The stage-0 plots add about 30 PNGs per city. That is CPU work, so it overlaps with the
#      reading in the other workers and adds little to the total.
#
# USAGE
#   1. Set ROOT, CALIB.CODE, N.SIM and N.CORES in the settings block.
#   2. Run from the folder that holds jheem_analyses (same as the other calibration scripts).
#   3. Run step 1 once, then rerun steps 2 and 3 as needed.
#   4. To remake the stage-0 plots after changing them, use replot.stage0() at the end of step 1.
#      It reads the saved last-20 files, not the full simsets.
#
# OUTPUT
#   <ROOT>/shield/calibrationPlots/<CALIB.CODE>/
#     1. <loc>/                       the stage-0 plots for each city
#     2. extracts/                    the pieces, two per city
#     3. ps_diagnoses_2022_check.csv  the step 2 table
#     4. ps_diagnoses_by_city.pdf     the step 3 plots, 12 cities per page
#
# NOTES ON PARALLEL
#   1. Uses parallel::mclapply (forked workers): works on the Mac and the shield servers, not Windows.
#   2. Each worker holds one full simset in memory, so peak memory is about N.CORES x one simset.
#      Check free memory before raising N.CORES.
#   3. Each piece is written as <name>.tmp and renamed when complete, so a killed run never leaves
#      a partial file that a rerun would mistake for a finished one.
# ****************************************************************************************************

source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source("../jheem_analyses/commoncode/locations_of_interest.R")
source("../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R")
library(parallel)
library(ggplot2)

# Settings ----
ROOT       <- get.jheem.root.directory()                 # /Volumes/jheem$ on the Mac
CALIB.CODE <- "calib.v2.11.25.s0"
N.SIM      <- 300
RUN        <- paste0(CALIB.CODE, "-", N.SIM)             # folder name on disk: <calibration.code>-<n.sim>
N.CORES    <- 4                                          # cities loaded at the same time
MAKE.STAGE.PLOTS <- TRUE                                 # FALSE = extract only, no stage-0 plots
YEARS      <- as.character(1970:2030)                    # years kept in the ps piece and plotted
                                                         # (changing it after step 1 needs step 1 redone:
                                                         #  delete extracts/*_ps.rds first)
REVIEW.DIR  <- file.path(ROOT, "shield", "calibrationPlots", CALIB.CODE)
EXTRACT.DIR <- file.path(REVIEW.DIR, "extracts")
dir.create(EXTRACT.DIR, recursive = TRUE, showWarnings = FALSE)


# ****************************************************************************************************
# STEP 1: EXTRACT (slow, run once) ----
# ****************************************************************************************************

## helpers ----
ps.file     <- function(loc) file.path(EXTRACT.DIR, paste0(loc, "_ps.rds"))
last20.file <- function(loc) file.path(EXTRACT.DIR, paste0(loc, "_last20.rds"))

simset.file <- function(loc) {
    file.path(ROOT, "simulations", "shield", RUN, loc,
              paste0("shield_", RUN, "_", loc, "_baseline.Rdata"))
}

save.safely <- function(obj, f) {
    tmp.f <- paste0(f, ".tmp")
    saveRDS(obj, tmp.f)
    file.rename(tmp.f, f)
}

city.name <- function(loc) {
    nm <- names(SHIELD.MSAS.OF.INTEREST)[match(loc, SHIELD.MSAS.OF.INTEREST)]
    ifelse(is.na(nm), loc, nm)
}

## make.stage0.plots ----
# 1. wraps one city into the calib.simsets shape that plot.calib.stages() expects
# 2. plots go to <ROOT>/shield/calibrationPlots/<CALIB.CODE>/<loc>/
# 3. returns TRUE when the plots were made
make.stage0.plots <- function(loc, last20, last) {
    entry <- list(full_simset   = last20,
                  last20_sims   = last20,
                  last_sim      = last,
                  location.name = city.name(loc),
                  location.code = loc,
                  calib.code    = CALIB.CODE,
                  title.suffix  = paste0(": ", CALIB.CODE))
    one  <- setNames(list(entry), .build.calib.key(city.name(loc), CALIB.CODE))
    done <- plot.calib.stages(one, calibration.code = CALIB.CODE, stage = 0,
                              root.dir = ROOT, verbose = FALSE)
    length(done) == 1
}

## extract.one ----
# 1. the full simset is dropped as soon as the pieces are taken; the plots use the last 20 only
# 2. the ps piece is written last, so it marks a finished city
# 3. returns "done", "skipped", "done, plots failed" or the error message,
#    so one bad city does not stop the others
extract.one <- function(loc) {
    if (file.exists(ps.file(loc))) return("skipped")
    tryCatch({
        ss <- load.simulation.set(simset.file(loc))
        n  <- ss$n.sim
        ps <- ss$get("diagnosis.ps", keep.dimensions = "year",
                     dimension.values = list(year = YEARS))
        ps <- matrix(ps, nrow = dim(ps)[1], dimnames = list(dimnames(ps)[[1]], NULL))   # year x sim
        last20 <- ss$subset((n - 19):n)
        last   <- ss$last.sim()
        rm(ss); gc()
        save.safely(last20, last20.file(loc))
        plots.ok <- if (MAKE.STAGE.PLOTS)
            tryCatch(make.stage0.plots(loc, last20, last), error = function(e) FALSE) else TRUE
        save.safely(ps, ps.file(loc))
        if (plots.ok) "done" else "done, plots failed"
    }, error = function(e) conditionMessage(e))
}

## build the job list: every city folder found on disk that is not extracted yet ----
all.locs <- list.dirs(file.path(ROOT, "simulations", "shield", RUN), full.names = FALSE, recursive = FALSE)
jobs     <- data.frame(loc = all.locs[!file.exists(ps.file(all.locs))], stringsAsFactors = FALSE)
cat(length(all.locs), "cities on disk,", nrow(jobs), "left to extract, using", N.CORES, "cores\n")

## run in parallel ----
# mc.preschedule = FALSE: each worker takes the next city as soon as it finishes one
if (nrow(jobs) > 0) {
    jobs$status <- unlist(mclapply(jobs$loc, extract.one,
                                   mc.cores = N.CORES, mc.preschedule = FALSE))
    print(table(ok = jobs$status %in% c("done", "skipped")))
    print(jobs[!jobs$status %in% c("done", "skipped"), ])   # failed cities and why
}

## replot.stage0 ----
# Remakes the stage-0 plots from the saved last-20 files (about 85 MB each), without the full
# simsets. Use it for cities whose plots failed, or after changing the plots.
#   replot.stage0()                         every extracted city
#   replot.stage0(c("C.12580", "C.35620"))  only these
replot.stage0 <- function(locs = sub("_last20\\.rds$", "", list.files(EXTRACT.DIR, pattern = "_last20\\.rds$")),
                          n.cores = N.CORES) {
    ok <- unlist(mclapply(locs, function(loc) {
        last20 <- readRDS(last20.file(loc))
        tryCatch(make.stage0.plots(loc, last20, last20$last.sim()), error = function(e) FALSE)
    }, mc.cores = n.cores, mc.preschedule = FALSE))
    if (any(!ok)) cat("Plots failed for:", locs[!ok], "\n")
    invisible(setNames(ok, locs))
}


# ****************************************************************************************************
# STEP 2: 2022 PS DIAGNOSES CHECK (fast) ----
# ****************************************************************************************************
# One row per city: 2022 data, the last simulation, median and range over all simulations,
# and the ratio last simulation / data. Flagged when the ratio is outside 0.67-1.5.

CHECK.YEAR  <- "2022"
RATIO.RANGE <- c(0.67, 1.5)

## helpers ----
extracted.locs <- function() sub("_ps\\.rds$", "", list.files(EXTRACT.DIR, pattern = "_ps\\.rds$"))

## pull.ps.data ----
# 1. one row per year and source; NULL when the city has no data
# 2. pull() returns more dimensions than asked for in some cases (e.g. source), so the
#    array is turned into a long table and only year, source and value are kept
pull.ps.data <- function(loc, years = YEARS) {
    d <- tryCatch(SURVEILLANCE.MANAGER$pull("ps.syphilis.diagnoses", keep.dimensions = "year",
                                            dimension.values = list(location = loc, year = years)),
                  error = function(e) NULL)
    if (is.null(d) || all(is.na(d))) return(NULL)
    df <- as.data.frame.table(d, responseName = "value", stringsAsFactors = FALSE)
    if (!"source" %in% names(df)) df$source <- "data"
    df <- df[!is.na(df$value), ]
    data.frame(loc = loc, year = as.numeric(df$year), source = df$source, value = df$value)
}

## make.year.table ----
make.year.table <- function(year = CHECK.YEAR) {
    do.call(rbind, lapply(extracted.locs(), function(loc) {
        sim <- readRDS(ps.file(loc))[year, ]
        dat <- pull.ps.data(loc, year)
        dat <- if (is.null(dat)) NA_real_ else mean(dat$value)   # average over sources
        data.frame(loc      = loc,
                   city     = city.name(loc),
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


# ****************************************************************************************************
# STEP 3: PS DIAGNOSES BY CITY, MODEL VS DATA (fast) ----
# ****************************************************************************************************
# One panel per city, 12 per page, same order as the step 2 table (flagged first).
#   1. grey lines:  the last 20 simulations
#   2. blue line:   the last simulation
#   3. points:      surveillance data, coloured by source
#   4. dashed line: CHECK.YEAR
# Panel titles show the city, its code and the 2022 ratio; "!" marks a flagged city.

N.LAST         <- 20
CITIES.PER.PAGE <- 12

## build the long tables ----
sims <- do.call(rbind, lapply(tab$loc, function(loc) {
    ps   <- readRDS(ps.file(loc))
    keep <- (ncol(ps) - N.LAST + 1):ncol(ps)
    data.frame(loc   = loc,
               year  = rep(as.numeric(rownames(ps)), length(keep)),
               sim   = rep(keep, each = nrow(ps)),
               value = as.vector(ps[, keep]),
               last  = rep(keep == ncol(ps), each = nrow(ps)))
}))
dat <- do.call(rbind, lapply(tab$loc, pull.ps.data))

## panel labels, in table order ----
labels <- setNames(paste0(tab$city, " (", tab$loc, ")  ratio ", tab$ratio, ifelse(tab$flag, "  !", "")),
                   tab$loc)
sims$label <- factor(labels[sims$loc], levels = labels)
dat$label  <- factor(labels[dat$loc],  levels = labels)

## one page ----
plot.page <- function(locs, page, n.pages) {
    s <- sims[sims$loc %in% locs, ]
    d <- dat[dat$loc %in% locs, ]
    ggplot() +
        geom_line(data = s[!s$last, ], aes(year, value, group = sim), colour = "grey60", alpha = 0.5, linewidth = 0.3) +
        geom_line(data = s[s$last, ],  aes(year, value), colour = "#1f4e79", linewidth = 0.8) +
        geom_point(data = d, aes(year, value, colour = source), size = 1.5) +
        geom_vline(xintercept = as.numeric(CHECK.YEAR), linetype = "dashed", colour = "grey40") +
        facet_wrap(~label, scales = "free_y", ncol = 4, drop = TRUE) +
        scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
        labs(x = NULL, y = "PS syphilis diagnoses", colour = "Data source",
             title = paste0(CALIB.CODE, ": PS diagnoses, last ", N.LAST, " simulations vs data"),
             subtitle = paste0("Page ", page, " of ", n.pages, ". Blue = last simulation. ",
                               "Ratio = last simulation / data in ", CHECK.YEAR, ". ! = outside ",
                               RATIO.RANGE[1], "-", RATIO.RANGE[2], ".")) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom", strip.text = element_text(size = 8))
}

## save all pages to one pdf ----
pages <- split(tab$loc, ceiling(seq_along(tab$loc) / CITIES.PER.PAGE))
pdf.file <- file.path(REVIEW.DIR, "ps_diagnoses_by_city.pdf")
pdf(pdf.file, width = 16, height = 11)
for (i in seq_along(pages)) print(plot.page(pages[[i]], i, length(pages)))
dev.off()
cat("Saved:", pdf.file, "\n")
