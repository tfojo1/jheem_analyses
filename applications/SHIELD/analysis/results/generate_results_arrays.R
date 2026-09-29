# ****************************************************************************************************
# GENERATE RESULTS ARRAYS, ONE SIMSET FILE AT A TIME ----
# ****************************************************************************************************
# Replaces generate_total_results_array.R.
#
# WHAT IT DOES
#   Step 1. For each location x intervention:
#             1. load that one simset file
#             2. pull out the outcomes for each stratification (total, sex, ...)
#             3. save each one as a small "extract" file
#             4. drop the simset from memory
#   Step 2. Check every extract file (missing, empty, unreadable, wrong shape). If any is damaged,
#           list them by city and intervention and stop before saving anything.
#   Step 3. For each stratification, stack the extract files into one array and save it.
#
# WHY
#   The old script loaded all 110 simsets (~130 GB) into memory before extracting anything.
#   That ran out of RAM, swapped to disk, and took hours. Here only one simset is in memory at a
#   time (one per worker when running in parallel).
#
# RE-RUNNING
#   1. Step 1 skips any extract file that already exists. When new runs finish (e.g. NYC and
#      Seattle), re-running only loads the new simsets. Set OVERWRITE = TRUE to redo everything.
#   2. Steps 2 and 3 always rebuild the arrays from all the extract files. A simset that has not
#      been extracted becomes NA in the array and is listed at the end.
#   3. To add age later: add "age" to RUN.STRATA and re-run. Only the age extracts get made.
#      This does need to load every simset again.
#
# PARALLEL
#   Set N.CORES > 1. Each worker loads its own simset, so memory use is about N.CORES simsets.
#   Parallel runs use forking (parallel::mclapply). Run it from Terminal, not RStudio (see below).
#
# RUNNING IN THE BACKGROUND WITH NOHUP
#   nohup keeps the script running if you close the terminal or lose the connection.
#   1. Go to the jheem_analyses folder (the source() paths below are relative to it):
#        cd ~/JHEEM/jheem_analyses
#   2. Start it. All output goes to the log file, and the command prints the process ID:
#        nohup Rscript applications/SHIELD/analysis/results/generate_results_arrays.R > generate_results_arrays.log 2>&1 &
#   3. Watch progress (Ctrl+C stops watching, not the script):
#        tail -f generate_results_arrays.log
#   4. Check it is still running:
#        ps aux | grep generate_results_arrays
#   5. Stop it if needed (extract files already saved are kept):
#        kill <process ID>
#
# OUTPUT (same layout as the old script, so downstream code does not change)
#   total_raw_results : year x sim x intervention x location x outcome
#   sex_raw_results   : year x sex x sim x outcome x intervention x location
#   age_raw_results   : year x age x sim x outcome x intervention x location  (if turned on)
# ****************************************************************************************************

source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R')


# ---- SETTINGS ----
CALIB.NAME <- "calib.9.23.stage3.pk"
N.SIM      <- 400
YEARS      <- 2000:2040
LOCATIONS  <- SHIELD.TEN.MSAS
INTERVENTION.CODES <- c("noint",
                        "doxy.cov.5",  "doxy.cov.10", "doxy.cov.15", "doxy.cov.20", "doxy.cov.25",
                        "doxy.cov.30", "doxy.cov.35", "doxy.cov.40", "doxy.cov.45", "doxy.cov.50")

# Which arrays to build. Add "age" to build the age array too.
RUN.STRATA <- c("total", "sex")

# For each stratification:
#   by       = the dimension to keep besides year (NULL for total)
#   outcomes = what to pull out
# All outcomes in one list must share the same groups for the 'by' dimension.
STRATA <- list(
    total = list(
        by = NULL,
        # "diagnosis.el.misclassified", "diagnosis.late.misclassified",
        outcomes = c(
                     "diagnosis.ps", "diagnosis.total", "incidence", "population","prevalence", "sti.screening", 
                     "population.msm",  "prop.male.ps.diag.among.msm","doxy.coverage")
    ),
    sex = list(
        by = "sex",
        # Left out: doxy.coverage (MSM only) and hiv.testing (different age groups)
        outcomes = c( "diagnosis.ps", "diagnosis.total", "incidence", "population","prevalence", "sti.screening")
    ),
    age = list(
        by = "age",
        # Left out: doxy.coverage (ages 15-64 only) and hiv.testing (different age groups)
        outcomes = c( "diagnosis.ps", "diagnosis.total", "incidence", "population","prevalence", "sti.screening")
    )
)

N.CORES   <- 1       # 1 = one file at a time. Try 3-4 from Terminal and watch memory.
OVERWRITE <- FALSE   # TRUE = redo extract files that already exist


# ---- FOLDERS ----
# Simsets are read from:  <root>/simulations/shield/<calib>-<n.sim>/<location>/
# Extracts are saved to:  <root>/shield/outputs/<calib>/extracts/<stratification>/
# Arrays are saved to:    <root>/shield/outputs/<calib>/
SIM.DIR     <- file.path(get.jheem.root.directory(), "simulations", "shield", paste0(CALIB.NAME, "-", N.SIM))
OUT.DIR     <- shield.output.path(CALIB.NAME, create = TRUE)
EXTRACT.DIR <- file.path(OUT.DIR, "extracts")
for (strat in RUN.STRATA)
    dir.create(file.path(EXTRACT.DIR, strat), recursive = TRUE, showWarnings = FALSE)

# One row per simset, ordered location 1 / all interventions, location 2 / all interventions, ...
# This order matches the intervention x location dimensions of the final arrays.
JOBS <- expand.grid(intervention = INTERVENTION.CODES,
                    location     = unname(LOCATIONS),
                    stringsAsFactors = FALSE)


# ---- STEP 1: EXTRACT ONE SIMSET AT A TIME ----
# Loads the simset in row i of JOBS, saves one extract file per stratification, and returns
# a one-line status message.
extract.one.simset <- function(i) {
    loc   <- JOBS$location[i]
    int   <- JOBS$intervention[i]
    label <- paste0(names(LOCATIONS)[LOCATIONS == loc], " / ", int)

    sim.file      <- file.path(SIM.DIR, loc, paste0("shield_", CALIB.NAME, "-", N.SIM, "_", loc, "_", int, ".Rdata"))
    extract.files <- setNames(file.path(EXTRACT.DIR, RUN.STRATA, paste0(loc, "_", int, ".Rdata")), RUN.STRATA)

    # 1. Work out which stratifications still need extracting
    todo <- RUN.STRATA[OVERWRITE | !file.exists(extract.files)]
    if (length(todo) == 0)      return(paste0("already done    : ", label))
    if (!file.exists(sim.file)) return(paste0("NO SIMSET FILE  : ", label))

    message("Loading ", label)
    start <- Sys.time()
    tryCatch({
        # 2. Load the simset
        simset <- load.simulation.set(sim.file)

        # 3. Pull out each stratification and save it
        for (strat in todo) {
            by <- STRATA[[strat]]$by
            rv <- simset$get(STRATA[[strat]]$outcomes,
                             keep.dimensions = c("year", "location", by),
                             dimension.values = list(year = YEARS),
                             drop.single.outcome.dimension = FALSE)

            # Reorder to year x [by] x sim x outcome x location, then drop location (only one value)
            x <- aperm(rv, c("year", by, "sim", "outcome", "location"))
            n <- length(dim(x))
            stopifnot(dim(x)[n] == 1)
            x <- array(x, dim = dim(x)[-n], dimnames = dimnames(x)[-n])

            save(x, file = extract.files[[strat]])
        }

        # 4. Free the memory before the next file
        rm(simset, rv, x)
        gc()

        mins <- round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1)
        paste0("extracted       : ", label, " [", paste(todo, collapse = ", "), "] in ", mins, " min")
    },
    error = function(e) paste0("ERROR           : ", label, " -- ", conditionMessage(e)))
}

if (N.CORES > 1) {
    # mc.preschedule = FALSE: each simset runs in its own fresh worker, which exits afterwards,
    # so its memory is fully returned
    status <- parallel::mclapply(seq_len(nrow(JOBS)), extract.one.simset,
                                 mc.cores = N.CORES, mc.preschedule = FALSE)
} else {
    status <- lapply(seq_len(nrow(JOBS)), extract.one.simset)
}

cat("\n---- STEP 1 SUMMARY ----\n")
cat(unlist(status), sep = "\n")


# ---- STEP 2: CHECK EVERY EXTRACT FILE BEFORE MERGING ----
# Reads every extract file and gives each one a status:
#   ok           loaded, with the expected years, sims and outcomes
#   missing      no file: the simset has not been run yet, or Step 1 failed for it
#   empty        zero-byte file: the save was cut off
#   unreadable   the file is there but load() fails: the save was cut off partway
#   wrong shape  loads, but its years / sims / outcomes / groups differ from the settings or
#                from the other extracts. Usually it was made before the outcome list changed.
# Missing extracts become NA in the arrays (Step 3). Any other problem stops the script here,
# before anything is saved, and prints the files to delete. Re-running then re-extracts just those.
CITY     <- names(LOCATIONS)[match(JOBS$location, LOCATIONS)]
EXTRACTS <- list()
PROBLEMS <- NULL

cat("\n---- STEP 2: CHECKING EXTRACT FILES ----\n")
for (strat in RUN.STRATA) {
    files  <- file.path(EXTRACT.DIR, strat, paste0(JOBS$location, "_", JOBS$intervention, ".Rdata"))
    status <- rep("ok", length(files))
    blocks <- vector("list", length(files))

    # 1. Check each file on its own
    for (i in seq_along(files)) {
        if (!file.exists(files[i]))    { status[i] <- "missing"; next }
        if (file.size(files[i]) == 0)  { status[i] <- "empty";   next }

        x <- tryCatch(get(load(files[i])), error = function(e) NULL)
        if (is.null(x))                { status[i] <- "unreadable"; next }

        if (!identical(dimnames(x)$year, as.character(YEARS)) ||
            length(dimnames(x)$sim) != N.SIM ||
            !identical(dimnames(x)$outcome, STRATA[[strat]]$outcomes)) {
            status[i] <- "wrong shape"; next
        }
        blocks[[i]] <- x
    }

    # 2. Every good extract must also match the first good one (e.g. same sex or age groups)
    good <- which(status == "ok")
    for (i in good[-1])
        if (!identical(dimnames(blocks[[i]]), dimnames(blocks[[good[1]]]))) status[i] <- "wrong shape"

    EXTRACTS[[strat]] <- list(blocks = blocks, status = status)
    PROBLEMS <- rbind(PROBLEMS,
                      data.frame(stratification = strat, city = CITY, intervention = JOBS$intervention,
                                 status = status, file = files)[status != "ok", ])
    cat(strat, ": ", sum(status == "ok"), " of ", length(files), " extracts ok\n", sep = "")
}

# 3. Report problems, by city and intervention
if (nrow(PROBLEMS) > 0) {
    cat("\nProblems found:\n")
    print(PROBLEMS[, c("stratification", "city", "intervention", "status")], row.names = FALSE)
}

# 4. Stop if any file is damaged or out of date. Missing files alone do not stop the script.
bad <- PROBLEMS[PROBLEMS$status != "missing", ]
if (nrow(bad) > 0) {
    cat("\nThese extract files are damaged or out of date. Delete them, then re-run this script.\n",
        "Step 1 will re-extract just these simsets:\n", sep = "")
    cat(paste0("  rm '", bad$file, "'"), sep = "\n")
    stop(nrow(bad), " bad extract file(s), listed above. No arrays were saved.")
}


# ---- STEP 3: STACK THE EXTRACTS INTO ONE ARRAY PER STRATIFICATION ----
for (strat in RUN.STRATA) {
    blocks <- EXTRACTS[[strat]]$blocks
    found  <- EXTRACTS[[strat]]$status == "ok"
    if (!any(found)) {
        warning("No extract files for '", strat, "', skipping")
        next
    }

    # 1. A missing extract becomes a block of NA, the same shape as the others
    template <- blocks[[which(found)[1]]]
    blocks[!found] <- list(array(NA_real_, dim(template)))

    # 2. Stack them. Intervention and location are the last two dimensions.
    dn  <- c(dimnames(template), list(intervention = INTERVENTION.CODES, location = unname(LOCATIONS)))
    res <- array(unlist(blocks), dim = sapply(dn, length), dimnames = dn)

    # 3. Total keeps the old layout, with outcome last
    if (strat == "total")
        res <- aperm(res, c("year", "sim", "intervention", "location", "outcome"))

    # 4. Save as total_raw_results / sex_raw_results / age_raw_results
    name <- paste0(strat, "_raw_results")
    assign(name, res)
    save(list = name, file = file.path(OUT.DIR, paste0(name, ".Rdata")))

    cat("\nSaved ", name, " (", sum(found), " of ", length(found), " simsets)\n", sep = "")
    if (any(!found)) {
        cat("  Missing (filled with NA):\n")
        cat(paste0("    ", CITY[!found], " / ", JOBS$intervention[!found]), sep = "\n")
    }
}
