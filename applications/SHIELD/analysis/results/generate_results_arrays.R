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
#   Step 2. For each stratification, stack the extract files into one array and save it.
#
# WHY
#   The old script loaded all 110 simsets (~130 GB) into memory before extracting anything.
#   That ran out of RAM, swapped to disk, and took hours. Here only one simset is in memory at a
#   time (one per worker when running in parallel).
#
# RE-RUNNING
#   1. Step 1 skips any extract file that already exists. When new runs finish (e.g. NYC and
#      Seattle), re-running only loads the new simsets. Set OVERWRITE = TRUE to redo everything.
#   2. Step 2 always rebuilds the arrays from all the extract files. A simset that has not been
#      extracted becomes NA in the array and is listed at the end.
#   3. To add age later: add "age" to RUN.STRATA and re-run. Only the age extracts get made.
#      This does need to load every simset again.
#
# PARALLEL
#   Set N.CORES > 1. Each worker loads its own simset, so memory use is about N.CORES simsets.
#   Parallel runs use forking (parallel::mclapply). Run it from Terminal with Rscript, not RStudio:
#     cd ~/JHEEM/jheem_analyses
#     Rscript applications/SHIELD/analysis/results/generate_results_arrays.R
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


# ---- STEP 2: STACK THE EXTRACTS INTO ONE ARRAY PER STRATIFICATION ----
for (strat in RUN.STRATA) {
    files <- file.path(EXTRACT.DIR, strat, paste0(JOBS$location, "_", JOBS$intervention, ".Rdata"))
    found <- file.exists(files)
    if (!any(found)) {
        warning("No extract files for '", strat, "', skipping")
        next
    }

    # 1. Use the first extract as the template. Every other extract must match its shape exactly.
    template <- get(load(files[found][1]))

    # 2. Read every extract in JOBS order. A missing one becomes a block of NA.
    blocks <- lapply(seq_along(files), function(i) {
        if (!found[i]) return(array(NA_real_, dim(template)))
        x <- get(load(files[i]))
        if (!identical(dimnames(x), dimnames(template)))
            stop("Extract has different dimensions from the others: ", files[i])
        x
    })

    # 3. Stack them. Intervention and location are the last two dimensions.
    dn  <- c(dimnames(template), list(intervention = INTERVENTION.CODES, location = unname(LOCATIONS)))
    res <- array(unlist(blocks), dim = sapply(dn, length), dimnames = dn)

    # 4. Total keeps the old layout, with outcome last
    if (strat == "total")
        res <- aperm(res, c("year", "sim", "intervention", "location", "outcome"))

    # 5. Save as total_raw_results / sex_raw_results / age_raw_results
    name <- paste0(strat, "_raw_results")
    assign(name, res)
    save(list = name, file = file.path(OUT.DIR, paste0(name, ".Rdata")))

    cat("\nSaved ", name, " (", sum(found), " of ", length(files), " simsets)\n", sep = "")
    if (any(!found)) {
        cat("  Missing (filled with NA):\n")
        cat(paste0("    ", JOBS$location[!found], " / ", JOBS$intervention[!found]), sep = "\n")
    }
}
