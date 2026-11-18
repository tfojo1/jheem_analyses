# ****************************************************************************************************
# 01 EXTRACT SEATTLE RESULTS (NO DOXY-PEP + KING COUNTY SCENARIOS) ----
# ****************************************************************************************************
# Loads Seattle's simsets one at a time (noint + the doxy.kc.* scenarios, + doxy.cov.* if
# INCLUDE.COV.SCENARIOS = TRUE), pulls the outcomes below, and saves them in this analysis's own
# folder. Nothing is written to, or read from, the main result arrays.
#
# MEMORY: one simset in memory at a time when N.CORES = 1 (about N.CORES simsets otherwise).
#
# OUTPUT: <KC.OUT.DIR>/seattle_kc_raw_results.Rdata, one object `seattle.kc.results`:
#   $total : year x sim x outcome x intervention
#            outcomes include population.msm.15.64 = MSM aged 15-64 (SEXUAL.ACTIVITY.AGES), the
#            population doxy-PEP coverage applies to; use it, not population.msm (all ages), as
#            the denominator for the share of MSM on doxy-PEP
#   $sex   : year x sex x sim x outcome x intervention   (sex = msm, heterosexual_male, female)
# ****************************************************************************************************

source('../jheem_analyses/applications/SHIELD/analysis/intervention/seattle_vs_king_county/00_settings_seattle_kc.R')


# ---- SETTINGS FOR THIS STEP ----
N.CORES   <- 5       # simsets loaded at once; >1 uses forking (run from Terminal, not RStudio)
OVERWRITE <- T   # TRUE = redo extract files that already exist. Extracts made before
                     # population.msm.15.64 was added are redone automatically (see below)

TOTAL.OUTCOMES <- c("diagnosis.total", "diagnosis.ps", "incidence", "population",
                    "population.msm", "doxy.coverage")
SEX.OUTCOMES   <- c("diagnosis.total", "diagnosis.ps", "incidence", "population")


# ---- STEP 1: EXTRACT ONE SIMSET AT A TIME ----
extract.one.simset <- function(int) {
  sim.file <- file.path(SIM.DIR, paste0("shield_", CALIB.NAME, "-", N.SIM, "_", LOC.CODE, "_", int, ".Rdata"))
  out.file <- file.path(KC.EXTRACT.DIR, paste0(LOC.CODE, "_", int, ".Rdata"))

  if (!OVERWRITE && file.exists(out.file)) {
    # Keep an existing extract only if it already has every outcome this version pulls
    e <- new.env()
    ok <- tryCatch({ load(out.file, envir = e)
                     all(c(TOTAL.OUTCOMES, "population.msm.15.64") %in% dimnames(e$total)$outcome) },
                   error = function(err) FALSE)
    if (ok) return(paste0("already done   : ", int))
    message("Re-extracting ", int, " (extract is out of date)")
  }
  if (!file.exists(sim.file))               return(paste0("NO SIMSET FILE : ", sim.file))

  message("Loading ", int)
  start <- Sys.time()
  tryCatch({
    simset <- load.simulation.set(sim.file)

    # year x [by] x sim x outcome, location dropped (only one)
    # extra.dims: dimensions to subset (and sum over) without keeping them, e.g. age
    get.part <- function(outcomes, by, extra.dims = list()) {
      rv <- simset$get(outcomes,
                       keep.dimensions  = c("year", "location", by),
                       dimension.values = c(list(year = YEARS), extra.dims),
                       drop.single.outcome.dimension = FALSE)
      x <- aperm(rv, c("year", by, "sim", "outcome", "location"))
      n <- length(dim(x))
      stopifnot(dim(x)[n] == 1)
      array(x, dim = dim(x)[-n], dimnames = dimnames(x)[-n])
    }
    total <- get.part(TOTAL.OUTCOMES, NULL)
    sex   <- get.part(SEX.OUTCOMES, "sex")

    # MSM aged 15-64: the ages doxy-PEP coverage applies to (doxy.coverage outcome and
    # sexual.susceptibility both use SEXUAL.ACTIVITY.AGES). Added to `total` as one more outcome.
    pop.1564 <- get.part("population", NULL, list(sex = "msm", age = SEXUAL.ACTIVITY.AGES))
    dn <- dimnames(pop.1564)
    dn$outcome <- "population.msm.15.64"
    dimnames(pop.1564) <- dn
    stopifnot(identical(dimnames(total)[c("year", "sim")], dimnames(pop.1564)[c("year", "sim")]))
    dn <- dimnames(total)
    dn$outcome <- c(dn$outcome, "population.msm.15.64")
    total <- array(c(total, pop.1564), dim = c(dim(total)[1:2], dim(total)[3] + 1), dimnames = dn)
    save(total, sex, file = out.file)

    rm(simset, total, sex, pop.1564)
    gc()
    mins <- round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1)
    paste0("extracted      : ", int, " in ", mins, " min")
  },
  error = function(e) paste0("ERROR          : ", int, " -- ", conditionMessage(e)))
}

if (N.CORES > 1) {
  status <- parallel::mclapply(INTERVENTION.CODES, extract.one.simset,
                               mc.cores = N.CORES, mc.preschedule = FALSE)
} else {
  status <- lapply(INTERVENTION.CODES, extract.one.simset)
}
cat("\n---- STEP 1 SUMMARY ----\n")
cat(unlist(status), sep = "\n")


# ---- STEP 2: CHECK EVERY EXTRACT, THEN STACK ----
files   <- setNames(file.path(KC.EXTRACT.DIR, paste0(LOC.CODE, "_", INTERVENTION.CODES, ".Rdata")),
                    INTERVENTION.CODES)
missing <- INTERVENTION.CODES[!file.exists(files)]
if (length(missing) > 0)
  stop("No extract for: ", paste(missing, collapse = ", "),
       "\nCheck that these simsets exist in ", SIM.DIR, " and re-run.")

load.extract <- function(f) {
  e <- new.env()
  load(f, envir = e)
  list(total = e$total, sex = e$sex)
}
extracts <- lapply(files, load.extract)

# Every extract must have the same years, sims, outcomes (and sex groups) as the first
stack.part <- function(part) {
  first <- extracts[[1]][[part]]
  for (int in INTERVENTION.CODES)
    if (!identical(dimnames(extracts[[int]][[part]]), dimnames(first)))
      stop("Extract for '", int, "' (", part, ") has different dimensions from '", INTERVENTION.CODES[1],
           "'. Delete it and re-run with OVERWRITE = TRUE:\n  ", files[[int]])
  array(unlist(lapply(extracts, `[[`, part), use.names = FALSE),
        dim      = c(dim(first), length(INTERVENTION.CODES)),
        dimnames = c(dimnames(first), list(intervention = INTERVENTION.CODES)))
}

seattle.kc.results <- list(total = stack.part("total"), sex = stack.part("sex"))
attr(seattle.kc.results, "calibration.code") <- CALIB.NAME
attr(seattle.kc.results, "location")         <- LOC.CODE
attr(seattle.kc.results, "created")          <- Sys.time()

save(seattle.kc.results, file = KC.RESULTS.FILE)
rm(extracts)
cat("\nSaved:", KC.RESULTS.FILE, "\n")
str(lapply(seattle.kc.results, dimnames), max.level = 2)
