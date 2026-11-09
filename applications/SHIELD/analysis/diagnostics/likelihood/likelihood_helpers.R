# ****************************************************************************************************
# SHIELD CALIBRATION — LIKELIHOOD DIAGNOSTICS: HELPER FUNCTIONS
# ****************************************************************************************************
# Functions only. Sourced by diagnostics_setup.R. To run the diagnostics, use likelihood/likelihood_breakdown.R
# (Moved out of mcmc_helpers.R: these use the saved simsets and the likelihoods, not the raw MCMC.)
#
# What the functions need when you call them:
#  1. jheem2 loaded and the jheem root directory set - for loading simsets and the output folder
#  2. The calibration registered (shield_calib_register.R) - for get.calibration.info():
#     the likelihood instructions and the data manager the calibration used
#  3. Optional: SHIELD.TEN.MSAS (commoncode/locations_of_interest.R) - to show city names instead of codes
#
# Contents:
#  1. likelihood.breakdown()      - one city: what each likelihood contributes
#  2. run.likelihood.breakdown()  - many cities, one at a time; posterior.sd as likelihood x city
#  3. save.likelihood.breakdown() - CSVs in calibration_diagnostics/<code>/likelihood/
# ****************************************************************************************************

# Output folder, on the server (shared with the team), next to the mixing folder:
#   <jheem root>/shield/calibration_diagnostics/<calibration code>/likelihood/
.likelihood.dir <- function(calibration.code) file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics",
                                                        calibration.code, "likelihood")

# Results are kept in these lists, keyed "location | calibration.code",
# so nothing is overwritten when you move to the next city
if (!exists("lik.list"))           lik.list           <- list()   # instantiated likelihoods
if (!exists("lik.breakdown.list")) lik.breakdown.list <- list()   # results of likelihood.breakdown()

.lik.key <- function(loc, code) paste0(loc, " | ", code)

# city name for a location code (e.g. "C.12580" -> "Baltimore"); the code itself if SHIELD.TEN.MSAS is not loaded
.lik.city.name <- function(loc) {
    if (!exists("SHIELD.TEN.MSAS")) return(loc)
    nm <- names(SHIELD.TEN.MSAS)[match(loc, SHIELD.TEN.MSAS)]
    ifelse(is.na(nm), loc, nm)
}

# Same likelihood the calibration used for this city (slow to build, so kept in lik.list)
.get.calibration.likelihood <- function(loc, calibration.code) {
    key <- .lik.key(loc, calibration.code)
    if (is.null(lik.list[[key]])) {
        info  <- get.calibration.info(calibration.code)
        instr <- info$special.case.likelihood.instructions[[loc]]
        if (is.null(instr)) instr <- info$likelihood.instructions
        lik.list[[key]] <<- instr$instantiate.likelihood(version = "shield", location = loc,
                                                         data.manager = info$data.manager)
    }
    lik.list[[key]]
}


# ---- 1. Likelihood breakdown for one city ----
# Uses the saved simset (not the raw MCMC), because the likelihood needs the simulations.
#  1. change.during.run - how much each likelihood improved from the start of each chain to its end
#                         (averaged over chains). Shows which targets the chains worked on.
#                         Needs a simset with no burn (n.sim.saved = N.SIM.NO.BURN), so it has the start of each chain.
#  2. posterior.sd      - how much each likelihood varies across the kept sims (last burn.keep of each chain).
#                         Large = it drives which sims are kept. Near zero = it barely constrains anything.
#  3. conflicts         - pairs of likelihoods that pull against each other (correlation < -0.5)
# Raw values are not comparable between likelihoods (constants, number of data points), so look at changes.
likelihood.breakdown <- function(loc, calibration.code, n.sims = 50, burn.keep = 0.5, n.sim.saved = NULL) {
    info <- get.calibration.info(calibration.code)
    if (is.null(n.sim.saved))   # e.g. 4 chains x 30000 / 80 = 1500
        n.sim.saved <- info$n.chains * floor((info$n.iter - info$n.burn) / info$thin)

    simset <- retrieve.simulation.set(version = "shield", location = loc,
                                      calibration.code = calibration.code, n.sim = n.sim.saved)
    lik    <- .get.calibration.likelihood(loc, calibration.code)
    score  <- function(i) lik$compute.piecewise(simset$subset(i))

    # 1. start vs end of each chain, averaged over chains
    chains <- simset$simulation.chain
    change <- rowMeans(sapply(unique(chains), function(ch) {
        i <- which(chains == ch)
        score(max(i)) - score(min(i))
    }))

    # 2. spread across the kept sims (second half of each chain by default)
    kept   <- simset$burn(keep = burn.keep)
    idx    <- round(seq(1, kept$n.sim, length.out = min(n.sims, kept$n.sim)))
    pieces <- t(sapply(idx, function(i) lik$compute.piecewise(kept$subset(i))))

    by.component <- data.frame(component         = colnames(pieces),
                               change.during.run = round(change[colnames(pieces)], 1),
                               posterior.sd      = round(apply(pieces, 2, sd), 2))
    by.component <- by.component[order(-by.component$posterior.sd), ]
    rownames(by.component) <- NULL

    # 3. pairs that pull against each other
    cm    <- suppressWarnings(cor(pieces))
    pairs <- which(upper.tri(cm) & cm < -0.5, arr.ind = TRUE)
    conflicts <- data.frame(component.1 = rownames(cm)[pairs[, 1]],
                            component.2 = colnames(cm)[pairs[, 2]],
                            correlation = round(cm[pairs], 2))
    conflicts <- conflicts[order(conflicts$correlation), ]

    list(by.component = by.component, conflicts = conflicts, pieces = pieces)
}


# ---- 2. Many cities ----
# One city at a time (each simset is large). Returns posterior.sd: likelihood x city
# A city that fails (e.g. no simset of that size) is reported and skipped.
run.likelihood.breakdown <- function(locations, calibration.code, force.recompute = FALSE, ...) {
    locations <- unname(locations)
    for (loc in locations) {
        key <- .lik.key(loc, calibration.code)
        if (!force.recompute && !is.null(lik.breakdown.list[[key]])) next
        message("Likelihood breakdown: ", .lik.city.name(loc))
        res <- tryCatch(likelihood.breakdown(loc, calibration.code, ...),
                        error = function(e) { message(loc, ": ", e$message); NULL })
        if (!is.null(res)) lik.breakdown.list[[key]] <<- res
        invisible(gc())
    }
    sds <- lapply(locations, function(loc) {
        r <- lik.breakdown.list[[.lik.key(loc, calibration.code)]]
        if (!is.null(r)) setNames(r$by.component$posterior.sd, r$by.component$component)
    })
    names(sds) <- .lik.city.name(locations)
    sds <- sds[!sapply(sds, is.null)]
    if (length(sds) == 0) return(NULL)
    comps <- unique(unlist(lapply(sds, names)))
    out <- sapply(sds, function(s) s[comps])
    out <- matrix(out, nrow = length(comps), dimnames = list(comps, names(sds)))
    out
}


# ---- 3. Save ----
# CSVs in calibration_diagnostics/<code>/likelihood/ (time-stamped, like the mixing files):
#  1. likelihood_posterior_sd_<code>_<stamp>.csv  - likelihood x city
#  2. likelihood_by_component_<code>_<stamp>.csv  - every city: change.during.run and posterior.sd
#  3. likelihood_conflicts_<code>_<stamp>.csv     - every city: pairs with correlation < -0.5
save.likelihood.breakdown <- function(locations, calibration.code, lik.by.city,
                                      folder = .likelihood.dir(calibration.code)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d_%H%M")
    file  <- function(what) file.path(folder, paste0("likelihood_", what, "_", calibration.code, "_", stamp, ".csv"))
    per.city <- function(part) do.call(rbind, lapply(unname(locations), function(loc) {
        r <- lik.breakdown.list[[.lik.key(loc, calibration.code)]]
        if (!is.null(r) && NROW(r[[part]]) > 0) cbind(city = .lik.city.name(loc), location = loc, r[[part]])
    }))
    if (!is.null(lik.by.city))
        write.csv(cbind(likelihood = rownames(lik.by.city), as.data.frame(lik.by.city)), file("posterior_sd"), row.names = FALSE)
    by.component <- per.city("by.component")
    if (!is.null(by.component)) write.csv(by.component, file("by_component"), row.names = FALSE)
    conflicts <- per.city("conflicts")
    if (!is.null(conflicts)) write.csv(conflicts, file("conflicts"), row.names = FALSE)
    message("Saved in ", folder)
    invisible(folder)
}
