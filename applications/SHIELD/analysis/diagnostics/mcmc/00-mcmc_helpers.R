# ****************************************************************************************************
# SHIELD CALIBRATION — MCMC MIXING DIAGNOSTICS: HELPER FUNCTIONS
# ****************************************************************************************************
# Functions only. Sourcing this file does not load the specification, data managers or calibrations,
# so you can source it from any script and use the functions on their own.
# To run the full diagnostics on a calibration, use mcmc/mixing_check.R (diagnostics_setup.R sources this file).
# Uses only the raw MCMC (parameter draws, log-likelihood, log-prior, acceptance), never the simsets.
# The likelihood breakdown, which needs the simsets, is in likelihood/likelihood_helpers.R.
#
# What the functions need when you call them:
#  1. jheem2 loaded and the jheem root directory set - for loading MCMCs and the default folders
#  2. The calibration registered (shield_calib_register.R) - for get.calibration.info():
#     the run settings in the report
#  3. Optional: SHIELD.TEN.MSAS (commoncode/locations_of_interest.R) - to show city names instead of codes
#
# Contents:
#  PART 1: loading           - load.mcmc()
#  PART 2: one MCMC          - mcmc.diagnostics(), plot.mcmc.diagnostics()
#  PART 3: extensions        - 3a verdict, 3b many cities, 3c params failing across cities,
#                              3d drop one chain, 3e burn-in, 3f save plots, 3g compare calibrations,
#                              3h HTML report
# ****************************************************************************************************

library(bayesian.simulations)

# Default folders, on the server (shared with the team):
#  1. outputs, in the same calibration folder as the fit and prior diagnostics:
#     <jheem root>/shield/calibration_diagnostics/<calibration code>/mixing/
#       reports and CSVs - from write.mixing.report() and mixing_correlated_parameter_pairs.R
#       plots/           - PDFs from save.mixing.plots()
#  2. mcmc_cache: <jheem root>/shield/mcmc_diagnostics/mcmc_cache/
#     slimmed MCMCs, for fast re-loading (load.mcmc); not an output, so it stays here and old caches are reused
# Worked out when a function is called, so sourcing this file does not need the root directory yet.
.mixing.dir <- function(calibration.code, sub = "") file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics",
                                                              calibration.code, "mixing", sub)
.cache.dir  <- function() file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "mcmc_cache")

# Thresholds used to flag a city (used by mixing.verdict)
# R-hat is the rank-normalized split R-hat; ESS is the bulk ESS across all chains (see PART 2)
MIXING.THRESHOLDS <- list(
    rhat.fail        = 1.1,    # any parameter, or the log-posterior, above this -> FAIL
    rhat.check       = 1.05,   # any parameter, or the log-posterior, above this -> CHECK
    ess.min          = 400,    # any parameter below this (about 100 per chain)   -> CHECK
    accept.low       = 0.10,   # any chain below this                             -> FAIL (stuck)
    accept.high      = 0.50,   # any chain above this                             -> CHECK (steps too small)
    block.accept.low = 0.05    # any block below this in any chain                -> CHECK (block stuck in that chain)
)
# Shown in the tables but not in the verdict:
#  1. Geweke z: one test per parameter per chain (692 for 173 parameters x 4 chains), so about 5% pass |z| > 2
#     by chance and every city would get CHECK. The split R-hat already catches drift within a chain.
#  2. Log-likelihood gap between chains: the cut-off depends on the scale of the likelihood.
#     R-hat of the log-posterior answers the same question without a scale.

# Version of the statistics; diag.list results made with another version are recomputed
.MIXING.METHOD <- "rank-split-rhat, bulk-ess (2026-11)"

# Share of each chain the diagnostics use (the last part). Default only; it is set in diagnostics_setup.R
#  1. 0.5 = second half of each chain, the same stretch as the burned simset (N.SIM) - recommended
#  2. 1   = the whole chain, including its start (not recommended: see mixing_check.R)
#  The cache and mcmc.list keep the whole chain; the burn is applied when the statistics are computed.
#  burn.in.check() always looks at the whole chain.
if (!exists("MIXING.BURN.KEEP")) MIXING.BURN.KEEP <- 0.5

# Results are kept in these lists, keyed "location | calibration.code",
# so nothing is overwritten when you move to the next city
if (!exists("mcmc.list")) mcmc.list <- list()
if (!exists("diag.list")) diag.list <- list()

.mcmc.key <- function(loc, code) paste0(loc, " | ", code)

# city name for a location code (e.g. "C.12580" -> "Baltimore"); the code itself if SHIELD.TEN.MSAS is not loaded
.city.name <- function(loc) {
    if (!exists("SHIELD.TEN.MSAS")) return(loc)
    nm <- names(SHIELD.TEN.MSAS)[match(loc, SHIELD.TEN.MSAS)]
    ifelse(is.na(nm), loc, nm)
}


# ****************************************************************************************************
# PART 1: LOADING
# ****************************************************************************************************

# TRUE when every chain of a calibration has finished
.calibration.complete <- function(loc, calibration.code) {
    pct <- tryCatch(get.calibration.progress("shield", loc, calibration.code),
                    error = function(e) NA)
    all(!is.na(pct) & pct == 100)
}

# An MCMC without its simulations, which the diagnostics do not need
# 1. Finished calibration: drops the simulations as each chunk is read, so one chunk's worth is in
#    memory at a time. Every chunk is still read from disk.
# 2. Unfinished calibration: assembles what is there, with all its simulations, then drops them.
# In case 1 @simulation.indices no longer lines up across chunks; nothing in the diagnostics uses it.
.assemble.mcmc.without.sims <- function(loc, calibration.code) {
    if (!.calibration.complete(loc, calibration.code)) {
        m <- assemble.mcmc.from.calibration(version = "shield", location = loc,
                                            calibration.code = calibration.code, allow.incomplete = TRUE)
        m@simulations <- list()
        return(m)
    }

    # the jheem2 in use: when SHIELD sources jheem2 from the repo, jheem2::: would reach an older installed
    # package with the old folder order (<location>/<calibration code>); runs are now <calibration code>/<location>
    calibration.dir <- if (exists("get.calibration.dir", mode = "function")) get.calibration.dir
                       else jheem2:::get.calibration.dir
    dir <- calibration.dir(version = "shield", location = loc, calibration.code = calibration.code,
                           root.dir = get.jheem.root.directory())
    global.control <- bayesian.simulations:::get.cache.global.control(dir)
    chunk.dir      <- bayesian.simulations:::get.cache.subdirectory(dir)

    one.chain <- function(chain) {
        chain.control <- bayesian.simulations:::get.cache.chain.control(dir, global.control, chain)
        chunks <- lapply(chain.control@chunk.filenames[global.control@save.chunk], function(filename) {
            load(file.path(chunk.dir, chain.control@chain.dir, filename))
            mcmc@simulations <- list()
            mcmc
        })
        do.call(mcmc.merge.serial, chunks)
    }

    chains <- lapply(seq_len(global.control@n.chains), one.chain)
    m <- if (length(chains) == 1) chains[[1]] else mcmc.merge.parallel(chains)
    m@simulations <- list()
    m
}

# Cached MCMC loader
# 1. Looks in mcmc.list (memory) first
# 2. Then looks for a saved .rds file in cache.dir
# 3. Otherwise assembles from the calibration without the simulations, and saves it
#    (only saved to disk if all chains are complete, so a partial run is not reused later)
load.mcmc <- function(locations, calibration.code,
                      cache.dir = .cache.dir(),
                      n.cores = 4,
                      force.reload = FALSE) {
    dir.create(cache.dir, showWarnings = FALSE, recursive = TRUE)
    locations <- unname(locations)
    keys <- .mcmc.key(locations, calibration.code)

    # which ones are not in memory yet
    to.load <- locations[force.reload | !(keys %in% names(mcmc.list))]
    if (length(to.load) == 0) return(invisible(mcmc.list[keys]))

    load.one <- function(loc) {
        file <- file.path(cache.dir, paste0(calibration.code, "_", loc, ".rds"))
        if (!force.reload && file.exists(file)) return(readRDS(file))

        m <- tryCatch(.assemble.mcmc.without.sims(loc, calibration.code),
                      error = function(e) { message(loc, ": ", e$message); NULL })
        if (is.null(m)) return(NULL)

        if (.calibration.complete(loc, calibration.code)) saveRDS(m, file)
        else message(loc, ": calibration not complete - kept in memory only, not saved to disk")
        m
    }

    # one city: run normally (errors are easier to see); several: in parallel
    n.cores <- min(n.cores, length(to.load))
    loaded <- if (n.cores == 1) lapply(to.load, load.one)
              else parallel::mclapply(to.load, load.one, mc.cores = n.cores)

    # a crashed parallel worker returns an error object, not NULL - drop those too
    ok <- sapply(loaded, function(x) is(x, "mcmcsim"))
    if (any(!ok)) message("Failed to load: ", paste(.city.name(to.load[!ok]), collapse = ", "),
                          "\n(if this was out-of-memory, try n.cores = 1)")

    names(loaded) <- .mcmc.key(to.load, calibration.code)
    mcmc.list[names(loaded)[ok]] <<- loaded[ok]

    invisible(mcmc.list[intersect(keys, names(mcmc.list))])
}


# ****************************************************************************************************
# PART 2: DIAGNOSTICS FOR ONE MCMC
# ****************************************************************************************************

# ---- helpers ----

# keep the last `keep` share of each chain's saved draws
# subset.mcmc(additional.burn = ) cannot be used: in bayesian.simulations it fails whenever
# additional.thin = 1 (its first.in.thin.group mask is all FALSE). With no extra thinning a burn
# only drops iterations, so this slices every slot that has one column per saved draw (chain x iteration [x ...]).
.burn.mcmc <- function(m, keep = MIXING.BURN.KEEP) {
    n.burn <- floor((1 - keep) * m@n.iter)
    if (n.burn <= 0) return(m)
    k <- (n.burn + 1):m@n.iter
    m@samples            <- m@samples[, k, , drop = FALSE]
    m@log.likelihoods    <- m@log.likelihoods[, k, drop = FALSE]
    m@log.priors         <- m@log.priors[, k, drop = FALSE]
    m@n.accepted         <- m@n.accepted[, k, , drop = FALSE]
    m@run.times          <- m@run.times[, k, drop = FALSE]
    m@first.step.for.iter <- m@first.step.for.iter[, k, drop = FALSE]
    m@simulation.indices <- m@simulation.indices[, k, drop = FALSE]
    m@burn               <- as.integer(m@burn + n.burn * m@thin)
    m@n.iter             <- length(k)
    m
}

# samples as a 3D array: iteration x chain x variable
.get.samples <- function(m) {
    s <- m@samples
    if (!is.null(names(dimnames(s))))
        s <- aperm(s, match(c("iteration", "chain", "variable"), names(dimnames(s))))
    s
}

# log-likelihood (or log-prior) as iteration x chain
.get.by.chain <- function(x, n.chains) {
    x <- as.matrix(x)
    if (nrow(x) == n.chains && ncol(x) != n.chains) x <- t(x)
    x
}

# ESS for one chain (Geyer initial positive sequence); used for ess.min.chain and Geweke
.ess.one.chain <- function(x) {
    n <- length(x)
    if (n < 4 || var(x) == 0) return(0)
    rho <- acf(x, lag.max = n - 1, plot = FALSE)$acf[-1]
    # sum autocorrelations in pairs, stop when a pair goes negative
    n.pairs <- floor(length(rho) / 2)
    pair.sums <- rho[2 * seq_len(n.pairs) - 1] + rho[2 * seq_len(n.pairs)]
    stop.at <- which(pair.sums < 0)[1]
    if (!is.na(stop.at)) pair.sums <- pair.sums[seq_len(stop.at - 1)]
    tau <- 1 + 2 * sum(pair.sums)
    min(n, n / max(tau, 1e-8))
}

# ---- R-hat and ESS (Vehtari et al. 2021, as in Stan and the posterior package) ----
# The posterior package is not installed, so these are written out here.
# x is always one parameter as iteration x chain.

# split each chain in half: iteration x chain -> (iteration / 2) x (2 x chain)
# A trend inside a chain then shows up as two halves that disagree. The middle draw is dropped if odd.
.split.chains <- function(x) {
    half <- floor(nrow(x) / 2)
    cbind(x[seq_len(half), , drop = FALSE], x[nrow(x) - half + seq_len(half), , drop = FALSE])
}

# replace each draw by the normal quantile of its rank among all draws (all chains together)
# so skewed or heavy-tailed parameters behave like normal ones
.rank.normalize <- function(x) {
    z <- qnorm((rank(x) - 3/8) / (length(x) + 1/4))
    dim(z) <- dim(x)
    z
}

# Gelman-Rubin R-hat: spread of all draws vs spread within chains
.rhat.basic <- function(x) {
    n <- nrow(x)
    W <- mean(apply(x, 2, var))
    if (!is.finite(W) || W == 0) return(NA_real_)
    B <- n * var(colMeans(x))
    sqrt(((n - 1) / n * W + B / n) / W)
}

# Rank-normalized split R-hat, the one used in the verdict
#  1. bulk: R-hat of the rank-normalized split chains (do the centres agree?)
#  2. tail: the same on the distance from the median (do the spreads agree?)
#  returns the larger of the two.
# Not get.rhats() from bayesian.simulations: it does not split the chains, so a drift shared by all
# chains is missed (and get.rhats(rank.normalize = TRUE) errors).
.rhat.rank.split <- function(x) {
    r <- c(.rhat.basic(.split.chains(.rank.normalize(x))),
           .rhat.basic(.split.chains(.rank.normalize(abs(x - median(x))))))
    if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE)
}

# R-hat for every parameter, worst first (s = iteration x chain x variable)
.rhats <- function(s, chains = seq_len(dim(s)[2])) {
    r <- sapply(dimnames(s)[[3]], function(v) .rhat.rank.split(matrix(s[, chains, v], ncol = length(chains))))
    sort(r, decreasing = TRUE, na.last = TRUE)
}

# autocovariance of one series at lags 0 to n-1 (divided by n), by FFT
.autocovariance <- function(y) {
    n  <- length(y)
    m2 <- 2 * nextn(n)
    f  <- fft(c(y - mean(y), rep(0, m2 - n)))
    Re(fft(Conj(f) * f, inverse = TRUE))[seq_len(n)] / (m2 * n)
}

# ESS across all chains
# 1. autocorrelation is worked out with all chains together, against the variance of all draws
#    (within and between chains), so chains that sit in different places give a low ESS.
#    Adding up the ESS of each chain would not: each chain can look well mixed on its own.
# 2. lags are summed in pairs while the pair sum is positive, and the pair sums are kept from going up
.ess.multi.chain <- function(x) {
    n <- nrow(x); k <- ncol(x)
    if (n < 4) return(NA_real_)
    acov <- apply(x, 2, .autocovariance)                      # lag x chain
    W    <- mean(acov[1, ]) * n / (n - 1)                     # mean variance within chains
    if (!is.finite(W)) return(NA_real_)
    if (W == 0) return(0)                                     # nothing moves within the chains
    var.plus <- W * (n - 1) / n + if (k > 1) var(colMeans(x)) else 0
    rho  <- 1 - (W - rowMeans(acov)) / var.plus               # autocorrelation by lag, all chains
    rho[1] <- 1
    n.pairs <- floor(n / 2)
    pair <- rho[2 * seq_len(n.pairs) - 1] + rho[2 * seq_len(n.pairs)]
    stop.at <- which(pair <= 0)[1]
    if (!is.na(stop.at)) pair <- pair[seq_len(stop.at - 1)]
    tau <- max(-1 + 2 * sum(cummin(pair)), 1 / log10(n * k))
    n * k / tau
}

# bulk ESS: ESS of the rank-normalized split chains, the one used in the verdict
.ess.bulk <- function(x) .ess.multi.chain(.split.chains(.rank.normalize(x)))

# Geweke z: mean of first 10% vs last 50%
.geweke.z <- function(x, first = 0.1, last = 0.5) {
    n <- length(x)
    a <- x[seq_len(floor(first * n))]
    b <- x[(n - floor(last * n) + 1):n]
    if (length(a) < 2 || var(a) + var(b) == 0) return(NA)
    # divide by ESS-based variance so autocorrelation is accounted for
    se.a <- var(a) / max(.ess.one.chain(a), 1)
    se.b <- var(b) / max(.ess.one.chain(b), 1)
    (mean(a) - mean(b)) / sqrt(se.a + se.b)
}


# ---- main function ----
# m is the whole chain; only the last burn.keep share of each chain is used
# No "stuck fraction" (share of saved draws with no change): the sampler proposes one block per iteration
# and there are about 70 blocks, so a block is proposed less than once per saved draw (thin = 50). No change
# between saved draws is then the normal case, not a sign of a stuck chain (stage 3: all 173 parameters
# "stuck" in every chain). Acceptance by block and chain (1) is the check for that.
mcmc.diagnostics <- function(m, burn.keep = MIXING.BURN.KEEP, th = MIXING.THRESHOLDS) {
    m        <- .burn.mcmc(m, burn.keep)
    s        <- .get.samples(m)          # iteration x chain x variable
    n.chains <- dim(s)[2]
    vars     <- dimnames(s)[[3]]

    # 1. Acceptance: total, by chain, by block, and block x chain
    accept.total    <- get.total.acceptance.rate(m)
    accept.by.chain <- get.total.acceptance.rate(m, aggregate.chains = FALSE)
    accept.by.block <- get.total.acceptance.rate(m, by.block = TRUE)
    accept.block.by.chain <- get.total.acceptance.rate(m, by.block = TRUE, aggregate.chains = FALSE)   # chain x block

    # blocks almost never accepted in at least one chain: their parameters barely move in that chain
    abc <- matrix(accept.block.by.chain, nrow = n.chains,
                  dimnames = list(chain = seq_len(n.chains), block = names(accept.by.block)))
    low <- which(abc < th$block.accept.low, arr.ind = TRUE)
    blocks.low <- data.frame(block      = colnames(abc)[low[, "block"]],
                             chain      = as.integer(low[, "chain"]),
                             acceptance = round(abc[low], 3))
    blocks.low <- blocks.low[order(blocks.low$acceptance), ]
    rownames(blocks.low) <- NULL

    # 2. R-hat: rank-normalized split (verdict), and the classic one from get.rhats() to compare with older reports
    rhat <- .rhats(s)
    rhat.classic <- if (n.chains > 1) get.rhats(m, sort = FALSE) else setNames(rep(NA_real_, length(vars)), vars)

    # 3-6. Per-parameter statistics
    per.param <- do.call(rbind, lapply(vars, function(v) {
        x <- s[, , v, drop = FALSE]; dim(x) <- dim(x)[1:2]   # iteration x chain
        ess.per.chain <- apply(x, 2, .ess.one.chain)
        lag1 <- apply(x, 2, function(y) if (var(y) == 0) 1 else acf(y, lag.max = 1, plot = FALSE)$acf[2])
        gz   <- apply(x, 2, .geweke.z)
        data.frame(
            parameter      = v,
            rhat           = round(unname(rhat[v]), 3),
            rhat.classic   = round(unname(rhat.classic[v]), 3),
            ess.bulk       = round(.ess.bulk(x)),
            ess.min.chain  = round(min(ess.per.chain)),
            lag1.acf.max   = round(max(lag1), 3),
            geweke.max.abs = suppressWarnings(round(max(abs(gz), na.rm = TRUE), 2)),
            chain.means.sd.ratio = round(sd(colMeans(x)) / sqrt(mean(apply(x, 2, var))), 2),
            stringsAsFactors = FALSE
        )
    }))
    per.param <- per.param[order(-per.param$rhat, per.param$ess.bulk), ]
    rownames(per.param) <- NULL

    # 7. Log-likelihood / log-posterior: by chain, and R-hat / ESS of the whole model in one number each
    ll <- .get.by.chain(m@log.likelihoods, n.chains)
    lp <- .get.by.chain(m@log.priors,      n.chains)
    by.chain <- data.frame(
        chain            = seq_len(n.chains),
        accept           = round(as.numeric(accept.by.chain), 3),
        min.block.accept = round(apply(abc, 1, min), 3),
        mean.loglik      = round(colMeans(ll), 1),
        last.loglik      = round(ll[nrow(ll), ], 1),
        mean.logpost     = round(colMeans(ll + lp), 1)
    )
    by.chain$loglik.gap.from.best <- round(max(by.chain$mean.loglik) - by.chain$mean.loglik, 1)
    log.density <- data.frame(
        quantity = c("log-likelihood", "log-posterior"),
        rhat     = round(c(.rhat.rank.split(ll), .rhat.rank.split(ll + lp)), 3),
        ess.bulk = round(c(.ess.bulk(ll), .ess.bulk(ll + lp))))

    # overall summary (one row)
    summary <- data.frame(row.names = NULL,
        n.chains            = n.chains,
        n.samples.per.chain = dim(s)[1],
        accept.total        = round(as.numeric(accept.total), 3),
        accept.min.chain    = round(min(accept.by.chain), 3),
        accept.max.chain    = round(max(accept.by.chain), 3),
        min.block.accept    = round(min(abc), 3),                         # any block, any chain
        n.blocks.low        = length(unique(blocks.low$block)),           # blocks < th$block.accept.low in a chain
        max.rhat            = round(max(rhat, na.rm = TRUE), 3),
        n.rhat.over.1.05    = sum(rhat > 1.05, na.rm = TRUE),
        n.rhat.over.1.1     = sum(rhat > 1.1,  na.rm = TRUE),
        rhat.logpost        = log.density$rhat[2],
        max.rhat.classic    = round(max(rhat.classic), 3),
        min.ess             = min(per.param$ess.bulk, na.rm = TRUE),
        n.ess.low           = sum(per.param$ess.bulk < th$ess.min, na.rm = TRUE),
        ess.logpost         = log.density$ess.bulk[2],
        n.geweke.over.2     = sum(per.param$geweke.max.abs > 2, na.rm = TRUE),   # not in the verdict
        max.loglik.gap      = max(by.chain$loglik.gap.from.best),                # not in the verdict
        run.time.hours      = round(sum(m@total.run.time) / 3600, 1),
        burn.keep           = burn.keep                      # share of each chain used
    )

    list(summary               = summary,
         by.chain              = by.chain,
         by.parameter          = per.param,
         log.density           = log.density,
         accept.by.block       = sort(accept.by.block),
         accept.block.by.chain = accept.block.by.chain,
         blocks.low            = blocks.low,
         loglik                = ll,
         method                = .MIXING.METHOD)
}


# ---- plots for one MCMC ----
# m is the whole chain; the plots use the same share of each chain as d
plot.mcmc.diagnostics <- function(m, d = mcmc.diagnostics(m), n.worst = 9, title = "") {
    force(d)                                   # computed from the whole chain, before m is burned below
    m  <- .burn.mcmc(m, d$summary$burn.keep)
    ll <- d$loglik
    matplot(ll, type = "l", lty = 1, col = seq_len(ncol(ll)),
            xlab = "saved iteration", ylab = "log-likelihood",
            main = paste(title, "log-likelihood by chain"))
    legend("bottomright", legend = paste("chain", seq_len(ncol(ll))),
           col = seq_len(ncol(ll)), lty = 1, bty = "n")

    print(acceptance.plot(m))                                            # acceptance over time, by chain
    print(acceptance.plot(m, by.block = TRUE, aggregate.chains = TRUE))  # acceptance over time, by block
    print(trace.plot(m, var.names = head(d$by.parameter$parameter, n.worst),
                     exact.var.names = TRUE))                            # worst-mixing parameters
}


# ****************************************************************************************************
# PART 3: EXTENSIONS
# ****************************************************************************************************

# ---- 3a. Verdict: PASS / CHECK / FAIL for one city, with the reasons ----
# Geweke and the log-likelihood gap are not used (see MIXING.THRESHOLDS)
mixing.verdict <- function(summary, th = MIXING.THRESHOLDS) {
    above <- function(x, cut) !is.na(x) && x > cut
    fail <- c(
        if (above(summary$max.rhat, th$rhat.fail))     paste0("R-hat ", summary$max.rhat, " > ", th$rhat.fail,
                                                              " (", summary$n.rhat.over.1.1, " params)"),
        if (above(summary$rhat.logpost, th$rhat.fail)) paste0("log-posterior R-hat ", summary$rhat.logpost, " > ", th$rhat.fail),
        if (summary$accept.min.chain < th$accept.low)  paste0("a chain has acceptance ", summary$accept.min.chain)
    )
    check <- c(
        if (above(summary$max.rhat, th$rhat.check) && !above(summary$max.rhat, th$rhat.fail))
            paste0("R-hat ", summary$max.rhat, " > ", th$rhat.check),
        if (above(summary$rhat.logpost, th$rhat.check) && !above(summary$rhat.logpost, th$rhat.fail))
            paste0("log-posterior R-hat ", summary$rhat.logpost, " > ", th$rhat.check),
        if (summary$min.ess < th$ess.min)              paste0(summary$n.ess.low, " params with ESS < ", th$ess.min),
        if (summary$accept.max.chain > th$accept.high) paste0("a chain has acceptance ", summary$accept.max.chain),
        if (summary$min.block.accept < th$block.accept.low)
            paste0(summary$n.blocks.low, " blocks with acceptance < ", th$block.accept.low, " in a chain")
    )
    data.frame(verdict = if (length(fail)) "FAIL" else if (length(check)) "CHECK" else "PASS",
               reasons = paste(c(fail, check), collapse = "; "))
}


# ---- 3b. Run diagnostics for many cities -> one overview table ----
# Loads (or reuses) each MCMC, stores results in diag.list, returns one row per city
run.mixing.diagnostics <- function(locations, calibration.code, n.cores = 4, force.recompute = FALSE) {
    locations <- unname(locations)
    load.mcmc(locations, calibration.code, n.cores = n.cores)

    rows <- lapply(locations, function(loc) {
        key <- .mcmc.key(loc, calibration.code)
        m   <- mcmc.list[[key]]
        if (is.null(m)) return(data.frame(city = .city.name(loc), location = loc, verdict = "NOT LOADED"))

        # recompute when MIXING.BURN.KEEP or the statistics have changed since the last run
        if (force.recompute || is.null(diag.list[[key]]) ||
            !identical(diag.list[[key]]$summary$burn.keep, MIXING.BURN.KEEP) ||
            !identical(diag.list[[key]]$method, .MIXING.METHOD))
            diag.list[[key]] <<- mcmc.diagnostics(m)
        d <- diag.list[[key]]

        cbind(city = .city.name(loc), location = loc,
              mixing.verdict(d$summary), d$summary)
    })
    overview <- do.call(rbind, lapply(rows, function(r) {
        # make sure all rows have the same columns (for NOT LOADED rows)
        all.cols <- unique(unlist(lapply(rows, names)))
        r[setdiff(all.cols, names(r))] <- NA
        r[all.cols]
    }))
    overview$calibration.code <- calibration.code
    rownames(overview) <- NULL
    overview
}


# ---- 3c. Which parameters mix badly in more than one city? ----
# Useful to find parameters that are a problem everywhere (prior / model issue),
# rather than a single city that needs more iterations
params.failing.across.cities <- function(locations, calibration.code,
                                         rhat.cutoff = MIXING.THRESHOLDS$rhat.fail, ess.cutoff = MIXING.THRESHOLDS$ess.min) {
    per.city <- do.call(rbind, lapply(unname(locations), function(loc) {
        d <- diag.list[[.mcmc.key(loc, calibration.code)]]
        if (is.null(d)) return(NULL)
        bad <- d$by.parameter[which(d$by.parameter$rhat > rhat.cutoff | d$by.parameter$ess.bulk < ess.cutoff), ]
        if (nrow(bad) == 0) return(NULL)
        data.frame(parameter = bad$parameter, city = .city.name(loc), rhat = round(bad$rhat, 2), ess = bad$ess.bulk)
    }))
    if (is.null(per.city)) return(data.frame())

    counts <- aggregate(city ~ parameter, per.city, function(x) length(x))
    names(counts)[2] <- "n.cities"
    counts$cities   <- tapply(per.city$city, per.city$parameter, paste, collapse = ", ")[counts$parameter]
    counts$max.rhat <- tapply(per.city$rhat, per.city$parameter, max)[counts$parameter]
    counts[order(-counts$n.cities, -counts$max.rhat), ]
}


# ---- 3d. Is one chain the problem? ----
# Recomputes R-hat leaving out each chain in turn, on the last burn.keep share of each chain.
# If dropping one chain brings R-hat down a lot, that chain is stuck somewhere else.
# R-hat is the rank-normalized split R-hat, as in the verdict.
drop.one.chain.check <- function(m, burn.keep = MIXING.BURN.KEEP) {
    if (m@n.chains < 3) stop("Need at least 3 chains to drop one and still compute R-hat")
    s <- .get.samples(.burn.mcmc(m, burn.keep))
    base <- max(.rhats(s), na.rm = TRUE)
    rv <- do.call(rbind, lapply(seq_len(m@n.chains), function(ch) {
        keep <- setdiff(seq_len(m@n.chains), ch)
        r <- .rhats(s, chains = keep)
        data.frame(dropped.chain   = ch,
                   max.rhat        = round(max(r, na.rm = TRUE), 3),
                   n.rhat.over.1.1 = sum(r > 1.1, na.rm = TRUE),
                   worst.parameter = names(r)[1])
    }))
    rv$improvement <- round(base - rv$max.rhat, 3)
    rv[order(-rv$improvement), ]
}


# ---- 3e. Would more burn-in help? ----
# Recomputes R-hat after throwing away the first 0%, 25%, 50%, 75% of each chain.
# Always on the whole chain (m as loaded, not burned by MIXING.BURN.KEEP).
# 1. R-hat drops a lot from 0% to 50%: the chains were still converging early on
#    (so a longer run, or a later start, would help)
# 2. R-hat still drops from 50% to 75%: the half that is kept is itself still settling
# R-hat is the rank-normalized split R-hat, as in the verdict.
# Burns with .burn.mcmc(), not get.rhats(additional.burn = ): get.rhats() drops the burned draws but
# still uses the whole chain's length as N, which inflates R-hat.
burn.in.check <- function(m, fractions = c(0, 0.25, 0.5, 0.75)) {
    do.call(rbind, lapply(fractions, function(f) {
        b  <- .burn.mcmc(m, keep = 1 - f)
        r  <- .rhats(.get.samples(b))
        ll <- .get.by.chain(b@log.likelihoods, b@n.chains)
        lp <- .get.by.chain(b@log.priors,      b@n.chains)
        data.frame(burn.fraction    = f,
                   samples.left     = b@n.iter,
                   max.rhat         = round(max(r, na.rm = TRUE), 3),
                   n.rhat.over.1.1  = sum(r > 1.1,  na.rm = TRUE),
                   n.rhat.over.1.05 = sum(r > 1.05, na.rm = TRUE),
                   rhat.logpost     = round(.rhat.rank.split(ll + lp), 3))
    }))
}


# ---- 3f. Save diagnostic plots to one PDF per city ----
save.mixing.plots <- function(locations, calibration.code, folder = .mixing.dir(calibration.code, "plots"), n.worst = 9) {
    dir.create(folder, showWarnings = FALSE, recursive = TRUE)
    for (loc in unname(locations)) {
        key <- .mcmc.key(loc, calibration.code)
        m <- mcmc.list[[key]]; d <- diag.list[[key]]
        if (is.null(m) || is.null(d)) { message("Skipping ", .city.name(loc), " (not loaded)"); next }

        file <- file.path(folder, paste0(calibration.code, "_", .city.name(loc), ".pdf"))
        pdf(file, width = 11, height = 8.5)
        tryCatch(plot.mcmc.diagnostics(m, d, n.worst = n.worst, title = .city.name(loc)),
                 error = function(e) message(.city.name(loc), ": ", e$message),
                 finally = dev.off())
        message("Saved ", file)
    }
}


# ---- 3g. Compare calibration codes side by side (e.g. stage3 vs stage3.pk) ----
compare.calibration.mixing <- function(locations, calibration.codes, n.cores = 4) {
    all <- do.call(rbind, lapply(calibration.codes, function(cc)
        run.mixing.diagnostics(locations, cc, n.cores = n.cores)))
    all[order(all$city, all$calibration.code),
        c("city", "calibration.code", "verdict", "accept.total", "max.rhat", "n.rhat.over.1.1", "rhat.logpost",
          "min.ess", "max.loglik.gap")]
}


# ---- 3h. Write a shareable report (one self-contained HTML file) ----
# Everything from the diagnostics in one file you can email / post:
#  1. Run settings + a "how to read this" table
#  2. Overview: one row per city with PASS / CHECK / FAIL and the reasons
#  3. Parameters that mix badly in several cities
#  4. Per city: chains, worst parameters, acceptance by block, drop-one-chain, burn-in, plots
# Also writes the overview and all per-parameter stats as CSV files next to it.
# (The likelihood breakdown has its own script and outputs: likelihood/likelihood_breakdown.R)
write.mixing.report <- function(locations, calibration.code,
                                folder = .mixing.dir(calibration.code),
                                n.worst.params = 15,
                                n.trace.params = 9,
                                include.plots = TRUE) {
    dir.create(folder, showWarnings = FALSE, recursive = TRUE)
    locations <- unname(locations)

    overview <- run.mixing.diagnostics(locations, calibration.code)

    # ---- small HTML helpers ----
    esc <- function(x) htmltools::htmlEscape(as.character(x))
    tbl <- function(df, verdict.col = NULL) {
        if (is.null(df) || NROW(df) == 0) return("<p><em>None.</em></p>")
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        head <- paste0("<tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr>")
        rows <- vapply(seq_len(nrow(df)), function(i) {
            cls <- if (!is.null(verdict.col)) paste0(" class='", gsub(" ", "", tolower(df[[verdict.col]][i])), "'") else ""
            cells <- vapply(df[i, ], function(v) {
                if (is.numeric(v)) v <- format(signif(v, 4), big.mark = ",")
                paste0("<td>", esc(v), "</td>")
            }, "")
            paste0("<tr", cls, ">", paste(cells, collapse = ""), "</tr>")
        }, "")
        paste0("<div class='scroll'><table>", head, paste(rows, collapse = ""), "</table></div>")
    }
    img <- function(draw, width = 1000, height = 600) {
        f <- tempfile(fileext = ".png")
        png(f, width = width, height = height, res = 110)
        ok <- tryCatch({ draw(); TRUE }, error = function(e) { message("plot failed: ", e$message); FALSE },
                       finally = dev.off())
        if (!ok) return("<p><em>Plot could not be drawn.</em></p>")
        paste0("<img src='data:image/png;base64,", base64enc::base64encode(f), "'/>")
    }

    # ---- run settings (from the registration) ----
    info <- tryCatch(get.calibration.info(calibration.code), error = function(e) NULL)
    settings <- data.frame(
        setting = c("calibration code", "report date", "cities", "share of each chain used (last part)",
                    "chains", "iterations", "thin", "preceded by"),
        value   = c(calibration.code, format(Sys.time(), "%Y-%m-%d %H:%M"), length(locations), MIXING.BURN.KEEP,
                    if (!is.null(info)) info$n.chains else NA,
                    if (!is.null(info)) info$n.iter else NA,
                    if (!is.null(info)) info$thin else NA,
                    if (!is.null(info)) paste(info$preceding.calibration.codes, collapse = ", ") else NA))

    th <- MIXING.THRESHOLDS
    how.to.read <- data.frame(
        statistic = c("Acceptance", "Block acceptance by chain", "R-hat (rank-normalized, split)",
                      "R-hat of the log-posterior", "ESS (bulk, all chains)", "R-hat classic",
                      "Lag-1 autocorrelation", "Geweke z", "Log-lik gap by chain", "Chain-means spread ratio"),
        meaning   = c("how often proposals are accepted",
                      "is a block stuck in one chain?",
                      "do the chains, and the two halves of each chain, agree?",
                      "the same for the whole model in one number",
                      "how many independent draws the samples are worth, counting disagreement between chains",
                      "get.rhats(), not split; to compare with older reports",
                      "how similar each draw is to the previous one",
                      "does the start of a chain match its end?",
                      "is one chain stuck in a worse spot?",
                      "spread of chain means vs spread within chains"),
        good      = c("0.15-0.35 (target 0.238)", "> 0.1", "< 1.05", "< 1.05", paste0("> ", th$ess.min),
                      "< 1.05", "< 0.7", "|z| < 2", "small", "< 0.1"),
        worry     = c(paste0("< ", th$accept.low, " stuck; > ", th$accept.high, " steps too small"),
                      paste0("< ", th$block.accept.low), paste0("> ", th$rhat.fail), paste0("> ", th$rhat.fail),
                      paste0("< ", th$ess.min), "not in the verdict", "close to 1",
                      "not in the verdict: about 5% of tests pass |z| > 2 by chance",
                      "not in the verdict: depends on the likelihood scale", "> 0.3"))

    # ---- verdict counts ----
    counts <- table(factor(overview$verdict, levels = c("PASS", "CHECK", "FAIL", "NOT LOADED")))
    counts.html <- paste0(vapply(names(counts), function(v)
        paste0("<span class='pill ", tolower(sub(" ", "", v)), "'>", v, ": ", counts[[v]], "</span>"), ""),
        collapse = " ")

    # ---- per-city sections ----
    city.sections <- vapply(locations, function(loc) {
        key <- .mcmc.key(loc, calibration.code)
        m <- mcmc.list[[key]]; d <- diag.list[[key]]
        name <- .city.name(loc)
        if (is.null(m) || is.null(d))
            return(paste0("<h2 id='", esc(loc), "'>", esc(name), "</h2><p><em>Not loaded.</em></p>"))

        v <- mixing.verdict(d$summary)
        accept.block <- data.frame(block = names(d$accept.by.block),
                                   acceptance = round(as.numeric(d$accept.by.block), 3))
        drop.chain <- tryCatch(drop.one.chain.check(m, d$summary$burn.keep), error = function(e) NULL)
        burn       <- tryCatch(burn.in.check(m),        error = function(e) NULL)   # whole chain
        m.used     <- .burn.mcmc(m, d$summary$burn.keep)                            # for the plots

        plots <- if (include.plots) paste0(
            "<h3>Log-likelihood by chain</h3>",
            img(function() {
                ll <- d$loglik
                matplot(ll, type = "l", lty = 1, col = seq_len(ncol(ll)),
                        xlab = "saved iteration", ylab = "log-likelihood", main = name)
                legend("bottomright", legend = paste("chain", seq_len(ncol(ll))),
                       col = seq_len(ncol(ll)), lty = 1, bty = "n")
            }),
            "<h3>Acceptance over time, by chain</h3>",
            img(function() print(acceptance.plot(m.used))),
            "<h3>Trace plots: worst-mixing parameters</h3>",
            img(function() print(trace.plot(m.used, var.names = head(d$by.parameter$parameter, n.trace.params),
                                            exact.var.names = TRUE)), height = 900)
        ) else ""

        paste0(
            "<h2 id='", esc(loc), "'>", esc(name), " <span class='pill ", tolower(v$verdict), "'>",
            v$verdict, "</span></h2>",
            if (nzchar(v$reasons)) paste0("<p>", esc(v$reasons), "</p>") else "",
            "<h3>Chains</h3>", tbl(d$by.chain),
            "<h3>Log-likelihood and log-posterior: R-hat and ESS</h3>", tbl(d$log.density),
            "<h3>Worst ", n.worst.params, " parameters</h3>", tbl(head(d$by.parameter, n.worst.params)),
            "<h3>Blocks with acceptance &lt; ", th$block.accept.low, " in a chain</h3>", tbl(d$blocks.low),
            "<h3>Acceptance by block (lowest first)</h3>", tbl(accept.block),
            "<h3>Leave one chain out</h3>",
            "<p class='note'>If dropping one chain brings R-hat down a lot, that chain is stuck somewhere else.</p>",
            tbl(drop.chain),
            "<h3>Burn-in check</h3>",
            "<p class='note'>If R-hat drops a lot as early samples are removed, the chains were still converging. ",
            "If it still drops from 0.5 to 0.75, the half that is kept is still settling.</p>",
            tbl(burn),
            plots)
    }, "")

    failing <- params.failing.across.cities(locations, calibration.code)

    ov.cols <- intersect(c("city", "verdict", "reasons", "accept.total", "accept.min.chain", "accept.max.chain",
                           "min.block.accept", "max.rhat", "n.rhat.over.1.1", "rhat.logpost", "min.ess", "n.ess.low",
                           "ess.logpost", "max.rhat.classic", "n.geweke.over.2", "max.loglik.gap", "run.time.hours"),
                         names(overview))

    css <- "
      :root { --bg:#fff; --fg:#1d1d1f; --muted:#666; --line:#ddd; --head:#f4f4f6;
              --pass:#e3f4e6; --check:#fff4d6; --fail:#fbe0e0; --na:#eee; }
      @media (prefers-color-scheme: dark) {
        :root { --bg:#1b1b1d; --fg:#e8e8ea; --muted:#aaa; --line:#3a3a3d; --head:#2a2a2d;
                --pass:#1f3a25; --check:#3d3418; --fail:#442222; --na:#333; } }
      body { background:var(--bg); color:var(--fg); font:14px/1.45 -apple-system, Segoe UI, Helvetica, Arial, sans-serif;
             max-width:1150px; margin:0 auto; padding:24px 16px; }
      h1 { margin-bottom:4px; } h2 { margin-top:40px; border-bottom:1px solid var(--line); padding-bottom:4px; }
      h3 { margin-top:22px; font-size:15px; }
      .scroll { overflow-x:auto; } table { border-collapse:collapse; font-size:12.5px; margin:6px 0; }
      th, td { border:1px solid var(--line); padding:4px 8px; text-align:left; white-space:nowrap; }
      th { background:var(--head); }
      tr.pass td { background:var(--pass); } tr.check td { background:var(--check); }
      tr.fail td { background:var(--fail); } tr.notloaded td { background:var(--na); }
      .pill { display:inline-block; padding:2px 10px; border-radius:10px; font-size:13px; font-weight:600; }
      .pill.pass { background:var(--pass); } .pill.check { background:var(--check); }
      .pill.fail { background:var(--fail); } .pill.notloaded { background:var(--na); }
      .note { color:var(--muted); margin:2px 0; } img { max-width:100%; background:#fff; }
      nav a { margin-right:12px; }"

    overview.for.table <- overview[, ov.cols]
    overview.for.table$verdict[is.na(overview.for.table$verdict)] <- "NOT LOADED"
    overview.for.table$reasons[is.na(overview.for.table$reasons)] <- ""

    html <- paste0(
        "<!doctype html><html><head><meta charset='utf-8'>",
        "<meta name='viewport' content='width=device-width, initial-scale=1'>",
        "<title>Mixing report: ", esc(calibration.code), "</title><style>", css, "</style></head><body>",
        "<h1>MCMC mixing report</h1><p class='note'>", esc(calibration.code), "</p>",
        "<p>", counts.html, "</p>",
        "<nav>", paste0("<a href='#", esc(locations), "'>", esc(.city.name(locations)), "</a>", collapse = ""), "</nav>",
        "<h2>Run settings</h2>", tbl(settings),
        "<h2>How to read this report</h2>", tbl(how.to.read),
        "<p class='note'>Verdict: FAIL if R-hat of any parameter or of the log-posterior &gt; ", th$rhat.fail,
        ", or a chain's acceptance &lt; ", th$accept.low, ". ",
        "CHECK if R-hat &gt; ", th$rhat.check, ", ESS &lt; ", th$ess.min, ", a chain's acceptance &gt; ", th$accept.high,
        ", or a block's acceptance &lt; ", th$block.accept.low, " in a chain. ",
        "R-hat is the rank-normalized split R-hat; ESS is the bulk ESS across all chains.</p>",
        "<h2>Overview</h2>", tbl(overview.for.table, verdict.col = "verdict"),
        "<h2>Parameters that mix badly, and in how many cities</h2>",
        "<p class='note'>R-hat &gt; ", th$rhat.fail, " or ESS &lt; ", th$ess.min,
        ". A parameter that fails in many cities points to a prior or model issue, not just run length.</p>",
        tbl(failing),
        paste(city.sections, collapse = ""),
        "</body></html>")

    stamp <- format(Sys.time(), "%Y%m%d_%H%M")
    html.file <- file.path(folder, paste0("mixing_report_", calibration.code, "_", stamp, ".html"))
    writeLines(html, html.file)

    # CSVs: overview + every parameter for every city
    write.csv(overview, file.path(folder, paste0("mixing_overview_", calibration.code, "_", stamp, ".csv")),
              row.names = FALSE)
    all.params <- do.call(rbind, lapply(locations, function(loc) {
        d <- diag.list[[.mcmc.key(loc, calibration.code)]]
        if (!is.null(d)) cbind(city = .city.name(loc), location = loc, d$by.parameter)
    }))
    if (!is.null(all.params))
        write.csv(all.params, file.path(folder, paste0("mixing_parameters_", calibration.code, "_", stamp, ".csv")),
                  row.names = FALSE)

    message("Report saved: ", html.file)
    invisible(html.file)
}
