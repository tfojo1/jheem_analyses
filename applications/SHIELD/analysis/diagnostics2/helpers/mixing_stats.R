# ****************************************************************************************************
# DIAGNOSTICS2: MIXING STATISTICS ----
# ****************************************************************************************************
# Functions only. Each one names the section of METHODS.md it implements.
# Needs bayesian.simulations (loaded by 0-setup.R) and the settings in 0-setup.R
# (MIXING.BURN.KEEP, calibration.dir(), OLD.CACHE.DIR).
#
# Contents
#   1. Loading               load.mcmc.cached()
#   2. R-hat and ESS         rhat.rank.split(), ess.bulk()  (+ the small steps they use)
#   3. One city              mixing.stats()
#   4. Verdict               mixing.verdict()
#   5. Supporting checks     leave.one.chain.out(), burn.in.check(), params.failing.across.cities()
# Copied from analysis/diagnostics/mcmc/00-mcmc_helpers.R; the statistics are unchanged.
# ****************************************************************************************************

# Cut-offs for the verdict (METHODS 3)
MIXING.THRESHOLDS <- list(
    rhat.fail        = 1.1,    # any parameter, or the log-posterior, above this -> FAIL
    rhat.check       = 1.05,   # above this -> CHECK
    ess.min          = 400,    # any parameter below this (about 100 per chain) -> CHECK
    accept.low       = 0.10,   # any chain below this -> FAIL
    accept.high      = 0.50,   # any chain above this -> CHECK
    block.accept.low = 0.05    # any block below this in any chain -> CHECK
)

# Transmission parameters (flagged in the tables; METHODS 6)
TRANSMISSION.PATTERN <- "transmission|pairings|el\\.rel\\.secondary|prop\\.early\\.latent|age\\.mixing|sexual\\.multi"

# city name for a location code ("C.12580" -> "Baltimore")
city.name <- function(loc) {
    nm <- names(SHIELD.TEN.MSAS)[match(loc, SHIELD.TEN.MSAS)]
    ifelse(is.na(nm), loc, nm)
}


## 1. LOADING (METHODS 1.5) ----

# The MCMC of one city without its simulations
#   1. the calibration's cache:   <diagnostics root>/<code>/mcmc_cache/<location>.rds
#   2. else the old cache:        OLD.CACHE.DIR/<code>_<location>.rds (copied into the new cache)
#   3. else assembled from the calibration chunks (slow) and saved to the cache if all chains are finished
load.mcmc.cached <- function(loc, code = CALIBRATION.CODE) {
    file <- file.path(calibration.dir("mcmc_cache", code), paste0(loc, ".rds"))
    if (file.exists(file)) return(readRDS(file))

    old <- if (!is.null(OLD.CACHE.DIR)) file.path(OLD.CACHE.DIR, paste0(code, "_", loc, ".rds")) else ""
    if (file.exists(old)) {
        message(city.name(loc), ": copying the old cache into ", dirname(file))
        file.copy(old, file)
        return(readRDS(file))
    }

    message(city.name(loc), ": no cache, assembling from the calibration (slow)")
    m <- assemble.mcmc.without.sims(loc, code)
    complete <- all(tryCatch(get.calibration.progress("shield", loc, code), error = function(e) NA) == 100)
    if (isTRUE(complete)) saveRDS(m, file) else message(city.name(loc), ": calibration not finished, not cached")
    m
}

# Reads each chunk and drops its simulations, so only one chunk's simulations are in memory at a time
assemble.mcmc.without.sims <- function(loc, code) {
    calib.dir <- if (exists("get.calibration.dir", mode = "function")) get.calibration.dir else jheem2:::get.calibration.dir
    dir <- calib.dir(version = "shield", location = loc, calibration.code = code, root.dir = get.jheem.root.directory())
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

# Keep the last `keep` share of each chain (METHODS 1.3)
# (subset.mcmc(additional.burn = ) fails when additional.thin = 1, so every slot with one column per draw is sliced here)
burn.mcmc <- function(m, keep = MIXING.BURN.KEEP) {
    n.burn <- floor((1 - keep) * m@n.iter)
    if (n.burn <= 0) return(m)
    k <- (n.burn + 1):m@n.iter
    m@samples             <- m@samples[, k, , drop = FALSE]
    m@log.likelihoods     <- m@log.likelihoods[, k, drop = FALSE]
    m@log.priors          <- m@log.priors[, k, drop = FALSE]
    m@n.accepted          <- m@n.accepted[, k, , drop = FALSE]
    m@run.times           <- m@run.times[, k, drop = FALSE]
    m@first.step.for.iter <- m@first.step.for.iter[, k, drop = FALSE]
    m@simulation.indices  <- m@simulation.indices[, k, drop = FALSE]
    m@burn                <- as.integer(m@burn + n.burn * m@thin)
    m@n.iter              <- length(k)
    m
}

# draws as iteration x chain x variable
get.samples <- function(m) {
    s <- m@samples
    if (!is.null(names(dimnames(s))))
        s <- aperm(s, match(c("iteration", "chain", "variable"), names(dimnames(s))))
    s
}

# one variable (or log-likelihood, log-prior) as iteration x chain
by.chain <- function(x, n.chains) {
    x <- as.matrix(x)
    if (nrow(x) == n.chains && ncol(x) != n.chains) x <- t(x)
    x
}
one.variable <- function(s, v) { x <- s[, , v]; dim(x) <- dim(s)[1:2]; x }


## 2. R-HAT AND ESS (METHODS 2.1, 2.2, 2.4) ----
# x is always one quantity as iteration x chain

# METHODS 2.2 step 1: each chain cut in half -> twice as many half-chains (middle draw dropped if odd)
split.chains <- function(x) {
    half <- floor(nrow(x) / 2)
    cbind(x[seq_len(half), , drop = FALSE], x[nrow(x) - half + seq_len(half), , drop = FALSE])
}

# METHODS 2.2 step 2: each draw -> normal quantile of its rank among all draws
rank.normalize <- function(x) {
    z <- qnorm((rank(x) - 3/8) / (length(x) + 1/4))
    dim(z) <- dim(x)
    z
}

# METHODS 2.1: classic formula, sqrt(V / W)
rhat.basic <- function(x) {
    n <- nrow(x)
    W <- mean(apply(x, 2, var))
    if (!is.finite(W) || W == 0) return(NA_real_)
    B <- n * var(colMeans(x))
    sqrt(((n - 1) / n * W + B / n) / W)
}

# METHODS 2.2: max of bulk and tail R-hat. The R-hat used in every verdict and ranking.
rhat.rank.split <- function(x) {
    bulk <- rhat.basic(split.chains(rank.normalize(x)))
    tail <- rhat.basic(split.chains(rank.normalize(abs(x - median(x)))))
    if (is.na(bulk) && is.na(tail)) NA_real_ else max(bulk, tail, na.rm = TRUE)
}

# autocovariance of one series at lags 0 to n-1, by FFT
autocovariance <- function(y) {
    n  <- length(y)
    m2 <- 2 * nextn(n)
    f  <- fft(c(y - mean(y), rep(0, m2 - n)))
    Re(fft(Conj(f) * f, inverse = TRUE))[seq_len(n)] / (m2 * n)
}

# METHODS 2.4 steps 2-4: ESS across all chains (Geyer initial monotone sequence)
ess.multi.chain <- function(x) {
    n <- nrow(x); k <- ncol(x)
    if (n < 4) return(NA_real_)
    acov <- apply(x, 2, autocovariance)                       # lag x chain
    W    <- mean(acov[1, ]) * n / (n - 1)
    if (!is.finite(W)) return(NA_real_)
    if (W == 0) return(0)
    var.plus <- W * (n - 1) / n + if (k > 1) var(colMeans(x)) else 0
    rho  <- 1 - (W - rowMeans(acov)) / var.plus
    rho[1] <- 1
    n.pairs <- floor(n / 2)
    pair <- rho[2 * seq_len(n.pairs) - 1] + rho[2 * seq_len(n.pairs)]
    stop.at <- which(pair <= 0)[1]
    if (!is.na(stop.at)) pair <- pair[seq_len(stop.at - 1)]
    tau <- max(-1 + 2 * sum(cummin(pair)), 1 / log10(n * k))
    n * k / tau
}

# METHODS 2.4 step 1: bulk ESS = ESS of the rank-normalized half-chains
ess.bulk <- function(x) ess.multi.chain(split.chains(rank.normalize(x)))

# worst-first R-hat of every parameter (s = iteration x chain x variable), optionally on some chains only
all.rhats <- function(s, chains = seq_len(dim(s)[2])) {
    r <- sapply(dimnames(s)[[3]], function(v) rhat.rank.split(one.variable(s, v)[, chains, drop = FALSE]))
    sort(r, decreasing = TRUE, na.last = TRUE)
}


## 3. ONE CITY ----
# m = the whole MCMC as loaded; the statistics use the last burn.keep share of each chain
mixing.stats <- function(m, burn.keep = MIXING.BURN.KEEP, th = MIXING.THRESHOLDS) {
    m.kept   <- burn.mcmc(m, burn.keep)
    s        <- get.samples(m.kept)
    n.chains <- dim(s)[2]
    vars     <- dimnames(s)[[3]]

    # METHODS 2.5: acceptance by chain, by block, and block x chain
    accept.by.chain <- as.numeric(get.total.acceptance.rate(m.kept, aggregate.chains = FALSE))
    accept.by.block <- get.total.acceptance.rate(m.kept, by.block = TRUE)
    abc <- matrix(get.total.acceptance.rate(m.kept, by.block = TRUE, aggregate.chains = FALSE), nrow = n.chains,
                  dimnames = list(chain = seq_len(n.chains), block = names(accept.by.block)))
    low <- which(abc < th$block.accept.low, arr.ind = TRUE)
    blocks.low <- data.frame(block = colnames(abc)[low[, "block"]], chain = as.integer(low[, "chain"]),
                             acceptance = round(abc[low], 3))
    blocks.low <- blocks.low[order(blocks.low$acceptance), ]

    # METHODS 2.1, 2.2, 2.4: per parameter
    rhat         <- all.rhats(s)
    rhat.classic <- get.rhats(m.kept, sort = FALSE)
    by.parameter <- data.frame(
        parameter    = vars,
        rhat         = round(rhat[vars], 3),
        rhat.classic = round(as.numeric(rhat.classic[vars]), 3),
        ess.bulk     = round(sapply(vars, function(v) ess.bulk(one.variable(s, v)))),
        transmission = grepl(TRANSMISSION.PATTERN, vars),
        stringsAsFactors = FALSE)
    by.parameter <- by.parameter[order(-by.parameter$rhat, by.parameter$ess.bulk), ]
    rownames(by.parameter) <- NULL

    # METHODS 2.3: log-likelihood and log-posterior
    ll <- by.chain(m.kept@log.likelihoods, n.chains)
    lp <- by.chain(m.kept@log.priors, n.chains)
    chains <- data.frame(chain = seq_len(n.chains),
                         acceptance       = round(accept.by.chain, 3),
                         min.block.accept = round(apply(abc, 1, min), 3),
                         mean.loglik      = round(colMeans(ll), 1),
                         gap.from.best    = round(max(colMeans(ll)) - colMeans(ll), 1))
    log.density <- data.frame(quantity = c("log-likelihood", "log-posterior"),
                              rhat     = round(c(rhat.rank.split(ll), rhat.rank.split(ll + lp)), 3),
                              ess.bulk = round(c(ess.bulk(ll), ess.bulk(ll + lp))))

    summary <- data.frame(
        n.chains         = n.chains,
        draws.per.chain  = dim(s)[1],
        accept.min.chain = round(min(accept.by.chain), 3),
        accept.max.chain = round(max(accept.by.chain), 3),
        min.block.accept = round(min(abc), 3),
        n.blocks.low     = length(unique(blocks.low$block)),
        max.rhat         = round(max(rhat, na.rm = TRUE), 3),
        n.rhat.over.1.1  = sum(rhat > 1.1, na.rm = TRUE),
        n.rhat.over.2    = sum(rhat > 2, na.rm = TRUE),
        rhat.logpost     = log.density$rhat[2],
        min.ess          = min(by.parameter$ess.bulk, na.rm = TRUE),
        n.ess.low        = sum(by.parameter$ess.bulk < th$ess.min, na.rm = TRUE),
        burn.keep        = burn.keep)

    list(summary = summary, chains = chains, log.density = log.density, by.parameter = by.parameter,
         accept.by.block = sort(accept.by.block), blocks.low = blocks.low, loglik = ll,
         samples = s)                                # kept draws, for the identifiability check
}


## 4. VERDICT (METHODS 3) ----
mixing.verdict <- function(summary, th = MIXING.THRESHOLDS) {
    above <- function(x, cut) !is.na(x) && x > cut
    fail <- c(
        if (above(summary$max.rhat, th$rhat.fail))
            paste0("R-hat ", summary$max.rhat, " > ", th$rhat.fail, " (", summary$n.rhat.over.1.1, " parameter", if (summary$n.rhat.over.1.1 == 1) ")" else "s)"),
        if (above(summary$rhat.logpost, th$rhat.fail))
            paste0("log-posterior R-hat ", summary$rhat.logpost, " > ", th$rhat.fail),
        if (summary$accept.min.chain < th$accept.low)
            paste0("a chain has acceptance ", summary$accept.min.chain))
    check <- c(
        if (above(summary$max.rhat, th$rhat.check) && !above(summary$max.rhat, th$rhat.fail))
            paste0("R-hat ", summary$max.rhat, " > ", th$rhat.check),
        if (above(summary$rhat.logpost, th$rhat.check) && !above(summary$rhat.logpost, th$rhat.fail))
            paste0("log-posterior R-hat ", summary$rhat.logpost, " > ", th$rhat.check),
        if (summary$min.ess < th$ess.min)
            paste0(summary$n.ess.low, " parameter", if (summary$n.ess.low == 1) "" else "s", " with ESS < ", th$ess.min),
        if (summary$accept.max.chain > th$accept.high)
            paste0("a chain has acceptance ", summary$accept.max.chain),
        if (summary$min.block.accept < th$block.accept.low)
            paste0(summary$n.blocks.low, " block", if (summary$n.blocks.low == 1) "" else "s", " with acceptance < ", th$block.accept.low, " in a chain"))
    list(verdict = if (length(fail)) "FAIL" else if (length(check)) "CHECK" else "PASS",
         reasons = paste(c(fail, check), collapse = "; "))
}


## 5. SUPPORTING CHECKS ----

# METHODS 4.1: worst R-hat with each chain left out (s = kept draws)
leave.one.chain.out <- function(s) {
    n.chains <- dim(s)[2]
    if (n.chains < 3) return(NULL)
    base <- max(all.rhats(s), na.rm = TRUE)
    out <- do.call(rbind, lapply(seq_len(n.chains), function(ch) {
        r <- all.rhats(s, chains = setdiff(seq_len(n.chains), ch))
        data.frame(chain.left.out = ch, max.rhat = round(r[[1]], 3), n.rhat.over.1.1 = sum(r > 1.1, na.rm = TRUE),
                   worst.parameter = names(r)[1], stringsAsFactors = FALSE)
    }))
    out$drop.in.max.rhat <- round(base - out$max.rhat, 3)
    out[order(-out$drop.in.max.rhat), ]
}

# METHODS 4.2: worst R-hat after removing the first 0, 25, 50, 75% of each chain (m = whole chain)
burn.in.check <- function(m, fractions = c(0, 0.25, 0.5, 0.75)) {
    do.call(rbind, lapply(fractions, function(f) {
        b  <- burn.mcmc(m, keep = 1 - f)
        r  <- all.rhats(get.samples(b))
        ll <- by.chain(b@log.likelihoods, b@n.chains)
        lp <- by.chain(b@log.priors, b@n.chains)
        data.frame(share.removed = f, draws.per.chain = b@n.iter, max.rhat = round(r[[1]], 3),
                   n.rhat.over.1.1 = sum(r > 1.1, na.rm = TRUE), rhat.logpost = round(rhat.rank.split(ll + lp), 3))
    }))
}

# METHODS 4.3: parameters with R-hat > 1.1 or ESS < 400, counted across cities
# by.parameter.list = named list (city -> by.parameter table)
params.failing.across.cities <- function(by.parameter.list, th = MIXING.THRESHOLDS) {
    bad <- do.call(rbind, lapply(names(by.parameter.list), function(city) {
        p <- by.parameter.list[[city]]
        p <- p[which(p$rhat > th$rhat.fail | p$ess.bulk < th$ess.min), ]
        if (nrow(p)) data.frame(parameter = p$parameter, city = city, rhat = p$rhat, stringsAsFactors = FALSE)
    }))
    if (is.null(bad)) return(data.frame())
    out <- data.frame(parameter = unique(bad$parameter), stringsAsFactors = FALSE)
    out$n.cities <- sapply(out$parameter, function(p) sum(bad$parameter == p))
    out$max.rhat <- sapply(out$parameter, function(p) max(bad$rhat[bad$parameter == p]))
    out$cities   <- sapply(out$parameter, function(p) paste(bad$city[bad$parameter == p], collapse = ", "))
    out <- out[order(-out$n.cities, -out$max.rhat), ]
    rownames(out) <- NULL
    out
}
