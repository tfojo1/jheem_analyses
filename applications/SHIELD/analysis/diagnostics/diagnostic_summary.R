# ****************************************************************************************************
# SHIELD CALIBRATION — MCMC MIXING & ACCEPTANCE DIAGNOSTICS
# ****************************************************************************************************
# Works on multi-chain calibrations (e.g. stage 3, 4 chains).
# Reads the raw MCMC (not the simsets), because acceptance rates only live there.
#
# How to use:
#  1. Set LOCATIONS and CALIBRATION.CODE in the SETUP section
#     (start with one city to test, then switch to SHIELD.TEN.MSAS)
#  2. Source the file up to "RUN", then run the RUN section line by line
#
# What each statistic tells you:
#  1. Acceptance rate       - how often proposals are accepted. Target 0.238; ~0.15-0.35 is fine.
#                             Very low = stuck. Very high = tiny steps (slow exploration).
#  2. R-hat                 - do the chains agree? < 1.05 good, < 1.1 ok, > 1.1 not mixed.
#  3. Rank-normalized R-hat - same idea, but robust to skewed / heavy-tailed parameters.
#  4. ESS                   - effective sample size: how many independent draws the samples are worth.
#                             < 100 is too few to trust summaries of that parameter.
#  5. Lag-1 autocorrelation - how similar each draw is to the previous one. Close to 1 = slow mixing.
#  6. Geweke z              - does the first 10% of a chain agree with the last 50%?
#                             |z| > 2 means the chain was still drifting (not converged).
#  7. Stuck fraction        - share of saved draws where the value did not change at all.
#                             (with thin = 50, "no change" means nothing accepted in 50 iterations)
#  8. Log-likelihood by chain - if one chain sits much lower, it is stuck in a worse spot.
# ****************************************************************************************************

library(bayesian.simulations)
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")   # also sets the jheem root dir

# ---- SETUP ----
CALIBRATION.CODE <- "calib.9.23.stage3.pk"
LOCATIONS        <- SHIELD.TEN.MSAS      # test with one city first; then use SHIELD.TEN.MSAS
# Saved on the server (shared with the team): <jheem root>/shield/mcmc_diagnostics/
MCMC.CACHE.DIR   <- file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "mcmc_cache")  # slimmed MCMCs, for fast re-loading
PLOT.DIR         <- file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "plots")       # PDFs from save.mixing.plots()
REPORT.DIR       <- file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "reports")     # HTML reports from write.mixing.report()

# Thresholds used to flag a city (used by mixing.verdict)
MIXING.THRESHOLDS <- list(
    rhat.fail       = 1.1,    # any parameter above this -> FAIL
    rhat.check      = 1.05,   # any parameter above this -> CHECK
    ess.min         = 100,    # any parameter below this -> CHECK
    accept.low      = 0.10,   # any chain below this     -> FAIL (stuck)
    accept.high     = 0.50,   # any chain above this     -> CHECK (steps too small)
    loglik.gap      = 10      # a chain this far below the best chain -> FAIL (stuck in a worse spot)
)

# Results are kept in these lists, keyed "location | calibration.code",
# so nothing is overwritten when you move to the next city
if (!exists("mcmc.list")) mcmc.list <- list()
if (!exists("diag.list")) diag.list <- list()

.mcmc.key <- function(loc, code) paste0(loc, " | ", code)

# city name for a location code (e.g. "C.12580" -> "Baltimore")
.city.name <- function(loc) {
    nm <- names(SHIELD.TEN.MSAS)[match(loc, SHIELD.TEN.MSAS)]
    ifelse(is.na(nm), loc, nm)
}


# ****************************************************************************************************
# PART 1: LOADING
# ****************************************************************************************************

# Cached MCMC loader
# 1. Looks in mcmc.list (memory) first
# 2. Then looks for a saved .rds file in cache.dir
# 3. Otherwise assembles from the calibration, drops the simulations, and saves it
#    (only saved to disk if all chains are complete, so a partial run is not reused later)
load.mcmc <- function(locations, calibration.code,
                      cache.dir = MCMC.CACHE.DIR,
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

        m <- tryCatch(assemble.mcmc.from.calibration(version = "shield",
                                                     location = loc,
                                                     calibration.code = calibration.code,
                                                     allow.incomplete = TRUE),
                      error = function(e) { message(loc, ": ", e$message); NULL })
        if (is.null(m)) return(NULL)

        m@simulations <- list()   # not needed for diagnostics

        pct <- tryCatch(get.calibration.progress("shield", loc, calibration.code),
                        error = function(e) NA)
        if (all(!is.na(pct) & pct == 100)) saveRDS(m, file)
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

# ESS for one chain (Geyer initial positive sequence)
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

# Rank-normalized R-hat for one parameter (x = iteration x chain)
# (get.rhats(rank.normalize = TRUE) in bayesian.simulations errors, so we compute it here)
.rank.rhat <- function(x) {
    n <- nrow(x)
    z <- qnorm((rank(x) - 3/8) / (length(x) + 1/4))
    dim(z) <- dim(x)
    W <- mean(apply(z, 2, var))
    B <- n * var(colMeans(z))
    if (W == 0) return(NA)
    sqrt(((n - 1) / n * W + B / n) / W)
}

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
mcmc.diagnostics <- function(m, stuck.tolerance = 0) {
    s        <- .get.samples(m)          # iteration x chain x variable
    n.chains <- dim(s)[2]
    vars     <- dimnames(s)[[3]]

    # 1. Acceptance
    accept.total    <- get.total.acceptance.rate(m)
    accept.by.chain <- get.total.acceptance.rate(m, aggregate.chains = FALSE)
    accept.by.block <- get.total.acceptance.rate(m, by.block = TRUE)
    accept.block.by.chain <- get.total.acceptance.rate(m, by.block = TRUE, aggregate.chains = FALSE)

    # 2-3. R-hat (plain and rank-normalized)
    if (n.chains > 1) {
        rhat      <- get.rhats(m, sort = FALSE)
        rhat.rank <- sapply(vars, function(v) .rank.rhat(matrix(s[, , v], ncol = n.chains)))
    } else {
        rhat <- rhat.rank <- setNames(rep(NA_real_, length(vars)), vars)
    }

    # 4-7. Per-parameter statistics
    per.param <- do.call(rbind, lapply(vars, function(v) {
        x <- s[, , v, drop = FALSE]; dim(x) <- dim(x)[1:2]   # iteration x chain
        ess.per.chain <- apply(x, 2, .ess.one.chain)
        lag1 <- apply(x, 2, function(y) if (var(y) == 0) 1 else acf(y, lag.max = 1, plot = FALSE)$acf[2])
        gz   <- apply(x, 2, .geweke.z)
        stuck <- apply(x, 2, function(y) mean(abs(diff(y)) <= stuck.tolerance))
        data.frame(
            parameter      = v,
            rhat           = unname(rhat[v]),
            rhat.rank      = unname(rhat.rank[v]),
            ess.total      = round(sum(ess.per.chain)),
            ess.min.chain  = round(min(ess.per.chain)),
            lag1.acf.max   = round(max(lag1), 3),
            geweke.max.abs = suppressWarnings(round(max(abs(gz), na.rm = TRUE), 2)),
            stuck.max      = round(max(stuck), 3),
            chain.means.sd.ratio = round(sd(colMeans(x)) / sqrt(mean(apply(x, 2, var))), 2),
            stringsAsFactors = FALSE
        )
    }))
    per.param <- per.param[order(-per.param$rhat, per.param$ess.total), ]
    rownames(per.param) <- NULL

    # 8. Log-likelihood / log-prior by chain
    ll <- .get.by.chain(m@log.likelihoods, n.chains)
    lp <- .get.by.chain(m@log.priors,      n.chains)
    by.chain <- data.frame(
        chain            = seq_len(n.chains),
        accept           = round(as.numeric(accept.by.chain), 3),
        mean.loglik      = round(colMeans(ll), 1),
        last.loglik      = round(ll[nrow(ll), ], 1),
        mean.logpost     = round(colMeans(ll + lp), 1),
        n.params.stuck50 = sapply(seq_len(n.chains), function(ch)
            sum(apply(s[, ch, , drop = FALSE], 3, function(y) mean(abs(diff(y)) <= stuck.tolerance)) > 0.5))
    )
    by.chain$loglik.gap.from.best <- round(max(by.chain$mean.loglik) - by.chain$mean.loglik, 1)

    # overall summary (one row)
    summary <- data.frame(row.names = NULL,
        n.chains            = n.chains,
        n.samples.per.chain = dim(s)[1],
        accept.total        = round(as.numeric(accept.total), 3),
        accept.min.chain    = round(min(accept.by.chain), 3),
        accept.max.chain    = round(max(accept.by.chain), 3),
        max.rhat            = round(max(rhat), 3),
        n.rhat.over.1.05    = sum(rhat > 1.05),
        n.rhat.over.1.1     = sum(rhat > 1.1),
        max.rhat.rank       = round(max(rhat.rank), 3),
        min.ess             = min(per.param$ess.total),
        n.ess.under.100     = sum(per.param$ess.total < 100),
        n.geweke.over.2     = sum(per.param$geweke.max.abs > 2, na.rm = TRUE),
        max.loglik.gap      = max(by.chain$loglik.gap.from.best),
        run.time.hours      = round(sum(m@total.run.time) / 3600, 1)
    )

    list(summary               = summary,
         by.chain              = by.chain,
         by.parameter          = per.param,
         accept.by.block       = sort(accept.by.block),
         accept.block.by.chain = accept.block.by.chain,
         loglik                = ll)
}


# ---- plots for one MCMC ----
plot.mcmc.diagnostics <- function(m, d = mcmc.diagnostics(m), n.worst = 9, title = "") {
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
mixing.verdict <- function(summary, th = MIXING.THRESHOLDS) {
    fail <- c(
        if (summary$max.rhat > th$rhat.fail)          paste0("R-hat ", summary$max.rhat, " > ", th$rhat.fail),
        if (summary$accept.min.chain < th$accept.low) paste0("a chain has acceptance ", summary$accept.min.chain),
        if (summary$max.loglik.gap > th$loglik.gap)   paste0("a chain is ", summary$max.loglik.gap, " log-lik below the best")
    )
    check <- c(
        if (summary$max.rhat > th$rhat.check && summary$max.rhat <= th$rhat.fail)
            paste0("R-hat ", summary$max.rhat, " > ", th$rhat.check),
        if (summary$min.ess < th$ess.min)              paste0(summary$n.ess.under.100, " params with ESS < ", th$ess.min),
        if (summary$accept.max.chain > th$accept.high) paste0("a chain has acceptance ", summary$accept.max.chain),
        if (summary$n.geweke.over.2 > 0)               paste0(summary$n.geweke.over.2, " params still drifting (Geweke)")
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

        if (force.recompute || is.null(diag.list[[key]]))
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
params.failing.across.cities <- function(locations, calibration.code, rhat.cutoff = 1.1, ess.cutoff = 100) {
    per.city <- do.call(rbind, lapply(unname(locations), function(loc) {
        d <- diag.list[[.mcmc.key(loc, calibration.code)]]
        if (is.null(d)) return(NULL)
        bad <- d$by.parameter[d$by.parameter$rhat > rhat.cutoff | d$by.parameter$ess.total < ess.cutoff, ]
        if (nrow(bad) == 0) return(NULL)
        data.frame(parameter = bad$parameter, city = .city.name(loc), rhat = round(bad$rhat, 2), ess = bad$ess.total)
    }))
    if (is.null(per.city)) return(data.frame())

    counts <- aggregate(city ~ parameter, per.city, function(x) length(x))
    names(counts)[2] <- "n.cities"
    counts$cities   <- tapply(per.city$city, per.city$parameter, paste, collapse = ", ")[counts$parameter]
    counts$max.rhat <- tapply(per.city$rhat, per.city$parameter, max)[counts$parameter]
    counts[order(-counts$n.cities, -counts$max.rhat), ]
}


# ---- 3d. Is one chain the problem? ----
# Recomputes R-hat leaving out each chain in turn.
# If dropping one chain brings R-hat down a lot, that chain is stuck somewhere else.
drop.one.chain.check <- function(m) {
    if (m@n.chains < 3) stop("Need at least 3 chains to drop one and still compute R-hat")
    base <- max(get.rhats(m))
    rv <- do.call(rbind, lapply(seq_len(m@n.chains), function(ch) {
        keep <- setdiff(seq_len(m@n.chains), ch)
        r <- get.rhats(m, chains = keep)
        data.frame(dropped.chain   = ch,
                   max.rhat        = round(max(r), 3),
                   n.rhat.over.1.1 = sum(r > 1.1),
                   worst.parameter = names(r)[1])
    }))
    rv$improvement <- round(base - rv$max.rhat, 3)
    rv[order(-rv$improvement), ]
}


# ---- 3e. Would more burn-in help? ----
# Recomputes R-hat after throwing away the first 0%, 25%, 50% of each chain.
# If R-hat drops a lot with more burn-in, the chains were still converging early on
# (so a longer run, or a later start, would help).
burn.in.check <- function(m, fractions = c(0, 0.25, 0.5)) {
    do.call(rbind, lapply(fractions, function(f) {
        burn <- floor(f * m@n.iter)
        r <- get.rhats(m, additional.burn = burn)
        data.frame(burn.fraction   = f,
                   samples.left    = m@n.iter - burn,
                   max.rhat        = round(max(r), 3),
                   n.rhat.over.1.1 = sum(r > 1.1),
                   n.rhat.over.1.05= sum(r > 1.05))
    }))
}


# ---- 3f. Save diagnostic plots to one PDF per city ----
save.mixing.plots <- function(locations, calibration.code, folder = PLOT.DIR, n.worst = 9) {
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
        c("city", "calibration.code", "verdict", "accept.total", "max.rhat", "n.rhat.over.1.1", "min.ess", "max.loglik.gap")]
}


# ---- 3h. Write a shareable report (one self-contained HTML file) ----
# Everything from the diagnostics in one file you can email / post:
#  1. Run settings + a "how to read this" table
#  2. Overview: one row per city with PASS / CHECK / FAIL and the reasons
#  3. Parameters that mix badly in several cities
#  4. Per city: chains, worst parameters, acceptance by block, drop-one-chain, burn-in, plots
# Also writes the overview and all per-parameter stats as CSV files next to it.
write.mixing.report <- function(locations, calibration.code,
                                folder = REPORT.DIR,
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
        setting = c("calibration code", "report date", "cities", "chains", "iterations", "thin", "preceded by"),
        value   = c(calibration.code, format(Sys.time(), "%Y-%m-%d %H:%M"), length(locations),
                    if (!is.null(info)) info$n.chains else NA,
                    if (!is.null(info)) info$n.iter else NA,
                    if (!is.null(info)) info$thin else NA,
                    if (!is.null(info)) paste(info$preceding.calibration.codes, collapse = ", ") else NA))

    th <- MIXING.THRESHOLDS
    how.to.read <- data.frame(
        statistic = c("Acceptance", "R-hat / rank R-hat", "ESS", "Lag-1 autocorrelation", "Geweke z",
                      "Stuck fraction", "Log-lik gap by chain", "Chain-means spread ratio"),
        meaning   = c("how often proposals are accepted",
                      "do the chains agree?",
                      "how many independent draws the samples are worth",
                      "how similar each draw is to the previous one",
                      "does the start of a chain match its end?",
                      "share of saved draws with no change",
                      "is one chain stuck in a worse spot?",
                      "spread of chain means vs spread within chains"),
        good      = c("0.15-0.35 (target 0.238)", "< 1.05", "> 400", "< 0.7", "|z| < 2", "low", "small", "< 0.1"),
        worry     = c(paste0("< ", th$accept.low, " stuck; > ", th$accept.high, " steps too small"),
                      paste0("> ", th$rhat.fail), paste0("< ", th$ess.min), "close to 1",
                      "|z| > 2 (still drifting)", "> 0.5", paste0("> ", th$loglik.gap), "> 0.3"))

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
        drop.chain <- tryCatch(drop.one.chain.check(m), error = function(e) NULL)
        burn       <- tryCatch(burn.in.check(m),        error = function(e) NULL)

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
            img(function() print(acceptance.plot(m))),
            "<h3>Trace plots: worst-mixing parameters</h3>",
            img(function() print(trace.plot(m, var.names = head(d$by.parameter$parameter, n.trace.params),
                                            exact.var.names = TRUE)), height = 900)
        ) else ""

        paste0(
            "<h2 id='", esc(loc), "'>", esc(name), " <span class='pill ", tolower(v$verdict), "'>",
            v$verdict, "</span></h2>",
            if (nzchar(v$reasons)) paste0("<p>", esc(v$reasons), "</p>") else "",
            "<h3>Chains</h3>", tbl(d$by.chain),
            "<h3>Worst ", n.worst.params, " parameters</h3>", tbl(head(d$by.parameter, n.worst.params)),
            "<h3>Acceptance by block (lowest first)</h3>", tbl(accept.block),
            "<h3>Leave one chain out</h3>",
            "<p class='note'>If dropping one chain brings R-hat down a lot, that chain is stuck somewhere else.</p>",
            tbl(drop.chain),
            "<h3>Burn-in check</h3>",
            "<p class='note'>If R-hat drops a lot as early samples are removed, the chains were still converging.</p>",
            tbl(burn),
            plots)
    }, "")

    failing <- params.failing.across.cities(locations, calibration.code)

    ov.cols <- intersect(c("city", "verdict", "reasons", "accept.total", "accept.min.chain", "accept.max.chain",
                           "max.rhat", "n.rhat.over.1.1", "max.rhat.rank", "min.ess", "n.ess.under.100",
                           "n.geweke.over.2", "max.loglik.gap", "run.time.hours"), names(overview))

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
        "<p class='note'>Verdict: FAIL if R-hat &gt; ", th$rhat.fail, ", a chain's acceptance &lt; ", th$accept.low,
        ", or a chain's log-likelihood is &gt; ", th$loglik.gap, " below the best chain. ",
        "CHECK if R-hat &gt; ", th$rhat.check, ", ESS &lt; ", th$ess.min, ", acceptance &gt; ", th$accept.high,
        ", or any |Geweke z| &gt; 2.</p>",
        "<h2>Overview</h2>", tbl(overview.for.table, verdict.col = "verdict"),
        "<h2>Parameters that mix badly, and in how many cities</h2>",
        "<p class='note'>R-hat &gt; 1.1 or ESS &lt; 100. A parameter that fails in many cities points to a prior or model issue, not just run length.</p>",
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


# ****************************************************************************************************
# RUN
# ****************************************************************************************************

# 1. Overview (one row per city). Slow the first time; fast after (memory / disk cache)
overview <- run.mixing.diagnostics(LOCATIONS, CALIBRATION.CODE)
overview[, c("city", "verdict", "reasons")]
overview

# 2. Look closer at one city
loc <- unname(LOCATIONS[1])
m   <- mcmc.list[[.mcmc.key(loc, CALIBRATION.CODE)]]
d   <- diag.list[[.mcmc.key(loc, CALIBRATION.CODE)]]

d$by.chain                  # acceptance + log-likelihood per chain
head(d$by.parameter, 15)    # worst-mixing parameters first
d$accept.by.block           # acceptance per parameter block, lowest first
d$accept.block.by.chain     # block x chain

# 3. Is one chain the problem? Would more burn-in help?
drop.one.chain.check(m)
burn.in.check(m)

# 4. Plots (on screen for one city; to PDF for all)
plot.mcmc.diagnostics(m, d, title = .city.name(loc))
# save.mixing.plots(LOCATIONS, CALIBRATION.CODE)

# 5. Once LOCATIONS = SHIELD.TEN.MSAS: parameters that mix badly in several cities
# params.failing.across.cities(LOCATIONS, CALIBRATION.CODE)

# 6. Compare two calibrations
# compare.calibration.mixing(LOCATIONS, c("calib.9.23.stage3", "calib.9.23.stage3.pk"))

# 7. Shareable report: one HTML file (+ 2 CSVs) in REPORT.DIR, to send to the team
write.mixing.report(LOCATIONS, CALIBRATION.CODE)
