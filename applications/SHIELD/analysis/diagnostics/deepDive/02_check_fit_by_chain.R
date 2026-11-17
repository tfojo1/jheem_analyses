# ****************************************************************************************************
# CHECK FIT BY CHAIN ----
# ****************************************************************************************************
# Do the four MCMC chains fit the calibration data equally well? Used to decide whether a chain that
# gives a different doxy-PEP effect (01_doxy_impact_by_chain.R) can be left out on the basis of fit.
#
# For each city in CITIES, from the posterior simset (N.SIM in diagnostics_setup.R: 1,000 = 250 per chain):
# 1. LIKELIHOOD BY CHAIN: the calibration's own likelihood, component by component, for N.LIK.PER.CHAIN
#    simulations per chain (evenly spaced along each chain). This is the fit the calibration optimised.
#      gap.from.best = chain mean - best chain mean (log-likelihood units; 0 = best chain, negative = worse)
#      gap.in.sd     = gap.from.best / within-chain SD, so gaps are comparable across components
# 2. MODEL / DATA BY CHAIN: diagnoses by sex and period (P&S, total, EL, late), model median of each
#    chain / observed. 1 = right, > 1 model too high, < 1 too low. Same data and periods as
#    check_fit_by_sex_over_time.R. "male" in the data is all men; model men = msm + heterosexual_male.
#    The MSM share of male P&S diagnoses is in step 1 (component prop.male.ps.diag.among.msm).
# 3. FIT VS DOXY-PEP EFFECT: one row per city x chain: total likelihood gap, the components with the
#    largest gaps, and the chain's median doxy-PEP effect at CCRIT from the latest 01_doxy_impact_by_chain.R output.
#    If the worse-fitting chains are also the ones with the different effect, leaving them out is justified;
#    if chains that fit equally well give different effects, it is not.
# 4. PLOTS: P&S diagnoses by sex over time, one line per chain (median) and the data as points.
#
# MEMORY AND TIME
#   One city's simset is loaded at a time and removed before the next.
#   Step 1 runs the likelihood N.LIK.PER.CHAIN x 4 times per city, plus building the likelihood once
#   per city (slow). Start with N.LIK.PER.CHAIN = 10 to time it. No simulation is run.
#
# USAGE (from jheem_analyses)
#   1. CALIBRATION.CODE and N.SIM are set in diagnostics_setup.R (calib.10.20.stage4, 1000)
#   2. set CITIES below (default: the three cities whose doxy-PEP effect depends on the chain)
#   3. source("applications/SHIELD/analysis/diagnostics/deepDive/02_check_fit_by_chain.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   fit_by_chain_likelihood_<code>_<date>.csv      city x component x chain
#   fit_by_chain_model_vs_data_<code>_<date>.csv   city x outcome x sex x period x chain
#   fit_by_chain_vs_doxy_<code>_<date>.csv         city x chain
#   fit_by_chain_ps_by_sex_<code>.png
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
library(ggplot2)


## SETTINGS ----
CITIES          <- c("Chicago", "Houston", "LA")   # names(SHIELD.TEN.MSAS); NULL = all LOCATIONS
N.LIK.PER.CHAIN <- 25                              # simulations per chain for step 1 (of 250)
CCRIT           <- "doxy.cov.20"                   # coverage for the doxy-PEP column in step 3
DOXY.OUTCOME    <- "pct_change_incidence_vs_noint"
PERIODS <- list("1993-1999" = 1993:1999, "2000-2004" = 2000:2004, "2005-2009" = 2005:2009,
                "2010-2014" = 2010:2014, "2015-2018" = 2015:2018, "2019-2021" = 2019:2021,
                "2022-2023" = 2022:2023)           # 2022-2023: after the calibration period
CALIBRATION.PERIODS <- names(PERIODS)[1:6]
FIT.OUTCOMES <- c(diagnosis.ps                 = "ps.syphilis.diagnoses",   # model outcome = data outcome
                  diagnosis.total              = "total.syphilis.diagnoses",
                  diagnosis.el.misclassified   = "early.syphilis.diagnoses",
                  diagnosis.late.misclassified = "unknown.duration.or.late.syphilis.diagnoses")
YEARS      <- as.character(min(unlist(PERIODS)):max(unlist(PERIODS)))
PLOT.YEARS <- as.character(1990:2025)


## FOLDERS ----
LOCS <- if (is.null(CITIES)) LOCATIONS else SHIELD.TEN.MSAS[CITIES]
if (is.null(LOCS)) LOCS <- SHIELD.TEN.MSAS
if (anyNA(LOCS)) stop("Unknown city in CITIES: ", paste(CITIES[is.na(LOCS)], collapse = ", "))
FIT.DIR  <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE, "deepDive")
DOXY.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE, "deepDive")
dir.create(FIT.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## COLUMN DESCRIPTIONS ----
# First two rows of each CSV: c(what it is, how to read it). Edit the text here, not in the CSVs.
COLUMN.NOTES <- list(
    city          = c("MSA name", "One city per block of rows"),
    chain         = c("MCMC chain (simulation.chain of the simset)", "Compare chains within the same city and component/outcome"),
    component     = c("Calibration likelihood component (compute.piecewise); TOTAL = sum of all components. A '.1' suffix = a second component with the same name, in the order compute.piecewise returns them",
                      "Same components as the likelihood diagnostics. TOTAL is what the sampler compares"),
    n.sims        = c(paste0("Simulations of this chain used (evenly spaced along the chain, out of ", N.SIM / 4, ")"),
                      "Set by N.LIK.PER.CHAIN"),
    mean.loglik   = c("Mean log-likelihood of this component over the chain's simulations",
                      "Higher (less negative) = better fit. Raw values cannot be compared between components: compare chains within a component"),
    sd.loglik     = c("SD of the log-likelihood of this component within the chain",
                      "How much the fit varies inside one chain. Used as the yardstick in gap.in.sd"),
    best.chain    = c("Chain with the highest mean.loglik for this component", "-"),
    gap.from.best = c("mean.loglik of this chain - mean.loglik of the best chain (log-likelihood units)",
                      "0 = best chain; negative = worse fit. On the TOTAL row this matches the log-likelihood gap in the mixing report"),
    gap.in.sd     = c("gap.from.best / pooled within-chain SD of this component",
                      "Comparable across components. About 0 to -1: within the usual variation of a chain. Below about -2: this chain fits this target clearly worse"),
    outcome       = c("Diagnoses outcome (model name). Data: ps = P&S, total, el = early latent, late = late/unknown duration",
                      "Data by sex: P&S 1993-2023; total, EL, late 2019-2023 only"),
    sex           = c("men = msm + heterosexual_male in the model, all men in the data; women; men.to.women = (model men / model women) / (data men / data women)",
                      "men.to.women < 1: the model's men are too low relative to women"),
    period        = c("Years summed for the ratio (only years with data for both sexes)",
                      "2022-2023 is after the calibration period: an out-of-sample check"),
    calibration.period = c("TRUE if the period is inside the calibration period", "-"),
    model.to.data = c("Model diagnoses (median over the chain's simulations, summed over the period) / observed diagnoses",
                      "1 = right; > 1 model too high; < 1 model too low. Compare chains: a chain far from 1 where the others are close fits that target worse"),
    total.gap.from.best = c("TOTAL log-likelihood gap of this chain from the best chain (see the likelihood file)",
                            "0 = best-fitting chain; more negative = worse fit"),
    total.gap.in.sd     = c("total.gap.from.best / pooled within-chain SD of the TOTAL log-likelihood",
                            "Below about -2: this chain fits clearly worse overall"),
    worst.components    = c("The 3 components where this chain is furthest below the best chain (gap.in.sd in brackets)",
                            "Which targets this chain fits worse"),
    doxy.median.msm     = c(paste0("Median ", DOXY.OUTCOME, " for MSM in this chain at ", CCRIT, " (from 01_doxy_impact_by_chain.R)"),
                            "Negative = fewer infections with doxy-PEP. Compare with the fit columns: do worse-fitting chains give a different effect?"),
    doxy.median.women   = c(paste0("Median ", DOXY.OUTCOME, " for women in this chain at ", CCRIT, " (from 01_doxy_impact_by_chain.R)"),
                            "As doxy.median.msm"),
    doxy.pooled.median.msm   = c("Pooled median of the MSM outcome, all chains", "The value the manuscript reports"),
    doxy.pooled.median.women = c("Pooled median of the women outcome, all chains", "The value the manuscript reports")
)
column.notes <- function(cols) {
    notes <- lapply(cols, function(col) if (!is.null(COLUMN.NOTES[[col]])) COLUMN.NOTES[[col]] else c("", ""))
    rows  <- as.data.frame(do.call(cbind, notes), stringsAsFactors = FALSE)
    names(rows) <- cols
    rows[[1]] <- paste0(c("WHAT IT IS: ", "HOW TO READ: "), rows[[1]])
    rows
}
write.annotated.csv <- function(df, file) {
    con <- file(file, open = "w")
    on.exit(close(con))
    write.table(column.notes(names(df)), con, sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double")
    write.table(df, con, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
}


## HELPERS ----
# Model diagnoses by year x sex group (men, women) x sim, from the simset
model.by.sex.sims <- function(simset, sim.outcome, years) {
    x <- simset$get(sim.outcome, year = years, keep.dimensions = c("year", "sex"))
    x <- aperm(x, c("year", "sex", setdiff(names(dimnames(x)), c("year", "sex"))))
    d <- dim(x)
    x <- array(x, dim = c(d[1], d[2], prod(d[-(1:2)])), dimnames = c(dimnames(x)[1:2], list(sim = NULL)))
    out <- array(NA_real_, dim = c(d[1], 2, dim(x)[3]),
                 dimnames = list(year = dimnames(x)$year, sex = c("male", "female"), sim = NULL))
    out[, "male", ]   <- x[, "msm", ] + x[, "heterosexual_male", ]
    out[, "female", ] <- x[, "female", ]
    out
}

# Model / data by period for men, women and men:women, from model (year x male/female) and obs (same)
ratios.by.period <- function(model, obs) {
    t(sapply(PERIODS, function(p) {
        y  <- intersect(as.character(p), rownames(obs))
        ok <- y[!is.na(obs[y, "male"]) & !is.na(obs[y, "female"])]
        if (length(ok) == 0) return(c(men = NA, women = NA, men.to.women = NA))
        m <- colSums(model[ok, , drop = FALSE]); o <- colSums(obs[ok, , drop = FALSE])
        c(men          = m[["male"]] / o[["male"]],
          women        = m[["female"]] / o[["female"]],
          men.to.women = (m[["male"]] / m[["female"]]) / (o[["male"]] / o[["female"]]))
    }))
}

# Latest chain summary from 01_doxy_impact_by_chain.R (handles files with or without the two description rows)
read.doxy.summary <- function() {
    f <- list.files(DOXY.DIR, pattern = "^doxy_chain_summary_.*\\.csv$", full.names = TRUE)
    f <- f[!grepl("_backup\\.csv$", f)]
    if (length(f) == 0) { message("No doxy_chain_summary file in ", DOXY.DIR, ": step 3 runs without the doxy-PEP columns"); return(NULL) }
    f <- f[which.max(file.mtime(f))]
    skip <- if (grepl("^\"?WHAT IT IS", readLines(f, n = 1))) 2 else 0
    message("Doxy-PEP effects from ", basename(f))
    read.csv(f, skip = skip, stringsAsFactors = FALSE)
}


## RUN, ONE CITY AT A TIME ----
check.session(LOCS[[1]])
lik.rows <- list(); ratio.rows <- list(); plot.rows <- list(); obs.rows <- list()

for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("\n==", city, "(", loc, ") ==\n")
    simset <- retrieve.simulation.set(version = "shield", location = loc,
                                      calibration.code = CALIBRATION.CODE, n.sim = N.SIM)
    chains <- as.integer(factor(simset$simulation.chain))
    cat("  sims per chain:", paste(table(chains), collapse = " / "), "\n")
    
    ## 1. Likelihood by chain ----
    lik <- .get.calibration.likelihood(loc, CALIBRATION.CODE)
    t0  <- Sys.time()
    for (k in sort(unique(chains))) {
        idx <- which(chains == k)
        idx <- idx[unique(round(seq(1, length(idx), length.out = min(N.LIK.PER.CHAIN, length(idx)))))]
        pieces <- t(sapply(idx, function(i) lik$compute.piecewise(simset$subset(i))))
        # Some likelihoods have two components with the same name (e.g. diagnosis.ps twice): keep them
        # apart as diagnosis.ps and diagnosis.ps.1, in the order compute.piecewise returns them
        colnames(pieces) <- make.unique(colnames(pieces))
        pieces <- cbind(pieces, TOTAL = rowSums(pieces))
        lik.rows[[length(lik.rows) + 1]] <- data.frame(
            city = city, component = colnames(pieces), chain = k, n.sims = nrow(pieces),
            mean.loglik = colMeans(pieces), sd.loglik = apply(pieces, 2, sd), stringsAsFactors = FALSE)
    }
    cat("  likelihood:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
    
    ## 2. Model / data by chain ----
    for (out in names(FIT.OUTCOMES)) {
        obs <- tryCatch(observed.by.sex(FIT.OUTCOMES[[out]], loc, years = YEARS), error = function(e) NULL)
        if (is.null(obs)) next
        sims <- model.by.sex.sims(simset, out, YEARS)
        for (k in sort(unique(chains))) {
            med <- apply(sims[, , chains == k, drop = FALSE], c(1, 2), median)
            r   <- ratios.by.period(med, obs)
            for (s in colnames(r))
                ratio.rows[[length(ratio.rows) + 1]] <- data.frame(
                    city = city, outcome = out, sex = s, period = rownames(r),
                    calibration.period = rownames(r) %in% CALIBRATION.PERIODS,
                    chain = k, model.to.data = r[, s], stringsAsFactors = FALSE)
        }
    }
    
    ## 4. Plot data: P&S by sex over time ----
    sims <- model.by.sex.sims(simset, "diagnosis.ps", PLOT.YEARS)
    for (k in sort(unique(chains))) {
        med <- apply(sims[, , chains == k, drop = FALSE], c(1, 2), median)
        plot.rows[[length(plot.rows) + 1]] <- data.frame(
            city = city, chain = factor(k), year = as.numeric(rownames(med)),
            sex = rep(c("men", "women"), each = nrow(med)), value = c(med[, "male"], med[, "female"]))
    }
    obs <- observed.by.sex("ps.syphilis.diagnoses", loc, years = PLOT.YEARS)
    if (!is.null(obs))
        obs.rows[[length(obs.rows) + 1]] <- data.frame(
            city = city, year = as.numeric(rownames(obs)),
            sex = rep(c("men", "women"), each = nrow(obs)), value = c(obs[, "male"], obs[, "female"]))
    
    rm(simset, sims); invisible(gc())
}


## 1. LIKELIHOOD: gaps from the best chain ----
lik.tab <- do.call(rbind, lik.rows)
key     <- paste(lik.tab$city, lik.tab$component)
best    <- tapply(lik.tab$mean.loglik, key, max)
pool.sd <- tapply(lik.tab$sd.loglik, key, function(s) sqrt(mean(s^2)))
lik.tab$best.chain    <- sapply(seq_len(nrow(lik.tab)), function(i) {
    sub <- lik.tab[key == key[i], ]; sub$chain[which.max(sub$mean.loglik)] })
lik.tab$gap.from.best <- lik.tab$mean.loglik - best[key]
lik.tab$gap.in.sd     <- ifelse(pool.sd[key] > 0, lik.tab$gap.from.best / pool.sd[key], NA_real_)
lik.tab <- lik.tab[order(lik.tab$city, lik.tab$component != "TOTAL", lik.tab$component, lik.tab$chain), ]
rownames(lik.tab) <- NULL

## 2. MODEL / DATA ----
ratio.tab <- do.call(rbind, ratio.rows)
ratio.tab <- ratio.tab[!is.na(ratio.tab$model.to.data), ]   # periods with no data by sex (e.g. total/EL/late before 2019)
rownames(ratio.tab) <- NULL

## 3. FIT VS DOXY-PEP EFFECT ----
tot  <- subset(lik.tab, component == "TOTAL")
comp <- subset(lik.tab, component != "TOTAL")
vs.doxy <- data.frame(city = tot$city, chain = tot$chain,
                      total.gap.from.best = tot$gap.from.best, total.gap.in.sd = tot$gap.in.sd,
                      stringsAsFactors = FALSE)
vs.doxy$worst.components <- mapply(function(cty, k) {
    s <- comp[comp$city == cty & comp$chain == k, ]
    s <- s[order(s$gap.in.sd), ][1:min(3, nrow(s)), ]
    s <- s[!is.na(s$gap.in.sd) & s$gap.in.sd < 0, ]
    if (nrow(s) == 0) "none (best on every component)" else paste0(s$component, " (", round(s$gap.in.sd, 1), ")", collapse = "; ")
}, vs.doxy$city, vs.doxy$chain)
doxy <- read.doxy.summary()
if (!is.null(doxy)) {
    for (pop in c("MSM", "Women")) {
        d <- doxy[doxy$coverage == CCRIT & doxy$outcome == DOXY.OUTCOME & doxy$population == pop, ]
        if (nrow(d) == 0) { message("No ", pop, " rows in the doxy summary: re-run 01_doxy_impact_by_chain.R"); next }
        vs.doxy[[paste0("doxy.median.", tolower(pop))]] <- mapply(function(cty, k) {
            v <- d[d$city == cty, paste0("median.chain", k)]; if (length(v) == 1) v else NA_real_ }, vs.doxy$city, vs.doxy$chain)
        vs.doxy[[paste0("doxy.pooled.median.", tolower(pop))]] <- d$median[match(vs.doxy$city, d$city)]
    }
}


## SAVE ----
f.lik   <- file.path(FIT.DIR, paste0("fit_by_chain_likelihood_",    CALIBRATION.CODE, "_", STAMP, ".csv"))
f.ratio <- file.path(FIT.DIR, paste0("fit_by_chain_model_vs_data_", CALIBRATION.CODE, "_", STAMP, ".csv"))
f.doxy  <- file.path(FIT.DIR, paste0("fit_by_chain_vs_doxy_",       CALIBRATION.CODE, "_", STAMP, ".csv"))
write.annotated.csv(lik.tab,   f.lik)
write.annotated.csv(ratio.tab, f.ratio)
write.annotated.csv(vs.doxy,   f.doxy)

plot.tab <- do.call(rbind, plot.rows)
obs.tab  <- if (length(obs.rows)) do.call(rbind, obs.rows) else NULL
p <- ggplot(plot.tab, aes(x = year, y = value, colour = chain)) +
    geom_line() +
    { if (!is.null(obs.tab)) geom_point(data = obs.tab, aes(x = year, y = value), inherit.aes = FALSE, size = 0.9) } +
    facet_grid(sex ~ city, scales = "free_y") +
    labs(title = paste("P&S syphilis diagnoses by chain:", CALIBRATION.CODE),
         subtitle = "Lines = median of each chain's simulations; points = surveillance data (men = all men, MSM included)",
         x = NULL, y = "P&S diagnoses", colour = "Chain") +
    theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(FIT.DIR, paste0("fit_by_chain_ps_by_sex_", CALIBRATION.CODE, ".png")), p,
       width = 4 * length(LOCS) + 1, height = 6, dpi = 150)


## PRINT ----
cat("\n== Fit vs doxy-PEP effect, by chain ==\n")
print(vs.doxy, row.names = FALSE, digits = 3)
cat("\n== Components where a chain fits clearly worse (gap.in.sd < -2) ==\n")
print(subset(lik.tab, component != "TOTAL" & !is.na(gap.in.sd) & gap.in.sd < -2,
             c("city", "component", "chain", "mean.loglik", "gap.from.best", "gap.in.sd")), row.names = FALSE, digits = 3)
cat("\nSaved in ", FIT.DIR, ":\n ", basename(f.lik), "\n ", basename(f.ratio), "\n ", basename(f.doxy),
    "\n  fit_by_chain_ps_by_sex_", CALIBRATION.CODE, ".png\n", sep = "")