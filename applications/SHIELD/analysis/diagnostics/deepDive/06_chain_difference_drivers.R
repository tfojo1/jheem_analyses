# ****************************************************************************************************
# DEEP DIVE 06: WHAT DRIVES THE DIFFERENCE BETWEEN CHAINS IN THE DOXY-PEP EFFECT? ----
# ****************************************************************************************************
# In some cities the four chains project different reductions in incidence among women (01: chain.gap.flag).
# This script attributes the gap between each pair of chains to parameters, using the existing posterior
# simulations only (no new runs).
#
# WHAT IT DOES, for each flagged city x population (Women; MSM as a contrast) x coverage:
#   1. PREDICTORS: every calibrated parameter on its sampling scale (log for rates and multipliers, logit for
#      proportions), except that the transmission groups the data cannot separate (script 05) are replaced by
#      the combinations they can:
#        level.msm, level.het      log global rate + mean log of its time, race and age multipliers
#        shape.<sex>.<knot>        each time multiplier relative to its group
#        shape.<sex>.age<a>        each age multiplier relative to its group
#        shape.race.<race>         each race multiplier relative to the other races
#      Parameters that do not vary across the simulations are dropped.
#   2. SENSITIVITY: ridge regression of the effect on the predictors using only the variation INSIDE each
#      chain (outcome and predictors centred on their chain means). Predictors are scaled by their SD over all
#      simulations; the penalty is chosen by generalized cross-validation. Inside-chain variation is not
#      confounded by the differences between chains.
#   3. ATTRIBUTION, for each pair of chains A and B:
#        contribution of predictor j = sensitivity_j x (mean of j in chain A - mean of j in chain B)
#        explained gap = sum of contributions; compared with the observed gap (mean effect A - mean effect B)
#      Contributions are also summed by group (bridging, transmission level, time shape, ...).
#   4. FIGURE: contributions by group for the widest pair of chains, one panel per city x coverage.
#
# HOW TO READ IT
#   share.explained close to 1: the parameters (as combined here) account for the gap; the largest
#     contributions show which ones. Contributions can be negative (they work against the gap).
#   share.explained far from 1: the gap comes from something the linear, inside-chain model does not capture
#     (non-linear effects, or parameters that barely vary inside a chain); treat the attribution with caution.
#   within.r2: how much of the variation inside the chains the predictors explain.
#   Each chain has an effective sample size of only a few draws for many parameters, so sensitivities for
#   parameters that hardly move inside a chain are uncertain. A simulation swap experiment (replace a parameter
#   in chain A's simulations with chain B's value and re-run) would confirm the top drivers.
#
# MEMORY: loads one 1,000-sim simset per city (for the parameter values), one at a time. No simulation is run.
#
# USAGE (from jheem_analyses)
#   1. run deepDive/01_doxy_impact_by_chain.R first (it writes the per-simulation effects and the flag)
#   2. source("applications/SHIELD/analysis/diagnostics/deepDive/06_chain_difference_drivers.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   chain_drivers_summary_<code>_<date>.csv         city x population x coverage x chain pair: observed vs explained gap
#   chain_drivers_groups_<code>_<date>.csv          ... x group: contribution of each group
#   chain_drivers_contributions_<code>_<date>.csv   ... x predictor: sensitivity, chain means, contribution
#   chain_drivers_widest_pair_<code>_<date>.png     group contributions for the widest pair of chains
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
library(ggplot2)


## SETTINGS ----
CITIES      <- NULL                              # NULL = cities flagged for women in 01 (chain.gap.flag); or e.g. c("LA")
POPULATIONS <- c("Women", "MSM")                 # MSM is the contrast
COVERAGES   <- paste0("doxy.cov.", c(10, 20, 30, 50))
OUTCOME     <- "pct_change_incidence_vs_noint"
TOP.N       <- 10                                # predictors listed per chain pair in the summary file
LAMBDAS     <- 10^seq(-3, 3, length.out = 61)    # ridge penalties tried (on the scaled predictors)

# Transmission structure (as in shield_specification.R / script 05)
KNOTS <- c("1970", "1990", "1995", "2000", "2010", "2022")
RACES <- c("black", "hispanic", "other")
AGES  <- c("14", "19", "24", "29", "34", "39", "44", "49", "54", "64", "65")
TRANSMISSION <- list(
    msm = list(global = "global.transmission.rate.msm",
               time = paste0("transmission.rate.multiplier.msm", KNOTS),
               age  = paste0("transmission.rate.multiplier.age", AGES, ".msm")),
    het = list(global = "global.transmission.rate.het",
               time = paste0("transmission.rate.multiplier.heterosexual", KNOTS),
               age  = paste0("transmission.rate.multiplier.age", AGES, ".heterosexual")))
RACE.MULTS <- paste0("transmission.rate.multiplier.", RACES)   # shared by MSM and het

# Groups for summing contributions: first matching pattern wins (applied to predictor names)
PREDICTOR.GROUPS <- c(
    "transmission level"       = "^level\\.",
    "transmission time shape"  = "^shape\\.(msm|het)\\.[0-9]",
    "transmission age shape"   = "^shape\\.(msm|het)\\.age",
    "transmission race shape"  = "^shape\\.race\\.",
    "future trend"             = "future",
    "bridging (MSM-female)"    = "pairings",
    "race mixing"              = "sexual\\.multi",
    "age mixing"               = "age\\.mixing",
    "natural history"          = "el\\.rel|latent|secondary\\.transmiss|relapse|infectious",
    "symptoms / care-seeking"  = "symptomatic|careseeking",
    "screening / testing"      = "screening|testing|syphilis\\.to\\.hiv",
    "MSM population"           = "proportion\\.msm",
    "demography"               = "fertility|mortality|aging|immigration|emigration",
    "other"                    = ".")


## FOLDERS ----
DEEP.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE, "deepDive")
dir.create(DEEP.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## COLUMN DESCRIPTIONS ----
# First two rows of each CSV: c(what it is, how to read it). Edit the text here, not in the CSVs.
COLUMN.NOTES <- list(
    city        = c("MSA name", "One city per block of rows"),
    population  = c("Whose doxy-PEP effect: Women or MSM", "MSM is included as a contrast"),
    coverage    = c("Doxy-PEP coverage scenario", "doxy.cov.20 = 20% coverage"),
    chain.a     = c("First chain of the pair", "-"),
    chain.b     = c("Second chain of the pair", "-"),
    widest.pair = c("TRUE for the pair of chains with the largest observed gap in this city x population x coverage",
                    "The pair shown in the figure"),
    observed.gap = c(paste0("Mean ", OUTCOME, " in chain A - mean in chain B (2030)"),
                     "Percentage points. Negative = chain A projects a larger reduction"),
    explained.gap = c("Sum of all predictor contributions", "Compare with observed.gap"),
    share.explained = c("explained.gap / observed.gap",
                        "Close to 1: the predictors account for the gap. Far from 1 (or negative): the attribution is unreliable for this pair"),
    within.r2   = c("Share of the variation inside the chains explained by the predictors (ridge fit)",
                    "Low values: the effect varies inside a chain for reasons the predictors do not capture"),
    lambda      = c("Ridge penalty chosen by generalized cross-validation (scaled predictors)", "-"),
    top.predictors = c("Predictors with the largest absolute contributions (points, in brackets)", "Where to look first"),
    top.groups  = c("Groups with the largest absolute contributions (points, in brackets)", "-"),
    group       = c("Group of predictors (see PREDICTOR.GROUPS in the script)", "-"),
    contribution = c("Sensitivity x (mean in chain A - mean in chain B), summed over the predictors in the row",
                     "Percentage points of the observed gap attributed to this predictor or group; the sign says whether it widens (+, same sign as observed.gap) or narrows the gap"),
    share.of.gap = c("contribution / observed.gap", "1 = this predictor or group alone accounts for the gap"),
    predictor   = c("Parameter (sampling scale) or identified combination (level.*, shape.*)", "-"),
    sensitivity = c("Change in the effect (percentage points) per unit change of the predictor on its sampling scale, estimated inside chains",
                    "Units: per 1 unit of log (rates, multipliers) or logit (proportions)"),
    mean.a      = c("Mean of the predictor in chain A (sampling scale)", "-"),
    mean.b      = c("Mean of the predictor in chain B (sampling scale)", "-"),
    sd.all      = c("SD of the predictor over all simulations (sampling scale)",
                    "Compare |mean.a - mean.b| with sd.all: how far apart the chains sit on this predictor")
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
# Latest file from 01_doxy_impact_by_chain.R (with or without the two description rows)
read.latest <- function(kind) {
    f <- list.files(DEEP.DIR, pattern = paste0("^doxy_chain_", kind, "_.*\\.csv$"), full.names = TRUE)
    f <- f[!grepl("_backup\\.csv$", f)]
    if (length(f) == 0) stop("No doxy_chain_", kind, " file in ", DEEP.DIR, ". Run 01_doxy_impact_by_chain.R first.")
    f <- f[which.max(file.mtime(f))]
    skip <- if (grepl("^\"?WHAT IT IS", readLines(f, n = 1))) 2 else 0
    cat("Reading", basename(f), "\n")
    read.csv(f, skip = skip, stringsAsFactors = FALSE)
}

# Parameter on its sampling scale: logit for proportions, log for positive rates and multipliers
to.sampling.scale <- function(x, name) {
    if (all(x > 0 & x < 1) && grepl("^(fraction|prop|prp)", name)) return(qlogis(x))
    if (all(x > 0)) return(log(x))
    x
}

# Predictor matrix (sim x predictor): parameters on the sampling scale, with the transmission groups replaced
# by their identified combinations (levels and shapes)
build.predictors <- function(P) {                      # P = sim x parameter, natural scale
    Z <- sapply(colnames(P), function(v) to.sampling.scale(P[, v], v))
    Z <- matrix(Z, nrow = nrow(P), dimnames = list(NULL, colnames(P)))
    has <- function(v) v[v %in% colnames(Z)]
    gmean <- function(v) if (length(v)) rowMeans(Z[, v, drop = FALSE]) else 0
    out <- list(); used <- character(0)
    race <- has(RACE.MULTS)
    for (sx in names(TRANSMISSION)) {
        g <- lapply(TRANSMISSION[[sx]], has)
        if (length(g$global) == 0) next
        out[[paste0("level.", sx)]] <- Z[, g$global] + gmean(g$time) + gmean(race) + gmean(g$age)
        for (v in g$time) out[[paste0("shape.", sx, ".", sub(".*multiplier\\.(msm|heterosexual)", "", v))]] <- Z[, v] - gmean(g$time)
        for (v in g$age)  out[[paste0("shape.", sx, ".age", sub("\\.(msm|heterosexual)$", "", sub(".*multiplier\\.age", "", v)))]] <- Z[, v] - gmean(g$age)
        used <- c(used, g$global, g$time, g$age)
    }
    for (v in race) out[[paste0("shape.race.", sub(".*multiplier\\.", "", v))]] <- Z[, v] - gmean(race)
    used <- c(used, race)
    X <- cbind(do.call(cbind, out), Z[, setdiff(colnames(Z), used), drop = FALSE])
    X[, apply(X, 2, function(x) sd(x) > 1e-10), drop = FALSE]   # drop parameters that do not vary
}

predictor.group <- function(names) sapply(names, function(n) names(PREDICTOR.GROUPS)[which(sapply(PREDICTOR.GROUPS, grepl, x = n))[1]])

# Ridge regression on within-chain-centred data, penalty by generalized cross-validation.
# Returns sensitivities on the predictors' own (sampling) scale.
within.chain.ridge <- function(y, X, chains) {
    yc <- y - ave(y, chains)
    Xc <- apply(X, 2, function(x) x - ave(x, chains))
    s  <- apply(X, 2, sd)                                # scale by the SD over all simulations
    Xs <- sweep(Xc, 2, s, "/")
    sv <- svd(Xs); d <- sv$d; Uy <- crossprod(sv$u, yc)
    n  <- length(yc) - length(unique(chains))            # residual df after the chain means
    gcv <- sapply(LAMBDAS, function(l) {
        f <- d^2 / (d^2 + l); fit <- sv$u %*% (f * Uy)
        sum((yc - fit)^2) / n / (1 - sum(f) / n)^2
    })
    l  <- LAMBDAS[which.min(gcv)]
    b  <- sv$v %*% ((d / (d^2 + l)) * Uy)
    fit <- Xs %*% b
    list(beta = setNames(as.vector(b) / s, colnames(X)), lambda = l,
         within.r2 = 1 - sum((yc - fit)^2) / sum(yc^2))
}


## INPUTS ----
VALS <- read.latest("values")
VALS <- VALS[VALS$outcome == OUTCOME & VALS$coverage %in% COVERAGES & VALS$population %in% POPULATIONS, ]
if (is.null(CITIES)) {
    S <- read.latest("summary")
    if (!"chain.gap.flag" %in% names(S)) stop("The 01 summary has no chain.gap.flag column: re-run 01_doxy_impact_by_chain.R")
    CITIES <- unique(S$city[S$population == "Women" & S$outcome == OUTCOME & S$coverage %in% COVERAGES & S$chain.gap.flag %in% TRUE])
    cat("Cities flagged for women in 01:", paste(CITIES, collapse = ", "), "\n")
}
LOCS <- SHIELD.TEN.MSAS[CITIES]
if (anyNA(LOCS)) stop("Unknown city: ", paste(CITIES[is.na(LOCS)], collapse = ", "))


## RUN, ONE CITY AT A TIME ----
sum.rows <- list(); grp.rows <- list(); con.rows <- list()
for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("==", city, "\n")
    simset <- retrieve.simulation.set(version = "shield", location = loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIM)
    P      <- t(simset$parameters)                       # sim x parameter
    chains <- as.integer(factor(simset$simulation.chain))
    rm(simset); invisible(gc())
    X <- build.predictors(P)
    cat("  ", ncol(X), "predictors\n")
    grp <- predictor.group(colnames(X))
    chain.means <- apply(X, 2, function(x) tapply(x, chains, mean))   # chain x predictor
    sd.all <- apply(X, 2, sd)

    for (pop in POPULATIONS) for (cov in COVERAGES) {
        d <- VALS[VALS$city == city & VALS$population == pop & VALS$coverage == cov, ]
        if (nrow(d) == 0) next
        if (nrow(d) != nrow(X) || !identical(as.integer(d$chain), chains))
            stop(city, " ", pop, " ", cov, ": the effects file and the simset do not line up (number of sims or chain order)")
        ok  <- !is.na(d$value)
        fit <- within.chain.ridge(d$value[ok], X[ok, , drop = FALSE], chains[ok])
        ym  <- tapply(d$value[ok], chains[ok], mean)
        pairs <- combn(sort(unique(chains)), 2, simplify = FALSE)
        gaps  <- sapply(pairs, function(p) ym[[as.character(p[1])]] - ym[[as.character(p[2])]])
        widest <- which.max(abs(gaps))
        for (i in seq_along(pairs)) {
            a <- pairs[[i]][1]; b <- pairs[[i]][2]
            diffs  <- chain.means[as.character(a), ] - chain.means[as.character(b), ]
            contr  <- fit$beta * diffs
            obs    <- gaps[i]
            top    <- order(-abs(contr))[seq_len(min(TOP.N, length(contr)))]
            gsum   <- tapply(contr, grp, sum)
            gtop   <- order(-abs(gsum))[seq_len(min(5, length(gsum)))]
            key    <- data.frame(city = city, population = pop, coverage = cov, chain.a = a, chain.b = b,
                                 widest.pair = i == widest, stringsAsFactors = FALSE)
            sum.rows[[length(sum.rows) + 1]] <- cbind(key, data.frame(
                observed.gap = obs, explained.gap = sum(contr), share.explained = sum(contr) / obs,
                within.r2 = fit$within.r2, lambda = fit$lambda,
                top.predictors = paste0(names(contr)[top], " (", sprintf("%+.1f", contr[top]), ")", collapse = "; "),
                top.groups     = paste0(names(gsum)[gtop], " (", sprintf("%+.1f", gsum[gtop]), ")", collapse = "; "),
                stringsAsFactors = FALSE))
            grp.rows[[length(grp.rows) + 1]] <- cbind(key[rep(1, length(gsum)), ], data.frame(
                group = names(gsum), contribution = as.vector(gsum), share.of.gap = as.vector(gsum) / obs,
                observed.gap = obs, stringsAsFactors = FALSE))
            con.rows[[length(con.rows) + 1]] <- cbind(key[rep(1, length(contr)), ], data.frame(
                predictor = names(contr), group = grp, sensitivity = fit$beta,
                mean.a = chain.means[as.character(a), ], mean.b = chain.means[as.character(b), ], sd.all = sd.all,
                contribution = contr, share.of.gap = contr / obs, stringsAsFactors = FALSE))
        }
    }
}


## SAVE ----
f <- function(what, ext = "csv") file.path(DEEP.DIR, paste0("chain_drivers_", what, "_", CALIBRATION.CODE, "_", STAMP, ".", ext))
sum.tab <- do.call(rbind, sum.rows); rownames(sum.tab) <- NULL
grp.tab <- do.call(rbind, grp.rows); rownames(grp.tab) <- NULL
con.tab <- do.call(rbind, con.rows); rownames(con.tab) <- NULL
con.tab <- con.tab[order(con.tab$city, con.tab$population, con.tab$coverage, con.tab$chain.a, con.tab$chain.b, -abs(con.tab$contribution)), ]
write.annotated.csv(sum.tab, f("summary"))
write.annotated.csv(grp.tab[order(grp.tab$city, grp.tab$population, grp.tab$coverage, grp.tab$chain.a, grp.tab$chain.b,
                                  -abs(grp.tab$contribution)), ], f("groups"))
write.annotated.csv(con.tab, f("contributions"))

pd <- grp.tab[grp.tab$widest.pair & grp.tab$population == "Women", ]
if (nrow(pd)) {
    pd$label <- paste0(pd$city, " (chains ", pd$chain.a, " vs ", pd$chain.b, ")")
    pd$coverage <- factor(pd$coverage, levels = COVERAGES)
    obs <- unique(pd[, c("label", "coverage", "observed.gap")])
    p <- ggplot(pd, aes(x = contribution, y = reorder(group, abs(contribution)))) +
        geom_col(aes(fill = contribution * sign(observed.gap) > 0), width = 0.7) +
        geom_vline(xintercept = 0, linewidth = 0.3) +
        geom_text(data = obs, aes(x = Inf, y = -Inf, label = sprintf("observed gap %.1f", observed.gap)),
                  inherit.aes = FALSE, hjust = 1.05, vjust = -0.5, size = 2.4) +
        facet_grid(label ~ coverage) +
        scale_fill_manual(values = c(`TRUE` = "#d95f02", `FALSE` = "#7f7f7f"),
                          labels = c(`TRUE` = "widens the gap", `FALSE` = "narrows the gap"), name = NULL) +
        labs(title = paste0("What drives the gap between chains in the doxy-PEP effect among women (", CALIBRATION.CODE, ")"),
             subtitle = paste0(OUTCOME, ", 2030. Widest pair of chains per city and coverage. ",
                               "Contribution = within-chain sensitivity x difference between the chain means."),
             x = "Contribution to the gap (percentage points)", y = NULL) +
        theme_bw(base_size = 8) + theme(legend.position = "bottom", strip.text.y = element_text(size = 6.5))
    ggsave(f("widest_pair", "png"), p, width = 13, height = 2.2 * length(unique(pd$label)) + 1.2, dpi = 200)
}


## PRINT ----
cat("\n== Widest pair of chains: observed vs explained gap, and top groups ==\n")
w <- sum.tab[sum.tab$widest.pair, ]
print(w[order(w$population, w$city, w$coverage), c("city", "population", "coverage", "chain.a", "chain.b",
        "observed.gap", "explained.gap", "share.explained", "within.r2", "top.groups")], row.names = FALSE, digits = 3)
cat("\nSaved in ", DEEP.DIR, "\n", sep = "")
