# ****************************************************************************************************
# DEEP DIVE: DOXY-PEP EFFECT IN WOMEN VS PARAMETERS, BY CHAIN ----
# ****************************************************************************************************
# Why does the doxy-PEP effect on women (and on the total population) depend on the MCMC chain, when
# the effect on MSM does not? This script plots each simulation's effect against selected parameters,
# coloured by chain, and ranks every parameter by how closely the effect follows it.
#
# What it does, for each city:
#   1. reads the per-simulation doxy-PEP effects from 01_doxy_impact_by_chain.R (run with USE.SEX = TRUE,
#      so the file has MSM and Women rows)
#   2. gets each simulation's parameters, either as ranks (PARAM.SOURCE = "ranks", from sorted_arrs.Rdata
#      made by analysis/sensitivity/sensitivity_step2_sort_sims.R; no simset is loaded) or as values
#      (PARAM.SOURCE = "values", from simset$parameters; loads one simset per city)
#   3. Spearman correlation of the effect with each parameter, two ways:
#        all sims      - all 1,000 together. The chains do not mix, so this mostly compares 4 chain
#                        clusters: ANY parameter whose chain averages line up with the chain effects
#                        looks correlated. Use it to see what separates the chains, not what causes it.
#        within chain  - the mean of the 4 correlations computed inside each chain. Variation inside a
#                        chain is local, so this is the cleaner test of whether the effect follows a parameter.
#   4. how much of the variation in the effect is between chains, before and after allowing for the
#      3 parameters most correlated with it (natural splines, 3 df each)
#   5. figure: one column per city; rows = the effect / parameter pairs in PANELS
#
# Spearman uses ranks, so "ranks" and "values" give the same correlations. "values" is only needed
# if you want the x axis on the parameter's own scale.
#
# MEMORY
#   "ranks": reads sorted_arrs.Rdata (~3 MB) and the per-simulation CSV (~20 MB). Light.
#   "values": also loads one 1,000-sim simset per city, one at a time.
#   No simulation is run.
#
# USAGE (from jheem_analyses)
#   1. CALIBRATION.CODE is set in diagnostics_setup.R (calib.10.20.stage4)
#   2. run deepDive/01_doxy_impact_by_chain.R first, with USE.SEX = TRUE
#   3. source("applications/SHIELD/analysis/diagnostics/deepDive/03_doxy_women_effect_vs_parameters.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   women_effect_vs_parameters_<code>_<date>.png           the figure
#   women_effect_key_parameters_<code>_<date>.csv          city x parameter x population: correlations, chain mean ranks
#   women_effect_top_parameters_<code>_<date>.csv          city: the TOP.N parameters most correlated with the women effect
#   women_effect_chain_share_<code>_<date>.csv             city: variance due to chain, before / after the top 3 parameters
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
library(ggplot2)


## SETTINGS ----
CITIES       <- NULL                       # NULL = all 10; or e.g. c("Chicago", "Houston", "LA")
COVERAGE     <- "doxy.cov.20"
OUTCOME      <- "pct_change_incidence_vs_noint"
PARAM.SOURCE <- "ranks"                    # "ranks" (sorted_arrs.Rdata) or "values" (simset$parameters)
TOP.N        <- 8                          # parameters listed per city in the top-parameters file

# Parameters reported for every city (key-parameters file)
KEY.PARAMS <- c("oe.female.pairings.with.msm",
                "fraction.msm.pairings.with.female",
                "fraction.heterosexual_male.pairings.with.male",
                "global.transmission.rate.het",
                "transmission.rate.multiplier.heterosexual2010",
                "transmission.rate.multiplier.heterosexual2022",
                "global.transmission.rate.msm",
                "transmission.rate.multiplier.msm2022")

# Figure rows: which effect (Women or MSM) against which parameter
PANELS <- data.frame(
    population = c("Women", "Women", "MSM"),
    parameter  = c("oe.female.pairings.with.msm", "fraction.msm.pairings.with.female", "oe.female.pairings.with.msm"),
    stringsAsFactors = FALSE)


## FOLDERS ----
LOCS <- if (is.null(CITIES)) SHIELD.TEN.MSAS else SHIELD.TEN.MSAS[CITIES]
if (anyNA(LOCS)) stop("Unknown city in CITIES: ", paste(CITIES[is.na(LOCS)], collapse = ", "))
DIAG.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE)
DEEP.DIR <- file.path(DIAG.DIR, "deepDive")
dir.create(DEEP.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## COLUMN DESCRIPTIONS ----
# First two rows of each CSV: c(what it is, how to read it). Edit the text here, not in the CSVs.
COLUMN.NOTES <- list(
    city       = c("MSA name", "One city per block of rows"),
    parameter  = c("Calibrated parameter", "-"),
    population = c("Whose doxy-PEP effect: Women or MSM", "Women depend on spillover from MSM; MSM is the direct effect"),
    rho.all    = c(paste0("Spearman correlation of the effect (", OUTCOME, ", ", COVERAGE, ", 2030) with the parameter, all simulations together"),
                   "The chains do not mix, so this mostly compares 4 chain clusters: it shows what separates the chains, not what causes the effect. Negative = higher parameter, larger reduction"),
    rho.within = c("Mean of the 4 Spearman correlations computed inside each chain",
                   "The cleaner test. Consistent values of about |0.3| or more across cities = the effect follows this parameter. Near 0 = it does not, even if rho.all is large"),
    chain.mean.rank = c("Mean rank of the parameter in each chain (chains 1/2/3/4; 1 = lowest of the 1,000 simulations)",
                        "Chains far apart (e.g. 200 vs 850) sit in different parts of this parameter's range"),
    rank       = c("Position in this city's list, by |rho.all|", "1 = most correlated"),
    rho.women.all    = c("Spearman correlation of the women effect with the parameter, all simulations", "See rho.all"),
    rho.women.within = c("Mean within-chain Spearman correlation of the women effect with the parameter", "See rho.within"),
    rho.msm.all      = c("Spearman correlation of the MSM effect with the parameter, all simulations",
                         "Compare with rho.women.all: a parameter that moves the women effect but not the MSM effect acts through spillover"),
    women.chain.medians = c("Median women effect in each chain (chains 1/2/3/4)", "Negative = fewer infections with doxy-PEP"),
    r2.chain   = c("Share of the variance in the women effect explained by chain (R-squared of effect ~ chain)",
                   "How much the women effect depends on the chain. 0 = not at all"),
    r2.top3    = c("Share of the variance in the women effect explained by the 3 parameters most correlated with it (natural splines, 3 df each)",
                   "How much those 3 parameters explain, inside and between chains"),
    r2.chain.after.top3 = c("Share of the variance left after the 3 parameters that is explained by chain",
                            "Near 0 = the chain differences are carried by parameter differences. The 3 parameters are picked from all of them after the fact, so this does not show which one is causal"),
    top3.parameters = c("The 3 parameters used in r2.top3", "-")
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
# Latest per-simulation file from 01_doxy_impact_by_chain.R (with or without the two description rows)
read.latest.values <- function() {
    d <- DEEP.DIR
    f <- list.files(d, pattern = "^doxy_chain_values_.*\\.csv$", full.names = TRUE)
    f <- f[!grepl("_backup\\.csv$", f)]
    if (length(f) == 0) stop("No doxy_chain_values file in ", d, ". Run 01_doxy_impact_by_chain.R first.")
    f <- f[which.max(file.mtime(f))]
    skip <- if (grepl("^\"?WHAT IT IS", readLines(f, n = 1))) 2 else 0
    cat("Doxy-PEP effects from", basename(f), "\n")
    v <- read.csv(f, skip = skip, stringsAsFactors = FALSE)
    v <- v[v$coverage == COVERAGE & v$outcome == OUTCOME, ]
    if (!all(c("Women", "MSM") %in% v$population))
        stop("No Women/MSM rows in ", basename(f), ". Re-run 01_doxy_impact_by_chain.R with USE.SEX = TRUE.")
    v
}

# Parameters as sim x parameter, in the simset's simulation order
get.parameters <- function(loc) {
    if (PARAM.SOURCE == "ranks") {
        if (!exists("SORTED.ARRS")) SORTED.ARRS <<- get(load(file.path(get.jheem.root.directory(), "shield", "outputs",
                                                                        CALIBRATION.CODE, "sorted_arrs.Rdata")))
        s <- SORTED.ARRS[[loc]]                 # column p = sims ordered by parameter p (lowest first)
        r <- s
        for (j in seq_len(ncol(s))) r[s[, j], j] <- seq_len(nrow(s))
        return(list(x = r, chains = NULL))
    }
    simset <- retrieve.simulation.set(version = "shield", location = loc,
                                      calibration.code = CALIBRATION.CODE, n.sim = N.SIM)
    out <- list(x = t(simset$parameters), chains = as.integer(factor(simset$simulation.chain)))
    rm(simset); invisible(gc())
    out
}

spearman.within <- function(y, x, chains) mean(sapply(sort(unique(chains)), function(k)
    suppressWarnings(cor(y[chains == k], x[chains == k], method = "spearman"))), na.rm = TRUE)


## RUN, ONE CITY AT A TIME ----
V <- read.latest.values()
key.rows <- list(); top.rows <- list(); share.rows <- list(); plot.rows <- list()

for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("==", city, "\n")
    P <- get.parameters(loc)
    eff <- lapply(setNames(nm = c("Women", "MSM")), function(pop) V[V$city == city & V$population == pop, ])
    n <- nrow(P$x)
    if (any(sapply(eff, nrow) != n)) stop(city, ": the effects file and the parameters have different numbers of simulations")
    chains <- eff$Women$chain
    if (!is.null(P$chains) && !identical(as.integer(chains), P$chains))
        stop(city, ": chain labels in the effects file do not match the simset; the simulation order differs")
    y.w <- eff$Women$value; y.m <- eff$MSM$value

    # 1. key parameters
    for (pop in names(eff)) for (p in KEY.PARAMS) {
        x <- P$x[, p]; y <- eff[[pop]]$value
        key.rows[[length(key.rows) + 1]] <- data.frame(
            city = city, parameter = p, population = pop,
            rho.all = cor(y, x, method = "spearman"), rho.within = spearman.within(y, x, chains),
            chain.mean.rank = paste(round(tapply(rank(x), chains, mean)), collapse = "/"),
            stringsAsFactors = FALSE)
    }

    # 2. all parameters, ranked by |rho| with the women effect
    rho.w <- apply(P$x, 2, function(x) suppressWarnings(cor(y.w, x, method = "spearman")))
    rho.w <- rho.w[!is.na(rho.w)]
    top   <- names(sort(abs(rho.w), decreasing = TRUE))[seq_len(min(TOP.N, length(rho.w)))]
    top.rows[[city]] <- data.frame(
        city = city, rank = seq_along(top), parameter = top,
        rho.women.all    = rho.w[top],
        rho.women.within = sapply(top, function(p) spearman.within(y.w, P$x[, p], chains)),
        rho.msm.all      = sapply(top, function(p) cor(y.m, P$x[, p], method = "spearman")),
        stringsAsFactors = FALSE)

    # 3. variance due to chain, before / after the top 3 parameters
    ch  <- factor(chains)
    fit <- lm(y.w ~ splines::ns(P$x[, top[1]], 3) + splines::ns(P$x[, top[2]], 3) + splines::ns(P$x[, top[3]], 3))
    share.rows[[city]] <- data.frame(
        city = city,
        women.chain.medians = paste(round(tapply(y.w, ch, median), 1), collapse = "/"),
        r2.chain            = summary(lm(y.w ~ ch))$r.squared,
        r2.top3             = summary(fit)$r.squared,
        r2.chain.after.top3 = summary(lm(resid(fit) ~ ch))$r.squared,
        top3.parameters     = paste(top[1:3], collapse = "; "),
        stringsAsFactors = FALSE)

    # 4. figure data
    for (i in seq_len(nrow(PANELS))) {
        x <- P$x[, PANELS$parameter[i]]; y <- eff[[PANELS$population[i]]]$value
        plot.rows[[length(plot.rows) + 1]] <- data.frame(
            city = city, panel = paste0(PANELS$population[i], " effect vs\n", PANELS$parameter[i]),
            chain = factor(chains), x = x, y = y,
            label = sprintf("all %.2f | within %.2f", cor(y, x, method = "spearman"), spearman.within(y, x, chains)),
            stringsAsFactors = FALSE)
    }
}


## SAVE ----
key.tab   <- do.call(rbind, key.rows);   rownames(key.tab) <- NULL
top.tab   <- do.call(rbind, top.rows);   rownames(top.tab) <- NULL
share.tab <- do.call(rbind, share.rows); rownames(share.tab) <- NULL
f <- function(what, ext = "csv") file.path(DEEP.DIR, paste0("women_effect_", what, "_", CALIBRATION.CODE, "_", STAMP, ".", ext))
write.annotated.csv(key.tab,   f("key_parameters"))
write.annotated.csv(top.tab,   f("top_parameters"))
write.annotated.csv(share.tab, f("chain_share"))

pd <- do.call(rbind, plot.rows)
pd$city  <- factor(pd$city, levels = names(LOCS))
pd$panel <- factor(pd$panel, levels = unique(pd$panel))
chain.pts <- merge(aggregate(x ~ city + panel + chain, data = pd, FUN = mean),      # diamonds: mean x,
                   aggregate(y ~ city + panel + chain, data = pd, FUN = median))    # median effect
labels <- unique(pd[, c("city", "panel", "label")])
x.lab  <- if (PARAM.SOURCE == "ranks") "Parameter rank among the posterior simulations (1 = lowest value)" else "Parameter value"

p <- ggplot(pd, aes(x = x, y = y, colour = chain)) +
    geom_point(size = 0.5, alpha = 0.45) +
    geom_point(data = chain.pts, aes(fill = chain), shape = 23, size = 3, colour = "black") +
    geom_text(data = labels, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = -0.05, vjust = 1.4, size = 2.4) +
    facet_grid(panel ~ city, scales = "free") +
    scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +
    labs(title = paste0("Doxy-PEP effect vs parameters, by chain: ", CALIBRATION.CODE),
         subtitle = paste0(OUTCOME, ", ", COVERAGE, ", 2030 (negative = fewer infections). Dots = simulations; ",
                           "diamonds = chain (mean x, median effect). Spearman rho: all simulations | mean within chain"),
         x = x.lab, y = "% change in incidence vs no doxy-PEP", colour = "Chain", fill = "Chain") +
    theme_bw(base_size = 9) + theme(legend.position = "bottom", strip.text.y = element_text(size = 7))
if (PARAM.SOURCE == "values") p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(f("vs_parameters", "png"), p, width = 2.2 * length(LOCS) + 1.5, height = 2.6 * nrow(PANELS) + 1.2, dpi = 200)


## PRINT ----
cat("\n== Women effect: within-chain Spearman with each key parameter (rows) by city (columns) ==\n")
w <- key.tab[key.tab$population == "Women", ]
print(round(tapply(w$rho.within, list(w$parameter, w$city), identity)[KEY.PARAMS, names(LOCS), drop = FALSE], 2))
cat("\n== Variance in the women effect due to chain ==\n")
print(share.tab[, c("city", "women.chain.medians", "r2.chain", "r2.top3", "r2.chain.after.top3")], row.names = FALSE, digits = 2)
cat("\nSaved in ", DEEP.DIR, "\n", sep = "")
