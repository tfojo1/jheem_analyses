# ****************************************************************************************************
# DEEP DIVE 07: DOES THE SHARE OF WOMEN'S EXPOSURE FROM MSM EXPLAIN THE CHAIN DIFFERENCES? ----
# ****************************************************************************************************
# Script 06 found that the chain differences in the doxy-PEP effect among women come from several parameters
# (MSM and heterosexual transmission levels, the recent heterosexual trend, bridging) that all change how much
# of women's infection comes from MSM partners rather than heterosexual men. Doxy-PEP for MSM protects women
# mainly through the first route. This script tests that mechanism with one quantity per simulation.
#
# THE QUANTITY (approximation, computed per simulation in BASE.YEAR without doxy-PEP)
#   In shield_specification.R, transmission from MSM to women and from heterosexual men to women use the same
#   rate (global.transmission.rate.het x transmission.rate.heterosexual), so women's exposure from each group is
#   proportional to (share of women's partnerships with that group) x (how common infection is in that group):
#     p      = MSM share of men = MSM / (MSM + heterosexual men), from the population outcome
#     s      = share of women's partnerships with MSM = p x oe / (1 - p + p x oe)   (specification, line ~656),
#              oe = oe.female.pairings.with.msm (simset parameters)
#     I.msm, I.het = infection in MSM and in heterosexual men per person, two versions:
#              "prevalence"  prevalence / population
#              "incidence"   incidence / population
#     share.from.msm = s x I.msm / (s x I.msm + (1 - s) x I.het)
#   It ignores age and race mixing and uses all-stage prevalence (or incidence) as a stand-in for infectiousness.
#
# WHAT IT DOES, for every city:
#   1. computes share.from.msm for each simulation (both versions) and summarises it by chain
#   2. checks whether it explains the doxy-PEP effect among women:
#        rho.within     Spearman correlation with the women effect inside each chain (mean of the 4)
#        slope          change in the women effect per 0.1 increase in share.from.msm, estimated inside chains
#        widest pair    observed gap between the two chains furthest apart in the women effect, and the gap
#                       predicted from share.from.msm alone (slope x difference in chain means)
#      The same for MSM as a contrast (MSM should not depend on it).
#   3. across cities: Spearman correlation of the city median share.from.msm with the city median women effect
#   4. figure: women effect vs share.from.msm by chain, one panel per city
#
# HOW TO READ IT
#   share.explained close to 1 with a clearly negative rho.within: one quantity, the share of women's exposure
#   coming from MSM, explains why the chains disagree. Then the uncertainty in the women effect can be described as
#   uncertainty in that share, and that is what data or priors would need to pin down.
#   Far from 1: the mechanism is only part of the story for that city (compare with script 06).
#
# MEMORY: reads sex_raw_results.Rdata (~0.6 GB on disk; cut to one year straight after loading) and one 1,000-sim
#   simset per city (for oe), one at a time. No simulation is run.
#
# USAGE (from jheem_analyses)
#   1. run deepDive/01_doxy_impact_by_chain.R first (per-simulation effects, MSM and women)
#   2. source("applications/SHIELD/analysis/diagnostics/deepDive/07_women_exposure_share_from_msm.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   women_share_from_msm_by_chain_<code>_<date>.csv     city x chain: p, s, infection by group, share.from.msm, effects
#   women_share_from_msm_values_<code>_<date>.csv       city x simulation: oe, p, s, share (both versions); used by script 08
#   women_share_from_msm_attribution_<code>_<date>.csv  city x population x coverage x version: rho, slope, gaps
#   women_share_from_msm_cities_<code>_<date>.csv       across cities: city medians and the correlation
#   women_share_from_msm_<code>_<date>.png              women effect vs share.from.msm, by chain
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
source("../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R")
library(ggplot2)


## SETTINGS ----
CITIES     <- NULL                               # NULL = all 10
BASE.YEAR  <- "2022"                             # year in which the share is computed (no doxy-PEP)
NOINT      <- "noint"
COVERAGES  <- paste0("doxy.cov.", c(10, 20, 30, 50))
CCRIT      <- "doxy.cov.20"                      # coverage in the figure and the by-chain file
OUTCOME    <- "pct_change_incidence_vs_noint"
OE.PARAMETER <- "oe.female.pairings.with.msm"
VERSIONS   <- c(prevalence = "prevalence", incidence = "incidence")   # infection measure per group
PLOT.VERSION <- "prevalence"


## FOLDERS ----
LOCS <- if (is.null(CITIES)) SHIELD.TEN.MSAS else SHIELD.TEN.MSAS[CITIES]
if (anyNA(LOCS)) stop("Unknown city in CITIES: ", paste(CITIES[is.na(LOCS)], collapse = ", "))
DEEP.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE, "deepDive")
dir.create(DEEP.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## COLUMN DESCRIPTIONS ----
# First two rows of each CSV: c(what it is, how to read it). Edit the text here, not in the CSVs.
COLUMN.NOTES <- list(
    city       = c("MSA name", "One city per block of rows"),
    chain      = c("MCMC chain", "-"),
    n.sims     = c("Simulations in the chain", "-"),
    p.msm.of.men = c(paste0("MSM share of men = MSM / (MSM + heterosexual men), population outcome, ", BASE.YEAR, ", median over the chain"), "-"),
    oe         = c(paste0(OE.PARAMETER, ", median over the chain"), "Higher = women partner with MSM more than their share of men implies"),
    s.partnerships.with.msm = c("Share of women's partnerships with MSM = p x oe / (1 - p + p x oe), median over the chain",
                                "From the model's mixing formula"),
    prev.msm   = c(paste0("Prevalence per person among MSM, ", BASE.YEAR, ", no doxy-PEP, median over the chain"), "-"),
    prev.het   = c(paste0("Prevalence per person among heterosexual men, ", BASE.YEAR, ", median over the chain"), "-"),
    share.from.msm.prevalence = c("s x prev.msm / (s x prev.msm + (1 - s) x prev.het), median over the chain",
                                  "Approximate share of women's exposure that comes from MSM partners"),
    share.from.msm.incidence  = c("The same with incidence per person instead of prevalence", "A check on the prevalence version"),
    women.effect = c(paste0("Median ", OUTCOME, " among women at ", CCRIT, " in the chain (from 01)"), "Negative = fewer infections with doxy-PEP"),
    msm.effect   = c(paste0("Median ", OUTCOME, " among MSM at ", CCRIT, " in the chain (from 01)"), "-"),
    population = c("Whose doxy-PEP effect: Women or MSM (contrast)", "-"),
    coverage   = c("Doxy-PEP coverage scenario", "-"),
    version    = c("Infection measure used for share.from.msm: prevalence or incidence", "-"),
    flagged    = c("TRUE if 01 flagged this city x population x coverage (chain medians >= 10 points apart)", "-"),
    rho.within = c("Spearman correlation of the effect with share.from.msm inside each chain, mean of the 4",
                   "Negative for women = more exposure from MSM, larger reduction. Near 0 for MSM expected"),
    slope.per.0.1 = c("Change in the effect (percentage points) per 0.1 increase in share.from.msm, linear fit inside chains", "-"),
    chain.a    = c("First chain of the widest pair (largest observed gap in the effect)", "-"),
    chain.b    = c("Second chain of the widest pair", "-"),
    observed.gap = c("Mean effect in chain A - mean effect in chain B", "Percentage points"),
    share.gap  = c("Mean share.from.msm in chain A - in chain B", "-"),
    predicted.gap = c("slope x share.gap: the gap expected from share.from.msm alone", "Compare with observed.gap"),
    share.explained = c("predicted.gap / observed.gap",
                        "Close to 1: share.from.msm alone explains the chain difference. Compare with share.explained in script 06 (all parameters)"),
    median.share.from.msm = c("City median share.from.msm over all simulations", "-"),
    median.women.effect   = c(paste0("City median women effect at ", CCRIT), "-"),
    sim              = c("Simulation number, in the order of the simset and of the 01 effects file", "-"),
    p                = c(paste0("MSM share of men in this simulation (", BASE.YEAR, ")"), "-"),
    s                = c("Share of women's partnerships with MSM in this simulation = p x oe / (1 - p + p x oe)", "-"),
    share.prevalence = c("share.from.msm in this simulation, prevalence version", "-"),
    share.incidence  = c("share.from.msm in this simulation, incidence version", "Recommended version (explains more of the chain gaps)"),
    rho.across.cities     = c("Spearman correlation of the two city medians across the cities in this file (same value on every row)",
                              "Does the share also explain why the women effect differs between cities?")
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
read.latest <- function(kind) {
    f <- list.files(DEEP.DIR, pattern = paste0("^doxy_chain_", kind, "_.*\\.csv$"), full.names = TRUE)
    f <- f[!grepl("_backup\\.csv$", f)]
    if (length(f) == 0) stop("No doxy_chain_", kind, " file in ", DEEP.DIR, ". Run 01_doxy_impact_by_chain.R first.")
    f <- f[which.max(file.mtime(f))]
    skip <- if (grepl("^\"?WHAT IT IS", readLines(f, n = 1))) 2 else 0
    cat("Reading", basename(f), "\n")
    read.csv(f, skip = skip, stringsAsFactors = FALSE)
}

# Subset an array by dimension name, keeping every dimension
slice.by.name <- function(arr, keep) {
    dn  <- dimnames(arr)
    idx <- lapply(names(dn), function(d) {
        if (is.null(keep[[d]])) return(seq_along(dn[[d]]))
        m <- match(keep[[d]], dn[[d]])
        if (anyNA(m)) stop("Not in the '", d, "' dimension: ", paste(keep[[d]][is.na(m)], collapse = ", "))
        m
    })
    do.call(`[`, c(list(arr), idx, list(drop = FALSE)))
}

# One value per simulation for one sex group and outcome
sim.values <- function(arr, loc, sex, outcome) {
    labs <- dimnames(arr)$location
    l <- if (loc %in% labs) loc else names(SHIELD.TEN.MSAS)[SHIELD.TEN.MSAS == loc]
    as.numeric(slice.by.name(arr, list(location = l, sex = sex, outcome = outcome)))
}

spearman.within <- function(y, x, chains) mean(sapply(sort(unique(chains)), function(k)
    suppressWarnings(cor(y[chains == k], x[chains == k], method = "spearman"))), na.rm = TRUE)

slope.within <- function(y, x, chains) {
    yc <- y - ave(y, chains); xc <- x - ave(x, chains)
    sum(xc * yc) / sum(xc^2)
}


## 1. LOAD sex_raw, KEEP BASE.YEAR AND NO DOXY-PEP ----
f.raw <- file.path(shield.output.path(CALIBRATION.CODE), "sex_raw_results.Rdata")
if (!file.exists(f.raw)) stop("Missing ", f.raw, ": run steps 01a for ", CALIBRATION.CODE)
cat("Loading", basename(f.raw), "(", round(file.size(f.raw) / 1e9, 2), "GB on disk ) ...\n")
e <- new.env(); nm <- load(f.raw, envir = e); RAW <- e[[nm[1]]]; rm(e)
RAW <- slice.by.name(RAW, list(year = BASE.YEAR, intervention = NOINT,
                               outcome = unique(c("population", unname(VERSIONS)))))
invisible(gc())
cat("  kept", paste(names(dimnames(RAW)), dim(RAW), sep = "=", collapse = " x "), "\n")
N.SIM.RAW <- length(dimnames(RAW)$sim)

VALS <- read.latest("values")
VALS <- VALS[VALS$outcome == OUTCOME & VALS$coverage %in% COVERAGES, ]
SUMM <- read.latest("summary")


## 2. RUN, ONE CITY AT A TIME ----
chain.rows <- list(); attr.rows <- list(); plot.rows <- list(); value.rows <- list()
for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("==", city, "\n")
    simset <- retrieve.simulation.set(version = "shield", location = loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIM)
    oe     <- as.numeric(simset$parameters[OE.PARAMETER, ])
    chains <- as.integer(factor(simset$simulation.chain))
    rm(simset); invisible(gc())
    if (length(oe) != N.SIM.RAW) stop(city, ": the simset has ", length(oe), " sims, sex_raw has ", N.SIM.RAW)

    pop.msm <- sim.values(RAW, loc, "msm", "population")
    pop.het <- sim.values(RAW, loc, "heterosexual_male", "population")
    p <- pop.msm / (pop.msm + pop.het)
    s <- p * oe / (1 - p + p * oe)
    share <- lapply(VERSIONS, function(v) {
        i.msm <- sim.values(RAW, loc, "msm", v) / pop.msm
        i.het <- sim.values(RAW, loc, "heterosexual_male", v) / pop.het
        s * i.msm / (s * i.msm + (1 - s) * i.het)
    })
    prev.msm <- sim.values(RAW, loc, "msm", "prevalence") / pop.msm
    prev.het <- sim.values(RAW, loc, "heterosexual_male", "prevalence") / pop.het

    eff <- function(pop, cov) {
        d <- VALS[VALS$city == city & VALS$population == pop & VALS$coverage == cov, ]
        if (nrow(d) == 0) return(NULL)
        if (nrow(d) != length(oe) || !identical(as.integer(d$chain), chains))
            stop(city, " ", pop, " ", cov, ": the effects file and the simset do not line up")
        d$value
    }

    # per simulation (for script 08)
    value.rows[[city]] <- data.frame(city = city, sim = seq_along(oe), chain = chains, oe = oe, p = p, s = s,
                                     share.prevalence = share$prevalence, share.incidence = share$incidence,
                                     stringsAsFactors = FALSE)

    # by chain
    w <- eff("Women", CCRIT); m <- eff("MSM", CCRIT)
    med <- function(x) tapply(x, chains, median, na.rm = TRUE)
    chain.rows[[city]] <- data.frame(
        city = city, chain = sort(unique(chains)), n.sims = as.integer(table(chains)),
        p.msm.of.men = med(p), oe = med(oe), s.partnerships.with.msm = med(s),
        prev.msm = med(prev.msm), prev.het = med(prev.het),
        share.from.msm.prevalence = med(share$prevalence), share.from.msm.incidence = med(share$incidence),
        women.effect = if (is.null(w)) NA else med(w), msm.effect = if (is.null(m)) NA else med(m),
        stringsAsFactors = FALSE)

    # attribution
    for (pop in c("Women", "MSM")) for (cov in COVERAGES) {
        y <- eff(pop, cov); if (is.null(y)) next
        ym <- tapply(y, chains, mean, na.rm = TRUE)
        pr <- combn(sort(unique(chains)), 2, simplify = FALSE)
        gaps <- sapply(pr, function(q) ym[[as.character(q[1])]] - ym[[as.character(q[2])]])
        wp <- pr[[which.max(abs(gaps))]]
        flag <- SUMM$chain.gap.flag[SUMM$city == city & SUMM$population == pop & SUMM$coverage == cov & SUMM$outcome == OUTCOME]
        for (v in names(VERSIONS)) {
            x  <- share[[v]]; ok <- !is.na(y) & is.finite(x)
            b  <- slope.within(y[ok], x[ok], chains[ok])
            xm <- tapply(x[ok], chains[ok], mean)
            sg <- xm[[as.character(wp[1])]] - xm[[as.character(wp[2])]]
            og <- max(abs(gaps)) * sign(gaps[which.max(abs(gaps))])
            attr.rows[[length(attr.rows) + 1]] <- data.frame(
                city = city, population = pop, coverage = cov, version = v,
                flagged = if (length(flag) == 1) flag else NA,
                rho.within = spearman.within(y[ok], x[ok], chains[ok]), slope.per.0.1 = 0.1 * b,
                chain.a = wp[1], chain.b = wp[2], observed.gap = og, share.gap = sg,
                predicted.gap = b * sg, share.explained = b * sg / og, stringsAsFactors = FALSE)
        }
    }
    if (!is.null(w))
        plot.rows[[city]] <- data.frame(city = city, chain = factor(chains), x = share[[PLOT.VERSION]], y = w)
}
rm(RAW); invisible(gc())


## 3. ACROSS CITIES ----
chain.tab <- do.call(rbind, chain.rows); rownames(chain.tab) <- NULL
attr.tab  <- do.call(rbind, attr.rows);  rownames(attr.tab)  <- NULL
pd <- do.call(rbind, plot.rows)
city.tab <- data.frame(city = names(plot.rows),
                       median.share.from.msm = sapply(plot.rows, function(d) median(d$x, na.rm = TRUE)),
                       median.women.effect   = sapply(plot.rows, function(d) median(d$y, na.rm = TRUE)),
                       stringsAsFactors = FALSE)
city.tab$rho.across.cities <- if (nrow(city.tab) >= 3) cor(city.tab$median.share.from.msm, city.tab$median.women.effect, method = "spearman") else NA
city.tab$flagged <- city.tab$city %in% SUMM$city[SUMM$population == "Women" & SUMM$chain.gap.flag %in% TRUE]


## SAVE ----
f <- function(what, ext = "csv") file.path(DEEP.DIR, paste0("women_share_from_msm", what, "_", CALIBRATION.CODE, "_", STAMP, ".", ext))
write.annotated.csv(chain.tab, f("_by_chain"))
write.annotated.csv(attr.tab,  f("_attribution"))
write.annotated.csv(city.tab,  f("_cities"))
write.annotated.csv(do.call(rbind, value.rows), f("_values"))

pd$city <- factor(pd$city, levels = names(LOCS))
labs <- attr.tab[attr.tab$population == "Women" & attr.tab$coverage == CCRIT & attr.tab$version == PLOT.VERSION, ]
labs$label <- sprintf("rho within %.2f | gap %.1f, explained %.0f%%%s", labs$rho.within, labs$observed.gap,
                      100 * labs$share.explained, ifelse(labs$flagged %in% TRUE, " | flagged", ""))
labs$city <- factor(labs$city, levels = names(LOCS))
cm <- merge(aggregate(x ~ city + chain, pd, mean), aggregate(y ~ city + chain, pd, median))
p <- ggplot(pd, aes(x = x, y = y, colour = chain)) +
    geom_point(size = 0.5, alpha = 0.4) +
    geom_point(data = cm, aes(fill = chain), shape = 23, size = 3, colour = "black") +
    geom_text(data = labs, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE, hjust = -0.03, vjust = 1.4, size = 2.2) +
    facet_wrap(~ city, ncol = 5, scales = "free") +
    scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +
    labs(title = paste0("Doxy-PEP effect among women vs share of women's exposure from MSM (", CALIBRATION.CODE, ")"),
         subtitle = paste0("share.from.msm (", PLOT.VERSION, " version, ", BASE.YEAR, ", no doxy-PEP) = s x I.msm / (s x I.msm + (1 - s) x I.het). ",
                           "Effect: ", OUTCOME, ", ", CCRIT, ", 2030. Diamonds = chain (mean share, median effect)."),
         x = "Share of women's exposure from MSM partners (approximation)", y = "% change in women's incidence vs no doxy-PEP",
         colour = "Chain", fill = "Chain") +
    theme_bw(base_size = 8) + theme(legend.position = "bottom")
ggsave(f("", "png"), p, width = 14, height = 6.5, dpi = 200)


## PRINT ----
cat("\n== By chain (", CCRIT, ") ==\n", sep = "")
print(chain.tab[, c("city", "chain", "s.partnerships.with.msm", "prev.msm", "prev.het", "share.from.msm.prevalence",
                    "women.effect", "msm.effect")], row.names = FALSE, digits = 3)
cat("\n== Women: does share.from.msm explain the widest chain gap? (", PLOT.VERSION, " version) ==\n", sep = "")
a <- attr.tab[attr.tab$population == "Women" & attr.tab$version == PLOT.VERSION, ]
print(a[, c("city", "coverage", "flagged", "rho.within", "observed.gap", "predicted.gap", "share.explained")], row.names = FALSE, digits = 3)
cat("\n== Across cities: Spearman of city medians (share vs women effect at ", CCRIT, "): ",
    round(city.tab$rho.across.cities[1], 2), "\n", sep = "")
cat("\nSaved in ", DEEP.DIR, "\n", sep = "")
