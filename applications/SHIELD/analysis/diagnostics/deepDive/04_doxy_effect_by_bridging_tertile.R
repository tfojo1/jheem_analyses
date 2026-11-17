# ****************************************************************************************************
# DEEP DIVE: DOXY-PEP EFFECT BY TERTILE OF MSM-FEMALE BRIDGING (OPTION A) ----
# ****************************************************************************************************
# How much does the doxy-PEP effect change with the level of MSM-female bridging
# (oe.female.pairings.with.msm, set in PARAMETER)? Uses the existing 1,000 posterior simulations only.
#
# What it does, for each city x population (Women, MSM, Total) x outcome x coverage:
#   1. splits the simulations into N.GROUPS groups (tertiles) of PARAMETER, two ways:
#        pooled        - tertiles of all 1,000 simulations together. Because the chains do not mix,
#                        a tertile can be mostly one chain: the column chains.in.group shows how many
#                        simulations of each chain fall in each group.
#        within.chain  - tertiles inside each chain, then pooled across chains (low = the lowest third
#                        of every chain, etc.). Each group then has the same number of simulations from
#                        every chain, so the comparison is not a comparison of chains.
#   2. median and 95% interval of the effect in each group
#   3. the gradient: median in the high group - median in the low group, also as a share of the
#      overall 95% interval
#   4. figure: box plots of the effect by tertile at CCRIT, one panel per city, one colour per population
#
# LIMIT: this covers only the range of PARAMETER that the 4 chains reached. The prior may allow
# lower or higher values; fixed-value runs (option B) are needed to cover those.
#
# MEMORY: reads sorted_arrs.Rdata (~3 MB) and the per-simulation CSV (~20 MB); PARAM.SOURCE = "values"
#   loads one simset per city instead. No simulation is run.
#
# USAGE (from jheem_analyses)
#   1. CALIBRATION.CODE is set in diagnostics_setup.R (calib.10.20.stage4)
#   2. run deepDive/01_doxy_impact_by_chain.R first, with USE.SEX = TRUE
#   3. source("applications/SHIELD/analysis/diagnostics/deepDive/04_doxy_effect_by_bridging_tertile.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   bridging_tertile_groups_<code>_<date>.csv     one row per city x population x outcome x coverage x grouping x group
#   bridging_tertile_gradient_<code>_<date>.csv   one row per city x population x outcome x coverage x grouping
#   bridging_tertile_boxplot_<code>_<date>.png    effect by tertile at CCRIT
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
library(ggplot2)


## SETTINGS ----
CITIES       <- NULL                          # NULL = all 10; or e.g. c("Chicago", "Houston", "LA")
PARAMETER    <- "oe.female.pairings.with.msm" # the bridging parameter
N.GROUPS     <- 3                             # 3 = tertiles
GROUP.LABELS <- c("low", "mid", "high")       # one label per group
COVERAGES    <- NULL                          # NULL = every coverage in the effects file
CCRIT        <- "doxy.cov.20"                 # coverage shown in the figure
PLOT.OUTCOME <- "pct_change_incidence_vs_noint"
PARAM.SOURCE <- "ranks"                       # "ranks" (sorted_arrs.Rdata) or "values" (simset$parameters)
if (length(GROUP.LABELS) != N.GROUPS) stop("GROUP.LABELS needs one label per group")


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
    population = c("Whose doxy-PEP effect: Women, MSM or Total",
                   "Women and Total include spillover from MSM; MSM is the direct effect"),
    outcome    = c(paste0("pct_change_incidence_vs_noint = % change in 2030 incidence vs no doxy-PEP, same simulation. ",
                          "abs_change_cum_incidence_vs_noint = cumulative change in infections 2022-2030 vs no doxy-PEP"),
                   "Negative = fewer infections with doxy-PEP. Units: % for pct_, number of infections for abs_"),
    coverage   = c("Doxy-PEP coverage scenario", "doxy.cov.20 = 20% coverage"),
    grouping   = c(paste0("How the groups of ", PARAMETER, " are formed. pooled = tertiles of all simulations; ",
                          "within.chain = tertiles inside each chain, then pooled"),
                   "Trust within.chain more: every group has the same number of simulations from each chain, so it is not a comparison of chains"),
    group      = c(paste0("Group of ", PARAMETER, ": ", paste(GROUP.LABELS, collapse = " / "), " (lowest to highest values)"),
                   "high = most MSM-female bridging"),
    n.sims     = c("Simulations in the group", "-"),
    chains.in.group = c("Simulations from each chain in the group (chains 1/2/3/4)",
                        "pooled grouping: a group made mostly of one chain is partly a comparison of chains. within.chain: always equal"),
    median     = c("Median effect in the group", "Units as in 'outcome'"),
    lower      = c("2.5th percentile of the effect in the group", "Lower end of the 95% interval"),
    upper      = c("97.5th percentile of the effect in the group", "Upper end of the 95% interval"),
    median.low  = c("Median effect in the low group", "Units as in 'outcome'"),
    median.mid  = c("Median effect in the mid group", "Units as in 'outcome'"),
    median.high = c("Median effect in the high group", "Units as in 'outcome'"),
    high.minus.low = c("median.high - median.low",
                       "Negative = more bridging gives a larger reduction. Size = how much the effect depends on bridging, in outcome units"),
    overall.median = c("Median effect over all simulations (what the manuscript reports)", "-"),
    overall.width  = c("Width of the overall 95% interval (97.5th - 2.5th percentile, all simulations)", "-"),
    gradient.share = c("|high.minus.low| / overall.width",
                       "How much of the reported uncertainty the bridging gradient spans. < 0.1 small; 0.1-0.3 moderate; > 0.3 bridging is a major source of the uncertainty")
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
    f <- list.files(DEEP.DIR, pattern = "^doxy_chain_values_.*\\.csv$", full.names = TRUE)
    f <- f[!grepl("_backup\\.csv$", f)]
    if (length(f) == 0) stop("No doxy_chain_values file in ", DEEP.DIR, ". Run 01_doxy_impact_by_chain.R first.")
    f <- f[which.max(file.mtime(f))]
    skip <- if (grepl("^\"?WHAT IT IS", readLines(f, n = 1))) 2 else 0
    cat("Doxy-PEP effects from", basename(f), "\n")
    v <- read.csv(f, skip = skip, stringsAsFactors = FALSE)
    if (!all(c("Women", "MSM") %in% v$population))
        stop("No Women/MSM rows in ", basename(f), ". Re-run 01_doxy_impact_by_chain.R with USE.SEX = TRUE.")
    v
}

# PARAMETER for every simulation, in the simset's simulation order (rank or value)
get.parameter <- function(loc) {
    if (PARAM.SOURCE == "ranks") {
        if (!exists("SORTED.ARRS")) SORTED.ARRS <<- get(load(file.path(get.jheem.root.directory(), "shield", "outputs",
                                                                        CALIBRATION.CODE, "sorted_arrs.Rdata")))
        ord <- SORTED.ARRS[[loc]][, PARAMETER]   # simulations ordered by PARAMETER, lowest first
        r <- integer(length(ord)); r[ord] <- seq_along(ord)
        return(list(x = r, chains = NULL))
    }
    simset <- retrieve.simulation.set(version = "shield", location = loc,
                                      calibration.code = CALIBRATION.CODE, n.sim = N.SIM)
    out <- list(x = simset$parameters[PARAMETER, ], chains = as.integer(factor(simset$simulation.chain)))
    rm(simset); invisible(gc())
    out
}

# Group of each simulation: N.GROUPS equal-sized groups by rank of x
make.groups <- function(x) factor(GROUP.LABELS[ceiling(rank(x, ties.method = "first") * N.GROUPS / length(x))],
                                  levels = GROUP.LABELS)


## RUN ----
V <- read.latest.values()
covs <- if (is.null(COVERAGES)) unique(V$coverage) else COVERAGES
group.rows <- list(); grad.rows <- list(); plot.rows <- list()

for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("==", city, "\n")
    P <- get.parameter(loc)
    vc <- V[V$city == city, ]
    n  <- length(P$x)

    for (pop in unique(vc$population)) for (out in unique(vc$outcome)) for (cov in covs) {
        d <- vc[vc$population == pop & vc$outcome == out & vc$coverage == cov, ]
        if (nrow(d) == 0) next
        if (nrow(d) != n) stop(city, ": the effects file and the parameters have different numbers of simulations")
        chains <- d$chain
        if (!is.null(P$chains) && !identical(as.integer(chains), P$chains))
            stop(city, ": chain labels in the effects file do not match the simset; the simulation order differs")
        y <- d$value
        q.all <- quantile(y, c(0.025, 0.5, 0.975), na.rm = TRUE, names = FALSE)

        groupings <- list(
            pooled       = make.groups(P$x),
            within.chain = factor(ave(P$x, chains, FUN = function(x) as.integer(make.groups(x))),
                                  levels = seq_len(N.GROUPS), labels = GROUP.LABELS))
        for (g.name in names(groupings)) {
            g <- groupings[[g.name]]
            med <- tapply(y, g, median, na.rm = TRUE)
            for (lev in GROUP.LABELS) {
                yy <- y[g == lev]
                q  <- quantile(yy, c(0.025, 0.5, 0.975), na.rm = TRUE, names = FALSE)
                group.rows[[length(group.rows) + 1]] <- data.frame(
                    city = city, population = pop, outcome = out, coverage = cov, grouping = g.name, group = lev,
                    n.sims = length(yy),
                    chains.in.group = paste(table(factor(chains[g == lev], levels = sort(unique(chains)))), collapse = "/"),
                    median = q[2], lower = q[1], upper = q[3], stringsAsFactors = FALSE)
            }
            grad <- med[[GROUP.LABELS[N.GROUPS]]] - med[[GROUP.LABELS[1]]]
            row <- data.frame(city = city, population = pop, outcome = out, coverage = cov, grouping = g.name,
                              stringsAsFactors = FALSE)
            for (lev in GROUP.LABELS) row[[paste0("median.", lev)]] <- med[[lev]]
            row$high.minus.low <- grad
            row$overall.median <- q.all[2]
            row$overall.width  <- q.all[3] - q.all[1]
            row$gradient.share <- abs(grad) / (q.all[3] - q.all[1])
            grad.rows[[length(grad.rows) + 1]] <- row

            if (cov == CCRIT && out == PLOT.OUTCOME)
                plot.rows[[length(plot.rows) + 1]] <- data.frame(city = city, population = pop, grouping = g.name,
                                                                 group = g, value = y)
        }
    }
}


## SAVE ----
group.tab <- do.call(rbind, group.rows); rownames(group.tab) <- NULL
grad.tab  <- do.call(rbind, grad.rows);  rownames(grad.tab)  <- NULL
f <- function(what, ext = "csv") file.path(DEEP.DIR, paste0("bridging_tertile_", what, "_", CALIBRATION.CODE, "_", STAMP, ".", ext))
write.annotated.csv(group.tab, f("groups"))
write.annotated.csv(grad.tab,  f("gradient"))

pd <- do.call(rbind, plot.rows)
pd$city       <- factor(pd$city, levels = names(LOCS))
pd$population <- factor(pd$population, levels = intersect(c("MSM", "Women", "Total"), unique(pd$population)))
pd$grouping   <- factor(pd$grouping, levels = c("within.chain", "pooled"),
                        labels = c("Tertiles within each chain", "Tertiles of all simulations"))
p <- ggplot(pd, aes(x = group, y = value, fill = population)) +
    geom_boxplot(outlier.size = 0.3, position = position_dodge(width = 0.8), linewidth = 0.3) +
    facet_grid(grouping ~ city) +
    scale_fill_manual(values = c(MSM = "#2c7fb8", Women = "#d95f02", Total = "#7fcdbb")) +
    labs(title = paste0("Doxy-PEP effect by tertile of ", PARAMETER, ": ", CALIBRATION.CODE),
         subtitle = paste0(PLOT.OUTCOME, ", ", CCRIT, ", 2030 (negative = fewer infections). ",
                           "Low = least MSM-female bridging. Covers only the range the 4 chains reached."),
         x = paste0("Tertile of ", PARAMETER), y = "% change in incidence vs no doxy-PEP", fill = NULL) +
    theme_bw(base_size = 9) + theme(legend.position = "bottom")
ggsave(f("boxplot", "png"), p, width = 2 * length(LOCS) + 1.5, height = 6.5, dpi = 200)


## PRINT ----
cat("\n== Women effect at", CCRIT, "by tertile within chains (", PLOT.OUTCOME, ") ==\n")
s <- grad.tab[grad.tab$population == "Women" & grad.tab$coverage == CCRIT & grad.tab$outcome == PLOT.OUTCOME &
              grad.tab$grouping == "within.chain", ]
print(s[, c("city", paste0("median.", GROUP.LABELS), "high.minus.low", "overall.median", "gradient.share")],
      row.names = FALSE, digits = 3)
cat("\nSaved in ", DEEP.DIR, "\n", sep = "")
