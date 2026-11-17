# ****************************************************************************************************
# DOXY-PEP IMPACT BY MCMC CHAIN ----
# ****************************************************************************************************
# Does the effect of doxy-PEP (vs no doxy-PEP, in the same simulation) depend on which chain a
# simulation came from? The stage 4 parameters do not mix (mixing report: FAIL in all 10 cities),
# but an outcome can still be well estimated if every chain gives the same answer for it.
# This script checks that, city by city, for the outcomes the manuscript reports.
#
# WHAT IT DOES
#   1. Reads the calc arrays from step 01b (total_calc, and sex_calc if USE.SEX = TRUE) and keeps
#      only EVAL.YEAR, the coverage levels in COVERAGE.CHECK and the outcomes in OUTCOMES.
#   2. Finds the chain of every simulation (CHAIN.SOURCE, below).
#   3. For every city x coverage x outcome, compares the chains:
#        median.chain<k>  median of the outcome in chain k
#        rhat             rank-normalized split R-hat of the outcome (same function as the mixing report)
#        ess              bulk ESS of the outcome across chains (same function as the mixing report)
#        spread.share     (highest chain median - lowest chain median) / width of the pooled 95% interval
#        loo.shift.share  largest move of the pooled median when one chain is left out, / pooled width
#        target.agree     pct outcomes only: TRUE if every chain median is on the same side of TARGET
#        verdict          AGREE / CHECK / DIFFER (rules in AGREEMENT, below)
#   4. If EXCLUDE.CHAINS names chains for a city, also gives the pooled median and 95% interval
#      without them (columns *.excl), so you can see whether dropping worse-fitting chains matters.
#   5. Saves two CSVs and one box-plot figure per outcome at CCRIT.
#
# HOW TO READ THE VERDICT
#   AGREE   the chains give the same answer: the outcome does not depend on where each chain sits
#           on the parameter ridge, and the pooled median and interval can be used as they are
#   CHECK   small differences between chains: report it, and look at the box plot
#   DIFFER  the answer depends on the chain: report chain-level results as a sensitivity analysis,
#           or exclude chains with a stated rule
#   Note: an outcome R-hat near 1 does NOT mean the chains have mixed. It means this outcome is
#   insensitive to the parts of parameter space the chains did not explore.
#
# CHAINS: the mapping from simulation to chain
#   "simset"  (recommended) reads simset$simulation.chain from each city's no-intervention simset,
#             one simset at a time; falls back to the baseline calibration simset of the same size
#   "blocks"  assumes the N.SIM simulations are N.CHAINS equal blocks in chain order (1-250 chain 1,
#             251-500 chain 2, ...). Quicker, but only use it once "simset" has confirmed the order
#
# MEMORY
#   1. total_calc is ~0.5 GB on disk for stage 4, several times that in memory while it loads.
#      It is cut down to the slice used here straight after loading.
#   2. sex_calc is ~1.6 GB on disk: USE.SEX = TRUE needs a lot more memory. Run total first.
#   3. CHAIN.SOURCE = "simset" loads one no-intervention simset per city, one at a time.
#   No simulation is run: this only reads saved results.
#
# USAGE (from jheem_analyses)
#   1. check the SETTINGS below
#   2. source("applications/SHIELD/analysis/diagnostics/deepDive/01_doxy_impact_by_chain.R")
#      Try one city first: CITIES <- c("Baltimore")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<CALIB.NAME>/deepDive/
#   doxy_chain_summary_<code>_<date>.csv     one row per city x coverage x outcome
#   doxy_chain_values_<code>_<date>.csv      one row per simulation (city x coverage x outcome x sim)
#   doxy_chain_boxplot_<outcome>_<code>.png  box plots by chain at CCRIT, one panel per city
#   Each CSV starts with two description rows, then the column names, then the results:
#     row 1  WHAT IT IS   what is calculated in each column
#     row 2  HOW TO READ  how to interpret it
#     row 3  column names
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/commoncode/locations_of_interest.R")
source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source("../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R")
# .rhat.rank.split() and .ess.bulk(): the same R-hat and ESS as the mixing report
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/mcmc/mcmc_helpers.R")
library(ggplot2)


## SETTINGS ----
CALIB.NAME     <- "calib.10.20.stage4"
N.SIM          <- 1000                       # the posterior set: burned (second half) + thinned, 250 per chain.
# The script stops if the arrays hold a different number of sims.
CITIES        <- NULL                       # NULL = all 10; or e.g. c("Baltimore", "Phoenix")
EVAL.YEAR      <- "2030"
NOINT          <- "noint"
COVERAGE.CHECK <- paste0("doxy.cov.", c(10, 20, 30, 50))   # any of doxy.cov.5 ... doxy.cov.50
CCRIT          <- "doxy.cov.20"              # coverage shown in the box plots (Figure 4 uses 20%)
TARGET         <- -50                        # % change vs no doxy-PEP used in Figure 3
N.CHAINS       <- 4
CHAIN.SOURCE   <- "simset"                   # "simset" or "blocks" (see CHAINS above)
USE.SEX        <- T                      # TRUE adds MSM and women (loads sex_calc, ~1.6 GB)

# Chains to leave out of the *.excl columns, by city name. Empty = none.
# Example, from the stage 4 mixing report (chains with the largest log-likelihood gaps):
# EXCLUDE.CHAINS <- list(Chicago = c(1, 3), LA = c(2, 3), NYC = 4)
EXCLUDE.CHAINS <- list()

# Verdict: DIFFER if rhat > rhat.differ or spread.share > spread.differ;
#          otherwise CHECK if rhat > rhat.check or spread.share > spread.check; otherwise AGREE
AGREEMENT <- list(rhat.check = 1.05, rhat.differ = 1.1, spread.check = 0.25, spread.differ = 0.5)

# What to check. A negative change means fewer cases.
OUTCOMES <- data.frame(
    array      = "total_calc",
    population = "Total",
    sex        = NA_character_,
    outcome    = c("pct_change_incidence_vs_noint",       # % change in incidence in EVAL.YEAR
                   "abs_change_cum_incidence_vs_noint"),  # cumulative change from YEAR0 to EVAL.YEAR
    stringsAsFactors = FALSE
)
if (USE.SEX)
    OUTCOMES <- rbind(OUTCOMES, data.frame(
        array      = "sex_calc",
        population = rep(c("MSM", "Women"), each = 2),
        sex        = rep(c("msm", "female"), each = 2),
        outcome    = rep(c("pct_change_incidence_vs_noint", "abs_change_cum_incidence_vs_noint"), 2),
        stringsAsFactors = FALSE))


## FOLDERS ----
LOCS <- SHIELD.TEN.MSAS                        # names = city, values = location code
if (!is.null(CITIES)) {
    if (!all(CITIES %in% names(LOCS))) stop("Unknown city: ", paste(setdiff(CITIES, names(LOCS)), collapse = ", "))
    LOCS <- LOCS[CITIES]
}
OUT.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIB.NAME, "deepDive")
dir.create(OUT.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## HELPERS ----
# Subset an array by dimension name, keeping every dimension (drop = FALSE).
# keep = named list, e.g. list(year = "2030", outcome = c("a", "b")); dimensions not named are kept whole.
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

# The location dimension may hold codes ("C.12580") or city names ("Baltimore")
location.label <- function(arr, loc) {
    labs <- dimnames(arr)$location
    if (loc %in% labs) return(loc)
    city <- names(SHIELD.TEN.MSAS)[SHIELD.TEN.MSAS == loc]
    if (length(city) == 1 && city %in% labs) return(city)
    stop("Location ", loc, " is not in the array")
}

# Load one calc array and keep only what this script uses, to free memory straight away
load.slim.array <- function(name) {
    path <- file.path(shield.output.path(CALIB.NAME), paste0(name, "_results.Rdata"))
    if (!file.exists(path)) stop("Missing: ", path, "\nRun steps 01a and 01b for ", CALIB.NAME, " first.")
    cat("Loading", basename(path), "(", round(file.size(path) / 1e9, 2), "GB on disk ) ...\n")
    e    <- new.env()
    nm   <- load(path, envir = e)
    arr  <- e[[nm[1]]]
    rm(e)
    keep <- list(year         = EVAL.YEAR,
                 intervention = COVERAGE.CHECK,
                 outcome      = unique(OUTCOMES$outcome[OUTCOMES$array == name]))
    if ("sex" %in% names(dimnames(arr)))
        keep$sex <- unique(na.omit(OUTCOMES$sex[OUTCOMES$array == name]))
    arr <- slice.by.name(arr, keep)
    gc(verbose = FALSE)
    cat("  kept", paste(names(dimnames(arr)), dim(arr), sep = "=", collapse = " x "), "\n")
    arr
}

# One value per simulation
sim.values <- function(arr, loc, intervention, outcome, sex = NA) {
    keep <- list(year = EVAL.YEAR, intervention = intervention,
                 location = location.label(arr, loc), outcome = outcome)
    if (!is.na(sex)) keep$sex <- sex
    as.numeric(slice.by.name(arr, keep))
}

# Chain of every simulation, in the order of the sim dimension
get.chains <- function(loc, n.sim) {
    if (CHAIN.SOURCE == "blocks") {
        if (n.sim %% N.CHAINS != 0) stop(n.sim, " simulations do not split into ", N.CHAINS, " equal blocks")
        return(rep(seq_len(N.CHAINS), each = n.sim / N.CHAINS))
    }
    # 1. the no-intervention simset the arrays were built from (same path as step 01a)
    sim.file <- file.path(get.jheem.root.directory(), "simulations", "shield", paste0(CALIB.NAME, "-", n.sim),
                          loc, paste0("shield_", CALIB.NAME, "-", n.sim, "_", loc, "_", NOINT, ".Rdata"))
    chains <- NULL
    if (file.exists(sim.file)) {
        simset <- load.simulation.set(sim.file)
        chains <- simset$simulation.chain
        rm(simset); gc(verbose = FALSE)
    }
    # 2. the baseline calibration simset of the same size
    if (is.null(chains)) {
        cat("  no chain labels in the intervention simset; reading the calibration simset\n")
        simset <- retrieve.simulation.set(version = "shield", location = loc,
                                          calibration.code = CALIB.NAME, n.sim = n.sim)
        chains <- simset$simulation.chain
        rm(simset); gc(verbose = FALSE)
    }
    if (is.null(chains)) stop("No chain labels found for ", loc, ". Set CHAIN.SOURCE <- \"blocks\" only if the order is known.")
    if (length(chains) != n.sim) stop("The simset for ", loc, " has ", length(chains), " sims, the arrays ", n.sim)
    as.integer(factor(chains))   # chains numbered 1..k
}

# Compare the chains for one outcome. x = one value per sim; chains = chain of each sim
compare.chains <- function(x, chains, exclude = NULL, is.pct = FALSE) {
    ok    <- !is.na(x)
    vals  <- split(x[ok], chains[ok])              # keeps the iteration order within each chain
    n.min <- min(lengths(vals))
    mat   <- sapply(vals, function(v) tail(v, n.min))   # iteration x chain, equal lengths
    q     <- quantile(x[ok], c(0.025, 0.5, 0.975), names = FALSE)
    width <- q[3] - q[1]
    med   <- sapply(vals, median)
    loo   <- sapply(names(vals), function(k) median(x[ok & chains != as.integer(k)]))
    
    out <- data.frame(n.sim = length(x), n.na = sum(!ok),
                      median = q[2], lower = q[1], upper = q[3],
                      rhat = .rhat.rank.split(mat), ess = .ess.bulk(mat),
                      spread.share    = if (width > 0) diff(range(med)) / width else NA_real_,
                      loo.shift.share = if (width > 0) max(abs(loo - q[2])) / width else NA_real_,
                      loo.worst.chain = names(vals)[which.max(abs(loo - q[2]))],
                      target.agree    = if (is.pct) length(unique(med <= TARGET)) == 1 else NA)
    for (k in names(vals)) out[[paste0("median.chain", k)]] <- med[[k]]
    
    out$verdict <- with(out, ifelse(is.na(rhat), "NA",
                                    ifelse(rhat > AGREEMENT$rhat.differ | (!is.na(spread.share) & spread.share > AGREEMENT$spread.differ), "DIFFER",
                                           ifelse(rhat > AGREEMENT$rhat.check  | (!is.na(spread.share) & spread.share > AGREEMENT$spread.check),  "CHECK", "AGREE"))))
    
    # pooled summary without the excluded chains
    if (length(exclude) > 0) {
        keep <- ok & !(chains %in% exclude)
        qe   <- quantile(x[keep], c(0.025, 0.5, 0.975), names = FALSE)
        out$chains.excluded <- paste(exclude, collapse = ",")
        out$median.excl <- qe[2]; out$lower.excl <- qe[1]; out$upper.excl <- qe[3]
    }
    out
}


## COLUMN DESCRIPTIONS ----
# Written as the first two rows of each CSV (WHAT IT IS, HOW TO READ), so the files explain themselves.
# One entry per column: c(what it is, how to read it). Edit the text here, not in the CSVs.
.agree.rule <- with(AGREEMENT, paste0(
    "DIFFER if rhat > ", rhat.differ, " or spread.share > ", spread.differ,
    "; otherwise CHECK if rhat > ", rhat.check, " or spread.share > ", spread.check, "; otherwise AGREE"))
COLUMN.NOTES <- list(
    city       = c("MSA name",
                   "One city per block of rows"),
    location   = c("Location code of the MSA",
                   "-"),
    coverage   = c("Doxy-PEP coverage scenario. Every value is compared with no doxy-PEP (noint) in the same simulation",
                   "doxy.cov.20 = 20% coverage"),
    population = c("Group the outcome refers to: Total (MSM and Women when USE.SEX = TRUE)",
                   "-"),
    outcome    = c(paste0("Outcome in ", EVAL.YEAR, ", made in step 01b within each simulation. ",
                          "pct_change_incidence_vs_noint = 100 x (incidence with doxy-PEP - without) / without. ",
                          "abs_change_cum_incidence_vs_noint = cumulative infections from 2022 (YEAR0) to ", EVAL.YEAR,
                          ", with doxy-PEP minus without"),
                   "Negative = fewer infections with doxy-PEP. Units: % for pct_ outcomes, number of infections for abs_ outcomes"),
    n.sim      = c("Number of simulations in the posterior set, all chains together",
                   paste0("Should equal N.SIM (", N.SIM, ")")),
    n.na       = c("Simulations with a missing value for this outcome. They are left out of every calculation in the row",
                   "0 = none missing. More than a few: check for failed runs"),
    median     = c("Median across all simulations, all chains pooled. This is what the manuscript reports",
                   "Units as in 'outcome'"),
    lower      = c("2.5th percentile across all simulations, all chains pooled",
                   "Lower end of the 95% interval. Units as in 'outcome'"),
    upper      = c("97.5th percentile across all simulations, all chains pooled",
                   "Upper end of the 95% interval. Units as in 'outcome'"),
    rhat       = c(paste0("Rank-normalized split R-hat of this outcome (same function as the mixing report): ",
                          "the simulations as iteration x chain, each chain cut in half, values replaced by normal scores of their ranks; ",
                          "the larger of the bulk and tail versions"),
                   paste0("About 1 = the chains (and the two halves of each chain) agree. > ", AGREEMENT$rhat.check, " check; > ",
                          AGREEMENT$rhat.differ, " differ. Also goes up when a chain drifts. Says WHETHER the chains disagree, not by how much: see spread.share")),
    ess        = c("Bulk effective sample size of this outcome across chains: how many independent draws the simulations are worth",
                   "Monte Carlo SE of the median is about ((upper - lower) / 3.92) / sqrt(ess). Below about 100: the median and interval are imprecise"),
    spread.share    = c("(highest chain median - lowest chain median) / (upper - lower)",
                        paste0("How far apart the chains are, as a share of the 95% interval. < ", AGREEMENT$spread.check, " minor; ",
                               AGREEMENT$spread.check, "-", AGREEMENT$spread.differ, " noticeable; > ", AGREEMENT$spread.differ,
                               " the answer depends on the chain. Multiply by (upper - lower) for the spread in outcome units")),
    loo.shift.share = c("Largest move of the pooled median when one chain is left out (one recalculation per chain), / (upper - lower)",
                        "How much the reported median depends on any one chain. Multiply by (upper - lower) for outcome units, and compare with the Monte Carlo SE (see ess)"),
    loo.worst.chain = c("The chain whose removal moves the pooled median the most",
                        "Points to the chain that pulls the median"),
    target.agree    = c(paste0("% outcomes only: TRUE if every chain median is on the same side of TARGET (", TARGET, "%)"),
                        "FALSE = whether this coverage reaches the target depends on the chain. TRUE = the chains agree (the target may or may not be reached). NA for other outcomes"),
    verdict         = c(paste0("Screening flag: ", .agree.rule),
                        "Flags rows to look at first; not a decision on its own. Cut-offs are set in AGREEMENT in the script"),
    chains.excluded = c("Chains left out of the *.excl columns (EXCLUDE.CHAINS in the script)",
                        "-"),
    median.excl     = c("Median across all simulations, without the chains in chains.excluded",
                        "Compare with 'median' to see what leaving those chains out changes"),
    lower.excl      = c("2.5th percentile, without the chains in chains.excluded",
                        "Compare with 'lower'"),
    upper.excl      = c("97.5th percentile, without the chains in chains.excluded",
                        "Compare with 'upper'"),
    # values file
    chain      = c("MCMC chain the simulation came from (simulation.chain of the simset)",
                   "One box per chain in the box plots"),
    value      = c("Outcome value in one simulation. Units as in 'outcome'",
                   paste0("One row per simulation: ", N.SIM / N.CHAINS, " per chain. The summary file is built from these values"))
)

# Two description rows for a set of columns. median.chain<k> columns share one description.
column.notes <- function(cols) {
    notes <- lapply(cols, function(col) {
        if (grepl("^median\\.chain\\d+$", col)) {
            k <- sub("^median\\.chain", "", col)
            return(c(paste0("Median of chain ", k, "'s simulations. Units as in 'outcome'"),
                     "Compare the chains with each other, and with lower-upper"))
        }
        if (!is.null(COLUMN.NOTES[[col]])) COLUMN.NOTES[[col]] else c("", "")
    })
    rows <- as.data.frame(do.call(cbind, notes), stringsAsFactors = FALSE)
    names(rows) <- cols
    rows[[1]] <- paste0(c("WHAT IT IS: ", "HOW TO READ: "), rows[[1]])
    rows
}

# Write a CSV with the two description rows on top (read back with read.csv(file, skip = 2))
write.annotated.csv <- function(df, file) {
    con <- file(file, open = "w")
    on.exit(close(con))
    write.table(column.notes(names(df)), con, sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double")
    write.table(df, con, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
}


## 1. LOAD THE ARRAYS ----
ARRAYS <- lapply(setNames(nm = unique(OUTCOMES$array)), load.slim.array)
n.sim.arrays <- sapply(ARRAYS, function(a) length(dimnames(a)$sim))
if (any(n.sim.arrays != N.SIM))
    stop("The arrays hold ", paste(n.sim.arrays, collapse = " / "), " simulations, not N.SIM = ", N.SIM,
         ". They were built from another stage 4 simset: rebuild them (steps 01a-01b) from the ", N.SIM, " set.")
cat("Simulations per city:", N.SIM, "(simsets in simulations/shield/", paste0(CALIB.NAME, "-", N.SIM), ")\n")


## 2. COMPARE THE CHAINS, CITY BY CITY ----
summary.rows <- list()
value.rows   <- list()
for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("\n==", city, "(", loc, ") ==\n")
    chains <- get.chains(loc, N.SIM)
    cat("  sims per chain:", paste(table(chains), collapse = " / "), "\n")
    exclude <- EXCLUDE.CHAINS[[city]]
    
    for (i in seq_len(nrow(OUTCOMES))) {
        o <- OUTCOMES[i, ]
        for (cov in COVERAGE.CHECK) {
            x   <- sim.values(ARRAYS[[o$array]], loc, cov, o$outcome, o$sex)
            res <- compare.chains(x, chains, exclude, is.pct = grepl("^pct_", o$outcome))
            summary.rows[[length(summary.rows) + 1]] <-
                cbind(data.frame(city = city, location = loc, coverage = cov,
                                 population = o$population, outcome = o$outcome), res)
            value.rows[[length(value.rows) + 1]] <-
                data.frame(city = city, coverage = cov, population = o$population,
                           outcome = o$outcome, chain = factor(chains), value = x)
        }
    }
}
# bind_rows-style: chain columns can differ between cities if a city has fewer chains
all.cols    <- unique(unlist(lapply(summary.rows, names)))
chain.summary <- do.call(rbind, lapply(summary.rows, function(r) { r[setdiff(all.cols, names(r))] <- NA; r[all.cols] }))
chain.values  <- do.call(rbind, value.rows)


## 3. SAVE ----
f.summary <- file.path(OUT.DIR, paste0("doxy_chain_summary_", CALIB.NAME, "_", STAMP, ".csv"))
f.values  <- file.path(OUT.DIR, paste0("doxy_chain_values_",  CALIB.NAME, "_", STAMP, ".csv"))
write.annotated.csv(chain.summary, f.summary)
write.annotated.csv(chain.values,  f.values)

for (out.name in unique(OUTCOMES$outcome)) {
    d <- subset(chain.values, coverage == CCRIT & outcome == out.name)
    p <- ggplot(d, aes(x = chain, y = value, fill = population)) +
        geom_boxplot(outlier.size = 0.4, position = position_dodge(width = 0.8)) +
        { if (grepl("^pct_", out.name)) geom_hline(yintercept = TARGET, linetype = "dashed") } +
        facet_wrap(~ city, scales = "free_y", ncol = 5) +
        labs(title = paste0(out.name, ", ", EVAL.YEAR, ", ", CCRIT, " (", CALIB.NAME, ")"),
             subtitle = "Each box = one MCMC chain. Boxes that overlap = the chains agree on this outcome.",
             x = "Chain", y = out.name, fill = NULL) +
        theme_bw()
    ggsave(file.path(OUT.DIR, paste0("doxy_chain_boxplot_", out.name, "_", CALIB.NAME, ".png")),
           p, width = 14, height = 6, dpi = 150)
}


## 4. PRINT ----
cat("\n== Verdict counts (city x coverage x outcome) ==\n")
print(table(chain.summary$population, chain.summary$verdict))
cat("\n== Verdict at", CCRIT, "==\n")
print(chain.summary[chain.summary$coverage == CCRIT,
                    c("city", "population", "outcome", "median", "lower", "upper",
                      "rhat", "spread.share", "loo.worst.chain", "target.agree", "verdict")],
      row.names = FALSE, digits = 3)
cat("\nSaved:\n ", f.summary, "\n ", f.values, "\n  box plots in", OUT.DIR, "\n")