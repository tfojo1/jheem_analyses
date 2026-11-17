# ****************************************************************************************************
# DEEP DIVE 05: IS THE TRANSMISSION MIXING PROBLEM AN IDENTIFIABILITY PROBLEM? ----
# ****************************************************************************************************
# In the specification, sexual transmission is global.transmission.rate.<msm|het> x transmission.rate.<msm|het>,
# and transmission.rate.<msm|het> is built from multipliers at the time knots (1970-2022), by race
# (black/hispanic/other, shared by MSM and heterosexual) and by age (separate for MSM and heterosexual).
# So multiplying a whole group of multipliers by c and the global rate by 1/c gives the same model:
# the data can only identify COMBINATIONS such as global x multiplier, not each parameter.
#
# If that is why the chains do not mix, the combinations should mix even though their parts do not.
# This script checks that, for each city:
#   1. FAILING PARAMETERS: parameters with rank-normalized split R-hat > RHAT.FAIL (default 2), flagged
#      if they are transmission parameters
#   2. DESIGNED COMBINATIONS (from the model structure; log scale, so a product = a sum of logs):
#        product   global x one multiplier, e.g. global.transmission.rate.het x transmission.rate.multiplier.heterosexual2022
#                  = the heterosexual transmission rate at the 2022 knot (before the race and age multipliers)
#        level     log(global) + mean log of the time, race and age multipliers = the overall rate level for MSM or het
#        shape     log(multiplier) - mean log of its group = the multiplier relative to its group
#                  (e.g. 2022 relative to the other knots; black relative to the other races)
#      If the level and shapes mix while the raw parameters do not, the parameters only trade off along the
#      redundant scale: an identifiability problem, not a sampler that cannot find the posterior.
#      Note: a product (global x one multiplier) cancels only the trade-off with that multiplier's group. The
#      global rate also trades off with the race and age groups, so a product can still mix poorly when the
#      level and shapes mix well. The level and shapes cancel every one of these trade-offs.
#   3. DATA-DRIVEN PAIRS: every pair of failing transmission parameters, as a product (log sum) and a ratio
#      (log difference); the pairs whose combination mixes best are listed. Picks up trade-offs that the
#      model structure does not make obvious.
#   4. TRACE PLOTS for TRACE.CITY: each part and their combination, one colour per chain
#
# Draws: the raw MCMC (mcmc cache), last MIXING.BURN.KEEP of each chain, the same as the mixing report,
# and the same R-hat and ESS functions (mcmc/mcmc_helpers.R).
#
# HOW TO READ IT (column verdict in the combinations file)
#   IDENTIFIED   the combination mixes (R-hat <= 1.1) although its parts do not (R-hat > RHAT.FAIL):
#                the data identify the combination only -> identifiability problem. Outputs that depend only on
#                such combinations are robust; a reparameterization (e.g. fix a reference multiplier) would fix mixing
#   PARTLY       the combination's R-hat is much lower than its parts' but still > 1.1: part of the trade-off
#                is captured; the ridge also involves other parameters
#   NOT          the combination does not mix better than its parts: not this trade-off
#
# MEMORY: one city's MCMC (from the mcmc cache, without simulations) at a time, removed before the next.
#   No simulation is run. Build the cache first if needed (mcmc/build_mcmc_cache.R).
#
# USAGE (from jheem_analyses)
#   1. CALIBRATION.CODE and MIXING.BURN.KEEP are set in diagnostics_setup.R
#   2. source("applications/SHIELD/analysis/diagnostics/deepDive/05_transmission_identifiability.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   identifiability_failing_parameters_<code>_<date>.csv   city x parameter with R-hat > RHAT.FAIL
#   identifiability_combinations_<code>_<date>.csv         city x designed combination
#   identifiability_pairs_<code>_<date>.csv                city x best data-driven pairs
#   identifiability_traces_<TRACE.CITY>_<code>_<date>.png  trace plots
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
library(ggplot2)


## SETTINGS ----
CITIES      <- NULL          # NULL = all 10; or e.g. c("LA")
RHAT.FAIL   <- 2             # "failing" parameter: R-hat above this
RHAT.OK     <- 1.1           # a combination "mixes" at or below this
TOP.PAIRS   <- 15            # data-driven pairs listed per city
TRACE.CITY  <- "LA"
# Trace plots: each entry = the parts of one combination (multiplied together)
TRACE.COMBOS <- list(
    c("global.transmission.rate.het", "transmission.rate.multiplier.heterosexual2022"),
    c("global.transmission.rate.msm", "transmission.rate.multiplier.msm2022"),
    c("global.transmission.rate.het", "transmission.rate.multiplier.other"),
    c("global.transmission.rate.msm", "transmission.rate.multiplier.age24.msm"))
# Trace plots of designed combinations (names as in the combinations file), shown in the last column.
# A product cancels only one trade-off; the level and the shapes cancel all of them, so show both.
TRACE.DESIGNED <- c("level.het", "shape.het.2022", "level.msm", "shape.race.other")
TRACE.POINTS <- 500          # saved draws per chain shown in the trace plots (evenly spaced)

# Transmission parameters (for the failing-parameter flag and the data-driven pairs)
TRANSMISSION.PATTERN <- "transmission|pairings|el\\.rel\\.secondary|prop\\.early\\.latent|age\\.mixing"

# Model structure: the multipliers that multiply each global rate
KNOTS <- c("1970", "1990", "1995", "2000", "2010", "2022")
RACES <- c("black", "hispanic", "other")
AGES  <- c("14", "19", "24", "29", "34", "39", "44", "49", "54", "64", "65")
GROUPS <- list(
    msm = list(global = "global.transmission.rate.msm",
               time = paste0("transmission.rate.multiplier.msm", KNOTS),
               race = paste0("transmission.rate.multiplier.", RACES),
               age  = paste0("transmission.rate.multiplier.age", AGES, ".msm")),
    het = list(global = "global.transmission.rate.het",
               time = paste0("transmission.rate.multiplier.heterosexual", KNOTS),
               race = paste0("transmission.rate.multiplier.", RACES),
               age  = paste0("transmission.rate.multiplier.age", AGES, ".heterosexual")))


## FOLDERS ----
LOCS <- if (is.null(CITIES)) SHIELD.TEN.MSAS else SHIELD.TEN.MSAS[CITIES]
if (anyNA(LOCS)) stop("Unknown city in CITIES: ", paste(CITIES[is.na(LOCS)], collapse = ", "))
DEEP.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE, "deepDive")
dir.create(DEEP.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## COLUMN DESCRIPTIONS ----
# First two rows of each CSV: c(what it is, how to read it). Edit the text here, not in the CSVs.
COLUMN.NOTES <- list(
    city        = c("MSA name", "One city per block of rows"),
    parameter   = c("Calibrated parameter", "-"),
    rhat        = c("Rank-normalized split R-hat of the parameter (same function and draws as the mixing report)",
                    paste0("Listed only if > RHAT.FAIL (", RHAT.FAIL, ")")),
    ess         = c("Bulk ESS of the parameter across chains", "Independent draws the 10,000 saved draws are worth"),
    transmission = c("TRUE if the parameter is part of transmission (matches TRANSMISSION.PATTERN)",
                     "TRUE rows are the ones combined in the other two files"),
    combination = c("Name of the combination. product: global x multiplier; level: overall rate level for MSM or het; shape: multiplier relative to its group",
                    "product.het.2022 = het rate at the 2022 knot (global x multiplier); level.msm = overall MSM rate; shape.het.2022 = 2022 knot relative to the other het knots; shape.race.black = black relative to the other races"),
    type        = c("product, level or shape", "See 'combination'"),
    parts       = c("Parameters that make up the combination (log scale: + = multiplied, - = divided)", "-"),
    rhat.combination = c("R-hat of the combination", paste0("<= ", RHAT.OK, ": the combination mixes")),
    ess.combination  = c("Bulk ESS of the combination", "Compare with min.ess.parts"),
    max.rhat.parts   = c("Highest R-hat among the parts", "How badly the parts mix"),
    min.ess.parts    = c("Lowest ESS among the parts", "-"),
    rhat.drop.share  = c("(max.rhat.parts - rhat.combination) / (max.rhat.parts - 1)",
                         "1 = the combination mixes completely; 0 = no better than its worst part; negative = worse"),
    verdict     = c(paste0("IDENTIFIED: combination R-hat <= ", RHAT.OK, " while a part has R-hat > ", RHAT.FAIL,
                           ". PARTLY: drop share >= 0.5 but R-hat > ", RHAT.OK, ". NOT: otherwise. NA: no part fails"),
                    "IDENTIFIED = the data identify only the combination: an identifiability problem, not a sampler failure"),
    parameter.1 = c("First parameter of the pair (failing transmission parameter)", "-"),
    parameter.2 = c("Second parameter of the pair", "-"),
    form        = c("product = parameter.1 x parameter.2 (log sum); ratio = parameter.1 / parameter.2 (log difference)",
                    "product mixes = they trade off (one up, the other down); ratio mixes = they move together"),
    within.chain.cor = c("Correlation of the two parameters (sampling scale) inside each chain, averaged on the Fisher-z scale",
                         "Strongly negative with a product that mixes, or strongly positive with a ratio that mixes = a two-parameter ridge")
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
# Parameter on its sampling scale: logit for proportions, log for positive rates and multipliers
to.sampling.scale <- function(x, name) {
    if (all(x > 0 & x < 1) && grepl("^(fraction|prop|prp)", name)) return(qlogis(x))
    if (all(x > 0)) return(log(x))
    x
}
# iteration x chain matrix of one variable from s (iteration x chain x variable), on the sampling scale
draws <- function(s, v) { x <- s[, , v]; dim(x) <- dim(s)[1:2]; to.sampling.scale(x, v) }

mix.stats <- function(x) c(rhat = .rhat.rank.split(x), ess = .ess.bulk(x))

# Combination on the log scale: sum of coef x log(part)
combine <- function(s, parts, coefs) {
    out <- 0
    for (i in seq_along(parts)) out <- out + coefs[i] * log(s[, , parts[i]])
    out
}

verdict <- function(r.comb, r.parts) {
    if (max(r.parts) <= RHAT.FAIL) return("NA (no part fails)")
    drop <- (max(r.parts) - r.comb) / (max(r.parts) - 1)
    if (r.comb <= RHAT.OK) "IDENTIFIED" else if (drop >= 0.5) "PARTLY" else "NOT"
}

# Designed combinations for one city: list of (type, parts, coefs). Multipliers that are not calibrated
# in this city (not in the MCMC) are left out.
designed.combinations <- function(vars) {
    out <- list()
    add <- function(name, type, parts, coefs) out[[name]] <<- list(type = type, parts = parts, coefs = coefs)
    for (sx in names(GROUPS)) {
        g <- lapply(GROUPS[[sx]], function(v) v[v %in% vars])
        if (length(g$global) == 0) next
        labels <- list(time = sub(".*multiplier\\.(msm|heterosexual)", "", g$time),
                       race = sub(".*multiplier\\.", "", g$race),
                       age  = paste0("age", sub("\\.(msm|heterosexual)$", "", sub(".*multiplier\\.age", "", g$age))))
        # products: global x each multiplier
        for (grp in c("time", "race", "age")) for (j in seq_along(g[[grp]]))
            add(paste0("product.", sx, ".", labels[[grp]][j]), "product", c(g$global, g[[grp]][j]), c(1, 1))
        # level: log global + mean log multiplier of each group
        grps <- Filter(length, g[c("time", "race", "age")])
        add(paste0("level.", sx), "level", c(g$global, unlist(grps)),
            c(1, unlist(lapply(grps, function(v) rep(1 / length(v), length(v))))))
        # shapes: each multiplier relative to the mean of its group (race is shared, so done once below)
        for (grp in c("time", "age")) {
            members <- g[[grp]]
            if (length(members) < 2) next
            for (j in seq_along(members)) {
                coefs <- rep(-1 / length(members), length(members)); coefs[j] <- coefs[j] + 1
                add(paste0("shape.", sx, ".", labels[[grp]][j]), "shape", members, coefs)
            }
        }
    }
    race <- GROUPS$msm$race[GROUPS$msm$race %in% vars]
    if (length(race) >= 2) for (j in seq_along(race)) {
        coefs <- rep(-1 / length(race), length(race)); coefs[j] <- coefs[j] + 1
        add(paste0("shape.race.", sub(".*multiplier\\.", "", race[j])), "shape", race, coefs)
    }
    out
}
# e.g. "+global.transmission.rate.het +0.167*transmission.rate.multiplier.heterosexual1970 ..."
parts.label <- function(parts, coefs)
    paste0(ifelse(coefs > 0, "+", "-"), ifelse(abs(abs(coefs) - 1) < 1e-9, "", paste0(round(abs(coefs), 3), "*")),
           parts, collapse = " ")


## RUN, ONE CITY AT A TIME ----
fail.rows <- list(); comb.rows <- list(); pair.rows <- list(); trace.rows <- list()

for (city in names(LOCS)) {
    loc <- LOCS[[city]]
    cat("==", city, "\n")
    m <- load.mcmc(loc, CALIBRATION.CODE, n.cores = 1)[[1]]
    if (is.null(m)) { cat("  no MCMC for", city, "- skipped\n"); next }
    s <- .get.samples(.burn.mcmc(m, MIXING.BURN.KEEP))       # iteration x chain x variable
    vars <- dimnames(s)[[3]]

    ## 1. failing parameters ----
    st <- t(sapply(vars, function(v) mix.stats(draws(s, v))))
    rhat.of <- setNames(st[, "rhat"], vars); ess.of <- setNames(st[, "ess"], vars)
    failing <- vars[!is.na(rhat.of) & rhat.of > RHAT.FAIL]
    trans   <- grepl(TRANSMISSION.PATTERN, failing)
    if (length(failing))
        fail.rows[[city]] <- data.frame(city = city, parameter = failing, rhat = rhat.of[failing], ess = ess.of[failing],
                                        transmission = trans, stringsAsFactors = FALSE)
    cat("  ", length(failing), "parameters with R-hat >", RHAT.FAIL, "(", sum(trans), "transmission )\n")

    ## 2. designed combinations ----
    for (nm in names(cb <- designed.combinations(vars))) {
        c1 <- cb[[nm]]
        ms <- mix.stats(combine(s, c1$parts, c1$coefs))
        rp <- rhat.of[c1$parts]
        comb.rows[[length(comb.rows) + 1]] <- data.frame(
            city = city, combination = nm, type = c1$type, parts = parts.label(c1$parts, c1$coefs),
            rhat.combination = ms[["rhat"]], ess.combination = ms[["ess"]],
            max.rhat.parts = max(rp), min.ess.parts = min(ess.of[c1$parts]),
            rhat.drop.share = (max(rp) - ms[["rhat"]]) / (max(rp) - 1),
            verdict = verdict(ms[["rhat"]], rp), stringsAsFactors = FALSE)
    }

    ## 3. data-driven pairs among failing transmission parameters ----
    ft <- failing[trans]
    if (length(ft) >= 2) {
        X  <- lapply(setNames(nm = ft), function(v) draws(s, v))
        pr <- combn(ft, 2, simplify = FALSE)
        res <- do.call(rbind, lapply(pr, function(p) {
            a <- X[[p[1]]]; b <- X[[p[2]]]
            z <- mean(sapply(seq_len(ncol(a)), function(k) atanh(max(min(cor(a[, k], b[, k]), 0.999999), -0.999999))))
            rbind(data.frame(parameter.1 = p[1], parameter.2 = p[2], form = "product", rhat.combination = .rhat.rank.split(a + b),
                             within.chain.cor = tanh(z), stringsAsFactors = FALSE),
                  data.frame(parameter.1 = p[1], parameter.2 = p[2], form = "ratio",   rhat.combination = .rhat.rank.split(a - b),
                             within.chain.cor = tanh(z), stringsAsFactors = FALSE))
        }))
        res$max.rhat.parts  <- pmax(rhat.of[res$parameter.1], rhat.of[res$parameter.2])
        res$rhat.drop.share <- (res$max.rhat.parts - res$rhat.combination) / (res$max.rhat.parts - 1)
        res <- res[order(res$rhat.combination), ][seq_len(min(TOP.PAIRS, nrow(res))), ]
        pair.rows[[city]] <- cbind(city = city, res, stringsAsFactors = FALSE)
    }

    ## 4. trace data ----
    if (city == TRACE.CITY) {
        it <- unique(round(seq(1, dim(s)[1], length.out = min(TRACE.POINTS, dim(s)[1]))))
        for (cbn in TRACE.COMBOS) {
            if (!all(cbn %in% vars)) { cat("  trace: skipped", paste(cbn, collapse = " x "), "(not in the MCMC)\n"); next }
            label <- paste(sub("transmission.rate.multiplier.", "mult.", sub("global.transmission.rate.", "global.", cbn)), collapse = " x ")
            series <- c(setNames(lapply(cbn, function(v) log(s[, , v])), cbn),
                        setNames(list(combine(s, cbn, rep(1, length(cbn)))), "PRODUCT"))
            for (nm in names(series)) {
                x <- series[[nm]]; r <- .rhat.rank.split(x)
                trace.rows[[length(trace.rows) + 1]] <- data.frame(
                    combo = label, panel = factor(if (nm == "PRODUCT") "product" else paste0("part ", match(nm, cbn)),
                                                  levels = c(paste0("part ", seq_along(cbn)), "product")),
                    title = sprintf("%s  (R-hat %.2f)", if (nm == "PRODUCT") "log product" else paste0("log ", nm), r),
                    iteration = rep(it, times = ncol(x)), chain = factor(rep(seq_len(ncol(x)), each = length(it))),
                    value = as.vector(x[it, ]), stringsAsFactors = FALSE)
            }
        }
        for (nm in TRACE.DESIGNED) {
            if (is.null(cb[[nm]])) { cat("  trace: skipped", nm, "(not built for this city)\n"); next }
            x <- combine(s, cb[[nm]]$parts, cb[[nm]]$coefs); r <- .rhat.rank.split(x)
            trace.rows[[length(trace.rows) + 1]] <- data.frame(
                combo = paste0(nm, " (", length(cb[[nm]]$parts), " parameters)"), panel = factor("product", levels = c("part 1", "part 2", "product")),
                title = sprintf("%s  (R-hat %.2f; worst part %.2f)", nm, r, max(rhat.of[cb[[nm]]$parts])),
                iteration = rep(it, times = ncol(x)), chain = factor(rep(seq_len(ncol(x)), each = length(it))),
                value = as.vector(x[it, ]), stringsAsFactors = FALSE)
        }
    }

    # free this city's MCMC before the next
    key <- .mcmc.key(loc, CALIBRATION.CODE); if (exists("mcmc.list") && !is.null(mcmc.list[[key]])) mcmc.list[[key]] <- NULL
    rm(m, s); invisible(gc())
}


## SAVE ----
f <- function(what, ext = "csv") file.path(DEEP.DIR, paste0("identifiability_", what, "_", CALIBRATION.CODE, "_", STAMP, ".", ext))
fail.tab <- do.call(rbind, fail.rows); rownames(fail.tab) <- NULL
comb.tab <- do.call(rbind, comb.rows); rownames(comb.tab) <- NULL
pair.tab <- do.call(rbind, pair.rows); rownames(pair.tab) <- NULL
if (!is.null(fail.tab)) write.annotated.csv(fail.tab[order(fail.tab$city, -fail.tab$rhat), ], f("failing_parameters"))
if (!is.null(comb.tab)) write.annotated.csv(comb.tab, f("combinations"))
if (!is.null(pair.tab)) write.annotated.csv(pair.tab[, c("city", "parameter.1", "parameter.2", "form", "within.chain.cor",
                                                         "rhat.combination", "max.rhat.parts", "rhat.drop.share")], f("pairs"))

if (length(trace.rows)) {
    td <- do.call(rbind, trace.rows)
    td$combo <- factor(td$combo, levels = unique(td$combo))
    titles <- unique(td[, c("combo", "panel", "title")])
    p <- ggplot(td, aes(x = iteration, y = value, colour = chain)) +
        geom_line(linewidth = 0.25, alpha = 0.8) +
        geom_text(data = titles, aes(x = -Inf, y = Inf, label = title), inherit.aes = FALSE, hjust = -0.02, vjust = 1.3, size = 2.5) +
        facet_grid(combo ~ panel, scales = "free_y") +
        scale_colour_brewer(palette = "Dark2") +
        labs(title = paste0("Trace plots, ", TRACE.CITY, ": parts and their product (", CALIBRATION.CODE, ")"),
             subtitle = paste0("Log scale; kept part of each chain (MIXING.BURN.KEEP = ", MIXING.BURN.KEEP,
                               "). If the parts wander but the product is flat and the chains overlap, only the product is identified."),
             x = "Saved draw (kept part of the chain)", y = "log value", colour = "Chain") +
        theme_bw(base_size = 9) + theme(legend.position = "bottom", strip.text.y = element_text(size = 7))
    ggsave(f(paste0("traces_", TRACE.CITY), "png"), p, width = 13, height = 2.4 * length(levels(td$combo)) + 1.2, dpi = 200)
}


## PRINT ----
if (!is.null(comb.tab)) {
    cat("\n== Designed combinations: verdict counts by type (all cities) ==\n")
    print(table(comb.tab$type, comb.tab$verdict))
    cat("\n== Median R-hat by type and city: combination vs its worst part ==\n")
    agg <- aggregate(cbind(rhat.combination, max.rhat.parts) ~ city + type, data = comb.tab, FUN = median)
    print(agg[order(agg$type, agg$city), ], row.names = FALSE, digits = 3)
}
cat("\nSaved in ", DEEP.DIR, "\n", sep = "")
