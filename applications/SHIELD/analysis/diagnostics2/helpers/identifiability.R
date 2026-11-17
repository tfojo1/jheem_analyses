# ****************************************************************************************************
# DIAGNOSTICS2: IDENTIFIABILITY (METHODS 5) ----
# ****************************************************************************************************
# Do combinations of parameters mix when the parameters themselves do not?
# Functions only; uses rhat.rank.split(), ess.bulk() and one.variable() from mixing_stats.R.
# Copied from analysis/diagnostics/deepDive/05_transmission_identifiability.R (version 2); the combinations,
# the pair search and the verdict rules are unchanged.
#
# Contents
#   1. Settings and model structure
#   2. Small steps         to.sampling.scale(), draws(), combine(), combination.verdict()
#   3. Designed            designed.combinations()   level, shape, product + race.mixing, natural.history,
#                                                    bridging, diagnosis
#   4. One city            identifiability.check()   designed combinations, data-driven pairs, best partner
# ****************************************************************************************************

## 1. SETTINGS AND MODEL STRUCTURE (METHODS 5.2, 5.5, 5.7) ----
IDENT.RHAT.FAIL <- 2      # a part "fails" above this
IDENT.RHAT.OK   <- 1.1    # a combination "mixes" at or below this
IDENT.PAIR.RHAT <- 1.5    # data-driven pairs: every parameter with R-hat above this

KNOTS <- c("1970", "1990", "1995", "2000", "2010", "2022")
RACES <- c("black", "hispanic", "other")
AGES  <- c("14", "19", "24", "29", "34", "39", "44", "49", "54", "64", "65")
TRANSMISSION.GROUPS <- list(
    msm = list(global = "global.transmission.rate.msm",
               time   = paste0("transmission.rate.multiplier.msm", KNOTS),
               race   = paste0("transmission.rate.multiplier.", RACES),
               age    = paste0("transmission.rate.multiplier.age", AGES, ".msm")),
    het = list(global = "global.transmission.rate.het",
               time   = paste0("transmission.rate.multiplier.heterosexual", KNOTS),
               race   = paste0("transmission.rate.multiplier.", RACES),
               age    = paste0("transmission.rate.multiplier.age", AGES, ".heterosexual")))
RACE.MIXING     <- setNames(paste0(RACES, ".", RACES, ".sexual.multi"), RACES)
NATURAL.HISTORY <- c(el.rel = "el.rel.secondary.transmissibility", relapse = "prop.early.latent.to.secondary")
BRIDGING        <- c("oe.female.pairings.with.msm", "fraction.msm.pairings.with.female",
                     "fraction.heterosexual_male.pairings.with.male")
DIAGNOSIS       <- list(c("prp.symptomatic.primary.msm", "or.careseeking.symptomatic.ps.msm"),
                        c("prp.symptomatic.secondary",   "or.careseeking.symptomatic.ps.msm"),
                        c("prp.symptomatic.primary.msm", "prp.symptomatic.secondary"))


## 2. SMALL STEPS ----

# METHODS 5.3: logit for proportions (by name), log for positive values
to.sampling.scale <- function(x, name) {
    if (all(x > 0 & x < 1) && grepl("^(fraction|prop|prp)", name)) return(qlogis(x))
    if (all(x > 0)) return(log(x))
    x
}
draws <- function(s, v) to.sampling.scale(one.variable(s, v), v)

# sum of coefficient x part, on the sampling scale (= log of a product or ratio)
combine <- function(s, parts, coefs) {
    out <- 0
    for (i in seq_along(parts)) out <- out + coefs[i] * draws(s, parts[i])
    out
}

# METHODS 5.7
drop.share <- function(r.comb, r.worst.part) (r.worst.part - r.comb) / (r.worst.part - 1)
combination.verdict <- function(r.comb, r.worst.part) {
    if (r.worst.part <= IDENT.RHAT.FAIL) return("NA")
    if (r.comb <= IDENT.RHAT.OK) "IDENTIFIED" else if (drop.share(r.comb, r.worst.part) >= 0.5) "PARTLY" else "NOT"
}

# "+global.transmission.rate.het +0.167*transmission.rate.multiplier.heterosexual1970 ..."
parts.label <- function(parts, coefs)
    paste0(ifelse(coefs > 0, "+", "-"), ifelse(abs(abs(coefs) - 1) < 1e-9, "", paste0(round(abs(coefs), 3), "*")),
           parts, collapse = " ")


## 3. DESIGNED COMBINATIONS (METHODS 5.4, 5.5) ----
# list of combinations, each = list(type, parts, coefs). Parameters not in this city's MCMC are left out.

# coefficients of a shape: member j minus the mean of the group
shape.coefs <- function(n, j) { coefs <- rep(-1 / n, n); coefs[j] <- coefs[j] + 1; coefs }

designed.combinations <- function(vars) {
    out <- list()
    # units (optional): the building blocks whose R-hat counts as "the parts" in the verdict, when a part is
    # itself a combination (e.g. a level). Without units, each parameter is a part.
    add <- function(name, type, parts, coefs, units = NULL)
        out[[name]] <<- list(type = type, parts = parts, coefs = coefs, units = units)
    # product and ratio of two parameters, if both are in the MCMC
    both <- function(name, type, p1, p2) {
        if (!all(c(p1, p2) %in% vars)) return(invisible())
        add(paste0(name, ".product"), type, c(p1, p2), c(1, 1))
        add(paste0(name, ".ratio"),   type, c(p1, p2), c(1, -1))
    }

    # METHODS 5.4: product, level, shape for MSM and het
    for (sx in names(TRANSMISSION.GROUPS)) {
        g <- lapply(TRANSMISSION.GROUPS[[sx]], function(v) v[v %in% vars])
        if (length(g$global) == 0) next
        labels <- list(time = sub(".*multiplier\\.(msm|heterosexual)", "", g$time),
                       race = sub(".*multiplier\\.", "", g$race),
                       age  = paste0("age", sub("\\.(msm|heterosexual)$", "", sub(".*multiplier\\.age", "", g$age))))
        for (grp in c("time", "race", "age")) for (j in seq_along(g[[grp]]))
            add(paste0("product.", sx, ".", labels[[grp]][j]), "product", c(g$global, g[[grp]][j]), c(1, 1))
        groups <- Filter(length, g[c("time", "race", "age")])
        add(paste0("level.", sx), "level", c(g$global, unlist(groups)),
            c(1, unlist(lapply(groups, function(v) rep(1 / length(v), length(v))))))
        for (grp in c("time", "age")) if (length(g[[grp]]) >= 2) for (j in seq_along(g[[grp]]))
            add(paste0("shape.", sx, ".", labels[[grp]][j]), "shape", g[[grp]], shape.coefs(length(g[[grp]]), j))
    }
    race <- TRANSMISSION.GROUPS$msm$race[TRANSMISSION.GROUPS$msm$race %in% vars]   # shared: shapes done once
    if (length(race) >= 2) for (j in seq_along(race))
        add(paste0("shape.race.", sub(".*multiplier\\.", "", race[j])), "shape", race, shape.coefs(length(race), j))

    # METHODS 5.5: race multiplier (and race shape) x the same race's assortative mixing
    for (r in RACES) both(paste0("race.mixing.", r), "race.mixing", paste0("transmission.rate.multiplier.", r), RACE.MIXING[[r]])
    if (length(race) == 3 && all(RACE.MIXING %in% vars)) for (j in 1:3) for (sg in c(1, -1)) {
        shape <- out[[paste0("shape.race.", RACES[j])]]
        add(paste0("race.mixing.shape.", RACES[j], if (sg > 0) ".product" else ".ratio"), "race.mixing",
            c(race, RACE.MIXING[[j]]), c(shape.coefs(3, j), sg),
            units = list(shape[c("parts", "coefs")], list(parts = RACE.MIXING[[j]], coefs = 1)))
    }

    # METHODS 5.5: natural history x each global rate and each level
    for (short in names(NATURAL.HISTORY)) for (sx in names(TRANSMISSION.GROUPS)) {
        nh <- NATURAL.HISTORY[[short]]
        both(paste0("natural.history.", short, ".x.global.", sx), "natural.history", nh, TRANSMISSION.GROUPS[[sx]]$global)
        lv <- out[[paste0("level.", sx)]]
        if (!is.null(lv) && nh %in% vars) for (sg in c(1, -1))
            add(paste0("natural.history.", short, ".x.level.", sx, if (sg > 0) ".product" else ".ratio"),
                "natural.history", c(lv$parts, nh), c(lv$coefs, sg),
                units = list(lv[c("parts", "coefs")], list(parts = nh, coefs = 1)))
    }

    # METHODS 5.5: bridging with each other and with the het global rate
    for (pr in combn(BRIDGING, 2, simplify = FALSE)) both(paste0("bridging.", pr[1], ".x.", pr[2]), "bridging", pr[1], pr[2])
    for (b in BRIDGING) both(paste0("bridging.", b, ".x.global.het"), "bridging", b, TRANSMISSION.GROUPS$het$global)

    # METHODS 5.5: diagnosis
    for (pr in DIAGNOSIS) both(paste0("diagnosis.", pr[1], ".x.", pr[2]), "diagnosis", pr[1], pr[2])
    out
}


## 4. ONE CITY ----
# s = kept draws (iteration x chain x variable), from mixing.stats()$samples
# Returns: rhat.of (every parameter, sampling scale), combinations, pairs (all tested), best.partner, combos (definitions)
identifiability.check <- function(s) {
    vars <- dimnames(s)[[3]]
    rhat.of <- sapply(vars, function(v) rhat.rank.split(draws(s, v)))
    ess.of  <- sapply(vars, function(v) ess.bulk(draws(s, v)))

    # METHODS 5.4-5.5, 5.7: designed combinations
    combos <- designed.combinations(vars)
    combinations <- do.call(rbind, lapply(names(combos), function(nm) {
        cb <- combos[[nm]]
        x  <- combine(s, cb$parts, cb$coefs)
        r  <- rhat.rank.split(x)
        # worst part: each parameter, or each unit when the combination is built from units (METHODS 5.7)
        worst <- if (is.null(cb$units)) max(rhat.of[cb$parts])
                 else max(sapply(cb$units, function(u) rhat.rank.split(combine(s, u$parts, u$coefs))))
        min.ess <- if (is.null(cb$units)) min(ess.of[cb$parts])
                   else min(sapply(cb$units, function(u) ess.bulk(combine(s, u$parts, u$coefs))))
        data.frame(combination = nm, type = cb$type, parts = parts.label(cb$parts, cb$coefs),
                   rhat.combination = round(r, 3), ess.combination = round(ess.bulk(x)),
                   max.rhat.parts = round(worst, 3), min.ess.parts = round(min.ess),
                   rhat.drop.share = round(drop.share(r, worst), 3), verdict = combination.verdict(r, worst),
                   stringsAsFactors = FALSE)
    }))

    # METHODS 5.6: every pair of parameters with R-hat > IDENT.PAIR.RHAT, product and ratio
    candidates <- vars[!is.na(rhat.of) & rhat.of > IDENT.PAIR.RHAT]
    pairs <- NULL; best.partner <- NULL
    if (length(candidates) >= 2) {
        X <- lapply(setNames(nm = candidates), function(v) draws(s, v))
        pairs <- do.call(rbind, lapply(combn(candidates, 2, simplify = FALSE), function(p) {
            a <- X[[p[1]]]; b <- X[[p[2]]]
            z <- mean(sapply(seq_len(ncol(a)), function(k) atanh(max(min(cor(a[, k], b[, k]), 0.999999), -0.999999))))
            data.frame(parameter.1 = p[1], parameter.2 = p[2], form = c("product", "ratio"),
                       within.chain.cor = round(tanh(z), 3),
                       rhat.combination = round(c(rhat.rank.split(a + b), rhat.rank.split(a - b)), 3),
                       stringsAsFactors = FALSE)
        }))
        pairs$max.rhat.parts  <- round(pmax(rhat.of[pairs$parameter.1], rhat.of[pairs$parameter.2]), 3)
        pairs$rhat.drop.share <- round(drop.share(pairs$rhat.combination, pairs$max.rhat.parts), 3)
        pairs <- pairs[order(pairs$rhat.combination), ]
        rownames(pairs) <- NULL

        # best partner: the pair with the lowest combination R-hat that includes the parameter
        best.partner <- do.call(rbind, lapply(candidates, function(v) {
            r <- pairs[pairs$parameter.1 == v | pairs$parameter.2 == v, ][1, ]
            partner <- if (r$parameter.1 == v) r$parameter.2 else r$parameter.1
            data.frame(parameter = v, own.rhat = round(rhat.of[[v]], 3), best.partner = partner,
                       partner.rhat = round(rhat.of[[partner]], 3), form = r$form,
                       within.chain.cor = r$within.chain.cor, rhat.combination = r$rhat.combination,
                       rhat.drop.share = r$rhat.drop.share, stringsAsFactors = FALSE)
        }))
        best.partner <- best.partner[order(best.partner$rhat.combination), ]
        rownames(best.partner) <- NULL
    }

    list(rhat.of = rhat.of, combinations = combinations, pairs = pairs, best.partner = best.partner, combos = combos)
}
