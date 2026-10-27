# ****************************************************************************************************
# PROPORTION MSM LIKELIHOOD (SHIELD) ----
# ****************************************************************************************************
# Keeps the model's MSM population in line with the data. Two parts, both compared once (in 2013) on the log scale:
# 1. TOTAL: share of men aged 15+ who are MSM in the MSA  vs  Emory 2013 for the MSA
# 2. RACE:  Black / other and Hispanic / other ratios of that share  vs  BRFSS race ratios, adjusted to the MSA
#
# Why ratios for race, not levels:
# 1. BRFSS levels rose ~58% over 2014-2024, while the race ratios stayed about flat
# 2. Emory sets the level; BRFSS only tells us how the share differs between races
#
# How the BRFSS race ratio for an MSA is built (get.prop.msm.targets):
# 1. each state-year: log(race share / other share); "other" = white, asian, NHPI and AIAN, averaged by number of men
# 2. each state: mean over its years
# 3. blend each state with the national mean: weight on the state = n.years / (n.years + k)
#    k = year-to-year noise / real state-to-state spread, both estimated from all states
# 4. remove the urban bias: divide by how much the state ratio is raised just by where each race lives
#    (Emory 2013 county share, averaged over the counties each race lives in, state-wide)
# 5. multi-state MSAs: average the states, weighted by the MSA's men living in each
# 6. add back the same effect inside the MSA's own counties (the model builds its population county by county)
#
# Differences from EHE's proportion.msm likelihood (applications/EHE/ehe_likelihoods.R):
# 1. log densities: dnorm(..., log = TRUE)
# 2. Emory 2013 only (Emory 2020 uses a different denominator)
# 3. the race part works for MSAs (EHE's only gets data for states) and compares ratios, one value per race
# 4. applies the weights it is given
#
# Not added to any stage yet. To use it, source this file after shield_likelihoods.R and add it to a stage, e.g.:
#   join.likelihood.instructions(proportion.msm.likelihood.instructions.shield,
#                                additional.weights = untempered(1, STAGE.2.WEIGHT))
# To see the targets for one city:  get.prop.msm.targets("C.37980")$steps
# ****************************************************************************************************

## SETTINGS ----
PROP.MSM.YEAR          <- "2013"   # Emory year; the model is compared in the same year
PROP.MSM.TOTAL.LOG.SD  <- 0.1      # total: about +/-20% (95%) around Emory
PROP.MSM.RACE.EXTRA.SD <- 0.2      # race: added on top of the BRFSS uncertainty (identity vs behavior, urban assumption)
# BRFSS races that make up the model's "other" -> matching census race names
BRFSS.OTHER.RACES <- c("white"                                  = "white",
                       "asian"                                  = "asian",
                       "native hawaiian/other pacific islander" = "native hawaiian and other pacific islander",
                       "american indian/alaska native"          = "american indian and alaska native")

## .prop.msm.men ----
# men in PROP.MSM.YEAR, one row per county: non-Hispanic census races + hispanic
.prop.msm.men <- function(counties) {
    pop <- SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity__sex
    counties <- intersect(counties, dimnames(pop)$location)
    x <- pop[PROP.MSM.YEAR, counties, , , "male", drop = FALSE]                    # year x county x race x ethnicity x sex
    cbind(apply(x[, , , "not hispanic", , drop = FALSE], c(2, 3), sum),          # county x census race
          hispanic = apply(x[, , , "hispanic", , drop = FALSE], 2, sum))
}

## .prop.msm.kbar ----
# Emory share averaged over the counties where each model race lives (weights = men of that race in each county)
.prop.msm.kbar <- function(counties) {
    k <- SURVEILLANCE.MANAGER$data$proportion.msm$estimate$emory$emory$year__location[PROP.MSM.YEAR, ]
    M <- .prop.msm.men(counties)
    M <- cbind(black = M[, "black"], hispanic = M[, "hispanic"],
               other = rowSums(M[, setdiff(colnames(M), c("black", "hispanic")), drop = FALSE]))
    k  <- k[rownames(M)]
    ok <- is.finite(k)
    colSums(M[ok, , drop = FALSE] * k[ok]) / colSums(M[ok, , drop = FALSE])
}

## .prop.msm.brfss ----
# BRFSS log race ratios for every state (black and hispanic, each vs other), plus the national mean,
# the year-to-year noise and the real state-to-state spread. Worked out once and kept.
.prop.msm.brfss <- local({
    cache <- NULL
    function() {
        if (!is.null(cache)) return(cache)
        b <- SURVEILLANCE.MANAGER$data$proportion.msm$estimate$brfss$brfss.msm$year__location__race   # year x state x BRFSS race
        states <- intersect(c(state.abb, "DC"), dimnames(b)$location)
        men <- t(sapply(states, function(s) colSums(.prop.msm.men(locations::get.contained.locations(s, "county")))))
        # BRFSS "other" for one state-year: white, asian, NHPI, AIAN averaged by men of each race (needs white)
        other.share <- function(y, s) {
            p <- b[y, s, names(BRFSS.OTHER.RACES)]
            if (!is.finite(p[["white"]])) return(NA)
            w <- men[s, BRFSS.OTHER.RACES]
            ok <- is.finite(p)
            sum(p[ok] * w[ok]) / sum(w[ok])
        }
        other <- outer(dimnames(b)$year, states, Vectorize(other.share))      # year x state
        cache <<- lapply(c(black = "black", hispanic = "hispanic"), function(r) {
            lr <- log(b[, states, r] / other)                                  # year x state
            lr[!is.finite(lr)] <- NA
            n     <- colSums(!is.na(lr))
            means <- colMeans(lr, na.rm = TRUE)
            use   <- n >= 3
            noise.var   <- median(apply(lr[, use], 2, var, na.rm = TRUE))
            between.var <- max(var(means[use]) - mean(noise.var / n[use]), 0.01)
            list(by.state    = data.frame(state = states, n = n, mean = ifelse(n > 0, means, NA)),
                 national    = weighted.mean(means[n > 0], n[n > 0]),
                 noise.var   = noise.var,
                 between.var = between.var)
        })
        cache
    }
})

## get.prop.msm.targets ----
# Targets for one MSA:
# 1. total     = Emory 2013 share for the MSA
# 2. race      = log(Black / other) and log(Hispanic / other), built as described at the top
# 3. race.sd   = uncertainty of each race target (log scale)
# 4. steps     = one row per state and race, to see how each target was built
get.prop.msm.targets <- function(location) {
    counties <- locations::get.contained.locations(location, "county")
    states   <- unname(locations::get.overlapping.locations(location, "state"))
    total    <- SURVEILLANCE.MANAGER$data$proportion.msm$estimate$emory$emory$year__location[PROP.MSM.YEAR, location]
    if (!is.finite(total)) stop("No Emory ", PROP.MSM.YEAR, " proportion MSM for ", location)

    brfss    <- .prop.msm.brfss()
    msa.kbar <- .prop.msm.kbar(counties)
    # share of the MSA's men living in each state (only matters for multi-state MSAs)
    state.w  <- sapply(states, function(s)
        sum(.prop.msm.men(intersect(counties, locations::get.contained.locations(s, "county")))))
    state.w  <- state.w / sum(state.w)

    steps <- do.call(rbind, lapply(c("black", "hispanic"), function(r) {
        br <- brfss[[r]]
        k  <- br$noise.var / br$between.var
        do.call(rbind, lapply(states, function(s) {
            row <- br$by.state[br$by.state$state == s, ]
            n   <- if (nrow(row)) row$n else 0
            w   <- n / (n + k)                                                   # weight on the state's own value
            blended <- if (n > 0) w * row$mean + (1 - w) * br$national else br$national
            kb  <- .prop.msm.kbar(locations::get.contained.locations(s, "county"))
            data.frame(race = r, state = s, state.weight.in.msa = state.w[[s]], n.years = n,
                       state.ratio  = if (n > 0) exp(row$mean) else NA,
                       national.ratio = exp(br$national),
                       weight.on.state = w,
                       blended.ratio = exp(blended),
                       urban.factor.state = kb[[r]] / kb[["other"]],
                       adjusted.ratio = exp(blended) / (kb[[r]] / kb[["other"]]),
                       var = 1 / (n / br$noise.var + 1 / br$between.var))
        }))
    }))

    race <- sapply(c("black", "hispanic"), function(r) {
        s <- steps[steps$race == r, ]
        sum(s$state.weight.in.msa * log(s$adjusted.ratio)) + log(msa.kbar[[r]] / msa.kbar[["other"]])
    })
    race.sd <- sapply(c("black", "hispanic"), function(r) {
        s <- steps[steps$race == r, ]
        sqrt(sum(s$state.weight.in.msa * s$var) + PROP.MSM.RACE.EXTRA.SD^2)
    })
    list(total = total, race = race, race.sd = race.sd,
         urban.factor.msa = c(black = msa.kbar[["black"]] / msa.kbar[["other"]],
                              hispanic = msa.kbar[["hispanic"]] / msa.kbar[["other"]]),
         steps = steps)
}

## .prop.msm.get.data ----
# the targets, plus instructions to read men aged 15+ by race and sex in PROP.MSM.YEAR from a simulation
.prop.msm.get.data <- function(version, location) {
    targets <- get.prop.msm.targets(location)
    sim.metadata <- get.simulation.metadata(version = version, location = location)
    targets$get.instr <- sim.metadata$prepare.optimized.get.instructions(
        "population",
        keep.dimensions  = c("race", "sex"),
        dimension.values = list(year = PROP.MSM.YEAR, age = ADULT.AGES, sex = c("msm", "heterosexual_male")),
        drop.single.sim.dimension = TRUE)
    targets$total.sd <- PROP.MSM.TOTAL.LOG.SD
    targets
}

## .prop.msm.compute ----
# one simulation -> log-likelihood (or likelihood if log = FALSE)
.prop.msm.compute <- function(sim, data, log = TRUE, weights = 1) {
    pop   <- sim$optimized.get(data$get.instr)                  # race x sex
    pop   <- aperm(pop, c("race", "sex"))
    msm   <- pop[, "msm"]
    men   <- pop[, "msm"] + pop[, "heterosexual_male"]
    share <- msm / men                                          # share of men who are MSM, by race
    model.total <- sum(msm) / sum(men)
    model.race  <- log(share[c("black", "hispanic")] / share[["other"]])

    ll <- dnorm(log(model.total), log(data$total), data$total.sd, log = TRUE) +
        sum(dnorm(model.race, data$race[c("black", "hispanic")], data$race.sd[c("black", "hispanic")], log = TRUE))
    ll <- weights * ll
    if (log) ll else exp(ll)
}

## proportion.msm.likelihood.instructions.shield ----
# jheem2 versions differ:
# 1. newer (1.12.3.9000, ~/JHEEM/jheem2): compute.function must take 'weights', and the stage weights are passed in
# 2. older (1.12.0, installed 10/2026):    compute.function must NOT take 'weights', and stage weights are NOT applied
#    (the likelihood then counts with weight 1 in every stage, whatever additional.weights says)
proportion.msm.likelihood.instructions.shield <- tryCatch(
    create.custom.likelihood.instructions(name              = "proportion.msm.shield",
                                          get.data.function = .prop.msm.get.data,
                                          compute.function  = .prop.msm.compute),
    error = function(e)
        create.custom.likelihood.instructions(name              = "proportion.msm.shield",
                                              get.data.function = .prop.msm.get.data,
                                              compute.function  = function(sim, data, log) .prop.msm.compute(sim, data, log)))
