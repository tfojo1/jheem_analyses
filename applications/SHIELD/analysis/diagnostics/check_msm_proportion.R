# ****************************************************************************************************
# CHECK MSM SHARE OF MEN: MODEL VS BRFSS / EMORY ----
# ****************************************************************************************************
# How the model sets the MSM share (the same in SHIELD and EHE):
# 1. starting share = Emory 2013 county estimate (total), split by race using BRFSS race ratios
#    from the overlapping states (get.best.guess.msm.proportions)
# 2. each race is multiplied by a calibrated multiplier (<race>.proportion.msm.of.male.mult, prior sd 0.125*log(2))
# 3. the share is fixed in time (functional form 2010 -> 2010); male births are split at the same share
# 4. no likelihood targets the MSM population, so only the prior keeps the multipliers near 1
#
# What this script does, for each city:
# 1. model: MSM / all men aged 15+, by race and total, from the "population" outcome (median + 95% interval across sims)
# 2. BRFSS: proportion MSM by race and total, for the states the city overlaps (averaged across states), 2014-2024
#    the model's "other" is everyone not Black or Hispanic; we compare it to BRFSS "white"
# 3. Emory: MSA total in 2013 (the year the model's starting value comes from)
# 4. plot: one row per city, one column per race
# 5. table: model / data, and the calibrated multipliers
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
# Run from jheem_analyses.
# ****************************************************************************************************

## SETTINGS ----
CALIBRATION.CODE <- "calib.10.15.stage3"
LOCATIONS        <- NULL       # NULL = all 10 cities; or e.g. c(Baltimore = "C.12580")
YEARS            <- 2000:2030  # model years to plot

## SOURCES ----
# only source what is not already loaded, so re-running in the same session is quick
if (!exists("SURVEILLANCE.MANAGER"))  source("../jheem_analyses/applications/SHIELD/shield_source_code.R")
if (!exists("SHIELD.SPECIFICATION"))  source("../jheem_analyses/applications/SHIELD/shield_specification.R")
if (!exists("SHIELD.TEN.MSAS"))       source("../jheem_analyses/commoncode/locations_of_interest.R")
if (!exists("load.calib.simsets"))    source("../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R")
library(ggplot2)
if (is.null(LOCATIONS)) LOCATIONS <- SHIELD.TEN.MSAS

## LOAD SIMSETS ----
calib.simsets <- load.calib.simsets(locations         = LOCATIONS,
                                    calibration.codes = CALIBRATION.CODE)

## men.15plus ----
# men aged 15+ in the given sex groups, as year x race x sim
men.15plus <- function(simset, sex) {
    x <- simset$get("population", keep.dimensions = c("year", "race"),
                    dimension.values = list(year = YEARS, age = ADULT.AGES, sex = sex))
    aperm(x, c("year", "race", setdiff(names(dimnames(x)), c("year", "race"))))
}

## model.msm.share ----
# share of men aged 15+ who are MSM: median and 95% interval across sims, by race and total
model.msm.share <- function(simset) {
    msm <- men.15plus(simset, "msm")
    men <- men.15plus(simset, c("msm", "heterosexual_male"))
    shares <- list(black    = msm[, "black", ]    / men[, "black", ],
                   hispanic = msm[, "hispanic", ] / men[, "hispanic", ],
                   other    = msm[, "other", ]    / men[, "other", ],
                   total    = apply(msm, c(1, 3), sum) / apply(men, c(1, 3), sum))
    do.call(rbind, lapply(names(shares), function(r) {
        q <- apply(shares[[r]], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)   # 3 x year
        data.frame(year = as.numeric(colnames(q)), race = r, lo = q[1, ], median = q[2, ], hi = q[3, ])
    }))
}

## BRFSS.RACE ----
# model race -> BRFSS race
BRFSS.RACE <- c(black = "black", hispanic = "hispanic", other = "white")

## data.msm.share ----
# BRFSS (states the city overlaps, averaged across states) by race and total, plus Emory MSA total in 2013
data.msm.share <- function(location) {
    states  <- locations::get.overlapping.locations(location, "state")
    by.race <- SURVEILLANCE.MANAGER$pull("proportion.msm", keep.dimensions = c("year", "location", "race"),
                                         dimension.values = list(location = states), sources = "brfss")
    total   <- SURVEILLANCE.MANAGER$pull("proportion.msm", keep.dimensions = c("year", "location"),
                                         dimension.values = list(location = states), sources = "brfss")
    by.race <- apply(by.race, c("year", "race"), mean, na.rm = TRUE)   # year x race
    total   <- apply(total, "year", mean, na.rm = TRUE)
    #
    brfss <- rbind(
        do.call(rbind, lapply(names(BRFSS.RACE), function(r)
            data.frame(year = as.numeric(rownames(by.race)), race = r, value = by.race[, BRFSS.RACE[[r]]]))),
        data.frame(year = as.numeric(names(total)), race = "total", value = total))
    brfss$source <- "BRFSS (state)"
    #
    emory <- SURVEILLANCE.MANAGER$pull("proportion.msm", keep.dimensions = c("year", "location"),
                                       dimension.values = list(location = location, year = "2013"), sources = "emory")
    emory <- data.frame(year = 2013, race = "total", value = as.numeric(emory), source = "Emory (MSA)")
    #
    rv <- rbind(brfss, emory)
    rv[is.finite(rv$value), ]
}

## RUN ----
# a city that fails is reported and skipped
entries <- Filter(function(e) e$calib.code == CALIBRATION.CODE, calib.simsets)
model <- list(); obs <- list(); mults <- list()
for (e in entries) {
    res <- tryCatch(list(model = model.msm.share(e$full_simset),
                         obs   = data.msm.share(e$location.code)),
                    error = function(err) { cat("SKIPPED", e$location.name, ":", conditionMessage(err), "\n"); NULL })
    if (is.null(res)) next
    model[[e$location.name]] <- cbind(city = e$location.name, res$model)
    obs[[e$location.name]]   <- cbind(city = e$location.name, res$obs)
    # calibrated multipliers (median across sims); 1 = starting value unchanged
    p <- e$full_simset$parameters
    mults[[e$location.name]] <- sapply(names(BRFSS.RACE), function(r) median(p[paste0(r, ".proportion.msm.of.male.mult"), ]))
}
model <- do.call(rbind, model)
obs   <- do.call(rbind, obs)

## PLOT ----
# line + band = model median and 95% interval; points = data
p <- ggplot() +
    geom_ribbon(data = model, aes(x = year, ymin = 100 * lo, ymax = 100 * hi), fill = "steelblue", alpha = 0.25) +
    geom_line(data = model, aes(x = year, y = 100 * median), colour = "steelblue") +
    geom_point(data = obs, aes(x = year, y = 100 * value, colour = source, shape = source)) +
    facet_grid(city ~ race, scales = "free_y") +
    labs(x = NULL, y = "% of men aged 15+ who are MSM", colour = NULL, shape = NULL,
         title = paste("MSM share of men, model vs data:", CALIBRATION.CODE),
         subtitle = "Model 'other' is compared to BRFSS 'white'. BRFSS is state level; Emory is MSA level.") +
    theme_bw() + theme(legend.position = "bottom")
print(p)

## TABLE ----
# model median / data, averaged over the years with data; 1 = the same, > 1 = the model has more MSM
# 1. BRFSS: all BRFSS years (2014-2024)
# 2. Emory: 2013 only (total)
ratio.table <- do.call(rbind, lapply(split(obs, list(obs$city, obs$race, obs$source), drop = TRUE), function(d) {
    m <- model[model$city == d$city[1] & model$race == d$race[1] & model$year %in% d$year, ]
    data.frame(city = d$city[1], race = d$race[1], source = d$source[1],
               model = round(100 * mean(m$median), 2), data = round(100 * mean(d$value), 2),
               model.over.data = round(mean(m$median) / mean(d$value), 2))
}))
rownames(ratio.table) <- NULL
ratio.table <- ratio.table[order(ratio.table$source, ratio.table$race, ratio.table$city), ]
cat("\n== MSM share of men (%), model vs data ==\n")
print(ratio.table, row.names = FALSE)

cat("\n== Calibrated multipliers on the starting MSM share (median; 1 = unchanged) ==\n")
print(round(do.call(rbind, mults), 3))

## SAVE ----
out.dir <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out.dir, paste0("msm_share_", CALIBRATION.CODE, ".pdf")), p,
       width = 11, height = 2.2 * length(unique(model$city)), limitsize = FALSE)
write.csv(ratio.table, file.path(out.dir, paste0("msm_share_", CALIBRATION.CODE, ".csv")), row.names = FALSE)
