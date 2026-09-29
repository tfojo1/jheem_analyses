# ****************************************************************************************************
# SHIELD CALIBRATION — CORRELATED PARAMETER PAIRS
# ****************************************************************************************************
# Trying to discover which pairs of parameters are strongly correlated across the MCMC chains 
# and whether those correlations occur across sampling blocks.
#
# Question:Which two parameters are moving together or in opposite directions, and is this pattern 
# happening consistently across cities?
#
# The main idea is that if two parameters have a strong correlation, the data may be identifying 
# their combination rather than each parameter separately.
# For example:    transmission=  global transmission×  time multiplier
# If increasing the global transmission parameter is consistently accompanied by decreasing the 
# time multiplier, you might see:        r = -0.9    That suggests a strong trade-off

# Output: ONE table, one row per parameter pair, across all cities:
#   param.1, param.2      the pair
#   same.block            TRUE if both are proposed together in one sampling block
#   block.1, block.2      the sampling blocks
#   n.cities.strong       number of cities with |r.within| > R.FLAG
#   median.r.within       median correlation across cities (sign: - = trade off, + = move together)
#   min.r.within, max.r.within
#   cities.strong         which cities
#
# r.within = correlation computed INSIDE each chain, averaged over chains (Fisher z).
#   Pooling chains is NOT used: the chains sit in different places, so a pooled correlation mostly
#   reflects where the 4 chains ended up, not a real trade-off.
# STEPS:
### 1. Loads the MCMC results
### 2. Transforms the parameters
# Correlations use log values for positive rates/multipliers and logit values for proportions.
# That's important because a correlation on the raw scale might look different from the correlation on the sampling scale. 
### 3. Calculates correlations within each chain
# For every possible pair of parameters, calculate r.within each chain and then averages the correlations 
# across chains using Fisher's z transformation.
### 4. It examines EVERY pair:  174 parameters (174 × 173 / 2 = 15,051 pairs)
### 5. Then it combines results across cities
# A pair that is strongly correlated in many cities or in a single one?
### 6. It checks the sampling blocks
# sampler can learn correlations within a block, but not necessarily between parameters in different blocks.
# if two parameters are strongly correlated, they should be in put in the same block
### 7.output: head(pairs, 30)
# How to read it: pairs at the top (strong in many cities) are structural trade-offs in the model.
#   same.block = TRUE  -> the sampler can learn the trade-off, but only with enough iterations
#   same.block = FALSE -> the sampler cannot learn it; consider putting the two in one block


# ****************************************************************************************************

library(bayesian.simulations)
library(dplyr)
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")   # also sets the jheem root dir
if (!exists("SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS"))
    source("../jheem_analyses/applications/SHIELD/shield_calib_parameters.R")

# ---- SETUP ----
CALIBRATION.CODE <- "calib.9.23.stage3.pk"
LOCATIONS        <- SHIELD.TEN.MSAS
MCMC.CACHE.DIR   <- file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "mcmc_cache")  # same cache as check_mixing.R
REPORT.DIR       <- file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "reports")

R.FLAG <- 0.8    # |r.within| above this = strong trade-off

if (!exists("pair.corr.list")) pair.corr.list <- list()    # per-city results, keyed "location | calibration.code"

.mcmc.key <- function(loc, code) paste0(loc, " | ", code)
.city.name <- function(loc) {
    nm <- names(SHIELD.TEN.MSAS)[match(loc, SHIELD.TEN.MSAS)]
    ifelse(is.na(nm), loc, nm)
}


# ****************************************************************************************************
# PART 1: HELPERS
# ****************************************************************************************************

# Load one slimmed MCMC: memory (mcmc.list from check_mixing.R) -> disk cache -> assemble
.load.one.mcmc <- function(loc, calibration.code, cache.dir = MCMC.CACHE.DIR) {
    key <- .mcmc.key(loc, calibration.code)
    if (exists("mcmc.list") && !is.null(mcmc.list[[key]])) return(mcmc.list[[key]])
    file <- file.path(cache.dir, paste0(calibration.code, "_", loc, ".rds"))
    if (file.exists(file)) return(readRDS(file))
    message(.city.name(loc), ": no cache found - assembling from the calibration (slower)")
    m <- assemble.mcmc.from.calibration(version = "shield", location = loc,
                                        calibration.code = calibration.code, allow.incomplete = TRUE)
    m@simulations <- list()
    m
}

# samples as a 3D array: iteration x chain x variable (same as check_mixing.R)
.get.samples <- function(m) {
    s <- m@samples
    if (!is.null(names(dimnames(s))))
        s <- aperm(s, match(c("iteration", "chain", "variable"), names(dimnames(s))))
    s
}

# logit for proportions, log for other positive parameters
.LOGIT.PATTERN <- "^prop\\.|^prp\\.|^fraction\\.|^el\\.rel\\.secondary"
.transform.samples <- function(s) {
    for (v in dimnames(s)[[3]]) {
        x <- s[, , v]
        if (grepl(.LOGIT.PATTERN, v) && all(x > 0 & x < 1, na.rm = TRUE)) s[, , v] <- qlogis(x)
        else if (all(x > 0, na.rm = TRUE))                               s[, , v] <- log(x)
    }
    s
}

# correlation inside each chain, averaged across chains on the Fisher-z scale
# (a parameter that never moved in a chain gives NA for that chain and is skipped)
.within.chain.cor <- function(s) {
    z.list <- lapply(seq_len(dim(s)[2]), function(ch) {
        r <- suppressWarnings(cor(s[, ch, ], use = "pairwise.complete.obs"))
        atanh(pmin(pmax(r, -0.999), 0.999))
    })
    tanh(apply(simplify2array(z.list), c(1, 2), mean, na.rm = TRUE))
}

# block name for each parameter (blocks defined with `<-` inside list() have no name -> "block.<index>")
.block.lookup <- function(blocks = SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS) {
    nms <- names(blocks)
    if (is.null(nms)) nms <- rep("", length(blocks))
    nms[nms == ""] <- paste0("block.", which(nms == ""))
    stats::setNames(rep(nms, lengths(blocks)), unlist(blocks, use.names = FALSE))
}


# ****************************************************************************************************
# PART 2: ONE CITY -> all pairs with their r.within
# ****************************************************************************************************

pair.correlations <- function(m, city = "") {
    s    <- .transform.samples(.get.samples(m))
    vars <- dimnames(s)[[3]]
    r    <- .within.chain.cor(s)
    ut   <- which(upper.tri(r), arr.ind = TRUE)
    data.frame(city     = city,
               param.1  = vars[ut[, 1]],
               param.2  = vars[ut[, 2]],
               r.within = round(r[ut], 3),
               stringsAsFactors = FALSE)
}


# ****************************************************************************************************
# PART 3: ALL CITIES -> one summary table
# ****************************************************************************************************

run.correlation.check <- function(locations, calibration.code, force.recompute = FALSE) {
    locations <- unname(locations)
    for (loc in locations) {
        key <- .mcmc.key(loc, calibration.code)
        if (!force.recompute && is.data.frame(pair.corr.list[[key]])) next   # recompute anything that is not a v2 table
        m <- tryCatch(.load.one.mcmc(loc, calibration.code),
                      error = function(e) { message(loc, ": ", e$message); NULL })
        if (is.null(m)) next
        pair.corr.list[[key]] <<- pair.correlations(m, city = .city.name(loc))
        rm(m); invisible(gc())
    }
    all.pairs <- bind_rows(pair.corr.list[intersect(.mcmc.key(locations, calibration.code), names(pair.corr.list))])
    block.of  <- .block.lookup()
    
    all.pairs %>%
        filter(!is.na(r.within)) %>%
        group_by(param.1, param.2) %>%
        summarise(n.cities.strong = sum(abs(r.within) > R.FLAG),
                  n.cities        = n(),
                  median.r.within = round(median(r.within), 2),
                  min.r.within    = min(r.within),
                  max.r.within    = max(r.within),
                  cities.strong   = paste(city[abs(r.within) > R.FLAG], collapse = ", "),
                  .groups = "drop") %>%
        mutate(block.1    = unname(block.of[param.1]),
               block.2    = unname(block.of[param.2]),
               same.block = block.1 == block.2) %>%
        filter(n.cities.strong > 0) %>%
        arrange(desc(n.cities.strong), desc(abs(median.r.within))) %>%
        select(param.1, param.2, same.block, n.cities.strong, n.cities,
               median.r.within, min.r.within, max.r.within, cities.strong, block.1, block.2)
}


# ****************************************************************************************************
# RUN
# ****************************************************************************************************

# 1. One city first, then set LOCATIONS <- SHIELD.TEN.MSAS and re-run
pairs <- run.correlation.check(LOCATIONS, CALIBRATION.CODE)

# 2. The main result: pairs that trade off in the most cities
head(pairs, 30)

# 3. Any strong pairs that are split across blocks? (the sampler cannot learn these)
pairs %>% filter(!same.block)

# 4. Save
write.csv(pairs,
          file.path(REPORT.DIR, paste0("correlated_pairs_", CALIBRATION.CODE, "_",
                                       format(Sys.time(), "%Y%m%d_%H%M"), ".csv")),
          row.names = FALSE)
