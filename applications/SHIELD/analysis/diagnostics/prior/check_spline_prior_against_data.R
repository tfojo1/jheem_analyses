# ****************************************************************************************************
# CHECK THE MSM/HET TRANSMISSION SPLINE PRIOR AGAINST DATA ----
# ****************************************************************************************************
# The prior transmission.rate.multipliers.by.sex (shield_calib_parameters.R) makes three assumptions
# about how transmission changes between spline years:
#   A. the expected change is 0 (no change expected), for MSM and heterosexuals alike
#   B. the spread of the change (SD, log scale) grows in proportion to the number of years
#   C. MSM and heterosexual changes are correlated (0.7)
# This script measures the same three things in P&S diagnoses by sex across MSAs, so you can compare.
# Data only: no simsets needed.
#
# Data: CDC surveillance reports, P&S diagnoses by sex, all MSAs, 1993-2019 (year x MSA x sex).
#   "male" here is all men; it stands in for MSM after 2000, when most male P&S is among MSM.
#
# Three things to keep in mind when reading the numbers:
#   1. these are diagnoses, not transmission rates; diagnoses change more (an epidemic amplifies a
#      small change in transmission), so the data values are upper bounds for the prior
#   2. "male" includes heterosexual men, which makes men and women look more alike than MSM and het are
#   3. small counts are noisy: a city with 25 cases could have had 20 or 30 by chance. We remove that
#      noise from the spreads (see step 2).
#
# USAGE
#   from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/prior/check_spline_prior_against_data.R")
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
MIN.CASES <- 20                                 # use a city only if both sexes have >= 20 cases in both years
YEARS     <- as.character(1993:2019)            # years with P&S by sex in the CDC reports
PERIODS   <- list(c(1993, 1995), c(1995, 2000), c(2000, 2005), c(2005, 2010), c(2000, 2010), c(2010, 2019))
GAPS      <- c(1, 2, 3, 5, 7, 10, 15)           # gap lengths (years) for table 2
TRATE_DELTA.NOW <- 1.5                          # current value in shield_calib_parameters.R
CORRELATION.NOW <- 0.7

## 1. GET THE DATA ----
# P&S diagnoses by sex from the CDC reports: an array of year x location x sex
reports <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location__sex
msas    <- grep("^C\\.", dimnames(reports)$location, value = TRUE)       # MSA codes start with "C."
male    <- reports[YEARS, msas, "male"]                                   # years x MSAs
female  <- reports[YEARS, msas, "female"]

## 2. CHANGE BETWEEN TWO YEARS ----
# For every MSA with enough cases: the log change for men and for women between two years.
#   log change = log(end / start):  0 = no change, +0.69 = doubled, -0.69 = halved
#   men.minus.women = how much men moved relative to women (0 = moved together)
# noise = how much the log change could vary just from small counts: about 1/start + 1/end
change.between <- function(start, end) {
    start <- as.character(start); end <- as.character(end)
    m0 <- male[start, ];   m1 <- male[end, ]
    f0 <- female[start, ]; f1 <- female[end, ]
    keep <- m0 >= MIN.CASES & m1 >= MIN.CASES & f0 >= MIN.CASES & f1 >= MIN.CASES
    keep[is.na(keep)] <- FALSE
    data.frame(msa         = msas[keep],
               start       = start,
               men         = log(m1[keep] / m0[keep]),
               women       = log(f1[keep] / f0[keep]),
               noise.men   = 1 / m0[keep] + 1 / m1[keep],
               noise.women = 1 / f0[keep] + 1 / f1[keep])
}

## sd.without.noise ----
# spread across MSAs, after taking out the part that small counts alone would produce
sd.without.noise <- function(values, noise) sqrt(max(var(values) - mean(noise), 0))

## 3. TABLE 1: AVERAGE CHANGE BY PERIOD (compare with assumption A: expected change = 0) ----
# If men and women both change by the same average amount, a shared trend explains it.
# If men.minus.women is far from 0 in every city, men moved away from women everywhere at once,
# which the prior does not expect.
table1 <- t(sapply(PERIODS, function(p) {
    d <- change.between(p[1], p[2])
    c(cities          = nrow(d),
      men             = mean(d$men),
      women           = mean(d$women),
      men.minus.women = mean(d$men - d$women),
      fold            = exp(mean(d$men - d$women)))                     # e.g. 3 = men rose 3x relative to women
}))
rownames(table1) <- sapply(PERIODS, paste, collapse = "-")
cat("\n== TABLE 1: average log change across MSAs (assumption A expects 0 in every column) ==\n")
print(round(table1, 2))
# Table 1 shows the average change in PS diagnoses across MSAs, for men and for women, in each period. Since it's on the log scale, e^value gives the multiplier: 
# −0.48 means diagnoses fell to 0.62 of their earlier level (−38%), and +0.89 means they rose 2.4x.

# The 1990s decline was shared. Men and women fell by the same amount (men − women ≈ 0). That was the end of the late-1980s/early-1990s epidemic, 
# which affected both sexes, so the men then were mostly heterosexual. A prior that expects no difference between MSM and heterosexual change fits this period.
#
# The divergence was a single event, in 2000–2005. In those five years men rose about 2.4x while women fell further. That's the MSM resurgence of the early 2000s,
# and it happened in every city with data, not just a few. After 2005, men and women moved nearly together again.
#
# This is the one step where "expect no change" is wrong. The prior expects MSM and heterosexual transmission to change by the same amount in every step. 
# That holds for every period except 2000–2010, where the data shows a shared ×4 shift. It's the evidence for revision #3 (an expected shift in that step) or #1 + #2 (more room).
# The recent rise, which matters most for projections, was shared. In 2010–2019 women rose almost as fast as men (×1.65 against ×1.8). 
# So the model doesn't need MSM to pull further ahead after 2010. It needs both groups rising, including heterosexuals. 
# That fits the earlier sign that the model's heterosexual men are too low recently.

## 4. TABLE 2: SPREAD BY GAP LENGTH (compare with assumption B: SD grows in proportion to years) ----
# For each gap (1, 2, 3 ... years), take every start year from 1993 on, compute the change for every MSA,
# and measure how spread out the changes are. We first subtract each start year's national average,
# so the spread is about cities differing from each other, not about the national trend (that is table 1).
# Then: does the spread grow in proportion to the gap (prior now), or with its square root (a random walk)?
spread.for.gap <- function(gap) {
    starts <- YEARS[as.numeric(YEARS) + gap <= max(as.numeric(YEARS))]
    d <- do.call(rbind, lapply(starts, function(s) change.between(s, as.numeric(s) + gap)))
    men   <- d$men   - ave(d$men,   d$start)                             # minus that start year's national average
    women <- d$women - ave(d$women, d$start)
    diff  <- men - women
    c(changes         = nrow(d),
      men             = sd.without.noise(men,   d$noise.men),
      women           = sd.without.noise(women, d$noise.women),
      men.minus.women = sd.without.noise(diff,  d$noise.men + d$noise.women),
      # correlation of men's and women's change; noise does not affect the covariance, only the variances
      correlation     = cov(men, women) / (sd.without.noise(men, d$noise.men) * sd.without.noise(women, d$noise.women)))
}
table2 <- sapply(GAPS, spread.for.gap)
colnames(table2) <- paste0(GAPS, "y")

# the current prior, for comparison: per-group SD = 0.5 * log(TRATE_DELTA) * years / 10
# (10-year step: 95% range x/÷ TRATE_DELTA; MSM-het SD = per-group SD * sqrt(2 * (1 - correlation)))
prior.now  <- 0.5 * log(TRATE_DELTA.NOW) * GAPS / 10
prior.sqrt <- 0.5 * log(TRATE_DELTA.NOW) * sqrt(GAPS / 10)              # same 10-year value, square-root growth
table2 <- rbind(table2,
                prior.per.group.now        = prior.now,
                prior.per.group.sqrt.years = prior.sqrt,
                prior.msm.minus.het.now    = prior.now * sqrt(2 * (1 - CORRELATION.NOW)))
cat("\n== TABLE 2: spread (SD) of log change across MSAs, by gap length, noise removed ==\n")
print(round(table2, 3))

# how fast does each spread grow with the gap?  SD ~ years^power
#   power 1 = in proportion to years (the prior now); 0.5 = square root (random walk); 0 = does not grow
growth.power <- function(sds) coef(lm(log(sds[sds > 0]) ~ log(GAPS[sds > 0])))[[2]]
cat("\nSD grows as years^power:\n")
print(round(c(men             = growth.power(table2["men", ]),
              women           = growth.power(table2["women", ]),
              men.minus.women = growth.power(table2["men.minus.women", ]),
              prior.now       = 1), 2))

## 5. TABLE 3: CORRELATION (compare with assumption C: 0.7) ----
# the "correlation" row of table 2: how closely men's and women's changes go together across cities,
# after removing the national average. 1 = always together, 0 = unrelated.
cat("\n== TABLE 3: correlation of men's and women's change across MSAs, by gap (prior: ",
    CORRELATION.NOW, ") ==\n", sep = "")
print(round(table2["correlation", ], 2))
