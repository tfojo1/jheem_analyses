# ****************************************************************************************************
# 1993 SEED-RATE BENCHMARK (SHIELD MSAs + US) ----
# ****************************************************************************************************
# Why this exists:
#  1. The model seeds each city's 1970 infected population from that city's 1993 diagnosis count,
#     with a minimum rate (floor) chosen here (see get_diag_seed_rate_1970 in R/shield_specification_helpers.R)
#  2. Some cities (e.g. Seattle) had very low syphilis in 1993, so their 1970 seed is tiny
#  3. This script shows the same 1993 rates for the SHIELD MSAs and the US, to help choose a minimum rate
#
# What it does:
#  1. 1993 diagnoses by stage (P&S, early latent, late) - same source the model uses
#  2. 1993 population - same source the model uses (US from census.population)
#  3. Rates per 100k, sorted from lowest to highest, plus a short summary
#
# Run from the jheem_analyses root. READ-ONLY: does not change any file.
# ****************************************************************************************************
source("../jheem_analyses/commoncode/locations_of_interest.R")
source("../jheem_analyses/applications/SHIELD/shield_specification.R")

## ---- 1. SETTINGS ----
YEAR      <- "1993"   # keep as text: it is a row name, not a row number
LOCATIONS <- c(SHIELD.MSAS.OF.INTEREST, US = "US")

# stages used to seed 1970
STAGE.OUTCOMES <- c(ps   = "ps.syphilis.diagnoses",
                    el   = "early.syphilis.diagnoses",
                    late = "unknown.duration.or.late.syphilis.diagnoses")

## ---- 2. DIAGNOSES ----
get.diagnoses <- function(outcome) {
    x <- SURVEILLANCE.MANAGER$data[[outcome]]$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location
    x[YEAR, match(LOCATIONS, colnames(x))]   # NA if a location has no data
}
dx <- sapply(STAGE.OUTCOMES, get.diagnoses)

## ---- 3. POPULATION ----
msa.pop <- SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location
us.pop  <- SURVEILLANCE.MANAGER$data$population$estimate$census.population$census$year__location
pop     <- c(msa.pop[YEAR, match(SHIELD.MSAS.OF.INTEREST, colnames(msa.pop))],
             us.pop[YEAR, "US"])

## ---- 4. RATES PER 100K ----
tab <- data.frame(name     = names(LOCATIONS),
                  location = unname(LOCATIONS),
                  dx,
                  pop      = unname(pop),
                  row.names = NULL)
tab$ps.per100k         <- round(1e5 * tab$ps   / tab$pop, 2)
tab$el.per100k         <- round(1e5 * tab$el   / tab$pop, 2)
tab$late.per100k       <- round(1e5 * tab$late / tab$pop, 2)
tab$infectious.per100k <- tab$ps.per100k + tab$el.per100k   # stages that can transmit

# locations missing diagnoses or population
missing <- is.na(tab$infectious.per100k)
cat("No", YEAR, "rate (missing diagnoses or population):", paste(tab$name[missing], collapse = ", "), "\n")
tab <- tab[!missing, ]

# MSAs lowest to highest, US last
tab <- tab[order(tab$location == "US", tab$infectious.per100k), ]
cat("\n==== ", YEAR, " diagnosis rates per 100k ====\n", sep = "")
print(tab, row.names = FALSE)

## ---- 5. SUMMARY ACROSS MSAs ----
rates <- tab[tab$location != "US", c("ps.per100k", "el.per100k", "late.per100k")]
cat("\n==== Summary across", nrow(rates), "MSAs (per 100k) ====\n")
print(round(rbind(min    = sapply(rates, min),
                  p10    = sapply(rates, quantile, 0.10),
                  p25    = sapply(rates, quantile, 0.25),
                  median = sapply(rates, median),
                  US     = unlist(tab[tab$location == "US", names(rates)])), 2))
