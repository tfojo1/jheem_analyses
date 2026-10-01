# Sensitivity analysis step 1: Calculate PRCC values

library(epiR)
library(tidyverse)

# SETTINGS ----
CALIB.CODE <- "calib.8.21.stage3.az"
locations <- setNames(SHIELD.TEN.MSAS[c(1:8, 10)], SHIELD.TEN.MSAS[c(1:8, 10)])

BASE.PATH <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE)

# Calculate PRCC ----
# Generate a data frame of n.sim rows
# and n.params cols + 1 for the outcome

calib.simsets <- load.calib.simsets(SHIELD.TEN.MSAS, "calib.8.21.stage3.az", 400)

total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))
sex_calc_results <- get(load(paste0(BASE.PATH, "sex_calc_results.Rdata")))

# prcc_df <- epi.prcc(df, sided.test=2, conf.level=0.95)

# For outcome 1 ----
print(paste0("Calculating PRCC for outcome 1 at ", Sys.time()))
prccs_outcome1 <- lapply(locations, function(city) {
    simset <- extract.calib.simsets(calib.simsets, city, CALIB.CODE, exact = T)[[1]]$full_simset

    # n.sim x 174
    all_params <- as.data.frame(t(simset$parameters))
    colnames(all_params) <- simset$parameter.names
    # browser()
    df <- cbind(all_params, data.frame(outcome = total_calc_results["2030", , "doxy.cov.30", city, "pct_incidence_averted"]))
    epi.prcc(df, sided.test = 2, conf.level = 0.95)
})

# For outcome 2 ----
print(paste0("Calculating PRCC for outcome 2 at ", Sys.time()))
prccs_outcome2 <- lapply(locations, function(city) {
    simset <- extract.calib.simsets(calib.simsets, city, CALIB.CODE, exact = T)[[1]]$full_simset

    # n.sim x 174
    all_params <- as.data.frame(t(simset$parameters))
    colnames(all_params) <- simset$parameter.names
    # browser()
    df <- cbind(all_params, data.frame(
        outcome =
            sex_calc_results["2030", "female", , "doxy.cov.30", city, "pct_incidence_averted"] /
                sex_calc_results["2030", "msm", , "doxy.cov.30", city, "pct_incidence_averted"]
    ))
    epi.prcc(df, sided.test = 2, conf.level = 0.95)
})

# Save ----
save(prccs_outcome1,
    file = paste0(BASE.PATH, "prccs_outcome1.Rdata")
)
save(prccs_outcome2,
    file = paste0(BASE.PATH, "prccs_outcome2.Rdata")
)
