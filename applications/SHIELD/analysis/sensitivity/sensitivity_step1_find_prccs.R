# Sensitivity analysis step 1: Calculate PRCC values

library(epiR)
library(tidyverse)

# SETTINGS ----
CALIB.CODE <- "calib.10.20.stage3"
locations <- setNames(SHIELD.TEN.MSAS, SHIELD.TEN.MSAS)

BASE.PATH <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")

# Calculate PRCC ----
# Generate a data frame of n.sim rows
# and n.params cols + 1 for the outcome

calib.simsets <- load.calib.simsets(SHIELD.TEN.MSAS, CALIB.CODE, 800)

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
    df <- cbind(all_params, data.frame(outcome = total_calc_results["2030", , "doxy.cov.20", city, "pct_change_incidence_vs_year0"]))
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
            sex_calc_results["2030", "female", , "doxy.cov.20", city, "pct_change_incidence_vs_noint"] /
                sex_calc_results["2030", "msm", , "doxy.cov.20", city, "pct_change_incidence_vs_noint"]
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
# 
# prccs_outcome1 <- get(load(file = paste0(BASE.PATH, "prccs_outcome1.Rdata")))
# prccs_outcome2 <- get(load(file = paste0(BASE.PATH, "prccs_outcome2.Rdata")))

## TO FIND MOST SIGNIFICANT PER CITY
p_ranked_1 <- lapply(prccs_outcome1, function(df) {
    head(arrange(df, p.value), n=10) %>%
        select(var, est, p.value)
})


# # Gets the ones that are most frequently in cities' top 15
# sort(table(unlist(sapply(p_ranked_1, function(x) {unique(x$var)}))), decreasing = T)[1:10]

## TO FIND MOST SIGNIFICANT PER CITY
p_ranked_2 <- lapply(prccs_outcome2, function(df) {
    head(arrange(df, p.value), n=15) %>%
        select(var, est, p.value)
})

# # Gets the ones that are most frequently in cities' top 15
# sort(table(unlist(sapply(p_ranked_2, function(x) {unique(x$var)}))), decreasing = T)[1:10]


unique(c(unlist(lapply(p_ranked_1, function(x) {x$var})),
         unlist(lapply(p_ranked_2, function(x) {x$var}))))

# save(p_ranked_1,
#      file = paste0(BASE.PATH, "p_ranked_1.Rdata")
# )
# save(p_ranked_2,
#      file = paste0(BASE.PATH, "p_ranked_2.Rdata")
# )

params_ranked <- list(
    "1" = p_ranked_1,
    "2" = p_ranked_2
)
save(params_ranked,
     file = paste0(BASE.PATH, "params_ranked.Rdata"))
