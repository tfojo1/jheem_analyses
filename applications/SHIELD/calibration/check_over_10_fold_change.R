# Function to take in simset and check the number of the last X sims that
# have a ratio of 2030 PS diagnoses to 2022 that is over 10.

# What you get from prepare_simsets_for_plots of the calibration plot code
# prepared_simset_list

find_prp_over_10 <- function(simset, last_n) {
    sim_subset <- simset$subset((1 + simset$n.sim - last_n) : simset$n.sim)
    vals <- sim_subset$get("diagnosis.ps", dimension.values = list(year=c(2020, 2030)), keep.dimensions = "year")
    ratio <- apply(vals, "sim", function(x) {x["2030"]/x["2020"]})
}

lapply(prepared_simset_list, function(x) {
    if (is.null(x)) return(NULL)
    y <- find_prp_over_10(x$full_simset, 10)
    mean(y > 10)
})