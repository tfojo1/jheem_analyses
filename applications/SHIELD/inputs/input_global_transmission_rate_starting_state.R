# Script to find global transmission rate optimal start value
# 3-18-2026

# For each city, make an engine.
# For each candidate value, run the engine in each city
# with just the global transmission rate changed
# Then assess, somehow. Maybe do sim$get diagnosis.ps totals
# in 2020 and compare to data.

# engine_list <- lapply(MSAS.OF.INTEREST, function(msa) {
#     create.jheem.engine("shield", msa, 2030)
# })
# 
# og_engine_list <- engine_list

my_cities <- c("C.12580", "C.12060", "C.35620")
engine_list <- setNames(lapply(my_cities, function(msa) {
    create.jheem.engine("shield", msa, 2030)
}), my_cities)
original_params <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)

# # prior is lognormal with meanlog log(2.2) and sdlog log(10)/2,
# # meaning 95% interval varies by a factor of 100 (10x or 0.1x)
# # We end up with 11 options including the mean value.
# prior_meanlog <- log(2.2)
# prior_sdlog <- log(10)/2
# candidate_values <- exp(seq(
#     prior_meanlog - 2 * prior_sdlog,
#     prior_meanlog + 2 * prior_sdlog,
#     4 * prior_sdlog / 10
# ))

candidate_values <- seq(1.8, 2.6, 0.1)

# Now run each candidate for each city.
sims_by_candidates <- lapply(candidate_values, function(candidate_value) {
    print(paste0("Running engines with candidate value: ", candidate_value))
    sims <- setNames(lapply(my_cities, function(msa) {
        print(paste0("Running engine for location: ", msa))
        params <- original_params
        params["global.transmission.rate"] <- candidate_value
        trCatch(
            {engine_list[[msa]]$run(params)},
            error=function(e) {NULL}
        )
    }), my_cities)
})

# Then check to see if they have any diagnosis.ps in 2020.
# Or, find benchmark value and see if it reaches 20% at least,
# AND that it doesn't go over 5x?
benchmark_values <- setNames(sapply(my_cities, function(msa) {
    SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location["2020",msa]
}), my_cities)

simulation_values <- lapply(sims_by_candidates, function(sims_this_candidate) {
    setNames(sapply(my_cities, function(msa) {
        sims_this_candidate[[msa]]$get("diagnosis.ps",
                                       keep.dimensions = NULL,
                                       dimension.values = list(year=2020),
                                       drop.single.sim.dimension = T)
    }), my_cities)
})

candidate_success_rate <- sapply(simulation_values, function(values_this_candidate) {
    mean(
        values_this_candidate / benchmark_values > 0.2 &
            values_this_candidate / benchmark_values < 20)
})
