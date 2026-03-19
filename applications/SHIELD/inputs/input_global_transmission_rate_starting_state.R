# Script to find global transmission rate optimal start value
# 3-18-2026

##### TO USE ####
# 1. open a terminal in the SHIELD/inputs directory
# 2. launch "\.launch_global_trate_multithreaded.bat"
# 3. run this script when that's finished

#### #####

# For each city, make an engine.
# For each candidate value, run the engine in each city
# with just the global transmission rate changed
# Then assess, somehow. Maybe do sim$get diagnosis.ps totals
# in 2020 and compare to data.

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

my_cities <- MSAS.OF.INTEREST

# Then check to see if they have any diagnosis.ps in 2020.
# Or, find benchmark value and see if it reaches 20% at least,
# AND that it doesn't go over 5x?
benchmark_values <- setNames(sapply(my_cities, function(msa) {
    SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location["2020",msa]
}), my_cities)

# Now consolidate threads
# thread_groupings <- c("1-11", "12-22", "23-33")
thread_groupings <- sapply(0:10, function(i) {paste(i * 3 + 1, i * 3 + 3, sep = "-")})
all_threads <- lapply(thread_groupings, function(grouping) {
    tryCatch({get(load(file = paste0("../jheem_analyses/applications/SHIELD/inputs/temp/msas_", grouping, ".Rdata")))},
             error = function(e) {NULL})
})

# Check for NULL
any(sapply(all_threads, length)==0)

sims_unthreaded <- lapply(seq_along(candidate_values), function(i) {
    unlist(lapply(all_threads, function(grouped_sims) {
        grouped_sims[i]
    }))
})

simulation_values <- lapply(sims_unthreaded, function(sims_this_candidate) {
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

candidate_values[candidate_success_rate==max(candidate_success_rate)]