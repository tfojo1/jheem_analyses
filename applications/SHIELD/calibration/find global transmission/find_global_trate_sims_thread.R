#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
LOC_INDICES <- as.numeric(unlist(strsplit(args[1], "-")))

# Expecting to be launched from SHIELD/inputs folder?
setwd("../../../../jheem_analyses")

print("Sourcing SHIELD specification...")
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
print("Finished sourcing SHIELD specification")


# Use subset of all MSAs for this thread
my_cities <- MSAS.OF.INTEREST[LOC_INDICES[1]:LOC_INDICES[2]]

# Create list of engines
engine_list <- setNames(lapply(my_cities, function(msa) {
    create.jheem.engine("shield", msa, 2030)
}), my_cities)

original_params <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)

candidate_values <- seq(1.8, 2.6, 0.1)

# Now run each candidate for each city and save the value
find_sims_by_candidates <- function(candidate_values, cities, filename, filepath = "../jheem_analyses/applications/SHIELD/inputs/temp/") {
    sims_by_candidates <- lapply(candidate_values, function(candidate_value) {
        print(paste0("Running engines with candidate value: ", candidate_value))
        sims <- setNames(lapply(my_cities, function(msa) {
            print(paste0("Running engine for location: ", msa))
            params <- original_params
            params["global.transmission.rate"] <- candidate_value
            tryCatch(
                {engine_list[[msa]]$run(params)},
                error=function(e) {NULL}
            )
        }), my_cities)
    })
    print(paste0(filepath, filename))
    print(getwd())
    save(sims_by_candidates, file = paste0(filepath, filename))
}

find_sims_by_candidates(candidate_values, my_cities, paste0("msas_", args[1], ".Rdata"))