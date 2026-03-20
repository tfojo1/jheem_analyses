#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
LOC_INDICES <- as.numeric(unlist(strsplit(args[1], "-")))

# Expecting to be launched from SHIELD/inputs folder?
setwd("../../../../jheem_analyses")

print("Sourcing SHIELD specification...")
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
source('../jheem_analyses/commoncode/locations_of_interest.R')
print("Finished sourcing SHIELD specification")


# Use subset of all MSAs for this thread
my_cities <- MSAS.OF.INTEREST[LOC_INDICES[1]:LOC_INDICES[2]]

# Create list of engines
engine_list <- setNames(lapply(my_cities, function(msa) {
    create.jheem.engine("shield", msa, 2030)
}), my_cities)

# In this version of the script, we will utilize the final sims of some stage 0
# calibrations to give us our demographic parameters. For the rest, we will use
# the medians, and of course the candidates for the global transmission rates.
cities_using_3_17 <- c(
    "C.12060",
    "C.12420",
    "C.12940",
    "C.14460",
    "C.16740",
    "C.17140",
    "C.17460",
    "C.26900",
    "C.27260",
    "C.35620",
    "C.40900",
    "C.41700",
    "C.42660",
    "C.45300"
)
cities_using_median <- c(
    "C.19100"
)

# Load city last simsets
last_sims_params <- setNames(lapply(my_cities, function(city) {
    
    # no need to retrieve a simset we won't use
    if (city %in% cities_using_median) return(NULL)
    
    calibration_to_use <-
        if (city %in% cities_using_3_17) "calib.3.17.stage0.az" else "calib.3.10.stage0.az"
    simset <- retrieve.simulation.set(
        version = "shield",
        location = city,
        calibration.code = calibration_to_use,
        n.sim = 400
    )
    
    simset$last.sim()$params
    
}), my_cities)

original_params <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
population_parameter_names <- POPULATION.PARAMETERS.PRIOR@var.names

candidate_values <- seq(1.8, 2.6, 0.1)

# Now run each candidate for each city and save the value
find_sims_by_candidates <- function(candidate_values, cities, filename, filepath = "../jheem_analyses/applications/SHIELD/inputs/temp/") {
    
    sims_by_candidates <- lapply(candidate_values, function(candidate_value) {
        
        print(paste0("Running engines with candidate value: ", candidate_value))
        sims <- setNames(lapply(my_cities, function(city) {
            
            print(paste0("Running engine for location: ", city))
            
            # We will substitute in the demographic parameters
            params <- original_params
            
            if (!is.null(last_sims_params[[city]]))
                params[population_parameter_names] <- last_sims_params[[city]][population_parameter_names]
            
            params["global.transmission.rate"] <- candidate_value
            
            tryCatch(
                {engine_list[[city]]$run(params)},
                error=function(e) {NULL}
            )
        }), my_cities)
    })
    
    print(paste0(filepath, filename))
    save(sims_by_candidates, file = paste0(filepath, filename))
}

find_sims_by_candidates(candidate_values, my_cities, paste0("msas_calib_", args[1], ".Rdata"))