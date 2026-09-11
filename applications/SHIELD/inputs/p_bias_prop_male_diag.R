# Script to calculate p-bias
# Andrew Z., August 14th, 2026

# For each city, we need to know its overlapping states.
# For each of those combinations, we need to pull five things:
# The MSM population for city and state
# The male population for city and state
# and the proportion male diagnoses among MSM in the state.
# Then we'll calculate a proportion for the city and find the difference.
# With a sample of these, we'll calculate mean and standard deviation.

# Years for which we have population data AND prop male ps diag among msm data

cities <- SHIELD.MSAS.OF.INTEREST

total_sample <- lapply(cities, function(city) {
    
    states <- get.overlapping.locations(city, "state")

    msm_pop_city <- SURVEILLANCE.MANAGER$pull(
        outcome = "estimated.count.msm",
        keep.dimensions = "year",
        dimension.values = list(location = city),
        sources = "emory.aggregated",
        from.ontology.names = "emory"
    )
    
    male_pop_city <- SURVEILLANCE.MANAGER$pull(
        outcome = "population",
        keep.dimensions = "year",
        dimension.values = list(location = city),
        sources = "census.aggregated.population",
        from.ontology.names = c("census", "stratified.census")
    )
    
    state_result <- lapply(states, function(state) {
        msm_pop_state <- SURVEILLANCE.MANAGER$pull(
            outcome = "estimated.count.msm",
            keep.dimensions = "year",
            dimension.values = list(location = state),
            sources = "emory.aggregated",
            from.ontology.names = "emory"
        )
        male_pop_state <- SURVEILLANCE.MANAGER$pull(
            outcome = "population",
            keep.dimensions = "year",
            dimension.values = list(location = state),
            sources = "census.aggregated.population",
            from.ontology.names = c("census", "stratified.census")
        )
        
        prop_male_diag_among_msm_state <- SURVEILLANCE.MANAGER$pull(
            outcome = "prop.male.ps.diag.among.msm",
            keep.dimensions = "year",
            dimension.values = list(location = state),
            source = "cdc.sti.surveillance.reports",
            from.ontology.names = "cdc.pdf.report"
        )
        
        years_all_data_available <- y <-
            intersect(intersect(intersect(intersect(dimnames(msm_pop_city)$year,
                                    dimnames(male_pop_city)$year),
                              dimnames(msm_pop_state)$year),
                        dimnames(male_pop_state)$year),
                  dimnames(prop_male_diag_among_msm_state)$year)
        
        prop_male_who_msm_city <- msm_pop_city[y,] / male_pop_city[y,]
        prop_male_who_msm_state <- msm_pop_state[y,] / male_pop_state[y,]
        
        # The grand calculation!
        prop_male_diag_among_msm_city <-
            1 /
            (1 +
                 ((1 - prop_male_who_msm_city) / prop_male_who_msm_city) *
                 (prop_male_who_msm_state / (1 - prop_male_who_msm_state)) *
                 (1 / prop_male_diag_among_msm_state[y,] - 1))
        
        # Find difference between and state proportion and this
        p_bias <- prop_male_diag_among_msm_state[y,] - prop_male_diag_among_msm_city
        p_bias[!is.na(p_bias)]
    })
})

p_bias_mean <- mean(unlist(total_sample, recursive = T)) # 0.0743
p_bias_sd <- sd(unlist(total_sample, recursive = T)) # 0.0572

prop_male_diag_among_msm_bias_estimates <- list(
    in.mean = NA,
    out.mean = p_bias_mean,
    in.sd = NA,
    out.sd = p_bias_sd,
    n.in = NA,
    n.out = length(unlist(total_sample, recursive = T))
)

if (1==2)
    cache.object.for.version(object = prop_male_diag_among_msm_bias_estimates, 
                             name = "prop_male_diag_among_msm_bias_estimates", 
                             version = 'shield', overwrite=T)  
