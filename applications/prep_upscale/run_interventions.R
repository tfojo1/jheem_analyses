simset <- join.simulation.sets(sim, sim, sim)

sim_upscale10 <- prepuse_upscale10$run(sim, start.year = 2025, end.year = 2035)

sim_noint <- get.null.intervention()$run(simset, start.year = 2025, end.year = 2035)

totalinc_noint <- sim_noint$get(outcomes = "incidence", 
                                dimension.values = list(year = "2035"))

totalinc_upscale10 <- sim_upscale10$get(outcomes = "incidence", 
                                        dimension.values = list(year = "2035"))

(totalinc_upscale10 - totalinc_noint)/totalinc_noint


raceinc_noint <- sim_noint$get(outcomes = "incidence", keep.dimensions = "race",
                                dimension.values = list(year = "2035"))

raceinc_upscale10 <- sim_upscale10$get(outcomes = "incidence", keep.dimensions = "race",
                                        dimension.values = list(year = "2035"))

pop_noint <- sim_noint$get(outcomes = "population", keep.dimensions = "race",
                           dimension.values = list(year = "2035")) 

# IR
incrate_noint <- raceinc_noint/pop_noint

irr_noint <- incrate_noint[c("black","hispanic"),] / incrate_noint["other",]

rowMeans(irr_noint)
apply(irr_noint, 1, quantile, probs=c(0.025,0.975)) # ci

