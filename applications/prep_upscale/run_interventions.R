source("../jheem2/R/ONTOLOGY_ontology.R")
source("../jheem2/R/tests/ENGINE_test.R")

simset <- join.simulation.sets(sim, sim, sim)

prep_intervention <- function(simset=simset, simulated_int, int_name = NULL){
  
  # null intervention -----
  
  sim_noint <- get.null.intervention()$run(simset, start.year = 2025, end.year = 2035)
  
  totalinc_noint <- sim_noint$get(outcomes = "incidence", 
                                  dimension.values = list(year = "2035"))
  
  raceinc_noint <- sim_noint$get(outcomes = "incidence",
                                 keep.dimensions = "race",
                                 dimension.values = list(year = "2035"))
  
  pop_noint <- sim_noint$get(outcomes = "population",
                             keep.dimensions = "race",
                             dimension.values = list(year = "2035")) 
  
  totalinc_int <- simulated_int$get(outcomes = "incidence", 
                                          dimension.values = list(year = "2035"))
  
  raceinc_int <- simulated_int$get(outcomes = "incidence",
                                         keep.dimensions = "race",
                                         dimension.values = list(year = "2035"))
  
  print(paste("Intervention:", int_name))
  # print("Total Incidence Difference:")
  # print((totalinc_int - totalinc_noint)/totalinc_noint) # incidence difference
  
  print("Incidence Difference by Race:")
  print(rowMeans((raceinc_int - raceinc_noint)/raceinc_noint))

  incrate_noint <- raceinc_noint/pop_noint # incidence rate
  
  # IRR
  irr_noint <- incrate_noint[c("black","hispanic"),] / incrate_noint["other",]
  
  print("Incidence Rate Ratios: (race versus other)")
  # print row means and CI
  print(rowMeans(irr_noint))
  print("Confidence Intervals for IRR:")
  print(apply(irr_noint, 1, quantile, probs=c(0.025,0.975))) # ci
  
  
}

# intervention simulation

sim_upscale10 <- prepuse_upscale10$run(simset, 
                                  start.year = 2025,
                                  end.year = 2035)

sim_upscale25 <- prepuse_upscale25$run(simset, 
                                       start.year = 2025,
                                       end.year = 2035)



sim_pers_30 <- preppers_upscale30$run(simset, 
                                       start.year = 2025,
                                       end.year = 2035)


sim_pers_55 <- preppers_upscale55$run(simset, 
                                      start.year = 2025,
                                      end.year = 2035)

prep_intervention(simset, sim_upscale10, int_name = "PrEP Use: Upscale by 10%")
prep_intervention(simset, sim_upscale25, int_name = "PrEP Use: Upscale by 25%")
prep_intervention(simset, sim_pers_30, int_name = "PrEP Persistence: Upscale by 30%")
prep_intervention(simset, sim_pers_30, int_name = "PrEP Persistence: Upscale by 55%")
