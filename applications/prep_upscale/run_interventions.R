source("../jheem2/R/ONTOLOGY_ontology.R")
source("../jheem2/R/tests/ENGINE_test.R")

simset <- join.simulation.sets(sim, sim, sim)


prep_intervention_general <- function(simset=simset, simulated_int, int_name = NULL){
  
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
  
  if(!is.null(int_name)){
    print(paste("Intervention:", int_name))
  }
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

prep_intervention_msm <- function(simset=simset, simulated_int, int_name = NULL){
  
  sim_noint <- get.null.intervention()$run(simset, start.year = 2025, end.year = 2035)
  
  totalinc_noint <- sim_noint$get(outcomes = "incidence", 
                                  dimension.values = list(year = "2035", sex = "msm"))
  
  raceinc_noint <- sim_noint$get(outcomes = "incidence",
                                 keep.dimensions = "race",
                                 dimension.values = list(year = "2035", sex = "msm"))
  
  pop_noint <- sim_noint$get(outcomes = "population",
                             keep.dimensions = "race",
                             dimension.values = list(year = "2035", sex = "msm")) 
  
  totalinc_int <- simulated_int$get(outcomes = "incidence", 
                                          dimension.values = list(year = "2035", sex = "msm"))
  
  raceinc_int <- simulated_int$get(outcomes = "incidence",
                                         keep.dimensions = "race",
                                         dimension.values = list(year = "2035", sex = "msm"))
  
  if(!is.null(int_name)){
    print(paste("Intervention:", int_name))
  }
  # print("Total Incidence Difference:")
  # print((totalinc_int - totalinc_noint)/totalinc_noint) # incidence difference
  
  print("Incidence Difference among MSM by Race:")
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

# intervention simulation -----

## run general ------

sim_upscale10 <- prep.use10$run(simset, 
                                  start.year = 2025,
                                  end.year = 2035)

sim_upscale25 <- prep.use25$run(simset, 
                                       start.year = 2025,
                                       end.year = 2035)

sim_pers_30 <- prep.pers30$run(simset, 
                                       start.year = 2025,
                                       end.year = 2035)


sim_pers_55 <- prep.pers55$run(simset, 
                                      start.year = 2025,
                                      end.year = 2035)

sim_pers_80 <- prep.pers80$run(simset, 
                                     start.year = 2025,
                                     end.year = 2035) 

## results general ------
prep_intervention_general(simset, sim_upscale10, int_name = "PrEP Use: Upscale by 10%")
prep_intervention_general(simset, sim_upscale25, int_name = "PrEP Use: Upscale by 25%")
prep_intervention_general(simset, sim_pers_30, int_name = "PrEP Persistence: Upscale by 30%")
prep_intervention_general(simset, sim_pers_55, int_name = "PrEP Persistence: Upscale by 55%")
prep_intervention_general(simset, sim_pers_80, int_name = "PrEP Persistence: Upscale by 80%")

## run MSM ------

sim_upscale10_msm <- prep.use10.msm$run(simset, 
                                start.year = 2025,
                                end.year = 2035)

sim_upscale25_msm <- prep.use25.msm$run(simset, 
                                start.year = 2025,
                                end.year = 2035)

sim_pers_30_msm <- prep.pers30.msm$run(simset, 
                               start.year = 2025,
                               end.year = 2035)

sim_pers_55_msm <- prep.pers55.msm$run(simset, 
                               start.year = 2025,
                               end.year = 2035)

sim_pers_80_msm <- prep.pers80.msm$run(simset, 
                               start.year = 2025,
                               end.year = 2035) 

## results MSM ------
prep_intervention_msm(simset, sim_upscale10_msm, int_name = "PrEP Use: Upscale by 10%")
prep_intervention_msm(simset, sim_upscale25_msm, int_name = "PrEP Use: Upscale by 25%")
prep_intervention_msm(simset, sim_pers_30_msm, int_name = "PrEP Persistence: Upscale by 30%")
prep_intervention_msm(simset, sim_pers_55_msm, int_name = "PrEP Persistence: Upscale by 55%")
prep_intervention_msm(simset, sim_pers_80_msm, int_name = "PrEP Persistence: Upscale by 80%")

