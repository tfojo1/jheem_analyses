source('../jheem_analyses/applications/EHE/ehe_specification.R')

# TODD RUN FROM HERE
load('../jheem_analyses/applications/EHE/calibration_runs/pop.debugging.Rdata')
# loads these objects: 
    # params.okay.pop,
    # sim.okay.pop,
    # params.bad.pop,
    # sim.bad.pop,
    # params.error.trigger

simplot(sim.okay.pop,
        sim.bad.pop,
        facet.by = "age", split.by = "race",
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 


# this is how to trigger the engine error - only happens with old versions of the params
engine = create.jheem.engine('ehe',sim.okay.pop$location,end.year = 2030) #to fix: comment out lines 1723-1737 of engine 
sim.trigger.error = engine$run(parameters = params.error.trigger) 




# Melissa ran this to start - don't need to run again
if(1==2){
  # load previous transmission mcmc with okay population
  load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-13_C.12580.Rdata") 
  sim.okay.pop = sim
  params.okay.pop = sim.okay.pop$parameters[,1]
  
  # load new transmission mcmc with messed up population
  load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-15_C.12580.Rdata")
  sim.bad.pop = sim
  params.bad.pop = sim.bad.pop$parameters[,1]
  
  # load old sim to trigger the error
  load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-02_C.12580.Rdata") 
  params.error.trigger = sim$parameters[,1]
  
  save(params.okay.pop,
       sim.okay.pop,
       params.bad.pop,
       sim.bad.pop,
       params.error.trigger,
       file = "applications/EHE/calibration_runs/pop.debugging.Rdata")  
}