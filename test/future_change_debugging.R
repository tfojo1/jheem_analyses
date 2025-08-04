source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( color.data.by = "source",shade.data.by =  "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")
sim.style.manager  = create.style.manager(color.data.by = "simulation")

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.33100' # Miami MSA


{sim.last.MIA.19      <-  extract.last.simulation.from.calibration(calibration.code = "calib.MIA.07.30" ,version = VERSION, location = LOCATION, )
    params.last.MIA.19   <- sim.last.MIA.19$last.sim()$params}


engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
{
    params.manual <- params.last.MIA.19
    params.manual["transmission.rate.future.change.mult"] = 1
    
    params.manual['transmission.rate.multiplier.msm2020'] = 0.0001
    params.manual['transmission.rate.multiplier.heterosexual2020'] = 0.001
    
    #params.manual['or.symptomatic.2020'] = 2
    #params.manual['sti.screening.multiplier.2020'] = 0
    
    
    sim.manual <- engine$run(params.manual)
    #
    simplot(
        sim.manual,
        #sim.last.MIA.19,
        outcomes = c("diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified")
        # dimension.values = list(year = 1970:2030),
        #style.manager = source.style.manager
    )
}

q = engine$extract.quantity.values()
y = sapply(q$transmission.rate.msm, mean)
qplot(as.numeric(names(y)), 
      y)