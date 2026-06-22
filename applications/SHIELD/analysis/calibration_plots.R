# ****************************************************************************************************
# SHIELD CALIBRATION COMPARISON — SAMPLE CODE
# ****************************************************************************************************
# Demonstrates how to compare calib.5.11.stage2.az vs calib.5.19.stage2.pk
# across all 10 cities using plot.calib.comparison() and plot.calib.location()
# ****************************************************************************************************
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
source('../jheem_analyses/applications/SHIELD/analysis/analysis_helper_functions.R')

# ---- SETUP ----

for (x in SHIELD.TEN.MSAS) {print(get.calibration.progress("shield",x,"calib.6.29.stage0.az"))}
 

calibration.codes <- c(
    "calib.6.29.stage0.az"
    )

# for (x in SHIELD.TEN.MSAS) {print(get.calibration.progress("shield",x,"calib.6.16.stage2.az"))}
 
calibration.codes <- c(
    "calib.6.9.stage2.az" ,#prior run after making a lot of revisions
    "calib.6.12.stage2.az", # prior trate_2000 for msm set to log(1)
    #     # "calib.6.12.stg2.penalty"
    
         "calib.6.16.stage2.az", #with Covid reductions for sti screening
    #      # "calib.6.16.stg2.penalty"
    
        "calib.6.19.stage2.az" #changed screening to spline
    
    
)
 

# read simulations into the simset
calib.simsets <- load.calib.simsets(
    locations         = SHIELD.TEN.MSAS,
    calibration.codes = calibration.codes,
    n.sim = 300
)
# Inspect mixing statistics -----
inspect_mixing (
    calib.simsets = calib.simsets,
    calibration.codes =calibration.codes,
    locations = SHIELD.TEN.MSAS,
    show.mixing = T,
    verbose = F
    )
    

LOCATION="C.12580"
VERSION="shield"
lastSim=calib.simsets$`Baltimore-Columbia-Towson, MD – calib.6.25.stage2.az`$last_sim
param_calib=lastSim$get.params()
param.manual=param_calib

engine= create.jheem.engine(VERSION, LOCATION, end.year = 2030)
sim.manual <- engine$run(param.manual)

simplot( sim.manual,
         c( "diagnosis.total",
            "diagnosis.total.via.symptomatic.testing",
            "diagnosis.total.via.sti.screening",
         "diagnosis.total.via.contact.tracing",
         "diagnosis.total.via.prenatal")
        # c( "diagnosis.ps",
        #     "diagnosis.ps.among.male" )
)
apply(sim.manual$diagnosis.ps,1,sum)
sim.manual$diagnosis.ps.among.male

apply(sim.manual$diagnosis.total,1,sum)==
    apply(sim.manual$diagnosis.total,1,sum)+
    apply(sim.manual$diagnosis.via.symptomatic.testing,1,sum)+
    apply(sim.manual$diagnosis.total.via.prenatal,1,sum)+
    apply(sim.manual$diagnosis.total.via.contact.tracing,1,sum)

{
    param.manual=param_calib
    param.manual["transmission.rate.multiplier.heterosexual2022"]=1.01*param.manual["transmission.rate.multiplier.heterosexual2022"]
    param.manual["transmission.rate.multiplier.msm2022"]=1.2*param.manual["transmission.rate.multiplier.msm2022"]
    param.manual["transmission.rate.multiplier.hispanic.msm"]=1*param.manual["transmission.rate.multiplier.hispanic.msm"]
    param.manual["transmission.rate.multiplier.hispanic.heterosexual"]=1*param.manual["transmission.rate.multiplier.hispanic.heterosexual"]
    
    sim.manual <- engine1$run(param.manual)
    
    simplot(lastSim,sim.manual,c("diagnosis.ps","hiv.testing"))
    simplot(lastSim,sim.manual,c("diagnosis.ps"),facet.by = "sex")
    simplot(lastSim,sim.manual,c("diagnosis.ps"),facet.by = "race")
    simplot(lastSim,sim.manual,c("diagnosis.ps"),facet.by = "age")
}




plot.calib.comparison(calib.simsets = calib.simsets,
                      calibration.codes = c("calib.6.16.stage2.az"),
                      sim.subset = "last20",
                      locations = SHIELD.TEN.MSAS,
                      separate.by = "outcome",
                      folder.name = "calib.6.16.stage2.summary",
                      outcomes = c("diagnosis.ps","hiv.testing","diagnosis.total"),
                      years = c(1970:2030),
                      # ncol=5
                      facet.by = "sex" ,
                      plot.which = "sim.only",
                      ncol = 2
)

 
# simplot(calib.simsets$`Seattle – calib.6.12.stage2.az`$last20_sims$diagnosis.ps, outcomes = "diagnosis.ps",facet.by = "sex")
# x=calib.simsets$`Seattle – calib.6.12.stage2.az`$last20_sims$diagnosis.ps
# x[,,1,1,,1]

plot.calib.comparison(calib.simsets = calib.simsets,
                      calibration.codes = c("calib.6.19.stage2.az","calib.6.16.stage2.az"),
                      sim.subset = "last20",
                      locations = SHIELD.TEN.MSAS,
                      separate.by = "outcome",
                      folder.name = "calib.6.19.stage.vs.6.16",
                      outcomes = c("diagnosis.ps","hiv.testing"),
                      years = c(1970:2030),
                      # ncol=5
                      # facet.by = "sex" ,
                      # plot.which = "sim.only",
                      ncol = 2
)
# CREATE ALL STAGE CALIBRATION PLOTS
plot.calib.stages(calib.simsets = calib.simsets,
                  calibration.code =calibration.codes,
                  stage = 0,
                  locations = SHIELD.TEN.MSAS)

# # SINGLE CALIB SINGLE LOCATION
# plot.single.calib.single.location(calib.simsets = calib.simsets,
#                     calibration.code = calibration.codes[2],
#                     location ="C.12580",
#                     outcomes = "hiv.testing",facet.by = "age",split.by = "sex")
# 
# simplot(calib.simsets$`Baltimore – calib.6.12.stg2.penalty`$last_sim,
#         outcomes = "hiv.testing",facet.by = "age",split.by = "sex")

# COMPARING CALIBRATION 
plot.calib.comparison(calib.simsets = calib.simsets,
                      calibration.codes = c("calib.6.12.stage2.az","calib.6.16.stage2.az"),
                      sim.subset = "last20",
                      locations = SHIELD.TEN.MSAS,
                      separate.by = "outcome",
                      folder.name = "calib.6.12.stage2.vs.6.16.stage2",
                      outcomes = c("diagnosis.ps","hiv.testing","diagnosis.total"),
                      # ncol = 1
                      # facet.by = "sex" ,
                      # plot.which = "sim.only",
                      # ncol = 2
)
# plot.calib.comparison(calib.simsets = calib.simsets,
#                       calibration.codes = c("calib.6.12.stage2.az","calib.6.12.stg2.penalty"),
#                       sim.subset = "last20",
#                       locations = SHIELD.TEN.MSAS,
#                       separate.by = "outcome",
#                       outcomes =  c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
#                                     "diagnosis.late.misclassified", "hiv.testing")
#                       # ncol = 1
#                       # facet.by = "sex" ,
#                       # plot.which = "sim.only",ncol = 2
# )


# head(calib.simsets$`Atlanta – calib.7.12.stage0.test`$full_simset$get.mcmc.mixing.statistic())
# simplot(calib.simsets$`Atlanta – calib.7.12.stage0.test`$full_simset,"diagnosis.ps")
# simplot(calib.simsets$`Atlanta – calib.7.12.stage0.test`$last_sim,"diagnosis.ps")
# 
# # head(calib.simsets$`Baltimore – calib.7.12.stage0.test`$full_simset$get.mcmc.mixing.statistic())
# # simplot(calib.simsets$`Baltimore – calib.7.12.stage0.test`$full_simset,"diagnosis.ps")
# 
# engine=create.jheem.engine('shield',"C.12060",end.year = 2030)
# lik=instantiate.likelihood(lik.inst.stage1,version = 'shield',location = "C.12060")
# lik$compute(calib.simsets$`Atlanta – calib.7.12.stage0.test`$last_sim)






lapply(calib.simsets,function(x){
    head(x$full_simset$get.mcmc.mixing.statistic())
})

calib.simsets$`Chicago – calib.6.9.stage2.az`$full_simset$traceplot(c("transmission.*19","transmission.*20"))
calib.simsets$`Chicago – calib.6.9.stage2.az`$last_sim$get.params(c("transmission.*20"))
calib.simsets$`LA – calib.5.29.stage2.az`$last_sim$get.params(c("future"))

calib.simsets$`Baltimore – calib.6.9.stage2.az`$full_simset$traceplot(c("screening", "slope"))
calib.simsets$`Baltimore – calib.5.29.stage2.az`$last_sim$get.params(c("transmission.*20"))
calib.simsets$`LA – calib.5.29.stage2.az`$last_sim$get.params(c("future"))


calib.simsets$`NYC – calib.5.29.stage2.az`$full_simset$traceplot(c("transmission.*19","transmission.*20"))
calib.simsets$`NYC – calib.5.29.stage2.az`$full_simset$traceplot("future")
calib.simsets$`NYC – calib.5.29.stage2.az`$last_sim$get.params("transmission.*20")
calib.simsets$`NYC – calib.5.29.stage2.az`$last_sim$get.params("screening")
calib.simsets$`NYC – calib.5.29.stage2.az`$last_sim$get.params("future")


# Outcome sets used across examples below
outcomes.all <- c("diagnosis.total", "diagnosis.ps",
                  "diagnosis.el.misclassified", "diagnosis.late.misclassified","hiv.testing")

# ****************************************************************************************************
# 1. COMPARE BOTH CALIBRATIONS ACROSS ALL CITIES — SEPARATE BY OUTCOME ----
# ****************************************************************************************************

# Save all outcome plots to disk
p1=plot.calib.comparison(
    calib.simsets = calib.simsets,
    calibration.codes = calibration.codes,
    outcomes          = outcomes.all,
    
    separate.by       = "outcome",
    years = 2018:2030,
    style.manager     =create.style.manager(color.sim.by = "simset",alpha.line = .1 
    )
)

p1

calib.simsets$`Atlanta – calib.5.11.stage2.az`$full_simset$traceplot("transmission.rate.multiplier.het")
head(calib.simsets$`Atlanta – calib.6.29.stage0.az`$full_simset$get.mcmc.mixing.statistic())
