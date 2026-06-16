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
# for (x in SHIELD.TEN.MSAS) {print(get.calibration.progress("shield",x,"calib.6.12.stage3.az"))}


calibration.codes <- c(
    "calib.6.12.stage2.az", 
    "calib.6.12.stg2.penalty"
)
 
# read simulations into the simset
calib.simsets <- load.calib.simsets(
    locations         = SHIELD.TEN.MSAS,
    calibration.codes = calibration.codes,
    n.sim = 300
)

# CREATE ALL STAGE CALIBRATION PLOTS
plot.calib.stages(calib.simsets = calib.simsets,
                  calibration.code =calibration.codes[2],
                  stage = 2,
                  locations = SHIELD.TEN.MSAS)

# SINGLE CALIB SINGLE LOCATION
plot.single.calib.single.location(calib.simsets = calib.simsets,
                    calibration.code = calibration.codes[2],
                    location ="C.12580",
                    outcomes = "hiv.testing",facet.by = "age",split.by = "sex")

simplot(calib.simsets$`Baltimore – calib.6.12.stg2.penalty`$last_sim,
        outcomes = "hiv.testing",facet.by = "age",split.by = "sex")

# COMPARING CALIBRATION 
plot.calib.comparison(calib.simsets = calib.simsets,
                      calibration.codes = c("calib.6.12.stage2.az","calib.6.12.stg2.penalty"),
                      sim.subset = "last20",
                      locations = SHIELD.TEN.MSAS,
                      separate.by = "outcome",
                      outcomes = c("diagnosis.ps","hiv.testing"),
                      # ncol = 1
                      facet.by = "sex" ,
                      # plot.which = "sim.only",
                      ncol = 2
                      )
plot.calib.comparison(calib.simsets = calib.simsets,
                      calibration.codes = c("calib.6.12.stage2.az","calib.6.12.stg2.penalty"),
                      sim.subset = "last20",
                      locations = SHIELD.TEN.MSAS,
                      separate.by = "outcome",
                      outcomes =  c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
                                    "diagnosis.late.misclassified", "hiv.testing")
                      # ncol = 1
                      # facet.by = "sex" ,
                      # plot.which = "sim.only",ncol = 2
)


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
