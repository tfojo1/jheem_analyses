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
for (x in SHIELD.TEN.MSAS) {print(get.calibration.progress("shield",x,"calib.7.16.stage2.az"))}
# for (x in SHIELD.TEN.MSAS) {print(get.calibration.progress("shield",x,"calib.7.1.stage1.az"))}

calibration.codes <- c(
    # "calib.6.29.stage0.az",
    # "calib.7.2.stage1.az",
    # "calib.7.14.stage2.az",
    "calib.7.16.stage3.az"
)

# for (x in SHIELD.TEN.MSAS) {print(get.calibration.progress("shield",x,"calib.6.16.stage2.az"))}

# Read simulations into calib.simset ----
calib.simsets <- load.calib.simsets(
    locations         =  SHIELD.TEN.MSAS,
    calibration.codes = calibration.codes,
    n.sim = 300
)

# Inspect mixing statistics -----
inspect_mixing (
    calib.simsets = calib.simsets,
    calibration.codes = "calib.7.16.stage3.az",
        # calibration.codes,
    locations = SHIELD.TEN.MSAS,
    show.mixing = T,
    verbose = T
)

head(calib.simsets$`Baltimore – calib.7.16.stage3.az`$full_simset$get.mcmc.mixing.statistic())
head(calib.simsets$`Baltimore – calib.7.16.stage3.az`$full_simset$get.mcmc.mixing.statistic())
calib.simsets$`Seattle – calib.7.16.stage3.az`$full_simset$traceplot("tri")

x# ****************************************************************************************************
# Save summary plots for a calibration version (compares the fit accross all cities)
# ****************************************************************************************************
# 1-compare accross various locations ----
save_summary_plots_by_outcome<-function(calibration.code,folder.name){
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes =c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                      "hiv.testing","sti.screening", "prop.male.ps.diag.among.msm"),
                          years = c(1970:2030),
                          ncol=5
    )
    #
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening" ),
                          years = c(1970:2030),
                          split.by ="sex",
                          ncol = 5
    )
    #
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening" ),
                          years = c(1970:2030),
                          split.by = "race" ,
                          ncol = 5
    )
    
    # Sim-only by sex (to see MSM)
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.ps","hiv.testing","sti.screening"),
                          years = c(1970:2030),
                          split.by = "sex" ,
                          ncol = 5,
                          plot.which = "sim.only"
    )
    # Two way break down by sex and race
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.ps" ,"diagnosis.el.misclassified"),
                          years = c(1970:2030),
                          split.by = "race" , facet.by="sex",
                          ncol = 2
    )
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.ps", "diagnosis.el.misclassified"),
                          years = c(1970:2030),
                          split.by = "race" , facet.by="sex",
                          ncol = 2, #2 cities in each row (6 columns)
                          plot.which = "sim.only"   
    )
}

# 2-compare various outcomes in a single location ----
save_summary_plots_by_location<-function(calibration.code,folder.name){
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "location",
                          folder.name = folder.name,
                          outcomes =c("diagnosis.total", "diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                      "hiv.testing","sti.screening", "prop.male.ps.diag.among.msm"),
                          years = c(1970:2030),
                          style.manager = create.style.manager(shade.increment= -50)
                          
    )
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.code,
                          sim.subset = "full",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "location",
                          folder.name = folder.name,
                          outcomes =c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                      "hiv.testing","sti.screening"),
                          split.by = "sex",
                          years = c(1970:2030),
                          style.manager = create.style.manager(shade.increment= -50)
    )
}

save_summary_plots_by_outcome(calibration.code = "calib.7.16.stage3.az",folder.name = "calib.7.16.stage3.summary")
save_summary_plots_by_location(calibration.code = "calib.7.16.stage3.az",folder.name = "calib.7.16.stage3.summary")

get.default.style.manager()$shade.increment

# **********************************************************************************************************************************************************
# 3-compare_calibrations_by_outcome ----
compare_calibrations_by_outcome<-function(calibration.codes,folder.name){
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.codes,
                          sim.subset = "last20",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening", "prop.male.ps.diag.among.msm"),
                          years = c(1970:2030),
                          ncol=5
    )
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.codes,
                          sim.subset = "last20",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening"),
                          years = c(1970:2030),
                          ncol=5,
                          split.by = "sex"  
    )
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.codes,
                          sim.subset = "last20",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening"),
                          years = c(1970:2030),
                          ncol=5,
                          split.by = "race"   
    )
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.codes,
                          sim.subset = "last20",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening"),
                          years = c(1970:2030),
                          facet.by = "sex" , split.by="race",
                          ncol = 2
    )
    plot.calib.comparison(calib.simsets = calib.simsets,
                          calibration.codes = calibration.codes,
                          sim.subset = "last20",
                          locations = SHIELD.TEN.MSAS,
                          separate.by = "outcome",
                          folder.name = folder.name,
                          outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified",
                                       "hiv.testing","sti.screening"),
                          years = c(1970:2030),
                          facet.by = "sex" , split.by="race",
                          ncol = 2,
                          plot.which = "sim.only"
    )
}

calibration.codes = c("calib.7.16.stage2.az","calib.7.14.stage2.az")
folder.name = "calib.7.16.vs.14.stage2"
compare_calibrations_by_outcome(calibration.codes,folder.name)

# **********************************************************************************************************************************************************
# 4- compute prp Male diag by MSM by race -----

sapply(calib.simsets,function(simCalib){
    # simCalib=calib.simsets$`Atlanta – calib.7.14.stage2.az`
    round(apply(apply(simCalib$full_simset$diagnosis.ps["2020",,,,,], c( "race", "sex"), sum), c( "race"), 
                function(x) {x[["msm"]]/sum(x[c("msm", "heterosexual_male")])}), 3)
    
})

cbind(unlist(calib.simsets$`Atlanta – calib.7.14.stage2.az`$last_sim$get.params()),unlist(calib.simsets$`Atlanta – calib.7.16.stage2.az`$last_sim$get.params()))

calib.simsets$`Atlanta – calib.7.16.stage0.az`$full_simset$traceplot("oe.")
calib.simsets$`Atlanta – calib.7.16.stage1.az`$full_simset$traceplot("oe.")
calib.simsets$`Atlanta – calib.7.16.stage2.az`$full_simset$traceplot("oe.")

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


# ****************************************************************************************************
# . TESTUNG ENGINE
# ****************************************************************************************************
if (1==2){
    LOCATION="C.12060"
    VERSION="shield"
    # lastSimB=calib.simsets$`Baltimore – calib.7.10.stage1.pk`$last_sim
    # lastSimP=calib.simsets$`Philadelphia – calib.7.10.stage1.pk`$last_sim
    lastSimA=calib.simsets$`Atlanta – calib.7.10.stage1.pk`$last_sim
    
    param_calib=lastSimA$get.params()
    param.manual=param_calib
    #
    engine= create.jheem.engine(VERSION, LOCATION, end.year = 2030)
    sim.manual <- engine$run(param.manual)
    #
    simplot( lastSimA,
             sim.manual,         
             # "sti.screening"
             c( "diagnosis.ps"
                # "prop.male.ps.diag.among.msm"
                #    "diagnosis.total.via.symptomatic.testing",
                #    "diagnosis.total.via.sti.screening",
                #    "diagnosis.total.via.contact.tracing",
                #    "diagnosis.total.via.prenatal"
             ),
             split.by = "sex",facet.by="race",
             plot.which = "sim.only"
             
             # # c( "diagnosis.ps",
             # #     "diagnosis.ps.among.male" )
    )
    {
        param.manual=param_calib
        # param.manual["transmission.rate.multiplier.black.msm"]=.97*param.manual["transmission.rate.multiplier.black.msm"]
        # 
        # param.manual["screening.rate.multiplier.heterosexuals.1990"]=2*param.manual["screening.rate.multiplier.heterosexuals.1990"]
        # param.manual["screening.rate.multiplier.msm.1990"]=2*param.manual["screening.rate.multiplier.msm.1990"]
        # 
        # param.manual["screening.rate.multiplier.heterosexuals.2000"]=2*param.manual["screening.rate.multiplier.heterosexuals.2000"]
        # param.manual["screening.rate.multiplier.msm.2000"]=2*param.manual["screening.rate.multiplier.msm.2000"]
        #########
        param.manual["transmission.rate.multiplier.heterosexual1990"]=1.2*param.manual["transmission.rate.multiplier.heterosexual1990"]
        param.manual["transmission.rate.multiplier.msm1990"]=1.2*param.manual["transmission.rate.multiplier.msm1990"]
        
        # param.manual["transmission.rate.multiplier.heterosexual2000"]=param.manual["transmission.rate.multiplier.heterosexual1990"]
        # param.manual["transmission.rate.multiplier.msm2000"]=param.manual["transmission.rate.multiplier.msm1990"]
        # #
        # param.manual["transmission.rate.multiplier.heterosexual2010"]=.9*param.manual["transmission.rate.multiplier.heterosexual2010"]
        # param.manual["transmission.rate.multiplier.msm2010"]=.9*param.manual["transmission.rate.multiplier.msm2010"]
        
        sim.manual <- engine$run(param.manual)
        simplot( sim.manual,lastSim,
                 c(  
                     # "incidence",
                     "diagnosis.ps",
                     "diagnosis.el.misclassified",
                     "diagnosis.late.misclassified",
                     "sti.screening"
                     # "prop.male.ps.diag.among.msm"
                 )
                 # split.by = "sex",
                 # facet.by="race",
                 # plot.which = "sim.only"
        )
    }
    #
    
    
    
    lik1=instantiate.likelihood(lik.inst.stage1,version = 'shield',location = "C.12580")
    
    lik1$compare(sim.manual,lastSim)
    lik1$compute(sim.manual)
    
    # lik23=instantiate.likelihood(lik.inst.stage23,version = 'shield',location = "C.12580")
    #lik23$compute(sim.manual)
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
    
}