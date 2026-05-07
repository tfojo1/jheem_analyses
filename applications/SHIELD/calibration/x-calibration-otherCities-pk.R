# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/calibration/shield_calibration_inspection_helpers.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# READ STATUS of ongoing runs
for(x in SHIELD.TEN.MSAS  ){
    status<-get.calibration.progress('shield', locations = x, calibration.code = "calib.5.5.stage1.pk") #new stage1+30% msm.female.partnership
    print(status)
}
# 
# # Reads completed runs
# assign_simset_vars(SHIELD.TEN.MSAS[10],calibration.codes = "calib.5.4.stage1.az",n.sim = 300,sim.id = 1)
# assign_simset_vars(SHIELD.TEN.MSAS[10],calibration.codes = "calib.5.5.stage1.pk",n.sim = 300,sim.id = 2)
# 
# <<<<<<< HEAD
# simsetX2<-assemble.simulations.from.calibration(
#     version = 'shield',
#     location = SHIELD.TEN.MSAS[9],
#     calibration.code = "calib.5.5.stage1.pk",
#     allow.incomplete = TRUE
# )
x<-simsetX2$last.sim()$get.params()
x
# *******************************************************************************
simplot(
    simsetP1$last.sim(),simsetP2$last.sim(),
# >>>>>>> 26b643e2421d6d91677cf1a69bd0c1ec7c53d9b4
    outcomes=c("population","diagnosis.ps")
    # outcomes = c("diagnosis.primary.symptomatic","diagnosis.secondary.symptomatic","diagnosis.primary.asymptomatic","diagnosis.secondary.asymptomatic")
    # outcomes = c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified","diagnosis.late.misclassified","hiv.testing")
    # ,split.by="sex",plot.which="sim.only"
    # ,split.by="race"
    # 
    # outcomes = c("diagnosis.ps"),
    # outcomes = c("diagnosis.el.misclassified")
    # facet.by = "age"
    # ,split.by="race"
    # ,split.by="sex"
    # dimension.values = list(year=c(1970:2025)),
)    
create_multipanel_location_comparison()

lik1=lik.inst.stage1$instantiate.likelihood(VERSION,LOCATION)
lik1$compare.sims( simsetL1$subset(142),simsetL1$subset(145) )
simsetL1$subset(c(140:145))$traceplot("trans")
simset
### looking into LA:
LOCATION= "C.31080"
VERSION='shield'

simsetL0$traceplot("trans")
simsetL1$traceplot("prp.symptomatic.")
lastL0=simsetL0$last.sim(); lastL0$get.params()
p

assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("A")],calibration.codes = "calib.4.8.stage2.az",n.sim = 300,sim.id = 2)
lik1=lik.inst.stage2$instantiate.likelihood("shield","C.12060")
lik2=lik.inst.stage2.wFC$instantiate.likelihood("shield","C.12060")
lik1$compute(simsetN2$subset(296))
lik2$compute(simsetN2$subset(296))

assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("A")],calibration.codes = "calib.4.8.stage2.az",n.sim = 300)
simplot(simsetA$subset(280:300),
        outcomes ="diagnosis.ps")

make_total_plot("diagnosis.total", 
                simsetA2$subset(280:300), lastA2, 
                style.manager = source.style.manager)
