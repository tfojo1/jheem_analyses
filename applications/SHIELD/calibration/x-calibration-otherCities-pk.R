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
# for(x in names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")] ){
    for(x in MSAS.OF.INTEREST  ){
        
    # print(MSAS.OF.INTEREST[MSAS.OF.INTEREST==x])
    status<-get.calibration.progress('shield', locations = x, calibration.code = "calib.4.13.stage1.pk")
    print(status)
}
#names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")]
#    

# Reads completed runs
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")],calibration.codes = "calib.4.8.stage0.az",n.sim = 300,sim.id = 0)
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("B","M","A","H","C","L","N")],calibration.codes = "calib.4.8.stage1.az",n.sim = 300,sim.id = 1)
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("B","M","A","H","C","L","N")],calibration.codes = "calib.4.8.stage2.az",n.sim = 300,sim.id = 2)

#before change
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("B","M","A","H","C","L","N")],calibration.codes = "calib.4.3.stage1.pk",n.sim = 300,sim.id=10)
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("B","M","A","H","C","L","N")],calibration.codes = "calib.4.3.stage2.pk",n.sim = 300,sim.id=20)

# for ( x in c("B","M","A","H","C","L","N")){
#     location= names(msa_var_names)[msa_var_names %in% x]
#     assign(
#         paste0("simset",x,0),
#         retrieve.simulation.set('shield',location = location,calibration.code = "calib.4.3.stage0.pk",n.sim = 300))
#     paste("read ",x," ")
# }


simplot(
    # simsetB1$last.sim(),  simsetB2$last.sim(),
    # simsetH1$last.sim(),  simsetH2$last.sim(),
    # simsetA1$last.sim(),  simsetA2$last.sim(),
    # simsetL1$last.sim(),  simsetL2$last.sim(),
    simsetP0$last.sim(),
    # outcomes=c("immigration")# facet.by = "age",split.by = "race"
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
        