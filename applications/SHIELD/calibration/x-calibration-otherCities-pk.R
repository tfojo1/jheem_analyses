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
for(x in names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")] ){
    print(MSAS.OF.INTEREST[MSAS.OF.INTEREST==x])
    status<-get.calibration.progress('shield', locations = x, calibration.code = "calib.3.30.stage2.pk")
    print(status)
}
# (msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")]
#    

# # Reads complete runs simSet0 Andrew
# assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")],
#                    calibration.codes = "calib.3.30.stage1.pk",sim.id = 9)

# STAGE0
for ( x in c("A","H","C","L")){
    location= names(msa_var_names)[msa_var_names %in% x]
    assign(
        paste0("simset",x,0),
        retrieve.simulation.set('shield',location = location,calibration.code = "calib.3.30.stage0.pk",n.sim = 300))
    paste("read ",x," ")
}

# STAGE1
for ( x in c("A","H","C","L")){  
    location= names(msa_var_names)[msa_var_names %in% x]
    assign(
        paste0("simset",x,1),
        retrieve.simulation.set('shield',location = location,calibration.code = "calib.3.30.stage1.pk",n.sim = 300))
    paste("read ",x," ")
}

# Simset2 PK
for ( x in c("A","H","C","L")){ 
    location= names(msa_var_names)[msa_var_names %in% x]
    assign(
        paste0("simset",x,2),
        retrieve.simulation.set('shield',location = location,calibration.code = "calib.3.30.stage2.pk",n.sim = 300))
    paste("read ",x," ")
}

# #
# simM2<-extract.last.simulation.from.calibration('shield', location = "C.33100", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
 

simplot(
    # simsetL0$last.sim(), #sim1$first.sim(),
    # simsetL1$last.sim(), simsetL1$first.sim(),
    simsetL1$subset(142),
    # simsetL1$subset(144),
    simsetL1$subset(145),
    # simsetL2$last.sim(), #sim2$first.sim(),
    # outcomes = c("diagnosis.primary.symptomatic","diagnosis.secondary.symptomatic","diagnosis.primary.asymptomatic","diagnosis.secondary.asymptomatic")
    outcomes = c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified","diagnosis.late.misclassified","hiv.testing")
    # ,split.by="sex"
     ,split.by="race"
    # ,plot.which="sim.only"
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
